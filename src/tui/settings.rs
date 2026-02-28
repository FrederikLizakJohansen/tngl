use crossterm::event::{KeyCode, KeyEvent};
use ratatui::Frame;
use ratatui::layout::{Constraint, Flex, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, BorderType, Borders, Clear, Padding, Paragraph};

use crate::parser::config::{Config, OnDelete};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SettingsEvent {
    None,
    Changed,
    Close,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct SettingsPanelState {
    pub selected_row: usize,
}

const SETTINGS_ROW_COUNT: usize = 4;

pub fn handle_key(
    key: KeyEvent,
    state: &mut SettingsPanelState,
    config: &mut Config,
) -> SettingsEvent {
    match key.code {
        KeyCode::Esc | KeyCode::Backspace | KeyCode::Char('q') => SettingsEvent::Close,
        KeyCode::Up | KeyCode::Char('k') => {
            state.selected_row = state.selected_row.saturating_sub(1);
            SettingsEvent::None
        }
        KeyCode::Down | KeyCode::Char('j') => {
            state.selected_row = (state.selected_row + 1).min(SETTINGS_ROW_COUNT - 1);
            SettingsEvent::None
        }
        KeyCode::Left | KeyCode::Char('h') => adjust(config, state.selected_row),
        KeyCode::Right | KeyCode::Char('l') | KeyCode::Enter | KeyCode::Char(' ') => {
            adjust(config, state.selected_row)
        }
        _ => SettingsEvent::None,
    }
}

pub fn draw(frame: &mut Frame, state: &SettingsPanelState, config: &Config) {
    let area = centered_rect(frame.area(), 56, 44);
    frame.render_widget(Clear, area);

    let title = Line::from(vec![
        Span::styled(
            "Setup",
            Style::default()
                .fg(Color::Cyan)
                .add_modifier(Modifier::BOLD),
        ),
        Span::raw("  "),
        Span::styled("[Esc] close", Style::default().fg(Color::Gray)),
    ]);

    let selected_row = state.selected_row.min(SETTINGS_ROW_COUNT - 1);
    let mut lines = vec![
        settings_row(
            selected_row == 0,
            "reveal linked paths",
            config.auto_reveal_links,
        ),
        settings_row(selected_row == 1, "git hook updates", config.git_hooks),
        settings_row(
            selected_row == 2,
            "warn blank comments",
            config.warn_uncommented_edges,
        ),
        settings_row(
            selected_row == 3,
            "auto-tag new nodes",
            config.mark_new_as_orphans,
        ),
        Line::from(""),
        Line::from(Span::styled(
            "About this option",
            Style::default()
                .fg(Color::Gray)
                .add_modifier(Modifier::BOLD),
        )),
    ];
    for text in selected_row_description(selected_row) {
        lines.push(Line::from(Span::styled(
            text,
            Style::default().fg(Color::DarkGray),
        )));
    }
    lines.extend([
        Line::from(""),
        Line::from(Span::styled(
            "Use arrows/hjkl or Enter/Space to toggle.",
            Style::default().fg(Color::DarkGray),
        )),
        Line::from(Span::styled(
            "Changes write to config immediately.",
            Style::default().fg(Color::DarkGray),
        )),
    ]);

    let panel = Paragraph::new(lines).block(
        Block::default()
            .title(title)
            .borders(Borders::ALL)
            .border_type(BorderType::Rounded)
            .border_style(Style::default().fg(Color::Cyan))
            .padding(Padding::new(1, 1, 1, 0)),
    );
    frame.render_widget(panel, area);
}

pub fn serialize_config(config: &Config) -> String {
    let on_delete = match config.on_delete {
        OnDelete::Prompt => "prompt",
        OnDelete::Delete => "delete",
        OnDelete::Preserve => "preserve",
    };
    let editor = config.editor.clone().unwrap_or_default();
    format!(
        "\
# tngl configuration
# Edit manually or run: tngl setup

# What to do with edges when a file is deleted
# Options: prompt | delete | preserve
on_delete: {}

# Whether to show orphan nodes in the default TUI view
show_orphans: {}

# In view mode, reveal linked nodes inside collapsed folders while focused
auto_reveal_links: {}

# Auto-update graph.tngl on git operations (requires hook install)
git_hooks: {}

# Preferred editor for `tngl edit` (falls back to $VISUAL, then $EDITOR)
editor: {}

# Warn in `tngl status` when an edge has an empty label/comment
warn_uncommented_edges: {}

# On `tngl update`, tag new files and folders as [orphan]
mark_new_as_orphans: {}
",
        on_delete,
        config.show_orphans,
        config.auto_reveal_links,
        config.git_hooks,
        editor,
        config.warn_uncommented_edges,
        config.mark_new_as_orphans
    )
}

fn settings_row(selected: bool, key: &str, enabled: bool) -> Line<'static> {
    let indicator = if selected { ">" } else { " " };
    let base_style = if selected {
        Style::default()
            .fg(Color::White)
            .bg(Color::DarkGray)
            .add_modifier(Modifier::BOLD)
    } else {
        Style::default().fg(Color::Gray)
    };
    let value_text = if enabled { "[ON]" } else { "[OFF]" };
    let mut value_style = if enabled {
        Style::default().fg(Color::Green)
    } else {
        Style::default().fg(Color::LightRed)
    };
    if selected {
        value_style = value_style.bg(Color::DarkGray).add_modifier(Modifier::BOLD);
    } else {
        value_style = value_style.add_modifier(Modifier::BOLD);
    }

    Line::from(vec![
        Span::styled(format!("{indicator} {key:<24}"), base_style),
        Span::styled(value_text, value_style),
    ])
}

fn selected_row_description(selected_row: usize) -> [&'static str; 2] {
    match selected_row {
        0 => [
            "Temporarily opens collapsed folders that contain links",
            "to or from the currently focused node.",
        ],
        1 => [
            "If hooks are installed, Git operations can run",
            "`tngl update --silent` automatically.",
        ],
        2 => [
            "Shows status warnings when an edge has an empty",
            "label/comment so tangles stay documented.",
        ],
        3 => [
            "When update discovers new files/folders, mark them",
            "as [orphan] for later review.",
        ],
        _ => ["", ""],
    }
}

fn adjust(config: &mut Config, selected_row: usize) -> SettingsEvent {
    match selected_row {
        0 => {
            config.auto_reveal_links = !config.auto_reveal_links;
            SettingsEvent::Changed
        }
        1 => {
            config.git_hooks = !config.git_hooks;
            SettingsEvent::Changed
        }
        2 => {
            config.warn_uncommented_edges = !config.warn_uncommented_edges;
            SettingsEvent::Changed
        }
        3 => {
            config.mark_new_as_orphans = !config.mark_new_as_orphans;
            SettingsEvent::Changed
        }
        _ => SettingsEvent::None,
    }
}

fn centered_rect(area: Rect, width_percent: u16, height_percent: u16) -> Rect {
    let vertical = Layout::vertical([
        Constraint::Percentage((100 - height_percent) / 2),
        Constraint::Percentage(height_percent),
        Constraint::Percentage((100 - height_percent) / 2),
    ])
    .flex(Flex::Center)
    .split(area);
    Layout::horizontal([
        Constraint::Percentage((100 - width_percent) / 2),
        Constraint::Percentage(width_percent),
        Constraint::Percentage((100 - width_percent) / 2),
    ])
    .flex(Flex::Center)
    .split(vertical[1])[1]
}
