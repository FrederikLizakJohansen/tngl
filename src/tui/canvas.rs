use std::collections::{HashMap, HashSet};
use std::fs;
use std::io;
use std::path::PathBuf;
use std::time::Duration;

use anyhow::{Result, bail};
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind};
use crossterm::execute;
use crossterm::terminal::{
    EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode,
};
use ratatui::backend::CrosstermBackend;
use ratatui::layout::{Constraint, Flex, Layout, Margin, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, BorderType, Borders, Clear, Padding, Paragraph, Wrap};
use ratatui::{Frame, Terminal};

use crate::graph::model::{Edge, EdgeKind, Graph, Node};
use crate::parser::{config, graph};
use crate::tangle;
use crate::tui::input::{self, Action, Direction};
use crate::tui::render::{
    self, CanvasRenderData, ConnectPreview, ConnectionFilter, PanelFocus, RenderEdge, RenderNode,
    Viewport,
};
use crate::tui::settings::{self, SettingsEvent, SettingsPanelState};

const DEFAULT_ZOOM: f32 = 0.10;
const MIN_ZOOM: f32 = 0.03;
const MAX_ZOOM: f32 = 0.40;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    Normal,
    Connecting,
    Detaching,
    EditingLabel,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SelectionEdge {
    source_idx: usize,
    edge_idx: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ConnectDraft {
    source_idx: usize,
    kind: EdgeKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DetachEnd {
    Source,
    Target,
}

#[derive(Debug, Clone)]
struct DetachContext {
    source_idx: usize,
    edge_idx: usize,
    end: Option<DetachEnd>,
}

#[derive(Debug, Clone)]
enum PendingTextKind {
    ConnectLabel {
        source_idx: usize,
        target_idx: usize,
        kind: EdgeKind,
    },
    EditEdgeLabel {
        source_idx: usize,
        edge_idx: usize,
    },
    AddNode,
    EditNodePath {
        node_idx: usize,
    },
}

#[derive(Debug, Clone)]
struct PendingText {
    title: String,
    buffer: String,
    cursor: usize,
    kind: PendingTextKind,
}

#[derive(Debug, Clone)]
enum PendingConfirm {
    DeleteEdge {
        source_idx: usize,
        edge_idx: usize,
    },
    DeleteNode {
        node_idx: usize,
    },
    BundleFolder {
        folder: String,
        removed_child_links: usize,
    },
}

#[derive(Debug)]
struct AppState {
    graph_path: Option<PathBuf>,
    config_path: Option<PathBuf>,
    graph: Graph,
    positions: HashMap<String, (i32, i32)>,
    focused_node: usize,
    selected_edge: Option<SelectionEdge>,
    viewport_x: i32,
    viewport_y: i32,
    zoom: f32,
    tree_view_rows: usize,
    mode: Mode,
    panel_focus: PanelFocus,
    locked_node: Option<usize>,
    details_cursor: usize,
    show_help: bool,
    pan_mode: bool,
    show_settings: bool,
    settings_state: SettingsPanelState,
    config: config::Config,
    show_orphans: bool,
    status_message: Option<String>,
    pending_text: Option<PendingText>,
    pending_confirm: Option<PendingConfirm>,
    connecting_source: Option<usize>,
    connect_draft: Option<ConnectDraft>,
    detaching: Option<DetachContext>,
    connection_filter: ConnectionFilter,
    delete_pick_idx: Option<usize>,
    collapsed_folders: HashSet<String>,
    bundled_folders: HashSet<String>,
    demo: bool,
}

impl AppState {
    fn load(demo: bool, open_settings: bool) -> Result<Self> {
        if demo {
            let (graph, positions, config) = demo_graph();
            let mut app = Self {
                graph_path: None,
                config_path: None,
                graph,
                positions,
                focused_node: 0,
                selected_edge: None,
                viewport_x: 0,
                viewport_y: 0,
                zoom: DEFAULT_ZOOM,
                tree_view_rows: 0,
                mode: Mode::Normal,
                panel_focus: PanelFocus::Tree,
                locked_node: None,
                details_cursor: 0,
                show_help: false,
                pan_mode: false,
                show_settings: open_settings,
                settings_state: SettingsPanelState::default(),
                config,
                show_orphans: true,
                status_message: Some("demo mode: changes are in-memory only".to_string()),
                pending_text: None,
                pending_confirm: None,
                connecting_source: None,
                connect_draft: None,
                detaching: None,
                connection_filter: ConnectionFilter::All,
                delete_pick_idx: None,
                collapsed_folders: HashSet::new(),
                bundled_folders: HashSet::new(),
                demo: true,
            };
            app.show_orphans = app.config.show_orphans;
            app.ensure_all_node_positions();
            app.reset_viewport();
            app.focused_node = app.visible_navigation_order().first().copied().unwrap_or(0);
            app.ensure_focus_visible();
            return Ok(app);
        }

        let root = tangle::find_root()?;
        let graph_path = tangle::graph_path(&root);
        let config_path = tangle::config_path(&root);

        let graph_text = fs::read_to_string(&graph_path)?;
        let graph_doc = graph::parse(&graph_text)?;
        let bundled_folders = graph::collapsed_subtree_roots(&graph_doc);
        let graph_model = graph::to_graph(&graph_doc)?;

        let cfg = if config_path.exists() {
            let content = fs::read_to_string(&config_path)?;
            config::parse(&content)?
        } else {
            config::Config::default()
        };

        let mut app = Self {
            graph_path: Some(graph_path),
            config_path: Some(config_path),
            graph: graph_model,
            positions: HashMap::new(),
            focused_node: 0,
            selected_edge: None,
            viewport_x: 0,
            viewport_y: 0,
            zoom: DEFAULT_ZOOM,
            tree_view_rows: 0,
            mode: Mode::Normal,
            panel_focus: PanelFocus::Tree,
            locked_node: None,
            details_cursor: 0,
            show_help: false,
            pan_mode: false,
            show_settings: open_settings,
            settings_state: SettingsPanelState::default(),
            config: cfg,
            show_orphans: false,
            status_message: None,
            pending_text: None,
            pending_confirm: None,
            connecting_source: None,
            connect_draft: None,
            detaching: None,
            connection_filter: ConnectionFilter::All,
            delete_pick_idx: None,
            collapsed_folders: bundled_folders.clone(),
            bundled_folders,
            demo: false,
        };
        app.show_orphans = app.config.show_orphans;
        app.ensure_all_node_positions();
        app.reset_viewport();
        app.focused_node = app.visible_navigation_order().first().copied().unwrap_or(0);
        app.ensure_focus_visible();
        Ok(app)
    }

    fn draw(&mut self, frame: &mut Frame) {
        self.update_tree_view_rows(frame.area());
        self.update_tree_scroll_for_focus();
        let transient_open_folders = self.transient_open_folders_for_highlight();
        let visible = self.visible_node_mask_for_render(&transient_open_folders);
        let nodes = self
            .graph
            .nodes
            .iter()
            .enumerate()
            .filter_map(|(idx, node)| {
                if !visible[idx] {
                    return None;
                }
                Some(RenderNode {
                    path: node.path.clone(),
                    focused: idx == self.focused_node,
                })
            })
            .collect::<Vec<_>>();

        let selected = self.resolve_selected_edge();
        let selected_tuple = selected
            .as_ref()
            .map(|s| (s.source_idx, s.edge_idx))
            .unwrap_or((usize::MAX, usize::MAX));

        let mut edges = Vec::new();
        for (source_idx, node) in self.graph.nodes.iter().enumerate() {
            for (edge_idx, edge) in node.edges.iter().enumerate() {
                edges.push(RenderEdge {
                    source: node.path.clone(),
                    target: edge.target.clone(),
                    kind: edge.kind.clone(),
                    label: edge.label.clone(),
                    selected: selected_tuple == (source_idx, edge_idx),
                });
            }
        }

        let selected_node = self.graph.nodes.get(self.focused_node);
        let selected_path = selected_node.map(|n| n.path.as_str()).unwrap_or("—");
        let edge_count = selected_node
            .as_ref()
            .map(|_| self.tangles_for_anchor(self.focused_node).len())
            .unwrap_or(0);
        let locked_path = self
            .locked_node
            .and_then(|idx| self.graph.nodes.get(idx))
            .map(|n| n.path.as_str());
        let connect_preview = self.current_connect_preview();
        let hints = self.hints();
        let delete_mode = self.delete_pick_idx.is_some();
        let create_mode = self.is_create_mode();
        let mut collapsed_folders = self
            .collapsed_folders
            .iter()
            .filter(|folder| !transient_open_folders.contains(*folder))
            .cloned()
            .collect::<Vec<_>>();
        collapsed_folders.sort();
        let mut bundled_folders = self.bundled_folders.iter().cloned().collect::<Vec<_>>();
        bundled_folders.sort();

        let data = CanvasRenderData {
            nodes: &nodes,
            edges: &edges,
            viewport: Viewport {
                x: self.viewport_x,
                y: self.viewport_y,
                zoom: self.zoom,
            },
            selected_path,
            selected_edge_count: edge_count,
            mode_label: self.mode_label(),
            hints: &hints,
            message: self.status_message.as_deref(),
            show_help: self.show_help,
            panel_focus: self.panel_focus,
            locked_path,
            details_cursor: self.details_cursor,
            connect_preview,
            collapsed_folders: &collapsed_folders,
            bundled_folders: &bundled_folders,
            connection_filter: self.connection_filter,
            delete_mode,
            create_mode,
        };
        render::draw(frame, &data);

        if self.show_settings {
            settings::draw(frame, &self.settings_state, &self.config);
        }
        if let Some(prompt) = &self.pending_text {
            if matches!(prompt.kind, PendingTextKind::ConnectLabel { .. }) {
                self.draw_connect_comment_prompt(frame, prompt);
            } else {
                self.draw_text_prompt(frame, prompt);
            }
        } else if let Some(confirm) = &self.pending_confirm {
            self.draw_confirm_prompt(frame, confirm);
        }
    }

    fn draw_connect_comment_prompt(&self, frame: &mut Frame, prompt: &PendingText) {
        let PendingTextKind::ConnectLabel {
            source_idx,
            target_idx,
            ref kind,
        } = prompt.kind
        else {
            return;
        };
        let source = self
            .graph
            .nodes
            .get(source_idx)
            .map(|n| n.path.as_str())
            .unwrap_or("?");
        let target = self
            .graph
            .nodes
            .get(target_idx)
            .map(|n| n.path.as_str())
            .unwrap_or("?");
        let kind_label = match kind {
            EdgeKind::Directed => "out",
            EdgeKind::Incoming => "in",
            EdgeKind::Undirected => "ref",
        };
        let border_color = match kind {
            EdgeKind::Directed => Color::Cyan,
            EdgeKind::Incoming => Color::Magenta,
            EdgeKind::Undirected => Color::Green,
        };

        let area = centered_rect(frame.area(), 58, 30);
        frame.render_widget(Clear, area);
        let paragraph = Paragraph::new(vec![
            Line::from(Span::styled(
                "Comment (optional)",
                Style::default()
                    .fg(Color::White)
                    .add_modifier(Modifier::BOLD),
            )),
            Line::from(""),
            Line::from(Span::styled(
                format!("source: {}", source),
                Style::default().fg(Color::Cyan),
            )),
            Line::from(Span::styled(
                format!("target: {}", target),
                Style::default().fg(Color::Cyan),
            )),
            Line::from(Span::styled(
                format!("type  : {}", kind_label),
                Style::default().fg(Color::Cyan),
            )),
            Line::from(""),
            line_with_cursor(
                &prompt.buffer,
                prompt.cursor,
                "write a short note...",
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD),
                Style::default().fg(Color::DarkGray),
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD | Modifier::SLOW_BLINK),
            ),
            Line::from(""),
            Line::from(Span::styled(
                "[Backspace] delete  [Enter] save  [Esc] back",
                Style::default().fg(Color::DarkGray),
            )),
        ])
        .block(
            Block::default()
                .title(" add tangle ")
                .borders(Borders::ALL)
                .border_type(BorderType::Double)
                .border_style(Style::default().fg(border_color))
                .padding(Padding::new(2, 2, 1, 1)),
        )
        .wrap(Wrap { trim: false });
        frame.render_widget(paragraph, area);
    }

    fn draw_text_prompt(&self, frame: &mut Frame, prompt: &PendingText) {
        let area = centered_rect(frame.area(), 70, 28);
        frame.render_widget(Clear, area);
        let paragraph = Paragraph::new(vec![
            Line::from(Span::styled(
                &prompt.title,
                Style::default().add_modifier(Modifier::BOLD),
            )),
            Line::from(""),
            line_with_cursor(
                &prompt.buffer,
                prompt.cursor,
                "",
                Style::default().fg(Color::White),
                Style::default().fg(Color::DarkGray),
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD | Modifier::SLOW_BLINK),
            ),
            Line::from(""),
            Line::from(Span::styled(
                "Backspace deletes char. Enter applies, Esc cancels.",
                Style::default().fg(Color::DarkGray),
            )),
        ])
        .block(Block::default().title("Input").borders(Borders::ALL));
        frame.render_widget(paragraph, area);
    }

    fn draw_confirm_prompt(&self, frame: &mut Frame, confirm: &PendingConfirm) {
        let area = centered_rect(frame.area(), 56, 22);
        frame.render_widget(Clear, area);
        let text = match confirm {
            PendingConfirm::DeleteEdge { .. } => "Delete this edge?".to_string(),
            PendingConfirm::DeleteNode { .. } => "Delete node + edges?".to_string(),
            PendingConfirm::BundleFolder {
                removed_child_links,
                ..
            } => format!(
                "Bundle folder and remove {} child link{}?",
                removed_child_links,
                if *removed_child_links == 1 { "" } else { "s" }
            ),
        };
        let paragraph = Paragraph::new(vec![
            Line::from(vec![
                Span::styled("  ", Style::default()),
                Span::styled(text, Style::default().add_modifier(Modifier::BOLD)),
            ]),
            Line::from(""),
            Line::from(Span::styled(
                match confirm {
                    PendingConfirm::BundleFolder { .. } => {
                        "Child links inside this folder will be deleted."
                    }
                    _ => "",
                },
                Style::default().fg(Color::DarkGray),
            )),
            Line::from(""),
            Line::from(vec![
                Span::styled("  ", Style::default()),
                Span::styled(
                    "[y/Enter]",
                    Style::default()
                        .fg(Color::Green)
                        .add_modifier(Modifier::BOLD),
                ),
                Span::styled(" yes   ", Style::default().fg(Color::DarkGray)),
                Span::styled(
                    "[n/Esc/Backspace]",
                    Style::default().fg(Color::Red).add_modifier(Modifier::BOLD),
                ),
                Span::styled(" no", Style::default().fg(Color::DarkGray)),
            ]),
        ])
        .block(
            Block::default()
                .title(" confirm ")
                .borders(Borders::ALL)
                .border_type(BorderType::Rounded)
                .border_style(Style::default().fg(Color::Yellow)),
        );
        frame.render_widget(paragraph, area);
    }

    fn mode_label(&self) -> &'static str {
        if self.show_settings {
            return "Settings";
        }
        if self.delete_pick_idx.is_some() {
            return "DELETE";
        }
        if self.is_create_mode() {
            return "CREATE";
        }
        if self.pending_text.is_some() {
            return "EditingLabel";
        }
        if self.pending_confirm.is_some() {
            return "Confirming";
        }
        match self.mode {
            Mode::Normal => {
                if self.pan_mode {
                    "Normal+Pan"
                } else if self.panel_focus == PanelFocus::Details {
                    "Details"
                } else {
                    "Tree"
                }
            }
            Mode::Connecting => "CREATE",
            Mode::Detaching => "Detaching",
            Mode::EditingLabel => "EditingLabel",
        }
    }

    fn is_create_mode(&self) -> bool {
        if self.mode == Mode::Connecting {
            return true;
        }
        self.pending_text
            .as_ref()
            .map(|p| matches!(p.kind, PendingTextKind::ConnectLabel { .. }))
            .unwrap_or(false)
    }

    fn hints(&self) -> String {
        if self.show_settings {
            return "[j/k or arrows] select  [h/l or Enter] change  [Esc/Backspace] close"
                .to_string();
        }
        if self
            .pending_text
            .as_ref()
            .map(|p| matches!(p.kind, PendingTextKind::ConnectLabel { .. }))
            .unwrap_or(false)
        {
            return "comment popup: type note, [Backspace] delete, [Enter] save, [Esc] back"
                .to_string();
        }
        if self.pending_text.is_some() {
            return "type text, [Backspace] delete, [Enter] apply, [Esc] cancel".to_string();
        }
        if self.pending_confirm.is_some() {
            return "[y] confirm  [n/Esc/Backspace] cancel".to_string();
        }
        match self.mode {
            Mode::Normal => {
                if self.pan_mode {
                    "[arrows/hjkl] scroll  [Space] exit scroll  [q] quit".to_string()
                } else if self.delete_pick_idx.is_some() {
                    "DELETE: [j/k/↑↓] edge  [Enter] delete  [Esc/Backspace] cancel".to_string()
                } else if self.panel_focus == PanelFocus::Details {
                    "[↑↓] navigate edges  [Enter] reroute/create(+ row)  [d] delete  [Esc/Backspace] tree"
                        .to_string()
                } else {
                    format!(
                        "[j/k/↑↓] move  [Enter] details  [z] fold  [b] bundle  [c] connect  [d] delete edge  [f] lines:{}  [s] setup  [q] quit",
                        self.connection_filter.label()
                    )
                }
            }
            Mode::Connecting => {
                "CREATE: [↑/↓] target  [←/→] type  [z] fold  [Enter] comment  [Esc/Backspace] cancel"
                    .to_string()
            }
            Mode::Detaching => {
                if let Some(detach) = &self.detaching {
                    if detach.end.is_none() {
                        "detaching: pick end [s] source or [t] target, [Esc/Backspace] cancel"
                            .to_string()
                    } else {
                        "detaching: move to new node and [Enter], [Esc/Backspace] cancel"
                            .to_string()
                    }
                } else {
                    "[Esc/Backspace] cancel".to_string()
                }
            }
            Mode::EditingLabel => {
                "typing label: [Backspace] delete  [Enter] apply  [Esc] cancel".to_string()
            }
        }
    }

    fn handle_key(&mut self, key: KeyEvent) -> Result<bool> {
        let preserve_status = self.delete_pick_idx.is_some() || self.mode == Mode::Connecting;
        if !preserve_status {
            self.status_message = None;
        }

        if self.show_settings {
            let event = settings::handle_key(key, &mut self.settings_state, &mut self.config);
            match event {
                SettingsEvent::Changed => {
                    self.show_orphans = self.config.show_orphans;
                    self.persist_config()?;
                    self.ensure_focus_visible();
                }
                SettingsEvent::Close => self.show_settings = false,
                SettingsEvent::None => {}
            }
            return Ok(false);
        }

        if self.pending_confirm.is_some() {
            return self.handle_confirm_key(key);
        }

        let in_text_mode = self.pending_text.is_some();
        let action = input::action_for_key(key, in_text_mode);

        if in_text_mode {
            return self.handle_text_action(action);
        }

        if self.mode == Mode::Connecting {
            return self.handle_connecting_action(action, key);
        }

        if self.mode == Mode::Normal && self.panel_focus == PanelFocus::Details {
            return self.handle_details_action(action, key);
        }

        if self.mode == Mode::Detaching
            && let Some(detach) = &mut self.detaching
            && detach.end.is_none()
        {
            match key.code {
                KeyCode::Char('s') => {
                    detach.end = Some(DetachEnd::Source);
                    self.status_message = Some("detach mode: source end selected".to_string());
                }
                KeyCode::Char('t') => {
                    detach.end = Some(DetachEnd::Target);
                    self.status_message = Some("detach mode: target end selected".to_string());
                }
                KeyCode::Esc | KeyCode::Backspace => {
                    self.mode = Mode::Normal;
                    self.detaching = None;
                }
                _ => {}
            }
            return Ok(false);
        }

        match action {
            Action::Quit => return Ok(true),
            Action::ToggleHelp => {
                self.show_help = !self.show_help;
            }
            Action::OpenSettings => {
                self.show_settings = true;
            }
            Action::ZoomIn => self.zoom = (self.zoom + 0.01).min(MAX_ZOOM),
            Action::ZoomOut => self.zoom = (self.zoom - 0.01).max(MIN_ZOOM),
            Action::NextNode => self.cycle_node(),
            Action::Move(direction) => {
                if self.delete_pick_idx.is_some() {
                    match direction {
                        Direction::Up => self.move_delete_pick(-1),
                        Direction::Down => self.move_delete_pick(1),
                        _ => {}
                    }
                } else {
                    self.move_or_pan(direction);
                }
            }
            Action::MoveRightOrEditLabel => {
                if self.delete_pick_idx.is_some() {
                    // In delete selection mode, right/l should not trigger label editing.
                } else if self.selected_edge_matches_focus() {
                    self.start_edit_selected_edge_label();
                } else {
                    self.move_or_pan(Direction::Right);
                }
            }
            Action::StartConnect => {
                self.cancel_delete_pick();
                self.start_connect_mode();
            }
            Action::StartDetach => {
                self.cancel_delete_pick();
                self.start_detach_mode();
            }
            Action::DeleteEdge => self.enter_delete_pick(),
            Action::Activate => {
                if self.delete_pick_idx.is_some() {
                    self.confirm_delete_pick();
                } else {
                    self.handle_enter()?;
                }
            }
            Action::ToggleEdgeDirection => self.toggle_selected_edge_direction()?,
            Action::ReverseEdgeDirection => self.reverse_selected_edge_direction()?,
            Action::AddNode => self.start_add_node_prompt(),
            Action::EditNode => self.start_edit_node_prompt(),
            Action::DeleteNode => self.start_delete_node_prompt(),
            Action::ToggleOrphans => {
                self.show_orphans = !self.show_orphans;
                self.config.show_orphans = self.show_orphans;
                self.persist_config()?;
                self.ensure_focus_visible();
            }
            Action::CycleConnectionFilter => {
                self.connection_filter = self.connection_filter.next();
            }
            Action::ToggleCollapse => self.toggle_folder_collapse(),
            Action::ToggleBundle => self.toggle_folder_bundle()?,
            Action::Cancel => {
                if self.delete_pick_idx.is_some() {
                    self.cancel_delete_pick();
                } else {
                    self.cancel_current_mode();
                }
            }
            Action::Noop | Action::SubmitText | Action::Backspace | Action::InputChar(_) => {}
        }
        Ok(false)
    }

    fn handle_confirm_key(&mut self, key: KeyEvent) -> Result<bool> {
        match key.code {
            KeyCode::Char('y') | KeyCode::Char('Y') | KeyCode::Enter => {
                if let Some(confirm) = self.pending_confirm.take() {
                    match confirm {
                        PendingConfirm::DeleteEdge {
                            source_idx,
                            edge_idx,
                        } => {
                            if self.remove_edge_by_index(source_idx, edge_idx) {
                                self.persist_graph()?;
                                self.status_message = Some("edge deleted".to_string());
                            }
                            self.delete_pick_idx = None;
                            self.selected_edge = None;
                        }
                        PendingConfirm::DeleteNode { node_idx } => {
                            self.delete_node(node_idx)?;
                        }
                        PendingConfirm::BundleFolder { folder, .. } => {
                            self.apply_bundle_with_child_link_prune(&folder)?;
                        }
                    }
                }
            }
            KeyCode::Esc | KeyCode::Backspace | KeyCode::Char('n') | KeyCode::Char('N') => {
                self.pending_confirm = None;
            }
            _ => {}
        }
        Ok(false)
    }

    fn handle_text_action(&mut self, action: Action) -> Result<bool> {
        match action {
            Action::SubmitText => {
                if let Some(prompt) = self.pending_text.take() {
                    self.apply_text_prompt(prompt)?;
                }
            }
            Action::Cancel => {
                if let Some(prompt) = self.pending_text.take() {
                    if let PendingTextKind::ConnectLabel {
                        source_idx, kind, ..
                    } = prompt.kind
                    {
                        self.mode = Mode::Connecting;
                        self.panel_focus = PanelFocus::Tree;
                        self.connect_draft = Some(ConnectDraft { source_idx, kind });
                    } else {
                        self.mode = Mode::Normal;
                    }
                } else {
                    self.mode = Mode::Normal;
                }
            }
            Action::Backspace => {
                if let Some(prompt) = &mut self.pending_text {
                    if prompt.cursor > 0 {
                        let from = byte_index_for_cursor(&prompt.buffer, prompt.cursor - 1);
                        let to = byte_index_for_cursor(&prompt.buffer, prompt.cursor);
                        prompt.buffer.replace_range(from..to, "");
                        prompt.cursor = prompt.cursor.saturating_sub(1);
                    }
                }
            }
            Action::InputChar(c) => {
                if let Some(prompt) = &mut self.pending_text {
                    let at = byte_index_for_cursor(&prompt.buffer, prompt.cursor);
                    prompt.buffer.insert(at, c);
                    prompt.cursor += 1;
                }
            }
            Action::Move(Direction::Left) => {
                if let Some(prompt) = &mut self.pending_text {
                    prompt.cursor = prompt.cursor.saturating_sub(1);
                }
            }
            Action::Move(Direction::Right) => {
                if let Some(prompt) = &mut self.pending_text {
                    let max = prompt.buffer.chars().count();
                    prompt.cursor = (prompt.cursor + 1).min(max);
                }
            }
            _ => {}
        }
        Ok(false)
    }

    fn apply_text_prompt(&mut self, prompt: PendingText) -> Result<()> {
        match prompt.kind {
            PendingTextKind::ConnectLabel {
                source_idx,
                target_idx,
                kind,
            } => {
                let Some(source_path) = self.graph.nodes.get(source_idx).map(|n| n.path.clone())
                else {
                    bail!("connect source no longer exists");
                };
                let Some(target_path) = self.graph.nodes.get(target_idx).map(|n| n.path.clone())
                else {
                    bail!("connect target no longer exists");
                };
                let new_label = prompt.buffer.trim().to_string();
                let mut reused_existing = false;
                let mut updated_label = false;
                if let Some(node) = self.graph.nodes.get_mut(source_idx) {
                    if let Some(existing_idx) = node
                        .edges
                        .iter()
                        .position(|e| e.target == target_path && e.kind == kind)
                    {
                        reused_existing = true;
                        if !new_label.is_empty()
                            && node
                                .edges
                                .get(existing_idx)
                                .map(|e| e.label != new_label)
                                .unwrap_or(false)
                        {
                            if let Some(existing) = node.edges.get_mut(existing_idx) {
                                existing.label = new_label.clone();
                            }
                            updated_label = true;
                        }
                    } else {
                        node.edges.push(Edge {
                            target: target_path.clone(),
                            kind: kind.clone(),
                            label: new_label.clone(),
                        });
                    }
                }
                self.mode = Mode::Normal;
                self.connecting_source = None;
                self.connect_draft = None;
                self.focused_node = source_idx;
                self.locked_node = None;
                self.panel_focus = PanelFocus::Tree;
                self.selected_edge = None;
                self.details_cursor = 0;
                self.persist_graph()?;
                self.status_message = Some(if reused_existing {
                    if updated_label {
                        format!(
                            "tangle already existed; updated label for {} {} {}",
                            source_path,
                            match kind {
                                EdgeKind::Directed => "->",
                                EdgeKind::Incoming => "<-",
                                EdgeKind::Undirected => "--",
                            },
                            target_path
                        )
                    } else {
                        format!(
                            "tangle already exists: {} {} {}",
                            source_path,
                            match kind {
                                EdgeKind::Directed => "->",
                                EdgeKind::Incoming => "<-",
                                EdgeKind::Undirected => "--",
                            },
                            target_path
                        )
                    }
                } else {
                    format!(
                        "connected {} {} {}",
                        source_path,
                        match kind {
                            EdgeKind::Directed => "->",
                            EdgeKind::Incoming => "<-",
                            EdgeKind::Undirected => "--",
                        },
                        target_path
                    )
                });
            }
            PendingTextKind::EditEdgeLabel {
                source_idx,
                edge_idx,
            } => {
                let new_label = prompt.buffer.trim().to_string();
                if let Some(edge) = self
                    .graph
                    .nodes
                    .get_mut(source_idx)
                    .and_then(|n| n.edges.get_mut(edge_idx))
                {
                    edge.label = new_label;
                    self.persist_graph()?;
                    self.status_message = Some("edge label updated".to_string());
                }
                self.mode = Mode::Normal;
            }
            PendingTextKind::AddNode => {
                let path = prompt.buffer.trim().to_string();
                if path.is_empty() {
                    self.mode = Mode::Normal;
                    return Ok(());
                }
                if self.graph.contains(&path) {
                    self.status_message = Some("node already exists".to_string());
                    self.mode = Mode::Normal;
                    return Ok(());
                }
                self.graph.add_node(Node::new(path.clone()));
                let idx = self.graph.nodes.len().saturating_sub(1);
                let (x, y) = self.suggest_new_node_position();
                self.positions.insert(path, (x, y));
                self.focused_node = idx;
                self.mode = Mode::Normal;
                self.persist_graph()?;
            }
            PendingTextKind::EditNodePath { node_idx } => {
                let new_path = prompt.buffer.trim().to_string();
                if new_path.is_empty() || node_idx >= self.graph.nodes.len() {
                    self.mode = Mode::Normal;
                    return Ok(());
                }
                let old_path = self.graph.nodes[node_idx].path.clone();
                if new_path != old_path && self.graph.contains(&new_path) {
                    self.status_message = Some("target path already exists".to_string());
                    self.mode = Mode::Normal;
                    return Ok(());
                }
                self.graph.nodes[node_idx].path = new_path.clone();
                for node in &mut self.graph.nodes {
                    for edge in &mut node.edges {
                        if edge.target == old_path {
                            edge.target = new_path.clone();
                        }
                    }
                }
                if let Some(pos) = self.positions.remove(&old_path) {
                    self.positions.insert(new_path, pos);
                }
                self.mode = Mode::Normal;
                self.persist_graph()?;
                self.status_message = Some("node path updated".to_string());
            }
        }
        Ok(())
    }

    fn handle_enter(&mut self) -> Result<()> {
        if self.mode == Mode::Connecting {
            self.begin_connect_comment_prompt();
            return Ok(());
        }

        if self.mode == Mode::Detaching
            && let Some(detach) = &self.detaching
            && detach.end.is_some()
        {
            self.apply_detach()?;
            return Ok(());
        }

        if self.panel_focus == PanelFocus::Tree {
            self.locked_node = Some(self.focused_node);
            self.panel_focus = PanelFocus::Details;
            self.details_cursor = 0;
            self.selected_edge = None;
            self.status_message = Some("details panel active for focused node".to_string());
        } else {
            self.activate_details_cursor();
        }
        Ok(())
    }

    fn cancel_current_mode(&mut self) {
        if self.mode == Mode::Connecting {
            self.mode = Mode::Normal;
            self.connecting_source = None;
            self.connect_draft = None;
            self.panel_focus = if self.locked_node.is_some() {
                PanelFocus::Details
            } else {
                PanelFocus::Tree
            };
            return;
        }
        if self.mode == Mode::Detaching {
            self.mode = Mode::Normal;
            self.detaching = None;
            return;
        }
        if self.panel_focus == PanelFocus::Details {
            self.panel_focus = PanelFocus::Tree;
            self.locked_node = None;
            self.details_cursor = 0;
            self.selected_edge = None;
            return;
        }
        self.pan_mode = false;
        self.selected_edge = None;
    }

    fn start_connect_mode(&mut self) {
        if self.locked_node.is_none() {
            self.locked_node = Some(self.focused_node);
        }
        self.mode = Mode::Connecting;
        self.panel_focus = PanelFocus::Tree;
        if let Some(source_idx) = self.locked_node {
            self.connecting_source = Some(source_idx);
            self.connect_draft = Some(ConnectDraft {
                source_idx,
                kind: EdgeKind::Directed,
            });
            if self.focused_node == source_idx {
                let _ = self.move_focus_relative(1);
            }
        }
    }

    fn activate_details_cursor(&mut self) {
        let Some(source_idx) = self.locked_node else {
            self.panel_focus = PanelFocus::Tree;
            return;
        };
        let tangles = self.tangles_for_anchor(source_idx);
        if self.details_cursor < tangles.len() {
            self.start_reroute_from_details_cursor();
            return;
        }
        self.start_connect_mode();
    }

    fn start_reroute_from_details_cursor(&mut self) {
        let Some(anchor_idx) = self.locked_node else {
            return;
        };
        let tangles = self.tangles_for_anchor(anchor_idx);
        let Some(selection) = tangles.get(self.details_cursor).cloned() else {
            return;
        };
        let end = if selection.source_idx == anchor_idx {
            DetachEnd::Target
        } else {
            DetachEnd::Source
        };
        self.selected_edge = Some(selection.clone());
        self.mode = Mode::Detaching;
        self.panel_focus = PanelFocus::Tree;
        self.detaching = Some(DetachContext {
            source_idx: selection.source_idx,
            edge_idx: selection.edge_idx,
            end: Some(end),
        });
        self.status_message =
            Some("reroute: move to new node and press Enter (Esc/Backspace cancels)".to_string());
    }

    fn begin_connect_comment_prompt(&mut self) {
        let Some(draft) = self.connect_draft.clone() else {
            return;
        };
        let target_idx = self.focused_node;
        if target_idx == draft.source_idx {
            self.status_message = Some("pick a different target node".to_string());
            return;
        }
        self.pending_text = Some(PendingText {
            title: "Comment (optional):".to_string(),
            buffer: String::new(),
            cursor: 0,
            kind: PendingTextKind::ConnectLabel {
                source_idx: draft.source_idx,
                target_idx,
                kind: draft.kind,
            },
        });
        self.mode = Mode::EditingLabel;
        self.panel_focus = PanelFocus::Details;
    }

    fn handle_details_action(&mut self, action: Action, key: KeyEvent) -> Result<bool> {
        match action {
            Action::Quit => return Ok(true),
            Action::ToggleHelp => {
                self.show_help = !self.show_help;
            }
            Action::OpenSettings => {
                self.show_settings = true;
            }
            Action::Move(Direction::Up) => {
                self.details_cursor = self.details_cursor.saturating_sub(1);
            }
            Action::Move(Direction::Down) => {
                let max = self
                    .locked_node
                    .map(|idx| self.tangles_for_anchor(idx).len())
                    .unwrap_or(0);
                self.details_cursor = (self.details_cursor + 1).min(max);
            }
            Action::Activate => {
                self.activate_details_cursor();
            }
            Action::Cancel => {
                self.panel_focus = PanelFocus::Tree;
                self.locked_node = None;
                self.details_cursor = 0;
                self.selected_edge = None;
            }
            Action::StartConnect => self.start_connect_mode(),
            Action::DeleteEdge => self.delete_edge_at_details_cursor(),
            Action::MoveRightOrEditLabel => self.start_edit_selected_edge_label(),
            Action::ToggleEdgeDirection => self.toggle_selected_edge_direction()?,
            Action::ReverseEdgeDirection => self.reverse_selected_edge_direction()?,
            Action::DeleteNode => self.start_delete_node_prompt(),
            Action::ToggleOrphans => {
                self.show_orphans = !self.show_orphans;
                self.config.show_orphans = self.show_orphans;
                self.persist_config()?;
                self.ensure_focus_visible();
            }
            Action::CycleConnectionFilter => {
                self.connection_filter = self.connection_filter.next();
            }
            Action::Noop
            | Action::SubmitText
            | Action::Backspace
            | Action::InputChar(_)
            | Action::Move(Direction::Left)
            | Action::Move(Direction::Right)
            | Action::StartDetach
            | Action::AddNode
            | Action::EditNode
            | Action::NextNode
            | Action::ToggleCollapse
            | Action::ToggleBundle
            | Action::ZoomIn
            | Action::ZoomOut => {}
        }

        if matches!(key.code, KeyCode::Esc) {
            self.panel_focus = PanelFocus::Tree;
            self.locked_node = None;
            self.details_cursor = 0;
            self.selected_edge = None;
        }
        Ok(false)
    }

    fn handle_connecting_action(&mut self, action: Action, key: KeyEvent) -> Result<bool> {
        match action {
            Action::Quit => return Ok(true),
            Action::ToggleHelp => {
                self.show_help = !self.show_help;
            }
            Action::Move(Direction::Up) => {
                self.move_connecting_focus(-1);
            }
            Action::Move(Direction::Down) => {
                self.move_connecting_focus(1);
            }
            Action::Move(Direction::Left) => self.cycle_connect_kind(-1),
            Action::Move(Direction::Right) => self.cycle_connect_kind(1),
            Action::MoveRightOrEditLabel => self.cycle_connect_kind(1),
            Action::ToggleCollapse => self.toggle_folder_collapse(),
            Action::Activate => self.begin_connect_comment_prompt(),
            Action::Cancel => {
                self.mode = Mode::Normal;
                self.connect_draft = None;
                self.connecting_source = None;
                self.panel_focus = if self.locked_node.is_some() {
                    PanelFocus::Details
                } else {
                    PanelFocus::Tree
                };
            }
            Action::Noop
            | Action::SubmitText
            | Action::Backspace
            | Action::InputChar(_)
            | Action::StartConnect
            | Action::StartDetach
            | Action::DeleteEdge
            | Action::ToggleEdgeDirection
            | Action::ReverseEdgeDirection
            | Action::AddNode
            | Action::EditNode
            | Action::DeleteNode
            | Action::ToggleOrphans
            | Action::CycleConnectionFilter
            | Action::OpenSettings
            | Action::ToggleBundle
            | Action::NextNode
            | Action::ZoomIn
            | Action::ZoomOut => {}
        }

        if matches!(key.code, KeyCode::Esc) {
            self.mode = Mode::Normal;
            self.connect_draft = None;
            self.connecting_source = None;
            self.panel_focus = if self.locked_node.is_some() {
                PanelFocus::Details
            } else {
                PanelFocus::Tree
            };
        }

        Ok(false)
    }

    fn cycle_connect_kind(&mut self, delta: i32) {
        let Some(draft) = &mut self.connect_draft else {
            return;
        };
        let kinds = [EdgeKind::Incoming, EdgeKind::Directed, EdgeKind::Undirected];
        let current = kinds.iter().position(|k| *k == draft.kind).unwrap_or(1) as i32;
        let next = (current + delta).rem_euclid(kinds.len() as i32) as usize;
        draft.kind = kinds[next].clone();
    }

    fn current_connect_preview(&self) -> Option<ConnectPreview<'_>> {
        if let Some(draft) = &self.connect_draft
            && let (Some(source), Some(target)) = (
                self.graph.nodes.get(draft.source_idx),
                self.graph.nodes.get(self.focused_node),
            )
        {
            return Some(ConnectPreview {
                source: source.path.as_str(),
                target: target.path.as_str(),
                kind: draft.kind.clone(),
            });
        }
        if let Some(PendingText {
            kind:
                PendingTextKind::ConnectLabel {
                    source_idx,
                    target_idx,
                    kind,
                },
            ..
        }) = &self.pending_text
            && let (Some(source), Some(target)) = (
                self.graph.nodes.get(*source_idx),
                self.graph.nodes.get(*target_idx),
            )
        {
            return Some(ConnectPreview {
                source: source.path.as_str(),
                target: target.path.as_str(),
                kind: kind.clone(),
            });
        }
        None
    }

    fn start_detach_mode(&mut self) {
        let Some(sel) = self.ensure_edge_selection() else {
            self.status_message = Some("no edge to detach".to_string());
            return;
        };
        self.mode = Mode::Detaching;
        self.detaching = Some(DetachContext {
            source_idx: sel.source_idx,
            edge_idx: sel.edge_idx,
            end: None,
        });
    }

    fn apply_detach(&mut self) -> Result<()> {
        let Some(detach) = self.detaching.clone() else {
            return Ok(());
        };
        let Some(source_node) = self.graph.nodes.get(detach.source_idx) else {
            self.mode = Mode::Normal;
            self.detaching = None;
            return Ok(());
        };
        let Some(edge) = source_node.edges.get(detach.edge_idx).cloned() else {
            self.mode = Mode::Normal;
            self.detaching = None;
            return Ok(());
        };
        let focus_path = self
            .graph
            .nodes
            .get(self.focused_node)
            .map(|n| n.path.clone())
            .unwrap_or_default();

        self.remove_edge_by_index(detach.source_idx, detach.edge_idx);
        match detach.end {
            Some(DetachEnd::Source) => {
                if let Some(new_source) = self.graph.nodes.get_mut(self.focused_node) {
                    new_source.edges.push(edge.clone());
                    self.selected_edge = Some(SelectionEdge {
                        source_idx: self.focused_node,
                        edge_idx: new_source.edges.len().saturating_sub(1),
                    });
                }
                self.status_message = Some(format!("edge source moved to {}", focus_path));
            }
            Some(DetachEnd::Target) => {
                if let Some(source) = self.graph.nodes.get_mut(detach.source_idx) {
                    let mut moved = edge.clone();
                    moved.target = focus_path.clone();
                    source.edges.push(moved);
                    self.selected_edge = Some(SelectionEdge {
                        source_idx: detach.source_idx,
                        edge_idx: source.edges.len().saturating_sub(1),
                    });
                }
                self.status_message = Some(format!("edge target moved to {}", focus_path));
            }
            None => {}
        }

        self.mode = Mode::Normal;
        self.detaching = None;
        self.persist_graph()?;
        Ok(())
    }

    fn enter_delete_pick(&mut self) {
        if self.delete_pick_idx.is_some() {
            return; // already in pick mode
        }
        let tangles = self.tangles_for_anchor(self.focused_node);
        if tangles.is_empty() {
            self.status_message = Some("no edges to delete".to_string());
            return;
        }
        self.delete_pick_idx = Some(0);
        let sel = tangles[0].clone();
        self.selected_edge = Some(sel.clone());
        self.status_message = Some(self.delete_pick_status(&sel, 0, tangles.len()));
    }

    fn move_delete_pick(&mut self, delta: isize) {
        let Some(idx) = self.delete_pick_idx else {
            return;
        };
        let tangles = self.tangles_for_anchor(self.focused_node);
        if tangles.is_empty() {
            return;
        }
        let next = ((idx as isize + delta).rem_euclid(tangles.len() as isize)) as usize;
        self.delete_pick_idx = Some(next);
        let sel = tangles[next].clone();
        self.selected_edge = Some(sel.clone());
        self.status_message = Some(self.delete_pick_status(&sel, next, tangles.len()));
    }

    fn delete_pick_status(&self, sel: &SelectionEdge, idx: usize, total: usize) -> String {
        let Some(node) = self.graph.nodes.get(sel.source_idx) else {
            return format!("({}/{})", idx + 1, total);
        };
        let Some(edge) = node.edges.get(sel.edge_idx) else {
            return format!("({}/{})", idx + 1, total);
        };
        let arrow = match edge.kind {
            EdgeKind::Directed => "→",
            EdgeKind::Incoming => "←",
            EdgeKind::Undirected => "─",
        };
        let label_part = if edge.label.is_empty() {
            String::new()
        } else {
            format!("  \"{}\"", edge.label)
        };
        format!(
            "({}/{})  {} {} {}{}",
            idx + 1,
            total,
            node.path,
            arrow,
            edge.target,
            label_part,
        )
    }

    fn confirm_delete_pick(&mut self) {
        let Some(sel) = self.selected_edge.clone() else {
            return;
        };
        self.pending_confirm = Some(PendingConfirm::DeleteEdge {
            source_idx: sel.source_idx,
            edge_idx: sel.edge_idx,
        });
    }

    fn cancel_delete_pick(&mut self) {
        self.delete_pick_idx = None;
        self.selected_edge = None;
    }

    fn delete_edge_at_details_cursor(&mut self) {
        let Some(source_idx) = self.locked_node else {
            self.status_message = Some("no node locked".to_string());
            return;
        };
        let tangles = self.tangles_for_anchor(source_idx);
        if let Some(sel) = tangles.get(self.details_cursor).cloned() {
            self.pending_confirm = Some(PendingConfirm::DeleteEdge {
                source_idx: sel.source_idx,
                edge_idx: sel.edge_idx,
            });
        } else {
            self.status_message = Some("no edge at cursor".to_string());
        }
    }

    fn start_edit_selected_edge_label(&mut self) {
        let Some(sel) = self.ensure_edge_selection() else {
            self.status_message = Some("no edge selected".to_string());
            return;
        };
        let existing = self
            .graph
            .nodes
            .get(sel.source_idx)
            .and_then(|n| n.edges.get(sel.edge_idx))
            .map(|e| e.label.clone())
            .unwrap_or_default();
        let existing_cursor = existing.chars().count();
        self.pending_text = Some(PendingText {
            title: "Edit edge label:".to_string(),
            buffer: existing,
            cursor: existing_cursor,
            kind: PendingTextKind::EditEdgeLabel {
                source_idx: sel.source_idx,
                edge_idx: sel.edge_idx,
            },
        });
        self.mode = Mode::EditingLabel;
    }

    fn start_add_node_prompt(&mut self) {
        self.pending_text = Some(PendingText {
            title: "New node path:".to_string(),
            buffer: String::new(),
            cursor: 0,
            kind: PendingTextKind::AddNode,
        });
        self.mode = Mode::EditingLabel;
    }

    fn start_edit_node_prompt(&mut self) {
        let Some(node) = self.graph.nodes.get(self.focused_node) else {
            return;
        };
        self.pending_text = Some(PendingText {
            title: "Edit selected node path/note:".to_string(),
            buffer: node.path.clone(),
            cursor: node.path.chars().count(),
            kind: PendingTextKind::EditNodePath {
                node_idx: self.focused_node,
            },
        });
        self.mode = Mode::EditingLabel;
    }

    fn start_delete_node_prompt(&mut self) {
        if self.graph.nodes.get(self.focused_node).is_none() {
            return;
        }
        self.pending_confirm = Some(PendingConfirm::DeleteNode {
            node_idx: self.focused_node,
        });
    }

    fn toggle_selected_edge_direction(&mut self) -> Result<()> {
        let Some(sel) = self.ensure_edge_selection() else {
            self.status_message = Some("no edge selected".to_string());
            return Ok(());
        };
        if let Some(edge) = self
            .graph
            .nodes
            .get_mut(sel.source_idx)
            .and_then(|n| n.edges.get_mut(sel.edge_idx))
        {
            edge.kind = match edge.kind {
                EdgeKind::Undirected => EdgeKind::Directed,
                EdgeKind::Directed | EdgeKind::Incoming => EdgeKind::Undirected,
            };
            self.persist_graph()?;
            self.status_message = Some("edge direction toggled".to_string());
        }
        Ok(())
    }

    fn reverse_selected_edge_direction(&mut self) -> Result<()> {
        let Some(sel) = self.ensure_edge_selection() else {
            self.status_message = Some("no edge selected".to_string());
            return Ok(());
        };
        let Some(source_node) = self.graph.nodes.get(sel.source_idx) else {
            return Ok(());
        };
        let Some(edge) = source_node.edges.get(sel.edge_idx).cloned() else {
            return Ok(());
        };

        if edge.kind == EdgeKind::Undirected {
            self.status_message = Some("cannot reverse undirected edge".to_string());
            return Ok(());
        }

        let source_path = source_node.path.clone();
        let target_idx = self.graph.nodes.iter().position(|n| n.path == edge.target);
        let Some(target_idx) = target_idx else {
            self.status_message = Some("target node missing; cannot reverse".to_string());
            return Ok(());
        };
        let target_path = self.graph.nodes[target_idx].path.clone();

        self.remove_edge_by_index(sel.source_idx, sel.edge_idx);
        let (new_source_path, new_target_path) = match edge.kind {
            EdgeKind::Directed => (target_path, source_path),
            EdgeKind::Incoming => (source_path, edge.target.clone()),
            EdgeKind::Undirected => unreachable!(),
        };
        let Some(new_source_idx) = self
            .graph
            .nodes
            .iter()
            .position(|n| n.path == new_source_path)
        else {
            self.status_message = Some("reverse failed: new source missing".to_string());
            return Ok(());
        };

        if let Some(node) = self.graph.nodes.get_mut(new_source_idx) {
            node.edges.push(Edge {
                target: new_target_path,
                kind: EdgeKind::Directed,
                label: edge.label,
            });
            self.selected_edge = Some(SelectionEdge {
                source_idx: new_source_idx,
                edge_idx: node.edges.len().saturating_sub(1),
            });
            self.focused_node = new_source_idx;
        }

        self.persist_graph()?;
        self.status_message = Some("edge reversed".to_string());
        Ok(())
    }

    fn delete_node(&mut self, node_idx: usize) -> Result<()> {
        if node_idx >= self.graph.nodes.len() {
            return Ok(());
        }
        let path = self.graph.nodes[node_idx].path.clone();
        if let Some(locked_idx) = self.locked_node {
            if locked_idx == node_idx {
                self.locked_node = None;
                self.panel_focus = PanelFocus::Tree;
                self.details_cursor = 0;
            } else if locked_idx > node_idx {
                self.locked_node = Some(locked_idx - 1);
            }
        }
        if let Some(sel) = &mut self.selected_edge {
            if sel.source_idx == node_idx {
                self.selected_edge = None;
            } else if sel.source_idx > node_idx {
                sel.source_idx -= 1;
            }
        }
        self.graph.nodes.remove(node_idx);
        for node in &mut self.graph.nodes {
            node.edges.retain(|e| e.target != path);
        }
        self.positions.remove(&path);
        self.pending_confirm = None;
        if self.graph.nodes.is_empty() {
            self.focused_node = 0;
            self.selected_edge = None;
        } else if self.focused_node >= self.graph.nodes.len() {
            self.focused_node = self.graph.nodes.len() - 1;
        }
        self.ensure_focus_visible();
        self.persist_graph()?;
        self.status_message = Some(format!("deleted node {}", path));
        Ok(())
    }

    fn move_or_pan(&mut self, direction: Direction) {
        if self.mode == Mode::Detaching
            && let Some(detach) = &mut self.detaching
            && detach.end.is_none()
        {
            // In detach mode, accept source/target choice before moving.
            return;
        }

        if self.pan_mode {
            let step = 1;
            match direction {
                Direction::Up => self.viewport_y = self.viewport_y.saturating_sub(step),
                Direction::Down => self.viewport_y += step,
                Direction::Left => self.viewport_x = self.viewport_x.saturating_sub(step),
                Direction::Right => self.viewport_x += step,
            }
            return;
        }

        let delta = match direction {
            Direction::Up | Direction::Left => -1,
            Direction::Down | Direction::Right => 1,
        };
        if self.move_focus_relative(delta) && self.mode == Mode::Normal {
            self.selected_edge = None;
        }
    }

    fn cycle_node(&mut self) {
        if self.move_focus_relative(1) {
            self.selected_edge = None;
        }
    }

    fn move_connecting_focus(&mut self, delta: isize) {
        let skip = self.connecting_source;
        let mut order = self.visible_navigation_order();
        if let Some(skip_idx) = skip {
            order.retain(|idx| *idx != skip_idx);
        }
        if order.is_empty() {
            return;
        }
        let pos = order
            .iter()
            .position(|idx| *idx == self.focused_node)
            .unwrap_or(0);
        let next_pos = (pos as isize + delta).rem_euclid(order.len() as isize) as usize;
        self.focused_node = order[next_pos];
        self.update_tree_scroll_for_focus();
    }

    fn move_focus_relative(&mut self, delta: isize) -> bool {
        let order = self.visible_navigation_order();
        if order.is_empty() {
            return false;
        }
        let pos = order
            .iter()
            .position(|idx| *idx == self.focused_node)
            .unwrap_or(0);
        let next_pos = (pos as isize + delta).rem_euclid(order.len() as isize) as usize;
        self.focused_node = order[next_pos];
        self.update_tree_scroll_for_focus();
        true
    }

    fn visible_navigation_order(&self) -> Vec<usize> {
        let visible = self.visible_node_mask();
        let mut items = self
            .graph
            .nodes
            .iter()
            .enumerate()
            .filter(|(idx, _)| visible.get(*idx).copied().unwrap_or(false))
            .map(|(idx, node)| (idx, node.path.as_str()))
            .collect::<Vec<_>>();
        items.sort_by_key(|(_idx, path)| tree_sort_key(path));
        items.into_iter().map(|(idx, _)| idx).collect()
    }
}

fn tree_sort_key(path: &str) -> String {
    let is_dir = path.ends_with('/');
    let parts: Vec<&str> = path.trim_end_matches('/').split('/').collect();
    let mut key = String::new();
    for (i, part) in parts.iter().enumerate() {
        let is_last = i + 1 == parts.len();
        let is_folder = !is_last || is_dir;
        key.push(if is_folder { '0' } else { '1' });
        key.push('/');
        key.push_str(part);
        key.push('/');
    }
    key
}

fn default_position_for_index(index: usize) -> (i32, i32) {
    const COLS: usize = 5;
    const X_START: i32 = 80;
    const Y_START: i32 = 80;
    const X_STEP: i32 = 160;
    const Y_STEP: i32 = 120;
    let col = index % COLS;
    let row = index / COLS;
    (X_START + col as i32 * X_STEP, Y_START + row as i32 * Y_STEP)
}

fn is_descendant_of_folder(path: &str, folder: &str) -> bool {
    path != folder && path.starts_with(folder)
}

impl AppState {
    fn ensure_all_node_positions(&mut self) {
        for (idx, node) in self.graph.nodes.iter().enumerate() {
            self.positions
                .entry(node.path.clone())
                .or_insert_with(|| default_position_for_index(idx));
        }
        self.positions
            .retain(|path, _| self.graph.nodes.iter().any(|n| n.path == *path));
    }

    fn reset_viewport(&mut self) {
        self.viewport_x = 0;
        self.viewport_y = 0;
    }

    fn update_tree_view_rows(&mut self, frame_area: Rect) {
        let area = frame_area.inner(Margin {
            horizontal: 3,
            vertical: 1,
        });
        let canvas_inner = Block::default()
            .borders(Borders::ALL)
            .padding(Padding::new(2, 2, 1, 1))
            .inner(area);
        let [panes_area, _gap, _status_area] = Layout::vertical([
            Constraint::Min(6),
            Constraint::Length(1),
            Constraint::Length(4),
        ])
        .areas(canvas_inner);
        let [tree_outer, _separator, _details_outer] = Layout::horizontal([
            Constraint::Percentage(57),
            Constraint::Length(2),
            Constraint::Fill(1),
        ])
        .areas(panes_area);
        let tree_inner = Block::default().borders(Borders::ALL).inner(tree_outer);
        self.tree_view_rows = tree_inner.height.max(1) as usize;
    }

    fn update_tree_scroll_for_focus(&mut self) {
        if self.pan_mode {
            return;
        }
        let order = self.visible_navigation_order();
        if order.is_empty() {
            self.viewport_y = 0;
            return;
        }
        let focus_pos = order
            .iter()
            .position(|idx| *idx == self.focused_node)
            .unwrap_or(0);
        let rows = self.tree_view_rows.max(1);
        let mut scroll = self.viewport_y.max(0) as usize;
        if focus_pos < scroll {
            scroll = focus_pos;
        } else if focus_pos >= scroll + rows {
            scroll = focus_pos + 1 - rows;
        }
        self.viewport_y = scroll as i32;
    }

    fn ensure_focus_visible(&mut self) {
        let order = self.visible_navigation_order();
        if order.is_empty() {
            self.focused_node = 0;
            return;
        }
        if !order.contains(&self.focused_node) {
            self.focused_node = order[0];
            self.selected_edge = None;
        }
        self.update_tree_scroll_for_focus();
    }

    fn create_source_folder(&self) -> Option<&str> {
        if self.mode != Mode::Connecting {
            return None;
        }
        let source_idx = self
            .connect_draft
            .as_ref()
            .map(|draft| draft.source_idx)
            .or(self.connecting_source)?;
        let source_path = self.graph.nodes.get(source_idx)?.path.as_str();
        if source_path.ends_with('/') {
            Some(source_path)
        } else {
            source_path.rfind('/').map(|idx| &source_path[..=idx])
        }
    }

    fn is_create_protected_folder(&self, folder: &str) -> bool {
        self.create_source_folder()
            .map(|source_folder| source_folder.starts_with(folder))
            .unwrap_or(false)
    }

    fn toggle_folder_collapse(&mut self) {
        if !(self.mode == Mode::Normal || self.mode == Mode::Connecting)
            || self.panel_focus != PanelFocus::Tree
            || self.delete_pick_idx.is_some()
        {
            return;
        }
        let Some(node) = self.graph.nodes.get(self.focused_node) else {
            return;
        };
        if !node.path.ends_with('/') {
            self.status_message = Some("focus a folder to fold/unfold".to_string());
            return;
        }

        let folder = node.path.clone();
        if self.mode == Mode::Connecting && self.is_create_protected_folder(&folder) {
            self.status_message =
                Some("cannot fold source folder or its parents in CREATE".to_string());
            return;
        }
        if self.bundled_folders.contains(folder.as_str()) {
            self.status_message = Some("folder is bundled; use [b] to unbundle".to_string());
            return;
        }
        if self.collapsed_folders.remove(&folder) {
            self.status_message = Some(format!("expanded {}", folder));
        } else {
            self.collapsed_folders.insert(folder.clone());
            self.status_message = Some(format!("collapsed {}", folder));
            self.clear_locked_if_hidden_by_folder(&folder);
        }
        self.ensure_focus_visible();
    }

    fn toggle_folder_bundle(&mut self) -> Result<()> {
        if self.mode != Mode::Normal
            || self.panel_focus != PanelFocus::Tree
            || self.delete_pick_idx.is_some()
        {
            return Ok(());
        }
        let Some(node) = self.graph.nodes.get(self.focused_node) else {
            return Ok(());
        };
        if !node.path.ends_with('/') {
            self.status_message = Some("focus a folder to bundle/unbundle".to_string());
            return Ok(());
        }
        let folder = node.path.clone();

        if self.bundled_folders.remove(&folder) {
            self.collapsed_folders.remove(&folder);
            self.status_message = Some(format!("unbundled {}", folder));
            self.persist_graph()?;
            self.ensure_focus_visible();
            return Ok(());
        }

        let removed_child_links = self.count_child_links_for_bundle(&folder);
        if removed_child_links > 0 {
            self.pending_confirm = Some(PendingConfirm::BundleFolder {
                folder,
                removed_child_links,
            });
            return Ok(());
        }

        self.apply_bundle_with_child_link_prune(&folder)?;
        Ok(())
    }

    fn apply_bundle_with_child_link_prune(&mut self, folder: &str) -> Result<()> {
        let removed_child_links = self.remove_child_links_for_bundle(folder);
        self.bundled_folders.insert(folder.to_string());
        self.collapsed_folders.insert(folder.to_string());
        self.clear_locked_if_hidden_by_folder(folder);
        self.persist_graph()?;
        self.ensure_focus_visible();
        if removed_child_links > 0 {
            self.status_message = Some(format!(
                "bundled {} (removed {} child link{})",
                folder,
                removed_child_links,
                if removed_child_links == 1 { "" } else { "s" }
            ));
        } else {
            self.status_message = Some(format!("bundled {}", folder));
        }
        Ok(())
    }

    fn count_child_links_for_bundle(&self, folder: &str) -> usize {
        self.graph
            .nodes
            .iter()
            .map(|node| {
                let source_is_child = is_descendant_of_folder(&node.path, folder);
                node.edges
                    .iter()
                    .filter(|edge| {
                        let target_is_child = is_descendant_of_folder(&edge.target, folder);
                        source_is_child || target_is_child
                    })
                    .count()
            })
            .sum()
    }

    fn remove_child_links_for_bundle(&mut self, folder: &str) -> usize {
        let mut removed = 0usize;
        for node in &mut self.graph.nodes {
            let source_is_child = is_descendant_of_folder(&node.path, folder);
            let before = node.edges.len();
            node.edges.retain(|edge| {
                let target_is_child = is_descendant_of_folder(&edge.target, folder);
                !(source_is_child || target_is_child)
            });
            removed += before.saturating_sub(node.edges.len());
        }
        if removed > 0 {
            self.selected_edge = None;
            self.detaching = None;
            self.delete_pick_idx = None;
            self.clamp_details_cursor();
        }
        removed
    }

    fn clear_locked_if_hidden_by_folder(&mut self, folder: &str) {
        if let Some(locked_idx) = self.locked_node
            && let Some(locked_node) = self.graph.nodes.get(locked_idx)
            && locked_node.path != folder
            && locked_node.path.starts_with(folder)
        {
            self.locked_node = None;
            self.panel_focus = PanelFocus::Tree;
            self.details_cursor = 0;
            self.selected_edge = None;
        }
    }

    fn visible_node_mask(&self) -> Vec<bool> {
        self.visible_node_mask_with_transient(None)
    }

    fn visible_node_mask_for_render(&self, transient_open_folders: &HashSet<String>) -> Vec<bool> {
        self.visible_node_mask_with_transient(Some(transient_open_folders))
    }

    fn visible_node_mask_with_transient(
        &self,
        transient_open_folders: Option<&HashSet<String>>,
    ) -> Vec<bool> {
        let base_visibility = if self.show_orphans {
            vec![true; self.graph.nodes.len()]
        } else {
            let mut incoming: HashSet<&str> = HashSet::new();
            for node in &self.graph.nodes {
                for edge in &node.edges {
                    incoming.insert(edge.target.as_str());
                }
            }
            self.graph
                .nodes
                .iter()
                .map(|n| !n.edges.is_empty() || incoming.contains(n.path.as_str()))
                .collect::<Vec<_>>()
        };

        self.graph
            .nodes
            .iter()
            .enumerate()
            .map(|(idx, n)| {
                base_visibility.get(idx).copied().unwrap_or(false)
                    && !self.is_path_hidden_by_collapse(&n.path, transient_open_folders)
            })
            .collect()
    }

    fn is_path_hidden_by_collapse(
        &self,
        path: &str,
        transient_open_folders: Option<&HashSet<String>>,
    ) -> bool {
        self.collapsed_folders
            .iter()
            .filter(|folder| {
                transient_open_folders
                    .map(|transient| !transient.contains(*folder))
                    .unwrap_or(true)
            })
            .chain(self.bundled_folders.iter())
            .any(|folder| is_descendant_of_folder(path, folder))
    }

    fn transient_open_folders_for_highlight(&self) -> HashSet<String> {
        if !self.config.auto_reveal_links {
            return HashSet::new();
        }
        if self.mode != Mode::Normal || self.delete_pick_idx.is_some() {
            return HashSet::new();
        }
        if self.panel_focus == PanelFocus::Tree
            && self.connection_filter == ConnectionFilter::Hidden
        {
            return HashSet::new();
        }
        let Some(anchor_path) = self.highlight_anchor_path() else {
            return HashSet::new();
        };

        let mut connected_paths = HashSet::new();
        for node in &self.graph.nodes {
            for edge in &node.edges {
                if node.path == anchor_path {
                    connected_paths.insert(edge.target.as_str());
                } else if edge.target == anchor_path {
                    connected_paths.insert(node.path.as_str());
                }
            }
        }

        self.collapsed_folders
            .iter()
            .filter(|folder| {
                connected_paths
                    .iter()
                    .any(|path| *path != folder.as_str() && path.starts_with(folder.as_str()))
            })
            .cloned()
            .collect()
    }

    fn highlight_anchor_path(&self) -> Option<&str> {
        if self.panel_focus == PanelFocus::Details
            && let Some(locked_idx) = self.locked_node
        {
            return self.graph.nodes.get(locked_idx).map(|n| n.path.as_str());
        }
        self.graph
            .nodes
            .get(self.focused_node)
            .map(|n| n.path.as_str())
    }

    fn tangles_for_anchor(&self, anchor_idx: usize) -> Vec<SelectionEdge> {
        let Some(anchor_node) = self.graph.nodes.get(anchor_idx) else {
            return Vec::new();
        };
        let anchor_path = anchor_node.path.as_str();
        let mut refs = Vec::new();

        for (source_idx, node) in self.graph.nodes.iter().enumerate() {
            for (edge_idx, edge) in node.edges.iter().enumerate() {
                if node.path == anchor_path || edge.target == anchor_path {
                    refs.push(SelectionEdge {
                        source_idx,
                        edge_idx,
                    });
                }
            }
        }

        refs.sort_by(|a, b| {
            self.tangle_sort_key(anchor_path, a)
                .cmp(&self.tangle_sort_key(anchor_path, b))
        });
        refs
    }

    fn tangle_sort_key(
        &self,
        anchor_path: &str,
        selection: &SelectionEdge,
    ) -> (String, u8, String, String, String, usize, usize) {
        let Some(node) = self.graph.nodes.get(selection.source_idx) else {
            return (
                String::new(),
                3,
                String::new(),
                String::new(),
                String::new(),
                selection.source_idx,
                selection.edge_idx,
            );
        };
        let Some(edge) = node.edges.get(selection.edge_idx) else {
            return (
                String::new(),
                3,
                node.path.clone(),
                String::new(),
                String::new(),
                selection.source_idx,
                selection.edge_idx,
            );
        };
        let (neighbor, marker) = if node.path == anchor_path {
            (
                edge.target.clone(),
                match edge.kind {
                    EdgeKind::Directed => '▶',
                    EdgeKind::Incoming => '◀',
                    EdgeKind::Undirected => '◆',
                },
            )
        } else {
            (
                node.path.clone(),
                match edge.kind {
                    EdgeKind::Directed => '◀',
                    EdgeKind::Incoming => '▶',
                    EdgeKind::Undirected => '◆',
                },
            )
        };
        (
            neighbor,
            match marker {
                '▶' => 0,
                '◀' => 1,
                '◆' => 2,
                _ => 3,
            },
            node.path.clone(),
            edge.target.clone(),
            edge.label.clone(),
            selection.source_idx,
            selection.edge_idx,
        )
    }

    fn clamp_details_cursor(&mut self) {
        let Some(anchor_idx) = self.locked_node else {
            self.details_cursor = 0;
            return;
        };
        let max = self.tangles_for_anchor(anchor_idx).len();
        if self.details_cursor > max {
            self.details_cursor = max;
        }
    }

    fn resolve_selected_edge(&self) -> Option<SelectionEdge> {
        let sel = self.selected_edge.as_ref()?;
        let source = self.graph.nodes.get(sel.source_idx)?;
        if sel.edge_idx >= source.edges.len() {
            return None;
        }
        Some(sel.clone())
    }

    fn ensure_edge_selection(&mut self) -> Option<SelectionEdge> {
        if let Some(sel) = self.resolve_selected_edge() {
            return Some(sel);
        }
        if let Some(anchor_idx) = self.locked_node {
            let tangles = self.tangles_for_anchor(anchor_idx);
            if let Some(sel) = tangles.get(self.details_cursor).cloned() {
                self.selected_edge = Some(sel.clone());
                return Some(sel);
            }
            if let Some(sel) = tangles.first().cloned() {
                self.selected_edge = Some(sel.clone());
                return Some(sel);
            }
        }
        let node = self.graph.nodes.get(self.focused_node)?;
        if node.edges.is_empty() {
            return None;
        }
        let sel = SelectionEdge {
            source_idx: self.focused_node,
            edge_idx: 0,
        };
        self.selected_edge = Some(sel.clone());
        Some(sel)
    }

    fn selected_edge_matches_focus(&self) -> bool {
        self.resolve_selected_edge()
            .map(|sel| sel.source_idx == self.focused_node)
            .unwrap_or(false)
    }

    fn remove_edge_by_index(&mut self, source_idx: usize, edge_idx: usize) -> bool {
        if let Some(node) = self.graph.nodes.get_mut(source_idx)
            && edge_idx < node.edges.len()
        {
            node.edges.remove(edge_idx);
            self.selected_edge = None;
            self.clamp_details_cursor();
            return true;
        }
        false
    }

    fn suggest_new_node_position(&self) -> (i32, i32) {
        if let Some(node) = self.graph.nodes.get(self.focused_node)
            && let Some((x, y)) = self.positions.get(&node.path)
        {
            return (x + 140, *y);
        }
        (
            self.viewport_x + (120.0 / self.zoom) as i32,
            self.viewport_y + (80.0 / self.zoom) as i32,
        )
    }

    fn persist_graph(&self) -> Result<()> {
        if self.demo {
            return Ok(());
        }
        let Some(graph_path) = &self.graph_path else {
            return Ok(());
        };
        let existing_graph_text = fs::read_to_string(graph_path)?;
        let serialized_graph =
            merge_graph_into_document(&existing_graph_text, &self.graph, &self.bundled_folders)?;
        fs::write(graph_path, serialized_graph)?;
        Ok(())
    }

    fn normalize_graph_file_post_view(&self) -> Result<()> {
        if self.demo {
            return Ok(());
        }
        let Some(graph_path) = &self.graph_path else {
            return Ok(());
        };

        let original = fs::read_to_string(graph_path)?;
        let mut doc = graph::parse(&original)?;
        let reordered = graph::sort_edges_by_kind(&mut doc);
        let lint_report = graph::lint(&mut doc);

        if reordered > 0 || lint_report.changed() {
            fs::write(graph_path, graph::serialize(&doc))?;
        }
        Ok(())
    }

    fn persist_config(&self) -> Result<()> {
        if self.demo {
            return Ok(());
        }
        if let Some(path) = &self.config_path {
            fs::write(path, settings::serialize_config(&self.config))?;
        }
        Ok(())
    }
}

pub fn run(demo: bool, open_settings: bool) -> Result<()> {
    let mut app = AppState::load(demo, open_settings)?;

    let mut stdout = io::stdout();
    enable_raw_mode()?;
    execute!(stdout, EnterAlternateScreen)?;
    let _guard = TerminalGuard;

    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    loop {
        terminal.draw(|f| app.draw(f))?;
        if !event::poll(Duration::from_millis(200))? {
            continue;
        }
        if let Event::Key(key) = event::read()? {
            if matches!(key.kind, KeyEventKind::Release | KeyEventKind::Repeat) {
                continue;
            }
            if app.handle_key(key)? {
                break;
            }
        }
    }

    app.persist_graph()?;
    app.normalize_graph_file_post_view()?;
    Ok(())
}

struct TerminalGuard;

impl Drop for TerminalGuard {
    fn drop(&mut self) {
        let _ = disable_raw_mode();
        let mut stdout = io::stdout();
        let _ = execute!(stdout, LeaveAlternateScreen);
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

fn line_with_cursor(
    text: &str,
    cursor: usize,
    placeholder: &str,
    text_style: Style,
    placeholder_style: Style,
    caret_style: Style,
) -> Line<'static> {
    let mut spans = Vec::new();
    let char_len = text.chars().count();
    let clamped = cursor.min(char_len);

    if char_len == 0 {
        spans.push(Span::styled("▌", caret_style));
        if !placeholder.is_empty() {
            spans.push(Span::raw(" "));
            spans.push(Span::styled(placeholder.to_string(), placeholder_style));
        }
        return Line::from(spans);
    }

    let split = byte_index_for_cursor(text, clamped);
    let (left, right) = text.split_at(split);
    if !left.is_empty() {
        spans.push(Span::styled(left.to_string(), text_style));
    }
    spans.push(Span::styled("▌", caret_style));
    if !right.is_empty() {
        spans.push(Span::styled(right.to_string(), text_style));
    }
    Line::from(spans)
}

fn byte_index_for_cursor(text: &str, cursor: usize) -> usize {
    text.char_indices()
        .nth(cursor)
        .map(|(idx, _)| idx)
        .unwrap_or(text.len())
}

fn demo_graph() -> (Graph, HashMap<String, (i32, i32)>, config::Config) {
    let mut graph = Graph::new();
    let mut main = Node::new("src/main.rs");
    main.edges.push(Edge {
        target: "src/tui/canvas.rs".to_string(),
        kind: EdgeKind::Directed,
        label: "starts tui".to_string(),
    });
    main.edges.push(Edge {
        target: "src/graph/model.rs".to_string(),
        kind: EdgeKind::Directed,
        label: "loads graph".to_string(),
    });
    main.edges.push(Edge {
        target: "README.md".to_string(),
        kind: EdgeKind::Undirected,
        label: "overview".to_string(),
    });

    let mut canvas = Node::new("src/tui/canvas.rs");
    canvas.edges.push(Edge {
        target: "src/tui/render.rs".to_string(),
        kind: EdgeKind::Directed,
        label: "renders ui".to_string(),
    });
    canvas.edges.push(Edge {
        target: "src/tui/input.rs".to_string(),
        kind: EdgeKind::Directed,
        label: "reads keys".to_string(),
    });
    canvas.edges.push(Edge {
        target: "src/tui/settings.rs".to_string(),
        kind: EdgeKind::Directed,
        label: "opens setup".to_string(),
    });
    canvas.edges.push(Edge {
        target: "docs/tui.md".to_string(),
        kind: EdgeKind::Undirected,
        label: "design notes".to_string(),
    });

    let mut render = Node::new("src/tui/render.rs");
    render.edges.push(Edge {
        target: "src/tui/input.rs".to_string(),
        kind: EdgeKind::Incoming,
        label: "refresh on input".to_string(),
    });

    let mut settings = Node::new("src/tui/settings.rs");
    settings.edges.push(Edge {
        target: "src/main.rs".to_string(),
        kind: EdgeKind::Incoming,
        label: "launched from app".to_string(),
    });

    let mut parser_graph = Node::new("src/parser/graph.rs");
    parser_graph.edges.push(Edge {
        target: "src/graph/model.rs".to_string(),
        kind: EdgeKind::Directed,
        label: "builds model".to_string(),
    });

    let mut docs_tui = Node::new("docs/tui.md");
    docs_tui.edges.push(Edge {
        target: "src/tui/render.rs".to_string(),
        kind: EdgeKind::Undirected,
        label: "visual reference".to_string(),
    });

    let mut test_nav = Node::new("tests/tui/test_navigation.rs");
    test_nav.edges.push(Edge {
        target: "src/tui/input.rs".to_string(),
        kind: EdgeKind::Directed,
        label: "asserts keymaps".to_string(),
    });
    test_nav.edges.push(Edge {
        target: "src/tui/canvas.rs".to_string(),
        kind: EdgeKind::Directed,
        label: "drives interactions".to_string(),
    });

    let readme = Node::new("README.md");
    let src = Node::new("src/");
    let src_tui = Node::new("src/tui/");
    let src_graph = Node::new("src/graph/");
    let src_parser = Node::new("src/parser/");
    let graph_model = Node::new("src/graph/model.rs");
    let input = Node::new("src/tui/input.rs");
    let docs = Node::new("docs/");
    let tests = Node::new("tests/");
    let tests_tui = Node::new("tests/tui/");

    graph.add_node(readme);
    graph.add_node(src);
    graph.add_node(main);
    graph.add_node(src_tui);
    graph.add_node(canvas);
    graph.add_node(render);
    graph.add_node(input);
    graph.add_node(settings);
    graph.add_node(src_graph);
    graph.add_node(graph_model);
    graph.add_node(src_parser);
    graph.add_node(parser_graph);
    graph.add_node(docs);
    graph.add_node(docs_tui);
    graph.add_node(tests);
    graph.add_node(tests_tui);
    graph.add_node(test_nav);

    let mut pos = HashMap::new();
    pos.insert("README.md".to_string(), (80, 70));
    pos.insert("src/".to_string(), (80, 130));
    pos.insert("src/main.rs".to_string(), (260, 140));
    pos.insert("src/tui/".to_string(), (450, 80));
    pos.insert("src/tui/canvas.rs".to_string(), (640, 90));
    pos.insert("src/tui/render.rs".to_string(), (860, 70));
    pos.insert("src/tui/input.rs".to_string(), (860, 160));
    pos.insert("src/tui/settings.rs".to_string(), (640, 200));
    pos.insert("src/graph/".to_string(), (450, 260));
    pos.insert("src/graph/model.rs".to_string(), (640, 300));
    pos.insert("src/parser/".to_string(), (450, 370));
    pos.insert("src/parser/graph.rs".to_string(), (640, 410));
    pos.insert("docs/".to_string(), (80, 300));
    pos.insert("docs/tui.md".to_string(), (260, 320));
    pos.insert("tests/".to_string(), (80, 420));
    pos.insert("tests/tui/".to_string(), (260, 440));
    pos.insert("tests/tui/test_navigation.rs".to_string(), (450, 470));

    let mut cfg = config::Config::default();
    cfg.show_orphans = true;
    (graph, pos, cfg)
}

fn merge_graph_into_document(
    existing: &str,
    target_graph: &Graph,
    bundled_roots: &HashSet<String>,
) -> Result<String> {
    let mut doc = graph::parse(existing)?;
    let existing_graph = graph::to_graph(&doc)?;

    if existing_graph != *target_graph {
        let existing_paths = existing_graph
            .nodes
            .iter()
            .map(|n| n.path.clone())
            .collect::<HashSet<_>>();
        let target_paths = target_graph
            .nodes
            .iter()
            .map(|n| n.path.clone())
            .collect::<HashSet<_>>();

        for removed in existing_paths.difference(&target_paths) {
            graph::remove_edges_targeting(&mut doc, removed);
            graph::remove_node(&mut doc, removed);
        }

        for added in target_paths.difference(&existing_paths) {
            graph::add_node(&mut doc, added);
        }

        for node in &target_graph.nodes {
            if let Some(old) = existing_graph.nodes.iter().find(|n| n.path == node.path) {
                let old_targets = old
                    .edges
                    .iter()
                    .map(|e| e.target.clone())
                    .collect::<Vec<_>>();
                for target in old_targets {
                    while graph::remove_edge(&mut doc, &node.path, &target) {}
                }
            }

            for edge in &node.edges {
                graph::add_edge(&mut doc, &node.path, edge)?;
            }
        }
    }

    reconcile_bundle_markers(&mut doc, bundled_roots);
    reconcile_orphan_markers(&mut doc, target_graph);
    mark_unattended_orphans(&mut doc, target_graph);

    let merged = graph::serialize(&doc);
    let merged_doc = graph::parse(&merged)?;
    let merged_graph = graph::to_graph(&merged_doc)?;
    if merged_graph != *target_graph {
        bail!("view save produced graph/doc mismatch");
    }
    Ok(merged)
}

fn reconcile_bundle_markers(doc: &mut graph::Document, bundled_roots: &HashSet<String>) {
    let existing_roots = graph::collapsed_subtree_roots(doc);
    let mut to_unbundle = existing_roots
        .difference(bundled_roots)
        .cloned()
        .collect::<Vec<_>>();
    to_unbundle.sort();
    for path in to_unbundle {
        graph::unmark_link_subtree(doc, &path);
    }

    let mut to_bundle = bundled_roots.iter().cloned().collect::<Vec<_>>();
    to_bundle.sort();
    for path in to_bundle {
        graph::mark_link_subtree(doc, &path);
    }
}

fn reconcile_orphan_markers(doc: &mut graph::Document, target_graph: &Graph) {
    let mut incoming_counts: HashMap<&str, usize> = HashMap::new();
    for node in &target_graph.nodes {
        for edge in &node.edges {
            *incoming_counts.entry(edge.target.as_str()).or_insert(0) += 1;
        }
    }

    for (path, marker_kind) in graph::explicit_orphan_markers(doc) {
        if total_link_count(target_graph, &incoming_counts, &path) == 0 {
            continue;
        }
        match marker_kind {
            graph::OrphanMarkerKind::Orphan => {
                graph::unmark_orphan(doc, &path);
            }
            graph::OrphanMarkerKind::OrphanSubtree => {
                graph::unmark_orphan_subtree(doc, &path);
            }
        }
    }
}

fn mark_unattended_orphans(doc: &mut graph::Document, target_graph: &Graph) {
    let mut incoming_counts: HashMap<&str, usize> = HashMap::new();
    for node in &target_graph.nodes {
        for edge in &node.edges {
            *incoming_counts.entry(edge.target.as_str()).or_insert(0) += 1;
        }
    }

    for node in &target_graph.nodes {
        if total_link_count(target_graph, &incoming_counts, &node.path) != 0 {
            continue;
        }
        graph::mark_orphan(doc, &node.path);
    }
}

fn total_link_count(graph: &Graph, incoming_counts: &HashMap<&str, usize>, path: &str) -> usize {
    let outgoing = graph.get(path).map(|n| n.edges.len()).unwrap_or(0);
    let incoming = incoming_counts.get(path).copied().unwrap_or(0);
    incoming + outgoing
}

#[cfg(test)]
mod tests {
    use super::*;

    fn node_index(app: &AppState, path: &str) -> usize {
        app.graph
            .nodes
            .iter()
            .position(|n| n.path == path)
            .expect("node must exist in demo graph")
    }

    #[test]
    fn render_visibility_temporarily_opens_collapsed_connected_folder() {
        let mut app = AppState::load(true, false).unwrap();
        let main_idx = node_index(&app, "src/main.rs");
        let canvas_idx = node_index(&app, "src/tui/canvas.rs");
        app.focused_node = main_idx;
        app.connection_filter = ConnectionFilter::All;
        app.collapsed_folders.insert("src/tui/".to_string());

        let persistent_mask = app.visible_node_mask();
        assert!(
            !persistent_mask[canvas_idx],
            "connected node in collapsed folder should remain hidden in persistent visibility"
        );

        let transient_open = app.transient_open_folders_for_highlight();
        assert!(
            transient_open.contains("src/tui/"),
            "focused node should auto-open collapsed folder that contains a connected node"
        );

        let render_mask = app.visible_node_mask_for_render(&transient_open);
        assert!(
            render_mask[canvas_idx],
            "connected node should be visible for rendering while focused"
        );
    }

    #[test]
    fn render_visibility_does_not_auto_open_when_connection_overlay_hidden() {
        let mut app = AppState::load(true, false).unwrap();
        let main_idx = node_index(&app, "src/main.rs");
        app.focused_node = main_idx;
        app.connection_filter = ConnectionFilter::Hidden;
        app.collapsed_folders.insert("src/tui/".to_string());

        let transient_open = app.transient_open_folders_for_highlight();
        assert!(
            transient_open.is_empty(),
            "tree view should not auto-open collapsed folders when connection overlay is off"
        );
    }

    #[test]
    fn bundling_folder_warns_before_removing_child_edges() {
        let mut app = AppState::load(true, false).unwrap();
        let folder = "src/tui/";
        let folder_idx = node_index(&app, folder);
        app.focused_node = folder_idx;

        let edge_count_before = app
            .graph
            .nodes
            .iter()
            .flat_map(|n| {
                n.edges
                    .iter()
                    .map(move |e| (n.path.as_str(), e.target.as_str()))
            })
            .filter(|(src, dst)| {
                is_descendant_of_folder(src, folder) || is_descendant_of_folder(dst, folder)
            })
            .count();

        app.toggle_folder_bundle().unwrap();
        let warned_links = match &app.pending_confirm {
            Some(PendingConfirm::BundleFolder {
                folder: warned_folder,
                removed_child_links,
            }) => {
                assert_eq!(warned_folder, folder);
                *removed_child_links
            }
            other => panic!("expected bundle warning confirm, got {other:?}"),
        };
        assert!(
            warned_links > 0,
            "bundle warning should report child links before deletion"
        );

        let edge_count_during_confirm = app
            .graph
            .nodes
            .iter()
            .flat_map(|n| {
                n.edges
                    .iter()
                    .map(move |e| (n.path.as_str(), e.target.as_str()))
            })
            .filter(|(src, dst)| {
                is_descendant_of_folder(src, folder) || is_descendant_of_folder(dst, folder)
            })
            .count();
        assert_eq!(
            edge_count_during_confirm, edge_count_before,
            "links should still be present before confirm"
        );

        if let Some(PendingConfirm::BundleFolder { folder, .. }) = app.pending_confirm.take() {
            app.apply_bundle_with_child_link_prune(&folder).unwrap();
        } else {
            panic!("expected bundle confirm to apply");
        }

        let edge_count_after = app
            .graph
            .nodes
            .iter()
            .flat_map(|n| {
                n.edges
                    .iter()
                    .map(move |e| (n.path.as_str(), e.target.as_str()))
            })
            .filter(|(src, dst)| {
                is_descendant_of_folder(src, folder) || is_descendant_of_folder(dst, folder)
            })
            .count();
        assert_eq!(
            edge_count_after, 0,
            "confirmed bundle should remove child links"
        );
        assert!(app.bundled_folders.contains(folder));
    }

    #[test]
    fn merge_graph_into_document_noop_preserves_text_exactly() {
        let existing = "\
a.rs
    -> b.rs : old

# keep this comment
b.rs
";
        let g = graph::to_graph(&graph::parse(existing).unwrap()).unwrap();
        let merged = merge_graph_into_document(existing, &g, &HashSet::new()).unwrap();
        assert_eq!(merged, existing);
    }

    #[test]
    fn merge_graph_into_document_preserves_tags_and_comments_on_edit() {
        let existing = "\
[orphan]
c.rs

[orphan]
a.rs
    -> b.rs : old

# keep this comment
b.rs
";
        let mut g = graph::to_graph(&graph::parse(existing).unwrap()).unwrap();
        let node = g
            .nodes
            .iter_mut()
            .find(|n| n.path == "a.rs")
            .expect("a.rs should exist");
        node.edges[0].label = "new".to_string();

        let merged = merge_graph_into_document(existing, &g, &HashSet::new()).unwrap();
        assert!(
            merged.contains("[orphan]\nc.rs\n"),
            "orphan tag should be preserved: {merged}"
        );
        assert!(
            !merged.contains("[orphan]\na.rs\n"),
            "linked node should be un-orphaned: {merged}"
        );
        assert!(
            merged.contains("# keep this comment\nb.rs\n"),
            "comments should be preserved: {merged}"
        );
        assert!(
            merged.contains("-> b.rs : new"),
            "edge label should be updated"
        );

        let merged_graph = graph::to_graph(&graph::parse(&merged).unwrap()).unwrap();
        assert_eq!(merged_graph, g);
    }

    #[test]
    fn merge_graph_into_document_unmarks_linked_orphans() {
        let existing = "\
[orphan]
a.rs
    -> b.rs : uses

b.rs
";
        let g = graph::to_graph(&graph::parse(existing).unwrap()).unwrap();
        let merged = merge_graph_into_document(existing, &g, &HashSet::new()).unwrap();
        assert!(
            !merged.contains("[orphan]\na.rs\n"),
            "linked node should be un-orphaned: {merged}"
        );
        let merged_graph = graph::to_graph(&graph::parse(&merged).unwrap()).unwrap();
        assert_eq!(merged_graph, g);
    }

    #[test]
    fn merge_graph_into_document_unmarks_linked_orphan_bundle_to_bundle() {
        let existing = "\
[orphan bundle]
src/
    <- consumer.rs : used by

    src/lib.rs

consumer.rs
    -> src/ : used by
";
        let g = graph::to_graph(&graph::parse(existing).unwrap()).unwrap();
        let mut bundled = HashSet::new();
        bundled.insert("src/".to_string());
        let merged = merge_graph_into_document(existing, &g, &bundled).unwrap();
        assert!(
            merged.contains("[bundle]\nsrc/\n"),
            "linked orphan-bundle should keep bundle and drop orphan: {merged}"
        );
        assert!(
            !merged.contains("[orphan bundle]\nsrc/\n"),
            "linked folder should not stay orphan-bundle: {merged}"
        );
        let merged_graph = graph::to_graph(&graph::parse(&merged).unwrap()).unwrap();
        assert_eq!(merged_graph, g);
    }

    #[test]
    fn merge_graph_into_document_marks_unattended_orphans() {
        let existing = "\
src/

    src/file.rs

lonely.rs
";
        let g = graph::to_graph(&graph::parse(existing).unwrap()).unwrap();
        let merged = merge_graph_into_document(existing, &g, &HashSet::new()).unwrap();
        assert!(
            merged.contains("[orphan]\nsrc/\n"),
            "totally orphaned folder should be marked orphan: {merged}"
        );
        assert!(
            merged.contains("    [orphan]\n    src/file.rs\n"),
            "child of orphan folder should be marked orphan: {merged}"
        );
        assert!(
            merged.contains("[orphan]\nlonely.rs\n"),
            "unattended orphan file should be marked [orphan]: {merged}"
        );
        let merged_graph = graph::to_graph(&graph::parse(&merged).unwrap()).unwrap();
        assert_eq!(merged_graph, g);
    }

    #[test]
    fn merge_graph_into_document_applies_bundle_tag_for_bundled_roots() {
        let existing = "\
src/
    <- consumer.rs : used by

    src/lib.rs

consumer.rs
    -> src/ : used by
";
        let g = graph::to_graph(&graph::parse(existing).unwrap()).unwrap();
        let mut bundled = HashSet::new();
        bundled.insert("src/".to_string());
        let merged = merge_graph_into_document(existing, &g, &bundled).unwrap();
        assert!(
            merged.contains("[bundle]\nsrc/\n"),
            "bundled folder should be tagged [bundle]: {merged}"
        );
        let merged_graph = graph::to_graph(&graph::parse(&merged).unwrap()).unwrap();
        assert_eq!(merged_graph, g);
    }
}
