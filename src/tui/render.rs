use std::collections::HashMap;

use ratatui::Frame;
use ratatui::layout::Margin;
use ratatui::layout::{Constraint, Flex, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, BorderType, Borders, Clear, Padding, Paragraph, Wrap};

use crate::graph::model::EdgeKind;

const HEAD_COLORS: [Color; 8] = [
    Color::Cyan,
    Color::Green,
    Color::Yellow,
    Color::Magenta,
    Color::Blue,
    Color::LightRed,
    Color::LightCyan,
    Color::LightGreen,
];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PanelFocus {
    Tree,
    Details,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConnectionFilter {
    Hidden,
    All,
    In,
    Out,
    Undirected,
}

impl ConnectionFilter {
    pub fn next(self) -> Self {
        match self {
            Self::Hidden => Self::All,
            Self::All => Self::In,
            Self::In => Self::Out,
            Self::Out => Self::Undirected,
            Self::Undirected => Self::Hidden,
        }
    }

    pub fn label(self) -> &'static str {
        match self {
            Self::Hidden => "off",
            Self::All => "all",
            Self::In => "in",
            Self::Out => "out",
            Self::Undirected => "ref",
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConnectPreview<'a> {
    pub source: &'a str,
    pub target: &'a str,
    pub kind: EdgeKind,
}

#[derive(Debug, Clone)]
pub struct RenderNode {
    pub path: String,
    pub focused: bool,
}

#[derive(Debug, Clone)]
pub struct RenderEdge {
    pub source: String,
    pub target: String,
    pub kind: EdgeKind,
    pub label: String,
    pub selected: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct Viewport {
    pub x: i32,
    pub y: i32,
    pub zoom: f32,
}

#[derive(Debug)]
pub struct CanvasRenderData<'a> {
    pub nodes: &'a [RenderNode],
    pub edges: &'a [RenderEdge],
    pub viewport: Viewport,
    pub selected_path: &'a str,
    pub selected_edge_count: usize,
    pub mode_label: &'a str,
    pub hints: &'a str,
    pub message: Option<&'a str>,
    pub show_help: bool,
    pub panel_focus: PanelFocus,
    pub locked_path: Option<&'a str>,
    pub details_cursor: usize,
    pub connect_preview: Option<ConnectPreview<'a>>,
    pub collapsed_folders: &'a [String],
    pub bundled_folders: &'a [String],
    pub connection_filter: ConnectionFilter,
    pub delete_mode: bool,
    pub create_mode: bool,
}

#[derive(Debug, Clone)]
struct NodeMeta<'a> {
    node: &'a RenderNode,
    depth: usize,
    basename: String,
    parent_prefix: String,
    ancestor_prefixes: Vec<String>,
}

#[derive(Debug, Clone)]
struct HeadMarker {
    ch: char,
    color: Color,
    selected: bool,
}

#[derive(Debug, Clone)]
struct AnchorTangle<'a> {
    edge: &'a RenderEdge,
    neighbor: &'a str,
    head_path: &'a str,
    marker: char,
    color: Color,
}

#[derive(Debug, Clone, Copy)]
struct ConnectRoute {
    foot_row: usize,
    head_row: usize,
    head_marker: char,
    color: Color,
    highlighted: bool,
}

#[derive(Debug, Clone)]
struct TreeRowText {
    full: String,
    basename_start: usize,
}

pub fn draw(frame: &mut Frame, data: &CanvasRenderData<'_>) {
    let _ = data.viewport.zoom;
    let area = frame.area().inner(Margin {
        horizontal: 3,
        vertical: 1,
    });

    let mode_badge = if data.delete_mode {
        Some(("DELETE", Color::LightRed))
    } else if data.create_mode {
        Some(("CREATE", Color::Green))
    } else {
        None
    };
    let mut title_spans = vec![
        Span::styled("tngl view", Style::default().add_modifier(Modifier::BOLD)),
        Span::raw("  "),
        Span::styled("[?] help", Style::default().fg(Color::DarkGray)),
        Span::raw("  "),
        Span::styled("[q] quit", Style::default().fg(Color::DarkGray)),
    ];
    if let Some((label, color)) = mode_badge {
        title_spans.push(Span::raw("  "));
        title_spans.push(Span::styled(
            format!("[{}]", label),
            Style::default().fg(color).add_modifier(Modifier::BOLD),
        ));
    }
    let title = Line::from(title_spans);
    let canvas_border_color = mode_badge
        .map(|(_, color)| color)
        .unwrap_or(Color::DarkGray);
    let canvas_block = Block::default()
        .borders(Borders::ALL)
        .border_type(BorderType::Double)
        .border_style(Style::default().fg(canvas_border_color))
        .padding(Padding::new(2, 2, 1, 1))
        .title(title);
    let canvas_area = canvas_block.inner(area);
    frame.render_widget(canvas_block, area);

    let [panes_area, _gap, status_area] = Layout::vertical([
        Constraint::Min(6),
        Constraint::Length(1),
        Constraint::Length(4),
    ])
    .areas(canvas_area);

    let sorted_nodes = sort_nodes(data.nodes);
    let row_by_path = sorted_nodes
        .iter()
        .enumerate()
        .map(|(idx, n)| (n.path.as_str(), idx))
        .collect::<HashMap<_, _>>();

    let anchor = data
        .connect_preview
        .as_ref()
        .map(|p| p.source)
        .or(data.locked_path)
        .or((data.selected_path != "—").then_some(data.selected_path));
    let anchor_tangles = collect_anchor_tangles(anchor, data.edges);
    let mut heads: HashMap<usize, Vec<HeadMarker>> = HashMap::new();
    let mut connect_routes: Vec<ConnectRoute> = Vec::new();

    if let Some(preview) = data.connect_preview.as_ref() {
        // Connecting mode: preview route.
        let (head_path, foot_path, marker, draw_route) = connect_head_foot(preview);
        if draw_route {
            if let (Some(&foot_row), Some(&head_row)) =
                (row_by_path.get(foot_path), row_by_path.get(head_path))
            {
                if foot_row != head_row {
                    connect_routes.push(ConnectRoute {
                        foot_row,
                        head_row,
                        head_marker: marker,
                        color: Color::Green,
                        highlighted: data.create_mode,
                    });
                }
            }
        } else if let Some(&head_row) = row_by_path.get(head_path) {
            heads.entry(head_row).or_default().push(HeadMarker {
                ch: marker,
                color: Color::Green,
                selected: true,
            });
        }
    } else if data.panel_focus == PanelFocus::Details && data.locked_path.is_some() {
        // Detail mode: only the selected tangle, as an L-shaped route.
        if let Some(tangle) = anchor_tangles.get(data.details_cursor) {
            let anchor_path = data.locked_path.unwrap();
            let (foot_path, head_path) = if tangle.head_path == anchor_path {
                (tangle.neighbor, anchor_path)
            } else {
                (anchor_path, tangle.head_path)
            };
            if let (Some(&foot_row), Some(&head_row)) =
                (row_by_path.get(foot_path), row_by_path.get(head_path))
            {
                if foot_row != head_row {
                    connect_routes.push(ConnectRoute {
                        foot_row,
                        head_row,
                        head_marker: tangle.marker,
                        color: tangle.color,
                        highlighted: false,
                    });
                } else {
                    heads.entry(head_row).or_default().push(HeadMarker {
                        ch: tangle.marker,
                        color: tangle.color,
                        selected: true,
                    });
                }
            }
        }
    } else if !data.delete_mode
        && data.connection_filter != ConnectionFilter::Hidden
        && data.panel_focus == PanelFocus::Tree
        && anchor.is_some()
    {
        // Tree browse mode with connection filter: show filtered tangles as stacked routes.
        let anchor_path = anchor.unwrap();
        for tangle in &anchor_tangles {
            let relation = relation_label(anchor_path, tangle.edge);
            let include = match data.connection_filter {
                ConnectionFilter::All => true,
                ConnectionFilter::In => relation == "in",
                ConnectionFilter::Out => relation == "out",
                ConnectionFilter::Undirected => relation == "ref",
                ConnectionFilter::Hidden => false,
            };
            if !include {
                continue;
            }
            let (foot_path, head_path) = if tangle.head_path == anchor_path {
                (tangle.neighbor, anchor_path)
            } else {
                (anchor_path, tangle.head_path)
            };
            if let (Some(&foot_row), Some(&head_row)) =
                (row_by_path.get(foot_path), row_by_path.get(head_path))
            {
                if foot_row != head_row {
                    connect_routes.push(ConnectRoute {
                        foot_row,
                        head_row,
                        head_marker: tangle.marker,
                        color: tangle.color,
                        highlighted: false,
                    });
                }
            }
        }
    } else if data.delete_mode && anchor.is_some() {
        // Delete mode: always surface only the selected tangle with strong emphasis.
        let anchor_path = anchor.unwrap();
        if let Some(tangle) = anchor_tangles.iter().find(|t| t.edge.selected) {
            let (foot_path, head_path) = if tangle.head_path == anchor_path {
                (tangle.neighbor, anchor_path)
            } else {
                (anchor_path, tangle.head_path)
            };
            if let (Some(&foot_row), Some(&head_row)) =
                (row_by_path.get(foot_path), row_by_path.get(head_path))
            {
                if foot_row != head_row {
                    connect_routes.push(ConnectRoute {
                        foot_row,
                        head_row,
                        head_marker: tangle.marker,
                        color: Color::LightRed,
                        highlighted: true,
                    });
                } else {
                    heads.entry(head_row).or_default().push(HeadMarker {
                        ch: tangle.marker,
                        color: Color::LightRed,
                        selected: true,
                    });
                }
            }
        }
    } else {
        // Default: all tangles as head markers.
        heads = collect_heads(&anchor_tangles, &row_by_path);
    }

    let [tree_outer, _, details_outer] = Layout::horizontal([
        Constraint::Percentage(57),
        Constraint::Length(2),
        Constraint::Fill(1),
    ])
    .areas(panes_area);

    let tree_focused = data.panel_focus == PanelFocus::Tree;
    let mut tree_border_style = if data.delete_mode {
        Style::default().fg(Color::LightRed)
    } else if data.create_mode {
        Style::default().fg(Color::Green)
    } else if tree_focused {
        Style::default().fg(Color::White)
    } else {
        Style::default().fg(Color::DarkGray)
    };
    if tree_focused && (data.delete_mode || data.create_mode) {
        tree_border_style = tree_border_style.add_modifier(Modifier::BOLD);
    }
    let tree_title_style = if tree_focused || data.delete_mode || data.create_mode {
        Style::default()
            .fg(Color::White)
            .add_modifier(Modifier::BOLD | Modifier::UNDERLINED)
    } else {
        Style::default()
            .fg(Color::Gray)
            .add_modifier(Modifier::BOLD)
    };
    let tree_block = Block::default()
        .borders(Borders::ALL)
        .border_type(BorderType::Rounded)
        .border_style(tree_border_style)
        .title(Line::from(vec![
            Span::styled("TREE", tree_title_style),
            Span::raw("  "),
            Span::styled(
                format!("{} nodes", sorted_nodes.len()),
                Style::default().fg(Color::DarkGray),
            ),
        ]));
    let tree_inner = tree_block.inner(tree_outer);
    frame.render_widget(tree_block, tree_outer);

    let tree_lines = build_tree_lines(
        &sorted_nodes,
        data,
        &heads,
        tree_inner.width,
        &connect_routes,
    );
    let tree = Paragraph::new(tree_lines)
        .scroll((data.viewport.y.max(0) as u16, data.viewport.x.max(0) as u16))
        .wrap(Wrap { trim: false });
    frame.render_widget(tree, tree_inner);

    let details_focused = data.panel_focus == PanelFocus::Details;
    let mut details_border_style = if data.delete_mode {
        Style::default().fg(Color::LightRed)
    } else if data.create_mode {
        Style::default().fg(Color::Green)
    } else if details_focused {
        Style::default().fg(Color::White)
    } else {
        Style::default().fg(Color::DarkGray)
    };
    if details_focused && (data.delete_mode || data.create_mode) {
        details_border_style = details_border_style.add_modifier(Modifier::BOLD);
    }
    let details_title_style = if details_focused || data.delete_mode || data.create_mode {
        Style::default()
            .fg(Color::White)
            .add_modifier(Modifier::BOLD | Modifier::UNDERLINED)
    } else {
        Style::default()
            .fg(Color::Gray)
            .add_modifier(Modifier::BOLD)
    };
    let details_block = Block::default()
        .borders(Borders::ALL)
        .border_type(BorderType::Rounded)
        .border_style(details_border_style)
        .title(Line::from(vec![
            Span::styled("DETAIL TREE", details_title_style),
            Span::raw("  "),
            Span::styled(data.mode_label, Style::default().fg(Color::DarkGray)),
        ]));
    let details_inner = details_block.inner(details_outer);
    frame.render_widget(details_block, details_outer);

    let details_lines = build_details_lines(data, &sorted_nodes, &anchor_tangles);
    let details = Paragraph::new(details_lines).wrap(Wrap { trim: false });
    frame.render_widget(details, details_inner);

    let top_status = format!(
        "SELECTED: {}   edges: {}",
        data.selected_path, data.selected_edge_count
    );
    let mut hint_line = data.hints.to_string();
    if let Some(msg) = data.message {
        hint_line.push_str("   ");
        hint_line.push_str(msg);
    }

    let status = Paragraph::new(vec![
        Line::from(Span::styled(
            top_status,
            Style::default()
                .fg(if data.delete_mode {
                    Color::LightRed
                } else if data.create_mode {
                    Color::Green
                } else {
                    Color::Cyan
                })
                .add_modifier(Modifier::BOLD),
        )),
        Line::from(Span::styled(
            hint_line,
            Style::default().fg(Color::DarkGray),
        )),
    ])
    .block(
        Block::default()
            .borders(Borders::ALL)
            .border_type(BorderType::Rounded)
            .border_style(Style::default().fg(if data.delete_mode {
                Color::LightRed
            } else if data.create_mode {
                Color::Green
            } else {
                Color::DarkGray
            }))
            .padding(Padding::new(1, 1, 0, 0)),
    );
    frame.render_widget(status, status_area);

    if data.show_help {
        render_help_overlay(frame);
    }
}

fn sort_nodes(nodes: &[RenderNode]) -> Vec<&RenderNode> {
    let mut out = nodes.iter().collect::<Vec<_>>();
    out.sort_by_key(|n| tree_sort_key(&n.path));
    out
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

fn build_tree_lines(
    sorted_nodes: &[&RenderNode],
    data: &CanvasRenderData<'_>,
    heads: &HashMap<usize, Vec<HeadMarker>>,
    tree_width: u16,
    connect_routes: &[ConnectRoute],
) -> Vec<Line<'static>> {
    if sorted_nodes.is_empty() {
        return vec![Line::from(Span::styled(
            "No nodes to display.",
            Style::default().fg(Color::DarkGray),
        ))];
    }

    let meta = sorted_nodes
        .iter()
        .map(|n| build_meta(n))
        .collect::<Vec<_>>();
    let mut lines = Vec::with_capacity(meta.len());
    let total_width = tree_width.max(1) as usize;

    // Pre-pass: assign a right_col lane to each route.
    // Routes are processed shortest-span first so shorter routes sit in inner (closer) lanes.
    let mut sorted_routes: Vec<&ConnectRoute> = connect_routes.iter().collect();
    sorted_routes.sort_by_key(|r| r.foot_row.max(r.head_row) - r.foot_row.min(r.head_row));

    let mut route_layout: Vec<(&ConnectRoute, usize)> = Vec::with_capacity(sorted_routes.len());
    for route in &sorted_routes {
        let low = route.foot_row.min(route.head_row);
        let high = route.foot_row.max(route.head_row);

        // Content-based minimum: widest row in this route's span plus per-row overhead.
        let mut max_col = 0usize;
        for (idx, m) in meta.iter().enumerate() {
            if idx < low || idx > high {
                continue;
            }
            let text = row_text_for(
                m,
                &meta,
                idx,
                data.locked_path,
                data.collapsed_folders,
                data.bundled_folders,
            );
            let marker_count = head_char_count(heads.get(&idx));
            let marker_gap = usize::from(marker_count > 0);
            let full_left = text.full.chars().count() + marker_gap + marker_count;
            // Endpoints need " ○ " + corner = 4; middles need "│" = 1.
            let overhead = if idx == route.foot_row || idx == route.head_row {
                4
            } else {
                1
            };
            max_col = max_col.max(full_left + overhead);
        }

        // Must sit to the right of all overlapping inner routes (+4 for endpoint clearance).
        let inner_max = route_layout
            .iter()
            .filter(|(inner, _)| {
                let il = inner.foot_row.min(inner.head_row);
                let ih = inner.foot_row.max(inner.head_row);
                low <= ih && il <= high
            })
            .map(|(_, col)| col + 4)
            .max()
            .unwrap_or(0);

        let right_col = max_col.max(inner_max).min(total_width.saturating_sub(1));
        route_layout.push((route, right_col));
    }
    // Sort by right_col so rendering goes inner-to-outer.
    route_layout.sort_by_key(|(_, col)| *col);

    // Main pass
    for (idx, m) in meta.iter().enumerate() {
        let row = row_text_for(
            m,
            &meta,
            idx,
            data.locked_path,
            data.collapsed_folders,
            data.bundled_folders,
        );

        let (scaffold_style, basename_style) = if m.node.focused {
            let focused_basename = if data.panel_focus == PanelFocus::Tree {
                Style::default()
                    .fg(Color::Black)
                    .bg(Color::Yellow)
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default()
                    .fg(Color::Black)
                    .bg(Color::Gray)
                    .add_modifier(Modifier::BOLD)
            };
            (Style::default().fg(Color::White), focused_basename)
        } else if m.node.path.ends_with('/') {
            (
                Style::default().fg(Color::White),
                Style::default().fg(Color::Cyan),
            )
        } else {
            (
                Style::default().fg(Color::White),
                Style::default().fg(Color::White),
            )
        };

        // Find all routes active in this row (range includes this row), inner-to-outer.
        let active: Vec<(&ConnectRoute, usize, bool)> = route_layout
            .iter()
            .filter_map(|(route, col)| {
                let low = route.foot_row.min(route.head_row);
                let high = route.foot_row.max(route.head_row);
                if idx >= low && idx <= high {
                    let is_endpoint = idx == route.foot_row || idx == route.head_row;
                    Some((*route, *col, is_endpoint))
                } else {
                    None
                }
            })
            .collect();

        let marker_count = head_char_count(heads.get(&idx));
        let marker_gap = usize::from(marker_count > 0);

        // Text budget: governed by the innermost active route.
        let text_budget = if let Some((_, col, is_endpoint)) = active.first() {
            if *is_endpoint {
                col.saturating_sub(4 + marker_count + marker_gap)
            } else {
                col.saturating_sub(1 + marker_count + marker_gap)
            }
        } else {
            total_width.saturating_sub(marker_count + marker_gap)
        };

        let row_text = truncate_text(&row.full, text_budget);
        let text_len = row_text.chars().count();
        let mut current_col = text_len + marker_gap + marker_count;

        let mut spans = Vec::new();
        spans.extend(spans_for_tree_row_text(
            &row_text,
            row.basename_start,
            scaffold_style,
            basename_style,
        ));
        if marker_count > 0 {
            spans.push(Span::styled(" ", scaffold_style));
            spans.extend(head_spans(heads.get(&idx)));
        }

        // Render active routes from inner (small col) to outer (large col).
        for (route, col, is_endpoint) in &active {
            let style = if route.highlighted {
                Style::default()
                    .fg(route.color)
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default()
                    .fg(route.color)
                    .add_modifier(Modifier::BOLD)
            };
            if *is_endpoint {
                let marker_ch = if idx == route.foot_row {
                    '○'
                } else {
                    route.head_marker
                };
                let corner = if idx == route.foot_row.min(route.head_row) {
                    '╮'
                } else {
                    '╯'
                };
                let dash_count = col.saturating_sub(current_col + 3);
                spans.push(Span::styled(
                    format!(" {} {}{}", marker_ch, "─".repeat(dash_count), corner),
                    style,
                ));
                current_col = col + 1;
            } else {
                let space_count = col.saturating_sub(current_col);
                spans.push(Span::styled(format!("{}│", " ".repeat(space_count)), style));
                current_col = col + 1;
            }
        }

        lines.push(Line::from(spans));
    }

    lines
}

fn row_text_for(
    m: &NodeMeta<'_>,
    meta: &[NodeMeta<'_>],
    idx: usize,
    locked_path: Option<&str>,
    collapsed_folders: &[String],
    bundled_folders: &[String],
) -> TreeRowText {
    let mut prefix = String::new();
    for level in 0..m.depth {
        if has_future_in_ancestor(meta, idx, level) {
            prefix.push_str("│  ");
        } else {
            prefix.push_str("   ");
        }
    }
    let branch = if m.depth == 0 {
        String::new()
    } else if has_later_sibling(meta, idx) {
        "├─ ".to_string()
    } else {
        "└─ ".to_string()
    };
    let fold = if m.node.path.ends_with('/') {
        if is_bundled_folder(m.node.path.as_str(), bundled_folders) {
            "▣ "
        } else if is_collapsed_folder(m.node.path.as_str(), collapsed_folders) {
            "▸ "
        } else {
            "▾ "
        }
    } else {
        ""
    };
    let lock = if locked_path == Some(m.node.path.as_str()) {
        '◆'
    } else {
        ' '
    };
    let scaffold = format!("{lock} {prefix}{branch}{fold}");
    let basename_start = scaffold.chars().count();
    let mut full = scaffold;
    full.push_str(&m.basename);
    TreeRowText {
        full,
        basename_start,
    }
}

fn spans_for_tree_row_text(
    text: &str,
    basename_start: usize,
    scaffold_style: Style,
    basename_style: Style,
) -> Vec<Span<'static>> {
    if text.is_empty() {
        return Vec::new();
    }
    let mut spans = Vec::new();
    let mut buf = String::new();
    let mut in_basename = false;
    for (idx, ch) in text.chars().enumerate() {
        let next_in_basename = idx >= basename_start;
        if idx == 0 {
            in_basename = next_in_basename;
        } else if next_in_basename != in_basename {
            let style = if in_basename {
                basename_style
            } else {
                scaffold_style
            };
            spans.push(Span::styled(std::mem::take(&mut buf), style));
            in_basename = next_in_basename;
        }
        buf.push(ch);
    }
    if !buf.is_empty() {
        let style = if in_basename {
            basename_style
        } else {
            scaffold_style
        };
        spans.push(Span::styled(buf, style));
    }
    spans
}

fn is_collapsed_folder(path: &str, collapsed_folders: &[String]) -> bool {
    collapsed_folders.iter().any(|f| f == path)
}

fn is_bundled_folder(path: &str, bundled_folders: &[String]) -> bool {
    bundled_folders.iter().any(|f| f == path)
}

fn head_spans(markers: Option<&Vec<HeadMarker>>) -> Vec<Span<'static>> {
    let mut spans = Vec::new();
    let Some(markers) = markers else {
        return spans;
    };

    for marker in markers.iter().take(3) {
        let style = if marker.selected {
            Style::default()
                .fg(marker.color)
                .add_modifier(Modifier::BOLD)
        } else {
            Style::default()
                .fg(marker.color)
                .add_modifier(Modifier::BOLD)
        };
        spans.push(Span::styled(format!("{}", marker.ch), style));
    }
    if markers.len() > 3 {
        spans.push(Span::styled(
            "+",
            Style::default()
                .fg(Color::DarkGray)
                .add_modifier(Modifier::BOLD),
        ));
    }
    spans
}

fn head_char_count(markers: Option<&Vec<HeadMarker>>) -> usize {
    match markers {
        None => 0,
        Some(m) if m.is_empty() => 0,
        Some(m) => {
            let shown = m.len().min(3);
            shown + usize::from(m.len() > 3)
        }
    }
}

fn collect_heads(
    tangles: &[AnchorTangle<'_>],
    row_by_path: &HashMap<&str, usize>,
) -> HashMap<usize, Vec<HeadMarker>> {
    let mut out: HashMap<usize, Vec<HeadMarker>> = HashMap::new();
    for tangle in tangles {
        let Some(row) = row_by_path.get(tangle.head_path).copied() else {
            continue;
        };
        out.entry(row).or_default().push(HeadMarker {
            ch: tangle.marker,
            color: tangle.color,
            selected: tangle.edge.selected,
        });
    }
    out
}

fn collect_anchor_tangles<'a>(
    anchor: Option<&'a str>,
    edges: &'a [RenderEdge],
) -> Vec<AnchorTangle<'a>> {
    let Some(anchor) = anchor else {
        return Vec::new();
    };

    let mut items = Vec::new();
    for (order_idx, edge) in edges.iter().enumerate() {
        if edge.source == anchor {
            let (head_path, marker) = head_for_edge_from_anchor(anchor, edge);
            items.push((order_idx, edge, edge.target.as_str(), head_path, marker));
        } else if edge.target == anchor {
            let (head_path, marker) = head_for_edge_to_anchor(anchor, edge);
            items.push((order_idx, edge, edge.source.as_str(), head_path, marker));
        }
    }

    items.sort_by(|a, b| {
        a.2.cmp(b.2)
            .then_with(|| head_sort_rank(a.4).cmp(&head_sort_rank(b.4)))
            .then_with(|| a.1.source.cmp(&b.1.source))
            .then_with(|| a.1.target.cmp(&b.1.target))
            .then_with(|| a.1.label.cmp(&b.1.label))
            .then_with(|| a.0.cmp(&b.0))
    });

    items
        .into_iter()
        .enumerate()
        .map(
            |(idx, (_, edge, neighbor, head_path, marker))| AnchorTangle {
                edge,
                neighbor,
                head_path,
                marker,
                color: HEAD_COLORS[idx % HEAD_COLORS.len()],
            },
        )
        .collect()
}

fn head_for_edge_from_anchor<'a>(anchor: &'a str, edge: &'a RenderEdge) -> (&'a str, char) {
    match edge.kind {
        EdgeKind::Directed => (edge.target.as_str(), directed_head_marker()),
        EdgeKind::Incoming => (anchor, directed_head_marker()),
        EdgeKind::Undirected => (edge.target.as_str(), undirected_marker()),
    }
}

fn head_for_edge_to_anchor<'a>(anchor: &'a str, edge: &'a RenderEdge) -> (&'a str, char) {
    match edge.kind {
        EdgeKind::Directed => (anchor, directed_head_marker()),
        EdgeKind::Incoming => (edge.source.as_str(), directed_head_marker()),
        EdgeKind::Undirected => (edge.source.as_str(), undirected_marker()),
    }
}

fn connect_head_foot<'a>(preview: &ConnectPreview<'a>) -> (&'a str, &'a str, char, bool) {
    match preview.kind {
        EdgeKind::Directed => (preview.target, preview.source, directed_head_marker(), true),
        EdgeKind::Incoming => (preview.source, preview.target, directed_head_marker(), true),
        EdgeKind::Undirected => (
            preview.target,
            preview.source,
            '○', // both ends use the foot symbol
            true,
        ),
    }
}

fn directed_head_marker() -> char {
    '◀'
}

fn undirected_marker() -> char {
    '◆'
}

fn relation_label(anchor: &str, edge: &RenderEdge) -> &'static str {
    match edge.kind {
        EdgeKind::Undirected => "ref",
        EdgeKind::Directed => {
            if edge.source == anchor {
                "out"
            } else {
                "in"
            }
        }
        EdgeKind::Incoming => {
            if edge.source == anchor {
                "in"
            } else {
                "out"
            }
        }
    }
}

fn marker_for_relation(relation: &str) -> char {
    match relation {
        "out" => '▶',
        "in" => '◀',
        _ => undirected_marker(),
    }
}

fn head_sort_rank(ch: char) -> u8 {
    match ch {
        '◀' => 0,
        '◆' => 1,
        _ => 2,
    }
}

fn truncate_text(text: &str, max_width: usize) -> String {
    if text.chars().count() <= max_width {
        return text.to_string();
    }
    if max_width <= 3 {
        return text.chars().take(max_width).collect();
    }
    let mut out = text
        .chars()
        .take(max_width.saturating_sub(3))
        .collect::<String>();
    out.push_str("...");
    out
}

fn build_details_lines(
    data: &CanvasRenderData<'_>,
    _sorted_nodes: &[&RenderNode],
    tangles: &[AnchorTangle<'_>],
) -> Vec<Line<'static>> {
    if let Some(preview) = data.connect_preview.as_ref() {
        let relation = match preview.kind {
            EdgeKind::Directed => "out",
            EdgeKind::Incoming => "in",
            EdgeKind::Undirected => "ref",
        };
        return vec![
            Line::from(Span::styled(
                "CREATE",
                Style::default()
                    .fg(Color::Green)
                    .add_modifier(Modifier::BOLD),
            )),
            Line::from(Span::styled(
                "New Tangle Draft",
                Style::default()
                    .fg(Color::Green)
                    .add_modifier(Modifier::BOLD),
            )),
            Line::from(""),
            Line::from(format!("source: {}", preview.source)),
            Line::from(format!("target: {}", preview.target)),
            Line::from(""),
            Line::from(Span::styled(
                "Type",
                Style::default()
                    .fg(Color::Green)
                    .add_modifier(Modifier::BOLD),
            )),
            Line::from(format!("• {}", relation)),
            Line::from(""),
            Line::from(Span::styled(
                "Controls",
                Style::default()
                    .fg(Color::Green)
                    .add_modifier(Modifier::BOLD),
            )),
            Line::from("• Up/Down: pick target"),
            Line::from("• Left/Right: switch type"),
            Line::from("• Enter: open comment popup"),
            Line::from("• Esc/Backspace: cancel"),
        ];
    }

    let anchor = data
        .locked_path
        .or((data.selected_path != "—").then_some(data.selected_path));
    if let Some(anchor_path) = anchor {
        let parts = path_parts(anchor_path);
        let depth = parts.len().saturating_sub(1);
        let parent = parent_path(anchor_path).unwrap_or_else(|| "(root)".to_string());
        let locked = data.locked_path == Some(anchor_path);

        let mut lines = vec![
            Line::from(Span::styled(
                if locked {
                    "Details Node"
                } else {
                    "Focused Node"
                },
                Style::default()
                    .fg(Color::Cyan)
                    .add_modifier(Modifier::BOLD),
            )),
            Line::from(Span::styled(
                anchor_path.to_string(),
                Style::default().add_modifier(Modifier::BOLD),
            )),
            Line::from(Span::styled(
                format!("depth: {}   parent: {}", depth, parent),
                Style::default().fg(Color::DarkGray),
            )),
            Line::from(""),
            Line::from(Span::styled(
                format!("Tangles ({})", tangles.len()),
                Style::default()
                    .fg(Color::Cyan)
                    .add_modifier(Modifier::BOLD),
            )),
        ];

        if tangles.is_empty() {
            lines.push(Line::from(Span::styled(
                "  (none)",
                Style::default().fg(Color::DarkGray),
            )));
        } else {
            for (idx, tangle) in tangles.iter().enumerate() {
                let cursor_active =
                    locked && data.panel_focus == PanelFocus::Details && data.details_cursor == idx;
                let edge_selected = tangle.edge.selected;
                let highlight_selected = edge_selected && data.delete_mode;
                let active = cursor_active || edge_selected;
                let prefix = if active { ">" } else { " " };
                let style = if highlight_selected {
                    Style::default()
                        .fg(Color::LightRed)
                        .add_modifier(Modifier::BOLD)
                } else if cursor_active || edge_selected {
                    Style::default().fg(Color::Black).bg(Color::Yellow)
                } else {
                    Style::default()
                        .fg(tangle.color)
                        .add_modifier(Modifier::BOLD)
                };
                let relation = relation_label(anchor_path, tangle.edge);
                let marker = marker_for_relation(relation);
                lines.push(Line::from(Span::styled(
                    format!("{} {} [{}] {}", prefix, marker, relation, tangle.neighbor),
                    style,
                )));
                if !tangle.edge.label.trim().is_empty() {
                    let label_style = if highlight_selected {
                        Style::default()
                            .fg(Color::LightRed)
                            .add_modifier(Modifier::BOLD)
                    } else if edge_selected {
                        Style::default().fg(Color::Yellow)
                    } else {
                        Style::default().fg(Color::DarkGray)
                    };
                    lines.push(Line::from(Span::styled(
                        format!("    {}", tangle.edge.label.trim()),
                        label_style,
                    )));
                }
            }
        }

        lines.push(Line::from(""));
        if locked {
            let create_idx = tangles.len();
            let create_active =
                data.panel_focus == PanelFocus::Details && data.details_cursor == create_idx;
            let create_style = if create_active {
                Style::default().fg(Color::Black).bg(Color::Yellow)
            } else {
                Style::default()
                    .fg(Color::Green)
                    .add_modifier(Modifier::BOLD)
            };
            lines.push(Line::from(Span::styled(
                format!(
                    "{} + Create new tangle",
                    if create_active { ">" } else { " " }
                ),
                create_style,
            )));
            lines.push(Line::from(""));
            lines.push(Line::from(Span::styled(
                "Enter reroutes edge; on '+' row it starts CREATE. Esc/Backspace returns to tree.",
                Style::default().fg(Color::DarkGray),
            )));
        } else {
            lines.push(Line::from(Span::styled(
                "Press Enter for details and tangle editing.",
                Style::default().fg(Color::DarkGray),
            )));
        }
        return lines;
    }

    vec![
        Line::from(Span::styled(
            "No Node Selected",
            Style::default()
                .fg(Color::Cyan)
                .add_modifier(Modifier::BOLD),
        )),
        Line::from(""),
        Line::from("Move in tree with j/k or arrow keys."),
    ]
}

fn build_meta(node: &RenderNode) -> NodeMeta<'_> {
    let parts = path_parts(&node.path);
    let depth = parts.len().saturating_sub(1);
    let basename = if parts.is_empty() {
        node.path.clone()
    } else if node.path.ends_with('/') {
        format!("{}/", parts.last().map(String::as_str).unwrap_or(""))
    } else {
        parts.last().map(String::as_str).unwrap_or("").to_string()
    };

    let mut ancestor_prefixes = Vec::new();
    if parts.len() >= 2 {
        for end in 1..parts.len() {
            ancestor_prefixes.push(format!("{}/", parts[..end].join("/")));
        }
    }

    let parent_prefix = if depth == 0 {
        String::new()
    } else {
        format!("{}/", parts[..depth].join("/"))
    };

    NodeMeta {
        node,
        depth,
        basename,
        parent_prefix,
        ancestor_prefixes,
    }
}

fn has_later_sibling(meta: &[NodeMeta<'_>], idx: usize) -> bool {
    let me = &meta[idx];
    meta.iter()
        .skip(idx + 1)
        .any(|n| n.depth == me.depth && n.parent_prefix == me.parent_prefix)
}

fn has_future_in_ancestor(meta: &[NodeMeta<'_>], idx: usize, level: usize) -> bool {
    let me = &meta[idx];
    let Some(prefix) = me.ancestor_prefixes.get(level) else {
        return false;
    };
    meta.iter()
        .skip(idx + 1)
        .any(|n| n.node.path.starts_with(prefix.as_str()))
}

fn parent_path(path: &str) -> Option<String> {
    let trimmed = path.trim_end_matches('/');
    let (parent, _) = trimmed.rsplit_once('/')?;
    if parent.is_empty() {
        Some("/".to_string())
    } else {
        Some(format!("{}/", parent))
    }
}

fn path_parts(path: &str) -> Vec<String> {
    let trimmed = path.trim_end_matches('/');
    if trimmed.is_empty() {
        return Vec::new();
    }
    trimmed.split('/').map(ToString::to_string).collect()
}

fn render_help_overlay(frame: &mut Frame) {
    let area = centered_rect(frame.area(), 84, 70);
    frame.render_widget(Clear, area);
    let help = Paragraph::new(vec![
        Line::from("TREE (left): tree view + edge heads only"),
        Line::from("  ▶ out   ◀ in   ◆ ref"),
        Line::from(""),
        Line::from("FLOW"),
        Line::from("  1) Up/down (j/k or arrows) to focus a node"),
        Line::from("     z folds/unfolds focused folder, b bundles/locks folder"),
        Line::from("  2) Enter opens details for focused node"),
        Line::from("  3) In details: Enter reroutes edge; '+ Create new tangle' enters CREATE"),
        Line::from("  4) Create: up/down target, left/right type, Enter"),
        Line::from("  5) Type comment in details, Enter to finalize"),
        Line::from(""),
        Line::from("Esc/Backspace backs out one step."),
    ])
    .block(Block::default().title("Help").borders(Borders::ALL));
    frame.render_widget(help, area);
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
