use std::collections::HashSet;
use std::fmt::Write as _;
use std::fs;
use std::path::Path;
use std::process::{Command, Stdio};

use anyhow::{Context, Result, bail};

use crate::graph::model::{EdgeKind, Graph};
use crate::parser::graph;
use crate::scanner::tree;
use crate::tangle;

const EXPORT_FILENAME: &str = "map.html";
const HEAD_COLORS_HEX: [&str; 8] = [
    "#22d3ee", "#4ade80", "#facc15", "#f472b6", "#60a5fa", "#fb7185", "#67e8f9", "#86efac",
];

#[derive(Debug, Clone, PartialEq, Eq)]
struct ExportNode {
    path: String,
    scaffold: String,
    basename: String,
    depth: usize,
    parent: String,
    is_dir: bool,
    bundled: bool,
    tangles: Vec<ExportTangle>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ExportTangle {
    neighbor: String,
    relation: &'static str,
    marker: char,
    comment: String,
    head_path: String,
}

#[derive(Debug, Clone)]
struct NodeMeta {
    path: String,
    depth: usize,
    basename: String,
    parent_prefix: String,
    ancestor_prefixes: Vec<String>,
    is_dir: bool,
}

pub fn run() -> Result<()> {
    let root = tangle::find_root()?;
    let graph_path = tangle::graph_path(&root);
    let graph_text = fs::read_to_string(&graph_path)
        .with_context(|| format!("failed to read {}", graph_path.display()))?;
    let doc = graph::parse(&graph_text)
        .with_context(|| format!("failed to parse {}", graph_path.display()))?;
    let graph_model = graph::to_graph(&doc)?;
    let bundled_roots = graph::collapsed_subtree_roots(&doc);
    let scanned_paths = tree::scan(&root)?;
    let visible_paths = scanned_paths.into_iter().collect::<HashSet<_>>();

    let nodes =
        build_export_nodes_with_visibility(&graph_model, &bundled_roots, Some(&visible_paths));
    let html = render_html(&nodes);

    let out_path = tangle::tangle_dir(&root).join(EXPORT_FILENAME);
    fs::write(&out_path, html)
        .with_context(|| format!("failed to write {}", out_path.display()))?;
    println!("  Exported static HTML to {}", out_path.display());
    if let Err(err) = try_open_in_browser(&out_path) {
        eprintln!("  Warning: could not open browser automatically: {err}");
    }
    Ok(())
}

#[cfg(target_os = "macos")]
fn try_open_in_browser(path: &Path) -> Result<()> {
    let status = Command::new("open")
        .arg(path)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .with_context(|| "failed to run `open`")?;
    if status.success() {
        Ok(())
    } else {
        bail!("`open` exited with status {status}")
    }
}

#[cfg(target_os = "windows")]
fn try_open_in_browser(path: &Path) -> Result<()> {
    let status = Command::new("cmd")
        .arg("/C")
        .arg("start")
        .arg("")
        .arg(path)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .with_context(|| "failed to run `cmd /C start`")?;
    if status.success() {
        Ok(())
    } else {
        bail!("`cmd /C start` exited with status {status}")
    }
}

#[cfg(not(any(target_os = "macos", target_os = "windows")))]
fn try_open_in_browser(path: &Path) -> Result<()> {
    let status = Command::new("xdg-open")
        .arg(path)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .with_context(|| "failed to run `xdg-open`")?;
    if status.success() {
        Ok(())
    } else {
        bail!("`xdg-open` exited with status {status}")
    }
}

fn build_export_nodes_with_visibility(
    graph: &Graph,
    bundled_roots: &HashSet<String>,
    visible_paths: Option<&HashSet<String>>,
) -> Vec<ExportNode> {
    let mut sorted_paths = graph
        .nodes
        .iter()
        .map(|n| n.path.as_str())
        .collect::<Vec<_>>();
    sorted_paths.sort_by_key(|path| tree_sort_key(path));
    sorted_paths.retain(|path| path_is_visible(path, visible_paths));
    sorted_paths.retain(|path| !hidden_by_bundle(path, bundled_roots));

    let meta = sorted_paths
        .iter()
        .map(|path| build_meta(path))
        .collect::<Vec<_>>();

    sorted_paths
        .iter()
        .enumerate()
        .map(|(idx, path)| {
            let m = &meta[idx];
            let (scaffold, basename) = tree_row_parts(&meta, idx, bundled_roots);
            let parent = parent_path(path).unwrap_or_else(|| "(root)".to_string());

            ExportNode {
                path: (*path).to_string(),
                scaffold,
                basename,
                depth: m.depth,
                parent,
                is_dir: path.ends_with('/'),
                bundled: bundled_roots.contains(*path),
                tangles: tangles_for_anchor_with_visibility(graph, path, visible_paths),
            }
        })
        .collect()
}

fn tangles_for_anchor_with_visibility(
    graph: &Graph,
    anchor: &str,
    visible_paths: Option<&HashSet<String>>,
) -> Vec<ExportTangle> {
    let mut items: Vec<(String, u8, usize, ExportTangle)> = Vec::new();
    let mut order_idx: usize = 0;

    for node in &graph.nodes {
        for edge in &node.edges {
            let source = node.path.as_str();
            let target = edge.target.as_str();
            if source != anchor && target != anchor {
                order_idx += 1;
                continue;
            }
            if !path_is_visible(source, visible_paths) || !path_is_visible(target, visible_paths) {
                order_idx += 1;
                continue;
            }

            let (neighbor, relation, head_path) = if source == anchor {
                let relation = relation_label(anchor, source, edge.kind.clone());
                let head_path = head_path_from_anchor(anchor, target, edge.kind.clone());
                (target.to_string(), relation, head_path)
            } else {
                let relation = relation_label(anchor, source, edge.kind.clone());
                let head_path = head_path_to_anchor(anchor, source, edge.kind.clone());
                (source.to_string(), relation, head_path)
            };

            items.push((
                neighbor.clone(),
                relation_sort_rank(relation),
                order_idx,
                ExportTangle {
                    neighbor,
                    relation,
                    marker: marker_for_relation(relation),
                    comment: edge.label.trim().to_string(),
                    head_path,
                },
            ));
            order_idx += 1;
        }
    }

    items.sort_by(|a, b| {
        a.0.cmp(&b.0)
            .then_with(|| a.1.cmp(&b.1))
            .then_with(|| a.2.cmp(&b.2))
    });
    items.into_iter().map(|(_, _, _, t)| t).collect()
}

fn path_is_visible(path: &str, visible_paths: Option<&HashSet<String>>) -> bool {
    visible_paths.map(|set| set.contains(path)).unwrap_or(true)
}

fn relation_label(anchor: &str, source: &str, kind: EdgeKind) -> &'static str {
    match kind {
        EdgeKind::Undirected => "ref",
        EdgeKind::Directed => {
            if source == anchor {
                "out"
            } else {
                "in"
            }
        }
        EdgeKind::Incoming => {
            if source == anchor {
                "in"
            } else {
                "out"
            }
        }
    }
}

fn relation_sort_rank(relation: &str) -> u8 {
    match relation {
        "out" => 0,
        "in" => 1,
        _ => 2,
    }
}

fn marker_for_relation(relation: &str) -> char {
    match relation {
        "out" => '▶',
        "in" => '◀',
        _ => '◆',
    }
}

fn head_path_from_anchor(anchor: &str, target: &str, kind: EdgeKind) -> String {
    match kind {
        EdgeKind::Directed => target.to_string(),
        EdgeKind::Incoming => anchor.to_string(),
        EdgeKind::Undirected => target.to_string(),
    }
}

fn head_path_to_anchor(anchor: &str, source: &str, kind: EdgeKind) -> String {
    match kind {
        EdgeKind::Directed => anchor.to_string(),
        EdgeKind::Incoming => source.to_string(),
        EdgeKind::Undirected => source.to_string(),
    }
}

fn tree_sort_key(path: &str) -> String {
    let is_dir = path.ends_with('/');
    let parts: Vec<&str> = path.trim_end_matches('/').split('/').collect();
    let mut key = String::new();
    for (idx, part) in parts.iter().enumerate() {
        let is_last = idx + 1 == parts.len();
        let is_folder = !is_last || is_dir;
        key.push(if is_folder { '0' } else { '1' });
        key.push('/');
        key.push_str(part);
        key.push('/');
    }
    key
}

fn hidden_by_bundle(path: &str, bundled_roots: &HashSet<String>) -> bool {
    bundled_roots
        .iter()
        .any(|root| root != path && path.starts_with(root.as_str()))
}

fn tree_row_parts(
    meta: &[NodeMeta],
    idx: usize,
    bundled_roots: &HashSet<String>,
) -> (String, String) {
    let m = &meta[idx];
    let mut prefix = String::new();
    for level in 0..m.depth {
        if has_future_in_ancestor(meta, idx, level) {
            prefix.push_str("│  ");
        } else {
            prefix.push_str("   ");
        }
    }
    let branch = if m.depth == 0 {
        ""
    } else if has_later_sibling(meta, idx) {
        "├─"
    } else {
        "└─"
    };
    let fold = if m.is_dir {
        if bundled_roots.contains(&m.path) {
            "▣ "
        } else {
            "▾"
        }
    } else {
        ""
    };
    (format!("{}{}{}", prefix, branch, fold), m.basename.clone())
}

fn build_meta(path: &str) -> NodeMeta {
    let parts = path_parts(path);
    let depth = parts.len().saturating_sub(1);
    let basename = if parts.is_empty() {
        path.to_string()
    } else if path.ends_with('/') {
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
        path: path.to_string(),
        depth,
        basename,
        parent_prefix,
        ancestor_prefixes,
        is_dir: path.ends_with('/'),
    }
}

fn has_later_sibling(meta: &[NodeMeta], idx: usize) -> bool {
    let me = &meta[idx];
    meta.iter()
        .skip(idx + 1)
        .any(|n| n.depth == me.depth && n.parent_prefix == me.parent_prefix)
}

fn has_future_in_ancestor(meta: &[NodeMeta], idx: usize, level: usize) -> bool {
    let me = &meta[idx];
    let Some(prefix) = me.ancestor_prefixes.get(level) else {
        return false;
    };
    meta.iter()
        .skip(idx + 1)
        .any(|n| n.path.starts_with(prefix.as_str()))
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

fn escape_js(value: &str) -> String {
    let mut out = String::with_capacity(value.len());
    for ch in value.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '<' => out.push_str("\\u003C"),
            '>' => out.push_str("\\u003E"),
            '&' => out.push_str("\\u0026"),
            '\u{2028}' => out.push_str("\\u2028"),
            '\u{2029}' => out.push_str("\\u2029"),
            c if c.is_control() => {
                let _ = write!(&mut out, "\\u{:04X}", c as u32);
            }
            c => out.push(c),
        }
    }
    out
}

fn render_html(nodes: &[ExportNode]) -> String {
    let mut html = String::new();
    html.push_str(
        r#"<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>tngl map</title>
  <style>
    :root {
      --bg: #0b1118;
      --panel: #101823;
      --panel-border: #3a495d;
      --text: #e7eef7;
      --muted: #7f90a5;
      --folder: #39c5f6;
      --focus-bg: #facc15;
      --focus-fg: #131313;
      --accent: #22d3ee;
      --warn: #f87171;
    }
    * { box-sizing: border-box; }
    body {
      margin: 0;
      padding: 20px;
      min-height: 100vh;
      background: radial-gradient(circle at top right, #1b2a3d 0%, var(--bg) 45%);
      color: var(--text);
      font: 15px/1.25 "SFMono-Regular", Menlo, Monaco, Consolas, "Liberation Mono", monospace;
    }
    .frame {
      border: 2px double var(--panel-border);
      border-radius: 10px;
      padding: 14px;
      background: linear-gradient(180deg, rgba(255,255,255,0.04), transparent);
      max-width: 1760px;
      margin: 0 auto;
    }
    .title {
      display: flex;
      justify-content: space-between;
      align-items: baseline;
      gap: 16px;
      margin-bottom: 10px;
      color: var(--text);
    }
    .title strong { font-size: 16px; letter-spacing: 0.04em; }
    .title span { color: var(--muted); font-size: 13px; }
    .panes {
      display: grid;
      grid-template-columns: minmax(420px, 58%) minmax(320px, 42%);
      gap: 12px;
      min-height: 60vh;
    }
    .panel {
      border: 1px solid var(--panel-border);
      border-radius: 9px;
      padding: 10px;
      background: color-mix(in srgb, var(--panel) 88%, black);
      overflow: auto;
    }
    .panel h2 {
      margin: 0 0 8px 0;
      font-size: 13px;
      color: var(--muted);
      letter-spacing: 0.08em;
      text-transform: uppercase;
      border-bottom: 1px solid #243243;
      padding-bottom: 6px;
    }
    .tree-wrap {
      min-height: 360px;
    }
    .tree-svg {
      width: 100%;
      display: block;
      outline: none;
    }
    .tree-svg text {
      font: 15px/1 "SFMono-Regular", Menlo, Monaco, Consolas, "Liberation Mono", monospace;
      dominant-baseline: alphabetic;
      white-space: pre;
      user-select: none;
    }
    .tree-svg .row-bg {
      fill: transparent;
      rx: 4;
      ry: 4;
    }
    .tree-svg .row-hit {
      fill: transparent;
      cursor: pointer;
    }
    .tree-svg .basename-hl {
      fill: var(--focus-bg);
      rx: 3;
      ry: 3;
    }
    .tree-svg .scaffold {
      fill: var(--text);
    }
    .tree-svg .basename {
      fill: var(--text);
    }
    .tree-svg .basename.dir {
      fill: var(--folder);
    }
    .tree-svg .marker.more {
      fill: var(--muted);
    }
    .tree-svg .row.active .basename {
      fill: var(--focus-fg);
      font-weight: 700;
    }
    .tree-svg .route-path {
      fill: none;
      stroke-width: 1.8;
      stroke-linecap: round;
      stroke-linejoin: round;
    }
    .details-head {
      margin-bottom: 8px;
    }
    .details-title {
      margin: 0;
      font-size: 15px;
      color: var(--accent);
      word-break: break-word;
    }
    .details-meta {
      color: var(--muted);
      font-size: 13px;
      margin-top: 3px;
      white-space: pre-wrap;
      word-break: break-word;
    }
    .details-empty {
      color: var(--muted);
      font-style: italic;
      margin-top: 18px;
    }
    .tangles { margin-top: 10px; display: grid; gap: 8px; }
    .tangle {
      border-left: 2px solid #32465f;
      padding-left: 8px;
    }
    .tangle-main { color: var(--text); word-break: break-word; }
    .tangle-comment {
      color: var(--muted);
      margin-top: 2px;
      white-space: pre-wrap;
      word-break: break-word;
    }
    .status {
      margin-top: 12px;
      border: 1px solid var(--panel-border);
      border-radius: 9px;
      padding: 9px 12px;
      display: flex;
      flex-wrap: wrap;
      gap: 12px;
      color: var(--muted);
    }
    .status strong { color: var(--accent); }
    .hint { color: var(--muted); }
    @media (max-width: 920px) {
      .panes { grid-template-columns: 1fr; min-height: unset; }
      body { padding: 12px; }
    }
  </style>
</head>
<body>
  <main class="frame">
    <div class="title">
      <strong>tngl open</strong>
      <span>use ↑/↓ or click a row to inspect tangles</span>
    </div>
    <section class="panes">
      <section class="panel" aria-label="Tree panel">
        <h2>TREE</h2>
        <div class="tree-wrap">
          <svg id="tree-svg" class="tree-svg" tabindex="0" role="listbox" aria-label="Tree view" preserveAspectRatio="xMinYMin meet"></svg>
        </div>
      </section>
      <section class="panel" aria-live="polite" aria-label="Details panel">
        <h2>DETAILS</h2>
        <div id="details-head" class="details-head">
          <p id="details-title" class="details-title">No Node Selected</p>
          <p id="details-meta" class="details-meta">Use ↑/↓ or click a row in TREE to inspect tangles.</p>
        </div>
        <div id="tangles" class="tangles"></div>
      </section>
    </section>
    <footer class="status">
      <span><strong>SELECTED:</strong> <span id="status-selected">—</span></span>
      <span><strong>edges:</strong> <span id="status-edges">0</span></span>
      <span class="hint">Controls: ↑/↓ navigate, click row. Markers: ▶ out, ◀ in, ◆ ref</span>
    </footer>
  </main>
  <script>
    const HEAD_COLORS = ["#,
    );

    for (idx, color) in HEAD_COLORS_HEX.iter().enumerate() {
        if idx > 0 {
            html.push(',');
        }
        let _ = write!(html, "\"{}\"", color);
    }

    html.push_str(
        r#"];
    const DATA = [
"#,
    );

    for node in nodes {
        let _ = write!(
            html,
            "      {{ path: \"{}\", depth: {}, parent: \"{}\", scaffold: \"{}\", basename: \"{}\", isDir: {}, bundled: {}, tangles: [",
            escape_js(&node.path),
            node.depth,
            escape_js(&node.parent),
            escape_js(&node.scaffold),
            escape_js(&node.basename),
            node.is_dir,
            node.bundled,
        );
        for (idx, t) in node.tangles.iter().enumerate() {
            if idx > 0 {
                html.push(',');
            }
            let _ = write!(
                html,
                "{{ neighbor: \"{}\", relation: \"{}\", marker: \"{}\", comment: \"{}\", head: \"{}\" }}",
                escape_js(&t.neighbor),
                t.relation,
                escape_js(&t.marker.to_string()),
                escape_js(&t.comment),
                escape_js(&t.head_path),
            );
        }
        html.push_str("] },\n");
    }

    html.push_str(
        r##"    ];

    const rowByPath = new Map(DATA.map((node, idx) => [node.path, idx]));
    const treeSvg = document.getElementById("tree-svg");
    const treePanel = treeSvg?.closest(".panel") || null;
    const detailsTitle = document.getElementById("details-title");
    const detailsMeta = document.getElementById("details-meta");
    const tanglesEl = document.getElementById("tangles");
    const statusSelected = document.getElementById("status-selected");
    const statusEdges = document.getElementById("status-edges");

    const FONT = "15px SFMono-Regular, Menlo, Monaco, Consolas, Liberation Mono, monospace";
    const ROW_HEIGHT = 18;
    const TOP_PAD = 8;
    const LEFT_PAD = 8;
    const BASELINE_OFFSET = 13;

    const measureCanvas = document.createElement("canvas");
    const measureCtx = measureCanvas.getContext("2d");
    if (measureCtx) {
      measureCtx.font = FONT;
    }

    function scaffoldTight(scaffold) {
      return scaffold.replace(/\s+$/, "");
    }

    function charCount(text) {
      return Array.from(text).length;
    }

    const cellWidth = (() => {
      if (!measureCtx) return 8.6;
      const measured = measureCtx.measureText("M").width;
      return Number.isFinite(measured) && measured > 0 ? measured : 8.6;
    })();
    const LANE_STEP_COLS = 4;
    const MIN_WIDTH_COLS = 80;

    const scaffoldByRow = DATA.map((node) => scaffoldTight(node.scaffold));
    const scaffoldColsByRow = scaffoldByRow.map((scaffold) => charCount(scaffold));
    const basenameColsByRow = DATA.map((node) => charCount(node.basename));
    const fullRowColsByRow = DATA.map(
      (_, idx) => scaffoldColsByRow[idx] + basenameColsByRow[idx],
    );

    let selectedIndex = DATA.length > 0 ? 0 : -1;

    function colLeft(col) {
      return LEFT_PAD + col * cellWidth;
    }

    function colCenter(col) {
      return colLeft(col) + cellWidth * 0.5;
    }

    function rowTop(idx) {
      return TOP_PAD + idx * ROW_HEIGHT;
    }

    function rowBaseline(idx) {
      return rowTop(idx) + BASELINE_OFFSET;
    }

    function svgEl(name, attrs = {}, text) {
      const el = document.createElementNS("http://www.w3.org/2000/svg", name);
      for (const [key, value] of Object.entries(attrs)) {
        el.setAttribute(key, String(value));
      }
      if (typeof text === "string") {
        el.textContent = text;
      }
      return el;
    }

    function ensureRowVisible(idx) {
      if (!treePanel || idx < 0) return;
      const top = rowTop(idx);
      const bottom = top + ROW_HEIGHT;
      if (top < treePanel.scrollTop) {
        treePanel.scrollTop = top;
      } else if (bottom > treePanel.scrollTop + treePanel.clientHeight) {
        treePanel.scrollTop = bottom - treePanel.clientHeight;
      }
    }

    function rangesOverlap(a, b) {
      return !(a.high < b.low || b.high < a.low);
    }

    function buildRoutes(node) {
      if (!node) return [];
      const routes = [];
      node.tangles.forEach((tangle, idx) => {
        const headPath = tangle.head;
        const footPath = headPath === node.path ? tangle.neighbor : node.path;
        const footRow = rowByPath.get(footPath);
        const headRow = rowByPath.get(headPath);
        if (footRow === undefined || headRow === undefined || footRow === headRow) {
          return;
        }
        routes.push({
          footRow,
          headRow,
          low: Math.min(footRow, headRow),
          high: Math.max(footRow, headRow),
          marker: tangle.marker,
          color: HEAD_COLORS[idx % HEAD_COLORS.length],
          rightCol: 0,
        });
      });

      routes.sort((a, b) => {
        const spanA = a.high - a.low;
        const spanB = b.high - b.low;
        return spanA - spanB || a.low - b.low || a.high - b.high;
      });
      const placed = [];
      for (const route of routes) {
        let maxCol = 0;
        for (let row = route.low; row <= route.high; row += 1) {
          const fullLeft = fullRowColsByRow[row] ?? 0;
          const overhead = row === route.footRow || row === route.headRow ? 4 : 1;
          maxCol = Math.max(maxCol, fullLeft + overhead);
        }
        const minByOverlap = placed
          .filter((inner) => rangesOverlap(inner, route))
          .reduce((acc, inner) => Math.max(acc, inner.rightCol + LANE_STEP_COLS), 0);
        route.rightCol = Math.max(maxCol, minByOverlap);
        placed.push(route);
      }

      return routes;
    }

    function computeEndpointCols(routes) {
      const endpoints = new Map();
      for (let row = 0; row < DATA.length; row += 1) {
        const active = [];
        routes.forEach((route, routeIdx) => {
          if (row >= route.low && row <= route.high) {
            active.push({
              route,
              routeIdx,
              endpoint: row === route.footRow || row === route.headRow,
            });
          }
        });
        active.sort((a, b) => a.route.rightCol - b.route.rightCol);

        let currentCol = fullRowColsByRow[row] ?? 0;
        for (const item of active) {
          if (item.endpoint) {
            const markerCol = currentCol + 1;
            const endpoint = endpoints.get(item.routeIdx) || {};
            if (row === item.route.footRow) {
              endpoint.footCol = markerCol;
            }
            if (row === item.route.headRow) {
              endpoint.headCol = markerCol;
            }
            endpoints.set(item.routeIdx, endpoint);
          }
          currentCol = item.route.rightCol + 1;
        }
      }
      return endpoints;
    }

    function renderTangles(node) {
      tanglesEl.replaceChildren();
      if (!node || node.tangles.length === 0) {
        const empty = document.createElement("p");
        empty.className = "details-empty";
        empty.textContent = "(none)";
        tanglesEl.appendChild(empty);
        return;
      }

      node.tangles.forEach((tangle, idx) => {
        const color = HEAD_COLORS[idx % HEAD_COLORS.length];
        const card = document.createElement("div");
        card.className = "tangle relation-" + tangle.relation;
        card.style.borderLeftColor = color;

        const main = document.createElement("div");
        main.className = "tangle-main";
        main.textContent = tangle.marker + " [" + tangle.relation + "] " + tangle.neighbor;
        main.style.color = color;
        card.appendChild(main);

        if (tangle.comment && tangle.comment.trim().length > 0) {
          const comment = document.createElement("div");
          comment.className = "tangle-comment";
          comment.textContent = tangle.comment;
          card.appendChild(comment);
        }

        tanglesEl.appendChild(card);
      });
    }

    function renderTree(node) {
      if (!treeSvg) return;
      treeSvg.replaceChildren();

      if (DATA.length === 0) {
        const height = 76;
        treeSvg.setAttribute("viewBox", "0 0 640 " + height);
        treeSvg.setAttribute("height", String(height));
        treeSvg.appendChild(
          svgEl(
            "text",
            { x: 0, y: 30, class: "scaffold" },
            "No nodes in graph.",
          ),
        );
        return;
      }

      const routes = buildRoutes(node);
      const maxTextCols = fullRowColsByRow.reduce((acc, cols) => Math.max(acc, cols), 0);
      const maxLaneCol = routes.reduce((acc, route) => Math.max(acc, route.rightCol), 0);
      const totalCols = Math.max(MIN_WIDTH_COLS, maxTextCols + 6, maxLaneCol + 2);
      const width = Math.max(640, colLeft(totalCols + 1));
      const height = TOP_PAD * 2 + DATA.length * ROW_HEIGHT;
      treeSvg.setAttribute("viewBox", "0 0 " + width + " " + height);
      treeSvg.setAttribute("height", String(height));

      const endpointCols = computeEndpointCols(routes);
      const routeLayer = svgEl("g");
      routes
        .map((route, routeIdx) => ({ route, routeIdx }))
        .sort((a, b) => a.route.rightCol - b.route.rightCol)
        .forEach(({ route, routeIdx }) => {
        const laneX = colCenter(route.rightCol);
        const footY = rowTop(route.footRow) + ROW_HEIGHT / 2;
        const headY = rowTop(route.headRow) + ROW_HEIGHT / 2;
        const endpoint = endpointCols.get(routeIdx) || {};
        const footMarkerCol = endpoint.footCol ?? (fullRowColsByRow[route.footRow] ?? 0) + 1;
        const headMarkerCol = endpoint.headCol ?? (fullRowColsByRow[route.headRow] ?? 0) + 1;
        const footCx = colCenter(footMarkerCol);
        const headCx = colCenter(headMarkerCol);
        const markerW = Math.max(9, cellWidth * 1.02);
        const footR = markerW * 0.23;
        const footJoinX = footCx + footR;
        let headJoinX = headCx + markerW * 0.35;
        const routeGroup = svgEl("g");

        // Foot marker: open circle.
        routeGroup.appendChild(
          svgEl("circle", {
            cx: footCx,
            cy: footY,
            r: footR,
            fill: "none",
            stroke: route.color,
            "stroke-width": 1.8,
          }),
        );

        if (route.marker === "◆") {
          // Undirected head marker: diamond.
          const cx = headCx;
          const cy = headY;
          const rx = markerW * 0.38;
          const ry = markerW * 0.32;
          routeGroup.appendChild(
            svgEl("polygon", {
              points:
                cx +
                "," +
                (cy - ry) +
                " " +
                (cx + rx) +
                "," +
                cy +
                " " +
                cx +
                "," +
                (cy + ry) +
                " " +
                (cx - rx) +
                "," +
                cy,
              fill: route.color,
            }),
          );
          headJoinX = cx + rx;
        } else {
          // Directed head marker: left-pointing triangle.
          const tipX = headCx - markerW * 0.46;
          const tailX = headCx + markerW * 0.42;
          const halfH = markerW * 0.34;
          routeGroup.appendChild(
            svgEl("polygon", {
              points:
                tipX +
                "," +
                headY +
                " " +
                tailX +
                "," +
                (headY - halfH) +
                " " +
                tailX +
                "," +
                (headY + halfH),
              fill: route.color,
            }),
          );
          headJoinX = tailX;
        }

        const path = svgEl("path", {
          class: "route-path",
          stroke: route.color,
          d:
            "M " +
            footJoinX +
            " " +
            footY +
            " L " +
            laneX +
            " " +
            footY +
            " L " +
            laneX +
            " " +
            headY +
            " L " +
            headJoinX +
            " " +
            headY,
        });
        routeGroup.insertBefore(path, routeGroup.firstChild);
        routeLayer.appendChild(routeGroup);
      });
      treeSvg.appendChild(routeLayer);

      DATA.forEach((row, idx) => {
        const group = svgEl("g", {
          class: idx === selectedIndex ? "row active" : "row",
          "data-index": idx,
        });
        const yTop = rowTop(idx);
        const yBase = rowBaseline(idx);
        const baseX = LEFT_PAD;
        const basenameX = baseX + scaffoldColsByRow[idx] * cellWidth;

        group.appendChild(
          svgEl("rect", {
            class: "row-bg",
            x: 0,
            y: yTop,
            width,
            height: ROW_HEIGHT - 1,
          }),
        );
        const rowText = svgEl("text", { x: baseX, y: yBase, "xml:space": "preserve" });
        const scaffoldSpan = svgEl("tspan", { class: "scaffold" }, scaffoldByRow[idx]);
        const basenameSpan = svgEl(
          "tspan",
          { class: row.isDir ? "basename dir" : "basename" },
          row.basename,
        );
        rowText.appendChild(scaffoldSpan);
        rowText.appendChild(basenameSpan);
        group.appendChild(rowText);

        if (row.bundled) {
          const underlineColor = idx === selectedIndex ? "#131313" : "#39c5f6";
          const nameWidth = basenameColsByRow[idx] * cellWidth;
          group.appendChild(
            svgEl("line", {
              x1: basenameX,
              y1: yBase + 2,
              x2: basenameX + nameWidth,
              y2: yBase + 2,
              stroke: underlineColor,
              "stroke-dasharray": "1.5 2.2",
              "stroke-width": 1,
            }),
          );
        }

        const hit = svgEl("rect", {
          class: "row-hit",
          x: 0,
          y: yTop,
          width,
          height: ROW_HEIGHT - 1,
        });
        hit.addEventListener("click", () => {
          setSelectedIndex(idx, true);
        });
        group.appendChild(hit);
        treeSvg.appendChild(group);

        if (idx === selectedIndex) {
          const highlightWidth = Math.max(1, basenameColsByRow[idx] * cellWidth);
          group.insertBefore(
            svgEl("rect", {
              class: "basename-hl",
              x: basenameX,
              y: yTop + 1,
              width: highlightWidth,
              height: ROW_HEIGHT - 3,
            }),
            rowText,
          );
        }
      });
    }

    function pinSceneHeight() {
      const panes = document.querySelector(".panes");
      const detailsPanel = tanglesEl?.closest(".panel");
      if (!panes || !detailsPanel) return;

      const treeNeeded = TOP_PAD * 2 + DATA.length * ROW_HEIGHT + 92;
      let maxDetailsNeeded = detailsPanel.scrollHeight;

      if (DATA.length > 0) {
        const current = DATA[Math.max(0, Math.min(DATA.length - 1, selectedIndex))];
        for (const node of DATA) {
          updateDetails(node);
          maxDetailsNeeded = Math.max(maxDetailsNeeded, detailsPanel.scrollHeight);
        }
        updateDetails(current);
      } else {
        updateDetails(null);
      }

      const sceneHeight = Math.max(treeNeeded, maxDetailsNeeded + 12);
      panes.style.height = sceneHeight + "px";
      panes.style.minHeight = sceneHeight + "px";
    }

    function updateDetails(node) {
      if (!node) {
        detailsTitle.textContent = "No Node Selected";
        detailsMeta.textContent = "Use ↑/↓ or click a row in TREE to inspect tangles.";
        statusSelected.textContent = "—";
        statusEdges.textContent = "0";
        renderTangles(null);
        return;
      }
      detailsTitle.textContent = node.path;
      detailsMeta.textContent = "depth: " + node.depth + "   parent: " + node.parent;
      statusSelected.textContent = node.path;
      statusEdges.textContent = String(node.tangles.length);
      renderTangles(node);
    }

    function setSelectedIndex(nextIdx, focusTree) {
      if (DATA.length === 0) {
        selectedIndex = -1;
        updateDetails(null);
        renderTree(null);
        return;
      }
      const clamped = Math.max(0, Math.min(DATA.length - 1, nextIdx));
      if (selectedIndex === clamped && !focusTree) {
        return;
      }
      selectedIndex = clamped;
      const node = DATA[selectedIndex];
      updateDetails(node);
      renderTree(node);
      ensureRowVisible(selectedIndex);
      if (focusTree && treeSvg) {
        treeSvg.focus({ preventScroll: true });
      }
    }

    function moveSelection(delta) {
      if (DATA.length === 0) return;
      const current = selectedIndex < 0 ? 0 : selectedIndex;
      setSelectedIndex(current + delta, true);
    }

    function onKeydown(event) {
      if (event.altKey || event.ctrlKey || event.metaKey) return;
      const target = event.target;
      if (
        target instanceof HTMLElement &&
        (target.tagName === "INPUT" ||
          target.tagName === "TEXTAREA" ||
          target.tagName === "SELECT" ||
          target.isContentEditable)
      ) {
        return;
      }
      if (event.key === "ArrowDown") {
        event.preventDefault();
        moveSelection(1);
      } else if (event.key === "ArrowUp") {
        event.preventDefault();
        moveSelection(-1);
      } else if (event.key === "Home") {
        event.preventDefault();
        setSelectedIndex(0, true);
      } else if (event.key === "End") {
        event.preventDefault();
        setSelectedIndex(DATA.length - 1, true);
      }
    }

    window.addEventListener("keydown", onKeydown);
    treeSvg?.addEventListener("click", () => treeSvg.focus({ preventScroll: true }));
    pinSceneHeight();

    if (DATA.length > 0) {
      setSelectedIndex(0, true);
    } else {
      updateDetails(null);
      renderTree(null);
    }
  </script>
</body>
</html>
"##,
    );
    html
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::model::{Edge, Node};

    #[test]
    fn relation_mapping_for_anchor_matches_tui_semantics() {
        let mut g = Graph::new();
        let mut a = Node::new("a.rs");
        a.edges.push(Edge {
            target: "b.rs".to_string(),
            kind: EdgeKind::Directed,
            label: "calls".to_string(),
        });
        a.edges.push(Edge {
            target: "c.rs".to_string(),
            kind: EdgeKind::Incoming,
            label: "called by".to_string(),
        });
        a.edges.push(Edge {
            target: "d.rs".to_string(),
            kind: EdgeKind::Undirected,
            label: String::new(),
        });
        let mut e = Node::new("e.rs");
        e.edges.push(Edge {
            target: "a.rs".to_string(),
            kind: EdgeKind::Directed,
            label: "feeds".to_string(),
        });
        g.add_node(a);
        g.add_node(e);

        let tangles = tangles_for_anchor_with_visibility(&g, "a.rs", None);
        let triples = tangles
            .iter()
            .map(|t| (t.neighbor.as_str(), t.relation, t.marker))
            .collect::<Vec<_>>();

        assert!(
            triples.contains(&("b.rs", "out", '▶')),
            "directed edge from anchor should be out"
        );
        assert!(
            triples.contains(&("c.rs", "in", '◀')),
            "incoming edge from anchor should be in"
        );
        assert!(
            triples.contains(&("d.rs", "ref", '◆')),
            "undirected edge from anchor should be ref"
        );
        assert!(
            triples.contains(&("e.rs", "in", '◀')),
            "directed edge to anchor should be in"
        );
    }

    #[test]
    fn export_filters_nodes_and_tangles_by_visibility() {
        let mut g = Graph::new();
        let mut a = Node::new("src/main.rs");
        a.edges.push(Edge {
            target: "src/hidden.rs".to_string(),
            kind: EdgeKind::Directed,
            label: "calls hidden".to_string(),
        });
        g.add_node(Node::new("src/"));
        g.add_node(a);
        g.add_node(Node::new("src/hidden.rs"));

        let mut visible = HashSet::new();
        visible.insert("src/".to_string());
        visible.insert("src/main.rs".to_string());

        let nodes = build_export_nodes_with_visibility(&g, &HashSet::new(), Some(&visible));
        assert_eq!(nodes.len(), 2);
        assert!(nodes.iter().any(|n| n.path == "src/"));
        let main = nodes
            .iter()
            .find(|n| n.path == "src/main.rs")
            .expect("visible node should be exported");
        assert!(
            main.tangles.is_empty(),
            "tangles targeting ignored paths should be removed from export"
        );
    }

    #[test]
    fn tree_rows_keep_hierarchical_scaffold() {
        let mut g = Graph::new();
        g.add_node(Node::new("src/"));
        g.add_node(Node::new("src/lib.rs"));
        g.add_node(Node::new("src/tui/"));
        g.add_node(Node::new("src/tui/render.rs"));
        g.add_node(Node::new("README.md"));

        let nodes = build_export_nodes_with_visibility(&g, &HashSet::new(), None);
        let rows = nodes
            .iter()
            .map(|n| format!("{}{}", n.scaffold, n.basename))
            .collect::<Vec<_>>();

        assert_eq!(rows[0], "▾src/");
        assert_eq!(rows[1], "│  ├─▾tui/");
        assert_eq!(rows[2], "│     └─render.rs");
        assert_eq!(rows[3], "   └─lib.rs");
        assert_eq!(rows[4], "README.md");
    }

    #[test]
    fn bundled_folders_render_collapsed_in_export_tree() {
        let mut g = Graph::new();
        g.add_node(Node::new("src/"));
        g.add_node(Node::new("src/lib.rs"));
        g.add_node(Node::new("src/tui/"));
        g.add_node(Node::new("src/tui/render.rs"));
        g.add_node(Node::new("README.md"));

        let mut bundled = HashSet::new();
        bundled.insert("src/".to_string());

        let nodes = build_export_nodes_with_visibility(&g, &bundled, None);
        let rows = nodes
            .iter()
            .map(|n| format!("{}{}", n.scaffold, n.basename))
            .collect::<Vec<_>>();

        assert_eq!(rows, vec!["▣ src/".to_string(), "README.md".to_string()]);
        assert_eq!(nodes[0].path, "src/");
        assert!(nodes[0].bundled);
    }

    #[test]
    fn html_contains_interactive_bindings() {
        let mut g = Graph::new();
        g.add_node(Node::new("a.rs"));
        let nodes = build_export_nodes_with_visibility(&g, &HashSet::new(), None);
        let html = render_html(&nodes);

        assert!(html.contains("addEventListener(\"keydown\""));
        assert!(html.contains("ArrowDown"));
        assert!(html.contains("id=\"details-title\""));
        assert!(html.contains("id=\"tree-svg\""));
        assert!(html.contains("createElementNS(\"http://www.w3.org/2000/svg\""));
    }
}
