//! Round-trip-safe parser and writer for `graph.tngl`.
//!
//! The central invariant: `serialize(parse(input)?) == input` for any valid input.
//! This guarantees that hand-authored comments, blank lines, and whitespace alignment
//! are never disturbed by tooling.

use std::collections::HashSet;

use anyhow::{Context, Result, bail};

use crate::graph::model::{Edge, EdgeKind, Graph, Node};

// ---------------------------------------------------------------------------
// Document model
// ---------------------------------------------------------------------------

/// A single line from `graph.tngl`, classified and stored verbatim.
///
/// The raw string stored in each variant is the exact text of the line
/// (without the terminating `\n`). Serialising by joining with `\n` and
/// appending the original trailing-newline flag reproduces the original bytes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DocLine {
    /// A blank or whitespace-only line.
    Blank(String),
    /// A comment line (starts with `#` after trimming).
    Comment(String),
    /// A marker tag line (`[orphan]`, `[bundle]`, `[orphan bundle]`).
    Tag(String),
    /// A node declaration line (optionally indented for visual hierarchy).
    Node(String),
    /// An edge declaration (`->` or `--`, usually indented).
    Edge(String),
}

impl DocLine {
    /// The raw text of this line (no newline character).
    pub fn raw(&self) -> &str {
        match self {
            Self::Blank(s) | Self::Comment(s) | Self::Tag(s) | Self::Node(s) | Self::Edge(s) => s,
        }
    }

    /// The semantic path for a Node line, or `None`.
    pub fn node_path(&self) -> Option<&str> {
        if let Self::Node(s) = self {
            Some(s.trim())
        } else {
            None
        }
    }
}

/// The complete document model for a `graph.tngl` file.
///
/// Preserves every line verbatim so that `serialize(parse(input)?) == input`.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Document {
    pub lines: Vec<DocLine>,
    /// Whether the original input ended with a newline character.
    pub trailing_newline: bool,
}

/// Summary of lint fixes applied to a graph document.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct LintReport {
    pub removed_floating_orphan_tags: usize,
    pub removed_extra_blank_lines: usize,
    pub normalized_tag_indentation: usize,
    pub normalized_edge_indentation: usize,
}

impl LintReport {
    pub fn changed(&self) -> bool {
        self.removed_floating_orphan_tags > 0
            || self.removed_extra_blank_lines > 0
            || self.normalized_tag_indentation > 0
            || self.normalized_edge_indentation > 0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OrphanMarkerKind {
    Orphan,
    OrphanSubtree,
}

// ---------------------------------------------------------------------------
// Parsing
// ---------------------------------------------------------------------------

/// Parse `graph.tngl` text into a `Document`.
///
/// Returns an error if any edge line is malformed. Unknown node references are
/// allowed — they are reported as dangling edges, not parse errors.
pub fn parse(input: &str) -> Result<Document> {
    let (raw_lines, trailing_newline) = split_lines_raw(input);

    let mut lines = Vec::with_capacity(raw_lines.len());

    for (line_num, raw) in raw_lines.into_iter().enumerate() {
        let trimmed = raw.trim();

        let doc_line = if trimmed.is_empty() {
            DocLine::Blank(raw)
        } else if trimmed.starts_with('#') {
            DocLine::Comment(raw)
        } else if is_tag_line(trimmed) {
            DocLine::Tag(raw)
        } else if is_edge_line(&raw) {
            // Validate immediately so callers get a clear error location.
            parse_edge_line(&raw)
                .with_context(|| format!("invalid edge at line {}", line_num + 1))?;
            DocLine::Edge(raw)
        } else if (raw.starts_with(' ') || raw.starts_with('\t')) && raw.trim_start().contains(':')
        {
            // Indented lines containing ':' are likely malformed edges.
            parse_edge_line(&raw)
                .with_context(|| format!("invalid edge at line {}", line_num + 1))?;
            unreachable!("parse_edge_line above always returns Err for malformed edges");
        } else {
            DocLine::Node(raw)
        };

        lines.push(doc_line);
    }

    Ok(Document {
        lines,
        trailing_newline,
    })
}

/// Serialise a `Document` back to text.
///
/// When applied to a parsed document this produces byte-identical output.
pub fn serialize(doc: &Document) -> String {
    if doc.lines.is_empty() {
        return String::new();
    }
    let mut out = doc
        .lines
        .iter()
        .map(DocLine::raw)
        .collect::<Vec<_>>()
        .join("\n");
    if doc.trailing_newline {
        out.push('\n');
    }
    out
}

// ---------------------------------------------------------------------------
// Semantic model conversion
// ---------------------------------------------------------------------------

/// Build a `Graph` from a `Document`.
///
/// The graph reflects the semantic content only; comments, blank lines and
/// whitespace formatting are not carried over.
pub fn to_graph(doc: &Document) -> Result<Graph> {
    let mut graph = Graph::new();
    let mut current_node: Option<String> = None;

    for (i, line) in doc.lines.iter().enumerate() {
        match line {
            DocLine::Blank(_) | DocLine::Comment(_) | DocLine::Tag(_) => {
                // No semantic effect; blank lines do not reset the current node
                // because `->` lines after a blank still belong to the same node.
            }
            DocLine::Node(raw) => {
                let path = raw.trim().to_string();
                if graph.contains(&path) {
                    bail!("duplicate node '{}' at line {}", path, i + 1);
                }
                graph.add_node(Node::new(path.clone()));
                current_node = Some(path);
            }
            DocLine::Edge(raw) => {
                let edge = parse_edge_line(raw)
                    .with_context(|| format!("invalid edge at line {}", i + 1))?;
                match current_node.as_deref() {
                    None => bail!("edge at line {} appears before any node declaration", i + 1),
                    Some(path) => {
                        graph
                            .get_mut(path)
                            .expect("current_node must be in graph")
                            .edges
                            .push(edge);
                    }
                }
            }
        }
    }

    Ok(graph)
}

/// Build a canonical `Document` from a `Graph`.
///
/// Produces clean, consistently formatted output. Does not preserve any
/// hand-authored formatting (use for fresh files only — `tngl init`).
pub fn from_graph(graph: &Graph) -> Document {
    let mut lines: Vec<DocLine> = Vec::new();

    for (idx, node) in graph.nodes.iter().enumerate() {
        if idx > 0 {
            lines.push(DocLine::Blank(String::new()));
        }
        lines.push(DocLine::Node(format_node_line(&node.path)));
        for edge in &node.edges {
            lines.push(DocLine::Edge(format_edge_line(edge)));
        }
    }

    Document {
        lines,
        trailing_newline: true,
    }
}

// ---------------------------------------------------------------------------
// Document mutation helpers (used by `tngl update`)
// ---------------------------------------------------------------------------

/// Insert a new orphan node into the document in hierarchical order.
pub fn add_node(doc: &mut Document, path: &str) {
    let indent = detect_indent_unit(doc);
    let node_line = DocLine::Node(format_node_line_with_indent(path, &indent));

    if let Some(insert_at) = find_insert_idx(doc, path) {
        let mut insert_at = insert_at;
        let mut suppress_leading_blank = false;

        if should_remove_edge_to_child_blank(doc, path, insert_at) {
            doc.lines.remove(insert_at - 1);
            insert_at -= 1;
            suppress_leading_blank = true;
        }

        let mut block: Vec<DocLine> = Vec::new();
        if !suppress_leading_blank
            && insert_at > 0
            && !matches!(doc.lines[insert_at - 1], DocLine::Blank(_))
        {
            block.push(DocLine::Blank(String::new()));
        }
        block.push(node_line);
        if insert_at < doc.lines.len() && !matches!(doc.lines[insert_at], DocLine::Blank(_)) {
            block.push(DocLine::Blank(String::new()));
        }
        doc.lines.splice(insert_at..insert_at, block);
    } else {
        if !doc.lines.is_empty() {
            doc.lines.push(DocLine::Blank(String::new()));
        }
        doc.lines.push(node_line);
    }
    doc.trailing_newline = true;
}

/// Remove a node and all its associated edge lines from the document.
///
/// Also removes any immediately preceding blank separator line so the document
/// stays clean. Returns `true` if the node was found and removed.
pub fn remove_node(doc: &mut Document, path: &str) -> bool {
    let Some((node_idx, end_idx)) = find_node_range(&doc.lines, path) else {
        return false;
    };

    let (tag_block_start, has_tag_block) = find_tag_block_start(doc, node_idx);

    if end_idx < doc.lines.len() {
        // There is another node after this one: remove this node's block and any
        // marker tags attached to it. Keep the separator before it.
        let delete_from = if has_tag_block {
            tag_block_start
        } else {
            node_idx
        };
        let mut delete_to = trailing_comment_block_start(&doc.lines, delete_from, end_idx);
        if let Some(next_tag_start) = attached_tag_block_start_before_node(&doc.lines, end_idx) {
            delete_to = delete_to.min(next_tag_start);
        }
        doc.lines.drain(delete_from..delete_to);
    } else {
        // Last node: remove attached marker tags plus a preceding separator blank.
        let base_from = if has_tag_block {
            tag_block_start
        } else {
            node_idx
        };
        let delete_from = if base_from > 0 && matches!(doc.lines[base_from - 1], DocLine::Blank(_))
        {
            base_from - 1
        } else {
            base_from
        };
        let delete_to = trailing_comment_block_start(&doc.lines, delete_from, end_idx);
        doc.lines.drain(delete_from..delete_to);
    }
    true
}

#[allow(dead_code)] // used by TUI (not yet implemented)
/// Add an edge to an existing node in the document.
///
/// Inserts the edge line after the last existing edge of that node (before any
/// trailing blank lines). Returns an error if the source node is not found.
pub fn add_edge(doc: &mut Document, source: &str, edge: &Edge) -> Result<()> {
    let indent_unit = detect_indent_unit(doc);
    let (node_idx, end_idx) = find_node_range(&doc.lines, source)
        .ok_or_else(|| anyhow::anyhow!("node '{}' not found in document", source))?;
    let edge_indent = edge_indent_for_node(doc, node_idx, end_idx, &indent_unit);

    // Find the last Edge line within this node's range.
    let insert_after = doc.lines[node_idx + 1..end_idx]
        .iter()
        .rposition(|l| matches!(l, DocLine::Edge(_)))
        .map(|rel| node_idx + 1 + rel)
        .unwrap_or(node_idx);

    doc.lines.insert(
        insert_after + 1,
        DocLine::Edge(format_edge_line_with_indent(edge, &edge_indent)),
    );
    Ok(())
}

#[allow(dead_code)] // used by TUI (not yet implemented)
/// Remove a specific edge from a node.
///
/// Matches on target path. Returns `true` if the edge was found and removed.
pub fn remove_edge(doc: &mut Document, source: &str, target: &str) -> bool {
    let Some((node_idx, end_idx)) = find_node_range(&doc.lines, source) else {
        return false;
    };

    let range = &doc.lines[node_idx + 1..end_idx];
    let rel_pos = range.iter().position(|l| {
        if let DocLine::Edge(raw) = l {
            parse_edge_line(raw)
                .map(|e| e.target == target)
                .unwrap_or(false)
        } else {
            false
        }
    });

    if let Some(rel) = rel_pos {
        doc.lines.remove(node_idx + 1 + rel);
        true
    } else {
        false
    }
}

/// Reorder edge lines under every node by kind:
/// 1) undirected (`--`)
/// 2) outgoing (`->`)
/// 3) incoming (`<-`)
///
/// Non-edge lines (comments/blanks/tags) keep their original positions; only
/// edge lines are permuted across existing edge slots. Returns the number of
/// nodes whose edge order changed.
pub fn sort_edges_by_kind(doc: &mut Document) -> usize {
    let mut changed_nodes = 0usize;
    let mut i = 0usize;

    while i < doc.lines.len() {
        if !matches!(doc.lines[i], DocLine::Node(_)) {
            i += 1;
            continue;
        }

        let end_idx = doc.lines[i + 1..]
            .iter()
            .position(|l| matches!(l, DocLine::Node(_)))
            .map(|rel| i + 1 + rel)
            .unwrap_or(doc.lines.len());

        let mut edge_positions = Vec::new();
        let mut edge_lines = Vec::new();
        for idx in i + 1..end_idx {
            if let DocLine::Edge(raw) = &doc.lines[idx] {
                edge_positions.push(idx);
                edge_lines.push(raw.clone());
            }
        }

        if edge_lines.len() > 1 {
            let mut sorted = edge_lines.clone();
            sorted.sort_by_key(|raw| {
                parse_edge_line(raw)
                    .map(|e| edge_kind_rank(&e.kind))
                    .unwrap_or(usize::MAX)
            });

            if sorted != edge_lines {
                for (idx, raw) in edge_positions.iter().zip(sorted.into_iter()) {
                    doc.lines[*idx] = DocLine::Edge(raw);
                }
                changed_nodes += 1;
            }
        }

        i = end_idx;
    }

    changed_nodes
}

/// Replace the label of matching edge lines under `source`.
///
/// Matches by `(target, kind, old_label)` and updates to `new_label`.
/// Returns the number of edge lines changed.
pub fn replace_edge_label(
    doc: &mut Document,
    source: &str,
    target: &str,
    kind: EdgeKind,
    old_label: &str,
    new_label: &str,
) -> usize {
    let Some((node_idx, end_idx)) = find_node_range(&doc.lines, source) else {
        return 0;
    };

    let mut changed = 0;
    for idx in node_idx + 1..end_idx {
        let DocLine::Edge(raw) = &doc.lines[idx] else {
            continue;
        };
        let Ok(mut edge) = parse_edge_line(raw) else {
            continue;
        };
        if edge.target != target || edge.kind != kind || edge.label != old_label {
            continue;
        }
        edge.label = new_label.to_string();
        let leading_len = raw.len() - raw.trim_start().len();
        let indent = &raw[..leading_len];
        doc.lines[idx] = DocLine::Edge(format_edge_line_with_indent(&edge, indent));
        changed += 1;
    }
    changed
}

/// Remove all edge lines in the document that point **to** `target`.
///
/// Used by the `delete` strategy in `tngl update` to clean up incoming edges
/// when a node is removed. Returns the number of edge lines removed.
pub fn remove_edges_targeting(doc: &mut Document, target: &str) -> usize {
    let mut count = 0;
    let mut i = 0;
    while i < doc.lines.len() {
        let remove = if let DocLine::Edge(raw) = &doc.lines[i] {
            parse_edge_line(raw)
                .map(|e| e.target == target)
                .unwrap_or(false)
        } else {
            false
        };
        if remove {
            doc.lines.remove(i);
            count += 1;
        } else {
            i += 1;
        }
    }
    count
}

/// Mark a node as `[missing]` by prepending a comment above it.
///
/// Used by the `preserve` deletion strategy in `tngl update`.
pub fn mark_missing(doc: &mut Document, path: &str) -> bool {
    let Some((node_idx, _)) = find_node_range(&doc.lines, path) else {
        return false;
    };
    doc.lines
        .insert(node_idx, DocLine::Comment(format!("# [missing] {}", path)));
    true
}

/// Mark a node as an intentional orphan by ensuring `[orphan]` above it.
///
/// Returns `true` when changed, `false` if the node was not found or already
/// had the orphan semantics.
pub fn mark_orphan(doc: &mut Document, path: &str) -> bool {
    let Some((node_idx, _)) = find_node_range(&doc.lines, path) else {
        return false;
    };

    if let Some(tag_idx) = attached_tag_line_idx(doc, node_idx) {
        return ensure_tag_tokens_on_line(doc, tag_idx, &[TAG_ORPHAN]);
    }

    let node_raw = doc.lines[node_idx].raw();
    let tag = format!("{}[orphan]", leading_whitespace(node_raw));
    doc.lines.insert(node_idx, DocLine::Tag(tag));
    true
}

/// Mark a folder node as intentional orphan bundle by ensuring
/// `[orphan bundle]` above it.
///
/// Returns `true` when changed, `false` if the node was not found, is not a
/// folder node, or already had orphan-bundle semantics.
#[allow(dead_code)] // kept as public document-mutation helper
pub fn mark_orphan_subtree(doc: &mut Document, path: &str) -> bool {
    if !path.ends_with('/') {
        return false;
    }

    let Some((node_idx, _)) = find_node_range(&doc.lines, path) else {
        return false;
    };

    if let Some(tag_idx) = attached_tag_line_idx(doc, node_idx) {
        return ensure_tag_tokens_on_line(doc, tag_idx, &[TAG_ORPHAN, TAG_BUNDLE]);
    }

    let node_raw = doc.lines[node_idx].raw();
    let tag = format!("{}[orphan bundle]", leading_whitespace(node_raw));
    doc.lines.insert(node_idx, DocLine::Tag(tag));
    true
}

/// Mark a folder node as bundled by ensuring `[bundle]` above it.
///
/// Returns `true` when changed, `false` if the node was not found, is not a
/// folder node, or already had bundle semantics.
pub fn mark_link_subtree(doc: &mut Document, path: &str) -> bool {
    if !path.ends_with('/') {
        return false;
    }

    let Some((node_idx, _)) = find_node_range(&doc.lines, path) else {
        return false;
    };

    if let Some(tag_idx) = attached_tag_line_idx(doc, node_idx) {
        return ensure_tag_tokens_on_line(doc, tag_idx, &[TAG_BUNDLE]);
    }

    let node_raw = doc.lines[node_idx].raw();
    let tag = format!("{}[bundle]", leading_whitespace(node_raw));
    doc.lines.insert(node_idx, DocLine::Tag(tag));
    true
}

/// Remove an `[orphan]` marker directly attached to `path`.
pub fn unmark_orphan(doc: &mut Document, path: &str) -> bool {
    unmark_marker(doc, path, OrphanMarkerKind::Orphan)
}

/// Remove orphan-bundle semantics directly attached to `path`.
///
/// For `[orphan bundle]`, this removes only `orphan`, leaving `[bundle]`.
pub fn unmark_orphan_subtree(doc: &mut Document, path: &str) -> bool {
    unmark_marker(doc, path, OrphanMarkerKind::OrphanSubtree)
}

/// Remove bundle semantics directly attached to `path`.
///
/// For `[orphan bundle]`, this removes only `bundle`, leaving `[orphan]`.
pub fn unmark_link_subtree(doc: &mut Document, path: &str) -> bool {
    let Some((node_idx, _)) = find_node_range(&doc.lines, path) else {
        return false;
    };
    let Some(tag_idx) = attached_tag_line_idx(doc, node_idx) else {
        return false;
    };
    let raw = match &doc.lines[tag_idx] {
        DocLine::Tag(raw) => raw.clone(),
        _ => return false,
    };
    let Some(mut tokens) = parse_tag_tokens(raw.trim()) else {
        return false;
    };
    if !tokens.iter().any(|t| t == TAG_BUNDLE) {
        return false;
    }
    tokens.retain(|t| t != TAG_BUNDLE);
    let tokens = canonicalize_tag_tokens(tokens);
    if tokens.is_empty() {
        doc.lines.remove(tag_idx);
    } else {
        doc.lines[tag_idx] = DocLine::Tag(format!(
            "{}{}",
            leading_whitespace(&raw),
            format_tag_tokens(&tokens)
        ));
    }
    true
}

/// Return explicitly tagged orphan markers as `(path, marker_kind)`.
pub fn explicit_orphan_markers(doc: &Document) -> Vec<(String, OrphanMarkerKind)> {
    let mut markers = Vec::new();

    for (idx, line) in doc.lines.iter().enumerate() {
        let DocLine::Tag(raw) = line else {
            continue;
        };
        let trimmed = raw.trim();
        let kind = if is_orphan_tag(trimmed) {
            OrphanMarkerKind::Orphan
        } else if is_orphan_subtree_tag(trimmed) {
            OrphanMarkerKind::OrphanSubtree
        } else {
            continue;
        };

        let Some(target_idx) = tag_target_node_idx(doc, idx) else {
            continue;
        };
        let path = doc.lines[target_idx]
            .node_path()
            .expect("target_idx guaranteed to be node");

        if matches!(kind, OrphanMarkerKind::OrphanSubtree) && !path.ends_with('/') {
            continue;
        }

        markers.push((path.to_string(), kind));
    }

    markers
}

/// Return folder roots explicitly tagged with `[bundle]`.
pub fn explicit_link_subtree_markers(doc: &Document) -> Vec<String> {
    let mut roots = Vec::new();
    for (idx, line) in doc.lines.iter().enumerate() {
        let DocLine::Tag(raw) = line else {
            continue;
        };
        if !is_link_subtree_tag(raw.trim()) {
            continue;
        }
        let Some(target_idx) = tag_target_node_idx(doc, idx) else {
            continue;
        };
        let path = doc.lines[target_idx]
            .node_path()
            .expect("target_idx guaranteed to be node");
        if path.ends_with('/') {
            roots.push(path.to_string());
        }
    }
    roots
}

/// Convert an attached `[bundle]` marker to `[orphan bundle]`.
///
/// Returns `true` if converted, `false` if no matching marker was attached.
pub fn convert_link_subtree_to_orphan_subtree(doc: &mut Document, path: &str) -> bool {
    add_tokens_to_matching_marker(doc, path, is_link_subtree_tag, &[TAG_ORPHAN, TAG_BUNDLE])
}

/// Convert an attached `[orphan]` marker to `[orphan bundle]`.
///
/// Returns `true` if converted, `false` if no matching marker was attached.
#[allow(dead_code)] // kept as public document-mutation helper
pub fn convert_orphan_to_orphan_subtree(doc: &mut Document, path: &str) -> bool {
    add_tokens_to_matching_marker(doc, path, is_orphan_tag, &[TAG_ORPHAN, TAG_BUNDLE])
}

/// Lint and normalize marker tags in-place.
///
/// Currently removes floating marker tags (`[orphan]`, `[bundle]`,
/// `[orphan bundle]`) that are not directly associated with a following node
/// block, removes invalid subtree tags that point to non-folder nodes, and
/// collapses repeated blank-line runs to single separators. Also normalizes tag
/// and edge indentation to match the hierarchy.
pub fn lint(doc: &mut Document) -> LintReport {
    let mut report = LintReport::default();
    let mut i = 0;
    while i < doc.lines.len() {
        let is_tag = matches!(doc.lines[i], DocLine::Tag(_));
        if !is_tag {
            i += 1;
            continue;
        }

        if tag_target_node_idx(doc, i).is_none() {
            doc.lines.remove(i);
            report.removed_floating_orphan_tags += 1;
            continue;
        }

        if let DocLine::Tag(raw) = &doc.lines[i] {
            let trimmed = raw.trim();
            let Some(mut tokens) = parse_tag_tokens(trimmed) else {
                i += 1;
                continue;
            };
            let target_idx = tag_target_node_idx(doc, i).expect("checked above");
            let target_path = doc.lines[target_idx]
                .node_path()
                .expect("target_idx guaranteed to be node");

            // `bundle` only applies to folders; strip it for non-folder targets.
            if !target_path.ends_with('/') {
                tokens.retain(|t| t != TAG_BUNDLE);
            }

            let normalized = canonicalize_tag_tokens(tokens);
            if normalized.is_empty() {
                doc.lines.remove(i);
                report.removed_floating_orphan_tags += 1;
                continue;
            }

            let desired = format!(
                "{}{}",
                leading_whitespace(raw),
                format_tag_tokens(&normalized)
            );
            if *raw != desired {
                doc.lines[i] = DocLine::Tag(desired);
            }
        }

        i += 1;
    }

    let indent_unit = detect_indent_unit(doc);
    report.normalized_tag_indentation = normalize_tag_indentation(doc);
    report.normalized_edge_indentation = normalize_edge_indentation(doc, &indent_unit);
    report.removed_extra_blank_lines = collapse_consecutive_blank_lines(doc);
    trim_leading_and_trailing_blanks(doc);

    report
}

/// Return node paths marked as intentional orphans from marker tags.
///
/// Preferred format:
/// - `[orphan]` immediately above a node
/// - `[orphan bundle]` above a folder node (applies to folder + descendants)
pub fn intentional_orphans(doc: &Document) -> HashSet<String> {
    let mut marked = HashSet::new();
    let subtree_roots: Vec<String> = intentional_orphan_subtree_roots(doc).into_iter().collect();
    let node_paths: Vec<String> = doc
        .lines
        .iter()
        .filter_map(|line| line.node_path().map(String::from))
        .collect();

    for (idx, line) in doc.lines.iter().enumerate() {
        let DocLine::Tag(raw) = line else {
            continue;
        };

        let trimmed = raw.trim();
        if let Some(target_idx) = tag_target_node_idx(doc, idx) {
            let next_path = doc.lines[target_idx]
                .node_path()
                .expect("target_idx guaranteed to be node");
            if is_orphan_tag(trimmed) {
                marked.insert(next_path.to_string());
            }
        }
    }

    for root in subtree_roots {
        for path in &node_paths {
            if is_in_subtree(path, &root) {
                marked.insert(path.clone());
            }
        }
    }

    marked
}

/// Return folder roots tagged with `[orphan bundle]`.
pub fn intentional_orphan_subtree_roots(doc: &Document) -> HashSet<String> {
    let mut roots = HashSet::new();
    for (idx, line) in doc.lines.iter().enumerate() {
        let DocLine::Tag(raw) = line else {
            continue;
        };
        if !is_orphan_subtree_tag(raw.trim()) {
            continue;
        }
        let Some(target_idx) = tag_target_node_idx(doc, idx) else {
            continue;
        };
        let path = doc.lines[target_idx]
            .node_path()
            .expect("target_idx guaranteed to be node");
        if path.ends_with('/') {
            roots.insert(path.to_string());
        }
    }
    roots
}

/// Return folder roots tagged for subtree collapse.
///
/// Includes both:
/// - `[orphan bundle]` (intentional orphan semantics + collapse)
/// - `[bundle]` (linked-folder semantics + collapse)
pub fn collapsed_subtree_roots(doc: &Document) -> HashSet<String> {
    let mut roots = HashSet::new();
    for (idx, line) in doc.lines.iter().enumerate() {
        let DocLine::Tag(raw) = line else {
            continue;
        };
        let trimmed = raw.trim();
        if !is_orphan_subtree_tag(trimmed) && !is_link_subtree_tag(trimmed) {
            continue;
        }
        let Some(target_idx) = tag_target_node_idx(doc, idx) else {
            continue;
        };
        let path = doc.lines[target_idx]
            .node_path()
            .expect("target_idx guaranteed to be node");
        if path.ends_with('/') {
            roots.insert(path.to_string());
        }
    }
    roots
}

// ---------------------------------------------------------------------------
// Private helpers
// ---------------------------------------------------------------------------

/// Split input into raw lines (without line terminators), plus a trailing-newline flag.
///
/// Splitting on `\n` and preserving any `\r` within lines ensures that CRLF
/// files are handled transparently: joining with `\n` reproduces the original.
fn split_lines_raw(input: &str) -> (Vec<String>, bool) {
    if input.is_empty() {
        return (Vec::new(), false);
    }

    let trailing_newline = input.ends_with('\n');
    // Remove the final `\n` before splitting so we don't get a spurious empty
    // last element in the Vec.
    let body = if trailing_newline {
        &input[..input.len() - 1]
    } else {
        input
    };
    let lines = body.split('\n').map(String::from).collect();
    (lines, trailing_newline)
}

fn is_edge_line(raw: &str) -> bool {
    let trimmed = raw.trim_start();
    trimmed.starts_with("->") || trimmed.starts_with("<-") || trimmed.starts_with("--")
}

fn unmark_marker(doc: &mut Document, path: &str, kind: OrphanMarkerKind) -> bool {
    let Some((node_idx, _)) = find_node_range(&doc.lines, path) else {
        return false;
    };

    let Some(tag_idx) = attached_tag_line_idx(doc, node_idx) else {
        return false;
    };

    let raw = match &doc.lines[tag_idx] {
        DocLine::Tag(raw) => raw.clone(),
        _ => return false,
    };
    let trimmed = raw.trim();
    let matches = match kind {
        OrphanMarkerKind::Orphan => is_orphan_tag(trimmed),
        OrphanMarkerKind::OrphanSubtree => is_orphan_subtree_tag(trimmed),
    };
    if !matches {
        return false;
    }

    let Some(mut tokens) = parse_tag_tokens(trimmed) else {
        return false;
    };
    tokens.retain(|t| t != TAG_ORPHAN);
    let tokens = canonicalize_tag_tokens(tokens);
    if tokens.is_empty() {
        doc.lines.remove(tag_idx);
    } else {
        doc.lines[tag_idx] = DocLine::Tag(format!(
            "{}{}",
            leading_whitespace(&raw),
            format_tag_tokens(&tokens)
        ));
    }
    true
}

fn add_tokens_to_matching_marker(
    doc: &mut Document,
    path: &str,
    matcher: fn(&str) -> bool,
    tokens: &[&str],
) -> bool {
    let Some((node_idx, _)) = find_node_range(&doc.lines, path) else {
        return false;
    };
    let Some(tag_idx) = attached_tag_line_idx(doc, node_idx) else {
        return false;
    };

    let raw = match &doc.lines[tag_idx] {
        DocLine::Tag(raw) => raw.clone(),
        _ => return false,
    };
    if !matcher(raw.trim()) {
        return false;
    }
    let _ = ensure_tag_tokens_on_line(doc, tag_idx, tokens);
    // A successful conversion is considered handled even if already normalized.
    true
}

fn find_tag_block_start(doc: &Document, node_idx: usize) -> (usize, bool) {
    let mut i = node_idx;
    let mut tag_start = node_idx;
    let mut has_tag = false;

    while i > 0 {
        match &doc.lines[i - 1] {
            DocLine::Tag(_) => {
                has_tag = true;
                tag_start = i - 1;
                i -= 1;
            }
            DocLine::Blank(_) => {
                i -= 1;
            }
            _ => break,
        }
    }

    (tag_start, has_tag)
}

fn trailing_comment_block_start(lines: &[DocLine], from: usize, to: usize) -> usize {
    let mut i = to;
    let mut saw_comment = false;
    while i > from {
        match &lines[i - 1] {
            DocLine::Blank(_) => i -= 1,
            DocLine::Comment(_) => {
                saw_comment = true;
                i -= 1;
            }
            _ => break,
        }
    }
    if saw_comment { i } else { to }
}

fn attached_tag_block_start_before_node(lines: &[DocLine], node_idx: usize) -> Option<usize> {
    let mut i = node_idx;
    let mut saw_tag = false;

    while i > 0 {
        match &lines[i - 1] {
            DocLine::Blank(_) | DocLine::Comment(_) => i -= 1,
            DocLine::Tag(_) => {
                saw_tag = true;
                i -= 1;
            }
            _ => break,
        }
    }

    if saw_tag { Some(i) } else { None }
}

const TAG_ORPHAN: &str = "orphan";
const TAG_BUNDLE: &str = "bundle";

fn is_tag_line(trimmed: &str) -> bool {
    parse_tag_tokens(trimmed).is_some()
}

fn is_orphan_tag(trimmed: &str) -> bool {
    tag_has_token(trimmed, TAG_ORPHAN) && !tag_has_token(trimmed, TAG_BUNDLE)
}

fn is_orphan_subtree_tag(trimmed: &str) -> bool {
    tag_has_token(trimmed, TAG_ORPHAN) && tag_has_token(trimmed, TAG_BUNDLE)
}

fn is_link_subtree_tag(trimmed: &str) -> bool {
    tag_has_token(trimmed, TAG_BUNDLE) && !tag_has_token(trimmed, TAG_ORPHAN)
}

fn tag_has_token(trimmed: &str, needle: &str) -> bool {
    parse_tag_tokens(trimmed)
        .map(|tokens| tokens.iter().any(|t| t == needle))
        .unwrap_or(false)
}

fn parse_tag_tokens(trimmed: &str) -> Option<Vec<String>> {
    let inner = trimmed.strip_prefix('[')?.strip_suffix(']')?.trim();
    if inner.is_empty() {
        return None;
    }
    Some(inner.split_whitespace().map(|t| t.to_string()).collect())
}

fn canonicalize_tag_tokens(tokens: Vec<String>) -> Vec<String> {
    let mut deduped: Vec<String> = Vec::new();
    let mut seen = HashSet::new();
    for token in tokens {
        if seen.insert(token.clone()) {
            deduped.push(token);
        }
    }

    let has_orphan = deduped.iter().any(|t| t == TAG_ORPHAN);
    let has_bundle = deduped.iter().any(|t| t == TAG_BUNDLE);

    let mut out: Vec<String> = Vec::new();
    if has_orphan {
        out.push(TAG_ORPHAN.to_string());
    }
    if has_bundle {
        out.push(TAG_BUNDLE.to_string());
    }
    for token in deduped {
        if token != TAG_ORPHAN && token != TAG_BUNDLE {
            out.push(token);
        }
    }
    out
}

fn format_tag_tokens(tokens: &[String]) -> String {
    format!("[{}]", tokens.join(" "))
}

fn attached_tag_line_idx(doc: &Document, node_idx: usize) -> Option<usize> {
    for i in (0..node_idx).rev() {
        match &doc.lines[i] {
            DocLine::Blank(_) | DocLine::Comment(_) => continue,
            DocLine::Tag(_) => return Some(i),
            DocLine::Node(_) | DocLine::Edge(_) => return None,
        }
    }
    None
}

fn ensure_tag_tokens_on_line(doc: &mut Document, tag_idx: usize, required: &[&str]) -> bool {
    let raw = match &doc.lines[tag_idx] {
        DocLine::Tag(raw) => raw.clone(),
        _ => return false,
    };
    let Some(mut tokens) = parse_tag_tokens(raw.trim()) else {
        return false;
    };
    let before = canonicalize_tag_tokens(tokens.clone());
    for token in required {
        if !tokens.iter().any(|existing| existing == token) {
            tokens.push((*token).to_string());
        }
    }
    let after = canonicalize_tag_tokens(tokens);
    if after == before {
        return false;
    }
    doc.lines[tag_idx] = DocLine::Tag(format!(
        "{}{}",
        leading_whitespace(&raw),
        format_tag_tokens(&after)
    ));
    true
}

fn is_in_subtree(path: &str, root: &str) -> bool {
    if root.ends_with('/') {
        path == root || path.starts_with(root)
    } else {
        path == root
    }
}

fn tag_target_node_idx(doc: &Document, tag_idx: usize) -> Option<usize> {
    let mut i = tag_idx + 1;
    while i < doc.lines.len() {
        match &doc.lines[i] {
            DocLine::Blank(_) | DocLine::Comment(_) => i += 1,
            DocLine::Node(_) => return Some(i),
            DocLine::Tag(_) | DocLine::Edge(_) => return None,
        }
    }
    None
}

fn trim_leading_and_trailing_blanks(doc: &mut Document) {
    while matches!(doc.lines.first(), Some(DocLine::Blank(_))) {
        doc.lines.remove(0);
    }
    while matches!(doc.lines.last(), Some(DocLine::Blank(_))) {
        doc.lines.pop();
    }
}

fn normalize_tag_indentation(doc: &mut Document) -> usize {
    let mut changed = 0usize;
    for i in 0..doc.lines.len() {
        let DocLine::Tag(raw) = &doc.lines[i] else {
            continue;
        };
        let Some(target_idx) = tag_target_node_idx(doc, i) else {
            continue;
        };
        let node_indent = leading_whitespace(doc.lines[target_idx].raw());
        let desired = format!("{}{}", node_indent, raw.trim());
        if *raw != desired {
            doc.lines[i] = DocLine::Tag(desired);
            changed += 1;
        }
    }
    changed
}

fn normalize_edge_indentation(doc: &mut Document, indent_unit: &str) -> usize {
    let mut changed = 0usize;
    let mut current_node_indent = String::new();
    let mut have_node = false;

    for i in 0..doc.lines.len() {
        match &doc.lines[i] {
            DocLine::Node(raw) => {
                current_node_indent = leading_whitespace(raw).to_string();
                have_node = true;
            }
            DocLine::Edge(raw) if have_node => {
                let desired = format!("{}{}", current_node_indent, indent_unit);
                let normalized = format!("{}{}", desired, raw.trim_start());
                if *raw != normalized {
                    doc.lines[i] = DocLine::Edge(normalized);
                    changed += 1;
                }
            }
            _ => {}
        }
    }

    changed
}

fn collapse_consecutive_blank_lines(doc: &mut Document) -> usize {
    let mut removed = 0usize;
    let mut i = 1usize;
    while i < doc.lines.len() {
        if matches!(doc.lines[i - 1], DocLine::Blank(_))
            && matches!(doc.lines[i], DocLine::Blank(_))
        {
            doc.lines.remove(i);
            removed += 1;
        } else {
            i += 1;
        }
    }
    removed
}

/// Parse the semantic content of an edge line.
///
/// Expected form (after stripping optional leading whitespace):
/// `-> <target> : <label>`, `<- <target> : <label>`, or `-- <target> : <label>`
fn parse_edge_line(raw: &str) -> Result<Edge> {
    let trimmed = raw.trim();

    let (kind, rest) = if let Some(r) = trimmed.strip_prefix("->") {
        (EdgeKind::Directed, r)
    } else if let Some(r) = trimmed.strip_prefix("<-") {
        (EdgeKind::Incoming, r)
    } else if let Some(r) = trimmed.strip_prefix("--") {
        (EdgeKind::Undirected, r)
    } else {
        bail!(
            "edge must start with '->', '<-', or '--', got: {:?}",
            trimmed
        );
    };

    let rest = rest.trim_start();

    let colon_pos = rest
        .find(':')
        .ok_or_else(|| anyhow::anyhow!("edge missing ':' separator: {:?}", raw))?;

    let target = rest[..colon_pos].trim().to_string();
    let label = rest[colon_pos + 1..].trim().to_string();

    if target.is_empty() {
        bail!("edge has empty target path: {:?}", raw);
    }

    Ok(Edge {
        target,
        kind,
        label,
    })
}

fn leading_whitespace(raw: &str) -> &str {
    let len = raw.len() - raw.trim_start_matches(|c| c == ' ' || c == '\t').len();
    &raw[..len]
}

/// Format an `Edge` as a canonical edge line (4-space indent, no extra padding).
fn format_edge_line(edge: &Edge) -> String {
    format_edge_line_with_indent(edge, DEFAULT_INDENT_UNIT)
}

fn format_edge_line_with_indent(edge: &Edge, indent: &str) -> String {
    let arrow = match edge.kind {
        EdgeKind::Directed => "->",
        EdgeKind::Incoming => "<-",
        EdgeKind::Undirected => "--",
    };
    if edge.label.is_empty() {
        format!("{indent}{} {} :", arrow, edge.target)
    } else {
        format!("{indent}{} {} : {}", arrow, edge.target, edge.label)
    }
}

fn format_node_line(path: &str) -> String {
    format_node_line_with_indent(path, DEFAULT_INDENT_UNIT)
}

fn edge_kind_rank(kind: &EdgeKind) -> usize {
    match kind {
        EdgeKind::Undirected => 0,
        EdgeKind::Directed => 1,
        EdgeKind::Incoming => 2,
    }
}

fn format_node_line_with_indent(path: &str, indent: &str) -> String {
    let depth = path_depth(path);
    format!("{}{}", indent.repeat(depth), path)
}

const DEFAULT_INDENT_UNIT: &str = "    ";

fn detect_indent_unit(doc: &Document) -> String {
    let mut space_gcd: Option<usize> = None;

    for line in &doc.lines {
        let raw = match line {
            DocLine::Node(s) | DocLine::Edge(s) => s.as_str(),
            _ => continue,
        };

        let leading_len = raw.len() - raw.trim_start_matches(|c| c == ' ' || c == '\t').len();
        if leading_len == 0 {
            continue;
        }
        let leading = &raw[..leading_len];
        if leading.contains('\t') {
            return "\t".to_string();
        }
        if leading.bytes().all(|b| b == b' ') {
            space_gcd = Some(space_gcd.map_or(leading.len(), |g| gcd(g, leading.len())));
        }
    }

    " ".repeat(space_gcd.unwrap_or(DEFAULT_INDENT_UNIT.len()).max(1))
}

fn edge_indent_for_node(
    doc: &Document,
    node_idx: usize,
    end_idx: usize,
    indent_unit: &str,
) -> String {
    for idx in node_idx + 1..end_idx {
        if let DocLine::Edge(raw) = &doc.lines[idx] {
            return leading_whitespace(raw).to_string();
        }
    }
    let node_indent = leading_whitespace(doc.lines[node_idx].raw());
    format!("{}{}", node_indent, indent_unit)
}

fn gcd(mut a: usize, mut b: usize) -> usize {
    while b != 0 {
        let r = a % b;
        a = b;
        b = r;
    }
    a
}

fn path_depth(path: &str) -> usize {
    path.trim_end_matches('/')
        .split('/')
        .filter(|segment| !segment.is_empty())
        .count()
        .saturating_sub(1)
}

/// Return `(node_line_idx, end_idx)` for the node with the given path.
///
/// `end_idx` is the exclusive upper bound of lines that "belong" to this node
/// (i.e. everything up to but not including the next Node line, or end of Vec).
fn find_node_range(lines: &[DocLine], path: &str) -> Option<(usize, usize)> {
    let node_idx = lines.iter().position(|l| l.node_path() == Some(path))?;

    let end_idx = lines[node_idx + 1..]
        .iter()
        .position(|l| matches!(l, DocLine::Node(_)))
        .map(|rel| node_idx + 1 + rel)
        .unwrap_or(lines.len());

    Some((node_idx, end_idx))
}

fn find_insert_idx(doc: &Document, path: &str) -> Option<usize> {
    doc.lines
        .iter()
        .enumerate()
        .find_map(|(idx, line)| match line.node_path() {
            Some(existing) if compare_hierarchical_paths(path, existing).is_lt() => Some(idx),
            _ => None,
        })
}

fn should_remove_edge_to_child_blank(doc: &Document, path: &str, insert_at: usize) -> bool {
    if insert_at < 2 {
        return false;
    }
    if !matches!(doc.lines[insert_at - 1], DocLine::Blank(_)) {
        return false;
    }
    if !matches!(doc.lines[insert_at - 2], DocLine::Edge(_)) {
        return false;
    }

    // Only collapse this blank when the insertion is for a descendant of the
    // nearest previous node (i.e. inserting child nodes under a folder that
    // currently ends with one or more edge lines).
    let parent = doc.lines[..insert_at]
        .iter()
        .rev()
        .find_map(DocLine::node_path);
    let Some(parent) = parent else {
        return false;
    };
    parent.ends_with('/') && path != parent && path.starts_with(parent)
}

fn compare_hierarchical_paths(a: &str, b: &str) -> std::cmp::Ordering {
    use std::cmp::Ordering;

    let (a_comps, a_is_dir) = path_components(a);
    let (b_comps, b_is_dir) = path_components(b);
    let min_len = a_comps.len().min(b_comps.len());

    for i in 0..min_len {
        if a_comps[i] == b_comps[i] {
            continue;
        }
        let a_kind = component_kind(i, a_comps.len(), a_is_dir);
        let b_kind = component_kind(i, b_comps.len(), b_is_dir);
        if a_kind != b_kind {
            return a_kind.cmp(&b_kind);
        }
        return a_comps[i].cmp(b_comps[i]);
    }

    match a_comps.len().cmp(&b_comps.len()) {
        Ordering::Equal => a_is_dir.cmp(&b_is_dir).reverse(),
        other => other,
    }
}

fn path_components(path: &str) -> (Vec<&str>, bool) {
    let is_dir = path.ends_with('/');
    let trimmed = path.trim_end_matches('/');
    let comps = if trimmed.is_empty() {
        Vec::new()
    } else {
        trimmed.split('/').collect()
    };
    (comps, is_dir)
}

fn component_kind(idx: usize, len: usize, is_dir: bool) -> u8 {
    if idx < len.saturating_sub(1) {
        0
    } else if is_dir {
        0
    } else {
        1
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // -----------------------------------------------------------------------
    // Round-trip tests — the core invariant
    // -----------------------------------------------------------------------

    fn rt(input: &str) {
        let doc = parse(input).expect("parse failed");
        let output = serialize(&doc);
        assert_eq!(output, input, "round-trip failed");
    }

    #[test]
    fn roundtrip_empty() {
        rt("");
    }

    #[test]
    fn roundtrip_single_orphan_with_newline() {
        rt("src/main.rs\n");
    }

    #[test]
    fn roundtrip_single_orphan_no_trailing_newline() {
        rt("src/main.rs");
    }

    #[test]
    fn roundtrip_directed_edge_with_label() {
        rt("src/main.rs\n    -> src/auth/ : bootstraps authentication\n");
    }

    #[test]
    fn roundtrip_directed_edge_empty_label() {
        rt("src/main.rs\n    -> src/auth/ :\n");
    }

    #[test]
    fn roundtrip_undirected_edge() {
        rt("src/auth/\n    -- docs/auth.md : documented by\n");
    }

    #[test]
    fn roundtrip_incoming_edge() {
        rt("src/auth/\n    <- src/main.rs : bootstrapped by\n");
    }

    #[test]
    fn roundtrip_multiple_nodes() {
        rt("src/main.rs\n    -> src/auth/ : bootstraps\n\nsrc/auth/\n    -> src/db/ : queries\n");
    }

    #[test]
    fn roundtrip_comments_preserved() {
        rt(
            "# This is a comment\n\nsrc/main.rs\n    -> src/auth/ : foo\n\n# Another comment\nsrc/auth/\n",
        );
    }

    #[test]
    fn roundtrip_blank_lines_preserved() {
        rt("\n\nsrc/main.rs\n\n\nsrc/auth/\n");
    }

    #[test]
    fn roundtrip_tab_indent() {
        rt("src/main.rs\n\t-> src/auth/ : foo\n");
    }

    #[test]
    fn roundtrip_indented_node() {
        rt("src/\n    src/main.rs\n");
    }

    #[test]
    fn roundtrip_orphan_tags() {
        rt("[orphan]\na.rs\n\n[orphan bundle]\nsrc/\n");
    }

    #[test]
    fn roundtrip_link_subtree_tag() {
        rt("[bundle]\nsrc/\n");
    }

    #[test]
    fn roundtrip_preserves_whitespace_aligned_edges() {
        // Hand-authored alignment — must be preserved verbatim.
        rt(
            "src/main.rs\n    -> src/auth/        : bootstraps authentication\n    -> config.toml      : reads secrets\n    -- docs/arch.md     : documented by\n",
        );
    }

    #[test]
    fn roundtrip_folder_node() {
        rt("src/auth/\n");
    }

    #[test]
    fn roundtrip_orphan_after_edges() {
        rt("src/main.rs\n    -> src/auth/ : foo\n\ndocs/arch.md\n\nREADME.md\n");
    }

    #[test]
    fn roundtrip_indented_comment_preserved() {
        // An indented comment is stored verbatim as a Comment line.
        rt("src/main.rs\n    # not an edge\n    -> src/auth/ : foo\n");
    }

    #[test]
    fn roundtrip_spec_example() {
        let input = "# This is a comment\n\nsrc/main.rs\n    -> src/auth/        : bootstraps authentication\n    -> config.toml      : reads secrets\n    -- docs/arch.md     : documented by\n\nsrc/auth/\n    -> src/db/          : queries user table\n    -> src/models/      : uses User model\n\ndocs/arch.md\n\nREADME.md\n";
        rt(input);
    }

    // -----------------------------------------------------------------------
    // Semantic extraction: to_graph
    // -----------------------------------------------------------------------

    #[test]
    fn to_graph_single_orphan() {
        let input = "src/main.rs\n";
        let doc = parse(input).unwrap();
        let graph = to_graph(&doc).unwrap();
        assert_eq!(graph.nodes.len(), 1);
        assert_eq!(graph.nodes[0].path, "src/main.rs");
        assert!(graph.nodes[0].is_orphan());
    }

    #[test]
    fn to_graph_directed_edge() {
        let input = "src/main.rs\n    -> src/auth/ : bootstraps\n";
        let doc = parse(input).unwrap();
        let graph = to_graph(&doc).unwrap();
        assert_eq!(graph.nodes[0].edges.len(), 1);
        let e = &graph.nodes[0].edges[0];
        assert_eq!(e.target, "src/auth/");
        assert_eq!(e.kind, EdgeKind::Directed);
        assert_eq!(e.label, "bootstraps");
    }

    #[test]
    fn to_graph_undirected_edge() {
        let input = "src/auth/\n    -- docs/auth.md : documented by\n";
        let doc = parse(input).unwrap();
        let graph = to_graph(&doc).unwrap();
        assert_eq!(graph.nodes[0].edges[0].kind, EdgeKind::Undirected);
        assert_eq!(graph.nodes[0].edges[0].label, "documented by");
    }

    #[test]
    fn to_graph_incoming_edge() {
        let input = "src/auth/\n    <- src/main.rs : bootstrapped by\n";
        let doc = parse(input).unwrap();
        let graph = to_graph(&doc).unwrap();
        assert_eq!(graph.nodes[0].edges[0].kind, EdgeKind::Incoming);
        assert_eq!(graph.nodes[0].edges[0].target, "src/main.rs");
    }

    #[test]
    fn to_graph_empty_label() {
        let input = "src/main.rs\n    -> src/auth/ :\n";
        let doc = parse(input).unwrap();
        let graph = to_graph(&doc).unwrap();
        assert_eq!(graph.nodes[0].edges[0].label, "");
    }

    #[test]
    fn to_graph_multiple_nodes_and_edges() {
        let input = "src/main.rs\n    -> src/auth/ : foo\n\nsrc/auth/\n    -> src/db/ : bar\n\ndocs/arch.md\n";
        let doc = parse(input).unwrap();
        let graph = to_graph(&doc).unwrap();
        assert_eq!(graph.nodes.len(), 3);
        assert_eq!(graph.nodes[0].edges.len(), 1);
        assert_eq!(graph.nodes[1].edges.len(), 1);
        assert!(graph.nodes[2].is_orphan());
    }

    #[test]
    fn to_graph_comments_and_blanks_ignored() {
        let input = "# comment\n\n[orphan]\nsrc/main.rs\n    -> src/auth/ : foo\n\n# another\n";
        let doc = parse(input).unwrap();
        let graph = to_graph(&doc).unwrap();
        assert_eq!(graph.nodes.len(), 1);
        assert_eq!(graph.nodes[0].edges.len(), 1);
    }

    #[test]
    fn to_graph_edge_after_blank_belongs_to_node() {
        // Blank lines between a node and its edges are allowed (blanks ignored semantically).
        let input = "src/main.rs\n\n    -> src/auth/ : foo\n";
        let doc = parse(input).unwrap();
        let graph = to_graph(&doc).unwrap();
        assert_eq!(graph.nodes[0].edges.len(), 1);
    }

    #[test]
    fn to_graph_preserves_node_order() {
        let input = "b.rs\na.rs\nc.rs\n";
        let doc = parse(input).unwrap();
        let graph = to_graph(&doc).unwrap();
        assert_eq!(graph.nodes[0].path, "b.rs");
        assert_eq!(graph.nodes[1].path, "a.rs");
        assert_eq!(graph.nodes[2].path, "c.rs");
    }

    #[test]
    fn to_graph_parses_indented_nodes() {
        let input = "src/\n    src/main.rs\n";
        let doc = parse(input).unwrap();
        let graph = to_graph(&doc).unwrap();
        assert_eq!(graph.nodes.len(), 2);
        assert_eq!(graph.nodes[1].path, "src/main.rs");
    }

    // -----------------------------------------------------------------------
    // Error cases
    // -----------------------------------------------------------------------

    #[test]
    fn error_duplicate_node() {
        let input = "src/main.rs\nsrc/main.rs\n";
        let doc = parse(input).unwrap();
        assert!(to_graph(&doc).is_err());
    }

    #[test]
    fn error_edge_before_any_node() {
        // parse succeeds (we validate edge syntax at parse time),
        // but to_graph should fail.
        let input = "    -> src/auth/ : foo\n";
        let doc = parse(input).unwrap();
        assert!(to_graph(&doc).is_err());
    }

    #[test]
    fn error_edge_missing_colon() {
        let input = "src/main.rs\n    -> src/auth/\n";
        assert!(parse(input).is_err());
    }

    #[test]
    fn error_edge_missing_target() {
        let input = "src/main.rs\n    -> :\n";
        assert!(parse(input).is_err());
    }

    #[test]
    fn error_edge_bad_arrow() {
        let input = "src/main.rs\n    => src/auth/ : foo\n";
        assert!(parse(input).is_err());
    }

    // -----------------------------------------------------------------------
    // from_graph
    // -----------------------------------------------------------------------

    #[test]
    fn from_graph_empty() {
        let graph = Graph::new();
        let doc = from_graph(&graph);
        assert!(doc.lines.is_empty());
        assert_eq!(serialize(&doc), "");
    }

    #[test]
    fn from_graph_single_orphan() {
        let mut graph = Graph::new();
        graph.add_node(Node::new("src/main.rs"));
        let doc = from_graph(&graph);
        assert_eq!(serialize(&doc), "    src/main.rs\n");
    }

    #[test]
    fn from_graph_with_edges() {
        let mut graph = Graph::new();
        let mut node = Node::new("src/main.rs");
        node.edges.push(Edge {
            target: "src/auth/".into(),
            kind: EdgeKind::Directed,
            label: "bootstraps".into(),
        });
        graph.add_node(node);
        let out = serialize(&from_graph(&graph));
        assert!(out.contains("src/main.rs\n"));
        assert!(out.contains("    -> src/auth/ : bootstraps"));
    }

    #[test]
    fn from_graph_empty_label_edge() {
        let mut graph = Graph::new();
        let mut node = Node::new("src/main.rs");
        node.edges.push(Edge {
            target: "src/auth/".into(),
            kind: EdgeKind::Directed,
            label: String::new(),
        });
        graph.add_node(node);
        let out = serialize(&from_graph(&graph));
        assert!(out.contains("    -> src/auth/ :"));
    }

    #[test]
    fn from_graph_separates_nodes_with_blank_line() {
        let mut graph = Graph::new();
        graph.add_node(Node::new("a.rs"));
        graph.add_node(Node::new("b.rs"));
        let out = serialize(&from_graph(&graph));
        assert_eq!(out, "a.rs\n\nb.rs\n");
    }

    // -----------------------------------------------------------------------
    // Document mutation: add_node
    // -----------------------------------------------------------------------

    #[test]
    fn add_node_to_empty_doc() {
        let mut doc = Document::default();
        add_node(&mut doc, "src/main.rs");
        assert_eq!(serialize(&doc), "    src/main.rs\n");
    }

    #[test]
    fn add_node_appends_with_blank_separator() {
        let mut doc = parse("a.rs\n").unwrap();
        add_node(&mut doc, "b.rs");
        assert_eq!(serialize(&doc), "a.rs\n\nb.rs\n");
    }

    #[test]
    fn add_node_inserts_by_hierarchy_before_root_file() {
        let mut doc = parse("src/\n\nroot.rs\n").unwrap();
        add_node(&mut doc, "src/new/");
        assert_eq!(serialize(&doc), "src/\n\n    src/new/\n\nroot.rs\n");
    }

    #[test]
    fn add_node_under_folder_with_edges_avoids_extra_blank_before_first_child() {
        let mut doc = parse("src/\n    <- consumer.rs : used by\n\nconsumer.rs\n").unwrap();
        add_node(&mut doc, "src/new.rs");
        let out = serialize(&doc);
        assert!(
            out.contains("src/\n    <- consumer.rs : used by\n    src/new.rs\n\nconsumer.rs\n")
        );
    }

    #[test]
    fn intentional_orphans_marker_and_subtree() {
        let doc = parse(
            "\
[orphan]
simple.rs

[orphan bundle]
src/
    src/lib.rs
    src/sub/
        src/sub/leaf.rs
",
        )
        .unwrap();
        let marked = intentional_orphans(&doc);
        assert!(marked.contains("simple.rs"));
        assert!(marked.contains("src/"));
        assert!(marked.contains("src/lib.rs"));
        assert!(marked.contains("src/sub/"));
        assert!(marked.contains("src/sub/leaf.rs"));
    }

    #[test]
    fn intentional_orphans_ignores_floating_tags() {
        let doc = parse(
            "\
[orphan]

[orphan]
a.rs
",
        )
        .unwrap();
        let marked = intentional_orphans(&doc);
        assert_eq!(marked.len(), 1);
        assert!(marked.contains("a.rs"));
    }

    #[test]
    fn mark_orphan_inserts_simple_marker() {
        let mut doc = parse("a.rs\n").unwrap();
        assert!(mark_orphan(&mut doc, "a.rs"));
        assert_eq!(serialize(&doc), "[orphan]\na.rs\n");
    }

    #[test]
    fn mark_orphan_no_duplicate() {
        let mut doc = parse("[orphan]\na.rs\n").unwrap();
        assert!(!mark_orphan(&mut doc, "a.rs"));
        assert_eq!(serialize(&doc), "[orphan]\na.rs\n");
    }

    #[test]
    fn mark_orphan_aligns_with_nested_node_indent() {
        let mut doc = parse("src/\n\n    src/main.rs\n").unwrap();
        assert!(mark_orphan(&mut doc, "src/main.rs"));
        assert_eq!(serialize(&doc), "src/\n\n    [orphan]\n    src/main.rs\n");
    }

    #[test]
    fn mark_orphan_subtree_inserts_marker() {
        let mut doc = parse("src/\n").unwrap();
        assert!(mark_orphan_subtree(&mut doc, "src/"));
        assert_eq!(serialize(&doc), "[orphan bundle]\nsrc/\n");
    }

    #[test]
    fn mark_orphan_subtree_rejects_file_nodes() {
        let mut doc = parse("a.rs\n").unwrap();
        assert!(!mark_orphan_subtree(&mut doc, "a.rs"));
        assert_eq!(serialize(&doc), "a.rs\n");
    }

    #[test]
    fn mark_orphan_subtree_aligns_with_nested_node_indent() {
        let mut doc = parse("src/\n\n    src/sub/\n").unwrap();
        assert!(mark_orphan_subtree(&mut doc, "src/sub/"));
        assert_eq!(
            serialize(&doc),
            "src/\n\n    [orphan bundle]\n    src/sub/\n"
        );
    }

    #[test]
    fn mark_link_subtree_inserts_marker() {
        let mut doc = parse("src/\n").unwrap();
        assert!(mark_link_subtree(&mut doc, "src/"));
        assert_eq!(serialize(&doc), "[bundle]\nsrc/\n");
    }

    #[test]
    fn explicit_orphan_markers_detects_both_tag_types() {
        let doc = parse("[orphan]\na.rs\n\n[orphan bundle]\nsrc/\n").unwrap();
        let markers = explicit_orphan_markers(&doc);
        assert_eq!(markers.len(), 2);
        assert_eq!(markers[0], ("a.rs".to_string(), OrphanMarkerKind::Orphan));
        assert_eq!(
            markers[1],
            ("src/".to_string(), OrphanMarkerKind::OrphanSubtree)
        );
    }

    #[test]
    fn explicit_link_subtree_markers_detects_folder_roots() {
        let doc = parse("[bundle]\nsrc/\n\n[orphan bundle]\nother/\n").unwrap();
        let roots = explicit_link_subtree_markers(&doc);
        assert_eq!(roots, vec!["src/".to_string()]);
    }

    #[test]
    fn unmark_orphan_removes_marker_only() {
        let mut doc = parse("[orphan]\na.rs\n").unwrap();
        assert!(unmark_orphan(&mut doc, "a.rs"));
        assert_eq!(serialize(&doc), "a.rs\n");
    }

    #[test]
    fn unmark_link_subtree_removes_bundle_and_keeps_orphan() {
        let mut doc = parse("[orphan bundle]\nsrc/\n").unwrap();
        assert!(unmark_link_subtree(&mut doc, "src/"));
        assert_eq!(serialize(&doc), "[orphan]\nsrc/\n");
    }

    #[test]
    fn unmark_orphan_subtree_removes_orphan_and_keeps_bundle() {
        let mut doc = parse("[orphan bundle]\nsrc/\n").unwrap();
        assert!(unmark_orphan_subtree(&mut doc, "src/"));
        assert_eq!(serialize(&doc), "[bundle]\nsrc/\n");
    }

    #[test]
    fn convert_link_subtree_to_orphan_subtree_rewrites_marker() {
        let mut doc = parse("[bundle]\nsrc/\n").unwrap();
        assert!(convert_link_subtree_to_orphan_subtree(&mut doc, "src/"));
        assert_eq!(serialize(&doc), "[orphan bundle]\nsrc/\n");
    }

    #[test]
    fn convert_link_subtree_to_orphan_subtree_preserves_indent() {
        let mut doc = parse("src/\n\n    [bundle]\n    src/html/\n").unwrap();
        assert!(convert_link_subtree_to_orphan_subtree(
            &mut doc,
            "src/html/"
        ));
        assert_eq!(
            serialize(&doc),
            "src/\n\n    [orphan bundle]\n    src/html/\n"
        );
    }

    #[test]
    fn convert_orphan_to_orphan_subtree_rewrites_marker() {
        let mut doc = parse("[orphan]\nsrc/\n").unwrap();
        assert!(convert_orphan_to_orphan_subtree(&mut doc, "src/"));
        assert_eq!(serialize(&doc), "[orphan bundle]\nsrc/\n");
    }

    #[test]
    fn lint_removes_floating_orphan_tags() {
        let mut doc = parse(
            "\
[orphan]

[orphan bundle]
# comment

a.rs
",
        )
        .unwrap();
        let report = lint(&mut doc);
        assert_eq!(report.removed_floating_orphan_tags, 1);
        assert_eq!(serialize(&doc), "[orphan]\n# comment\n\na.rs\n");
    }

    #[test]
    fn lint_removes_invalid_link_subtree_tag_on_file() {
        let mut doc = parse("[bundle]\na.rs\n").unwrap();
        let report = lint(&mut doc);
        assert_eq!(report.removed_floating_orphan_tags, 1);
        assert_eq!(serialize(&doc), "a.rs\n");
    }

    #[test]
    fn lint_keeps_orphan_subtree_on_empty_folder() {
        let mut doc = parse("[orphan bundle]\nempty/\n").unwrap();
        let report = lint(&mut doc);
        assert_eq!(report.removed_floating_orphan_tags, 0);
        assert_eq!(report.removed_extra_blank_lines, 0);
        assert_eq!(serialize(&doc), "[orphan bundle]\nempty/\n");
    }

    #[test]
    fn lint_collapses_consecutive_blank_lines() {
        let mut doc = parse("a.rs\n\n\nb.rs\n\n\nc.rs\n").unwrap();
        let report = lint(&mut doc);
        assert_eq!(report.removed_floating_orphan_tags, 0);
        assert_eq!(report.removed_extra_blank_lines, 2);
        assert_eq!(serialize(&doc), "a.rs\n\nb.rs\n\nc.rs\n");
    }

    #[test]
    fn lint_normalizes_tag_and_edge_indentation_to_node_depth() {
        let mut doc =
            parse("src/\n\n[orphan]\n    src/main.rs\n    -> README.md : documents\n").unwrap();
        let report = lint(&mut doc);
        assert_eq!(report.normalized_tag_indentation, 1);
        assert_eq!(report.normalized_edge_indentation, 1);
        assert_eq!(
            serialize(&doc),
            "src/\n\n    [orphan]\n    src/main.rs\n        -> README.md : documents\n"
        );
    }

    // -----------------------------------------------------------------------
    // Document mutation: remove_node
    // -----------------------------------------------------------------------

    #[test]
    fn remove_node_present() {
        let mut doc = parse("a.rs\n\nb.rs\n    -> a.rs : foo\n\nc.rs\n").unwrap();
        let removed = remove_node(&mut doc, "b.rs");
        assert!(removed);
        let out = serialize(&doc);
        assert!(!out.contains("b.rs"));
        assert!(out.contains("a.rs\n"));
        assert!(out.contains("c.rs\n"));
    }

    #[test]
    fn remove_node_not_present() {
        let mut doc = parse("a.rs\n").unwrap();
        assert!(!remove_node(&mut doc, "z.rs"));
    }

    #[test]
    fn remove_middle_node_preserves_separator_spacing() {
        let mut doc = parse("a/\n\n    a/file.rs\n\n    b/\n").unwrap();
        assert!(remove_node(&mut doc, "a/file.rs"));
        assert_eq!(serialize(&doc), "a/\n\n    b/\n");
    }

    #[test]
    fn remove_middle_node_preserves_following_tagged_block() {
        let mut doc = parse(
            "[orphan bundle]\na/\n\n    a/file.rs\n\n    [orphan bundle]\n    b/\n\n        b/file.rs\n",
        )
        .unwrap();
        assert!(remove_node(&mut doc, "a/file.rs"));
        let out = serialize(&doc);
        assert!(out.contains("    [orphan bundle]\n    b/\n"));
    }

    #[test]
    fn remove_node_also_removes_attached_orphan_tag() {
        let mut doc = parse("[orphan]\nnode.rs\n\nnext.rs\n").unwrap();
        assert!(remove_node(&mut doc, "node.rs"));
        assert_eq!(serialize(&doc), "next.rs\n");
    }

    #[test]
    fn remove_last_tagged_node_removes_separator_blank() {
        let mut doc = parse("prev.rs\n\n[orphan]\nnode.rs\n").unwrap();
        assert!(remove_node(&mut doc, "node.rs"));
        assert_eq!(serialize(&doc), "prev.rs\n");
    }

    #[test]
    fn remove_node_only_node() {
        let mut doc = parse("a.rs\n").unwrap();
        remove_node(&mut doc, "a.rs");
        assert_eq!(serialize(&doc), "");
    }

    // -----------------------------------------------------------------------
    // Document mutation: add_edge
    // -----------------------------------------------------------------------

    #[test]
    fn add_edge_to_orphan() {
        let mut doc = parse("src/main.rs\n").unwrap();
        let edge = Edge {
            target: "src/auth/".into(),
            kind: EdgeKind::Directed,
            label: "bootstraps".into(),
        };
        add_edge(&mut doc, "src/main.rs", &edge).unwrap();
        let out = serialize(&doc);
        assert_eq!(out, "src/main.rs\n    -> src/auth/ : bootstraps\n");
    }

    #[test]
    fn add_edge_appends_after_existing() {
        let mut doc = parse("src/main.rs\n    -> src/auth/ : foo\n").unwrap();
        let edge = Edge {
            target: "config.toml".into(),
            kind: EdgeKind::Directed,
            label: "reads".into(),
        };
        add_edge(&mut doc, "src/main.rs", &edge).unwrap();
        let out = serialize(&doc);
        assert_eq!(
            out,
            "src/main.rs\n    -> src/auth/ : foo\n    -> config.toml : reads\n"
        );
    }

    #[test]
    fn add_edge_does_not_affect_other_nodes() {
        let mut doc = parse("a.rs\n\nb.rs\n    -> a.rs : foo\n").unwrap();
        let edge = Edge {
            target: "c.rs".into(),
            kind: EdgeKind::Undirected,
            label: String::new(),
        };
        add_edge(&mut doc, "a.rs", &edge).unwrap();
        let out = serialize(&doc);
        // a.rs gets the new edge, b.rs's edges are unchanged
        assert!(out.starts_with("a.rs\n    -- c.rs :\n\nb.rs\n    -> a.rs : foo\n"));
    }

    #[test]
    fn add_edge_node_not_found() {
        let mut doc = parse("a.rs\n").unwrap();
        let edge = Edge {
            target: "b.rs".into(),
            kind: EdgeKind::Directed,
            label: String::new(),
        };
        assert!(add_edge(&mut doc, "z.rs", &edge).is_err());
    }

    #[test]
    fn add_node_respects_existing_two_space_indent() {
        let mut doc = parse("src/\n\n  src/main.rs\n").unwrap();
        add_node(&mut doc, "src/new.rs");
        let out = serialize(&doc);
        assert!(out.contains("  src/new.rs\n"));
        assert!(!out.contains("    src/new.rs\n"));
    }

    #[test]
    fn add_edge_respects_existing_tab_indent() {
        let mut doc = parse("src/main.rs\n\t-> src/auth/ : bootstraps\n").unwrap();
        let edge = Edge {
            target: "src/db/".into(),
            kind: EdgeKind::Directed,
            label: "queries".into(),
        };
        add_edge(&mut doc, "src/main.rs", &edge).unwrap();
        let out = serialize(&doc);
        assert!(out.contains("\t-> src/db/ : queries\n"));
        assert!(!out.contains("    -> src/db/ : queries\n"));
    }

    #[test]
    fn add_edge_under_nested_node_uses_nested_indent() {
        let mut doc = parse("src/\n\n    src/main.rs\n").unwrap();
        let edge = Edge {
            target: "README.md".into(),
            kind: EdgeKind::Directed,
            label: "documents".into(),
        };
        add_edge(&mut doc, "src/main.rs", &edge).unwrap();
        assert!(serialize(&doc).contains("    src/main.rs\n        -> README.md : documents\n"));
    }

    #[test]
    fn sort_edges_by_kind_orders_undir_then_out_then_in() {
        let mut doc = parse(
            "a.rs\n    <- d.rs : in\n    -> b.rs : out\n    -- c.rs : undirected\n\nb.rs\n\nc.rs\n\nd.rs\n",
        )
        .unwrap();
        let changed = sort_edges_by_kind(&mut doc);
        assert_eq!(changed, 1);
        assert!(
            serialize(&doc)
                .contains("a.rs\n    -- c.rs : undirected\n    -> b.rs : out\n    <- d.rs : in\n")
        );
    }

    #[test]
    fn intentional_orphan_subtree_roots_returns_folder_roots_only() {
        let doc = parse(
            "\
[orphan bundle]
src/
    src/lib.rs

[orphan]
other.rs
",
        )
        .unwrap();
        let roots = intentional_orphan_subtree_roots(&doc);
        assert_eq!(roots.len(), 1);
        assert!(roots.contains("src/"));
        assert!(!roots.contains("other.rs"));
    }

    #[test]
    fn collapsed_subtree_roots_includes_link_subtree() {
        let doc = parse(
            "\
[bundle]
linked/

[orphan bundle]
orphaned/
",
        )
        .unwrap();
        let roots = collapsed_subtree_roots(&doc);
        assert!(roots.contains("linked/"));
        assert!(roots.contains("orphaned/"));
    }

    // -----------------------------------------------------------------------
    // Document mutation: remove_edge
    // -----------------------------------------------------------------------

    #[test]
    fn remove_edge_present() {
        let mut doc =
            parse("src/main.rs\n    -> src/auth/ : foo\n    -> config.toml : bar\n").unwrap();
        let removed = remove_edge(&mut doc, "src/main.rs", "src/auth/");
        assert!(removed);
        let out = serialize(&doc);
        assert!(!out.contains("src/auth/"));
        assert!(out.contains("config.toml"));
    }

    #[test]
    fn remove_edge_not_present() {
        let mut doc = parse("src/main.rs\n    -> src/auth/ : foo\n").unwrap();
        assert!(!remove_edge(&mut doc, "src/main.rs", "nonexistent"));
    }

    #[test]
    fn replace_edge_label_updates_matching_edge_only() {
        let mut doc = parse("a.rs\n    -> b.rs : old\n    -> c.rs : keep\n").unwrap();
        let changed =
            replace_edge_label(&mut doc, "a.rs", "b.rs", EdgeKind::Directed, "old", "new");
        assert_eq!(changed, 1);
        let out = serialize(&doc);
        assert!(out.contains("-> b.rs : new"));
        assert!(out.contains("-> c.rs : keep"));
    }

    #[test]
    fn replace_edge_label_preserves_tab_indent() {
        let mut doc = parse("a.rs\n\t<- b.rs : old\n").unwrap();
        let changed =
            replace_edge_label(&mut doc, "a.rs", "b.rs", EdgeKind::Incoming, "old", "new");
        assert_eq!(changed, 1);
        assert_eq!(serialize(&doc), "a.rs\n\t<- b.rs : new\n");
    }

    // -----------------------------------------------------------------------
    // Document mutation: mark_missing
    // -----------------------------------------------------------------------

    #[test]
    fn mark_missing_inserts_comment() {
        let mut doc = parse("src/main.rs\n").unwrap();
        assert!(mark_missing(&mut doc, "src/main.rs"));
        let out = serialize(&doc);
        assert!(out.contains("# [missing] src/main.rs"));
    }

    // -----------------------------------------------------------------------
    // Document mutation: remove_edges_targeting
    // -----------------------------------------------------------------------

    #[test]
    fn remove_edges_targeting_basic() {
        let mut doc = parse("a.rs\n    -> b.rs : foo\n\nb.rs\n    -> a.rs : bar\n").unwrap();
        let count = remove_edges_targeting(&mut doc, "b.rs");
        assert_eq!(count, 1);
        let out = serialize(&doc);
        // The edge from a.rs -> b.rs is gone; b.rs node and its edges remain.
        assert!(!out.contains("-> b.rs"));
        assert!(out.contains("b.rs\n"));
        assert!(out.contains("-> a.rs : bar"));
    }

    #[test]
    fn remove_edges_targeting_multiple_sources() {
        let mut doc =
            parse("a.rs\n    -> c.rs : foo\n\nb.rs\n    -> c.rs : bar\n\nc.rs\n").unwrap();
        let count = remove_edges_targeting(&mut doc, "c.rs");
        assert_eq!(count, 2);
        let out = serialize(&doc);
        assert!(!out.contains("-> c.rs"));
        assert!(out.contains("c.rs\n"));
    }

    #[test]
    fn remove_edges_targeting_none() {
        let mut doc = parse("a.rs\n    -> b.rs : foo\n").unwrap();
        let count = remove_edges_targeting(&mut doc, "nonexistent");
        assert_eq!(count, 0);
    }

    // -----------------------------------------------------------------------
    // Edge cases and special paths
    // -----------------------------------------------------------------------

    #[test]
    fn crlf_roundtrip() {
        // Simulates a file with CRLF line endings.
        let input = "src/main.rs\r\n    -> src/auth/ : foo\r\n";
        let doc = parse(input).unwrap();
        assert_eq!(serialize(&doc), input);
    }

    #[test]
    fn node_path_with_spaces_in_name() {
        // Paths with spaces are valid on some filesystems.
        rt("src/my file.rs\n    -> src/other file.rs : foo\n");
    }

    #[test]
    fn multiple_blank_lines_preserved() {
        rt("a.rs\n\n\n\nb.rs\n");
    }
}
