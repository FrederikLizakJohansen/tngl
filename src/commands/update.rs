//! `tngl update` — reconcile graph.tngl against the current filesystem.

use std::fs;
use std::io::{self, Write};
use std::path::Path;

use anyhow::{Result, bail};
use crossterm::style::Stylize;

use crate::graph::model::{EdgeKind, Graph, Node};
use crate::parser::config::{Config, OnDelete};
use crate::parser::{config, graph, layout};
use crate::scanner::{diff, tree};
use crate::tangle;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct UpdatePreview {
    pub orphan_link_conflicts: usize,
    pub folder_orphan_scope_conflicts: usize,
    pub folder_orphan_scope_paths: Vec<String>,
    pub orphaned_bundle_conflicts: usize,
    pub collapsed_subtree_nodes: usize,
    pub lint_removed_floating_tags: usize,
    pub lint_removed_extra_blank_lines: usize,
    pub lint_normalized_tag_indentation: usize,
    pub lint_normalized_edge_indentation: usize,
    pub mirrored_edges: usize,
    pub reordered_edge_nodes: usize,
}

impl UpdatePreview {
    pub fn has_any_change(&self) -> bool {
        self.orphan_link_conflicts > 0
            || self.folder_orphan_scope_conflicts > 0
            || self.orphaned_bundle_conflicts > 0
            || self.lint_removed_floating_tags > 0
            || self.lint_removed_extra_blank_lines > 0
            || self.lint_normalized_tag_indentation > 0
            || self.lint_normalized_edge_indentation > 0
            || self.mirrored_edges > 0
            || self.reordered_edge_nodes > 0
    }
}

// ---------------------------------------------------------------------------
// Public entry points
// ---------------------------------------------------------------------------

pub fn run(silent: bool, mark_new_as_orphans: bool) -> Result<()> {
    let root = tangle::find_root()?;
    run_in_with_options(&root, silent, None, mark_new_as_orphans)
}

pub fn preview_in(root: &Path) -> Result<UpdatePreview> {
    let graph_path = tangle::graph_path(root);
    let graph_content = fs::read_to_string(&graph_path)?;
    let mut doc = graph::parse(&graph_content)?;
    let g = graph::to_graph(&doc)?;

    let orphan_link_conflicts = detect_orphan_link_conflicts(&doc, &g).len();
    let folder_orphan_scope_conflicts = 0usize;
    let folder_orphan_scope_paths: Vec<String> = Vec::new();
    let orphaned_bundle_conflicts = detect_orphaned_link_subtree_conflicts(&doc, &g).len();

    let lint_report = graph::lint(&mut doc);
    let reordered_edge_nodes = graph::sort_edges_by_kind(&mut doc);

    Ok(UpdatePreview {
        orphan_link_conflicts,
        folder_orphan_scope_conflicts,
        folder_orphan_scope_paths,
        orphaned_bundle_conflicts,
        collapsed_subtree_nodes: 0,
        lint_removed_floating_tags: lint_report.removed_floating_orphan_tags,
        lint_removed_extra_blank_lines: lint_report.removed_extra_blank_lines,
        lint_normalized_tag_indentation: lint_report.normalized_tag_indentation,
        lint_normalized_edge_indentation: lint_report.normalized_edge_indentation,
        mirrored_edges: 0,
        reordered_edge_nodes,
    })
}

/// Run update inside `root`.
///
/// `accept_fn` replaces interactive prompts during update.
/// For delete handling (`on_delete: prompt`), return:
/// `'d'` (delete), `'p'` (preserve), or `'s'` (skip).
/// For orphan/bundle/tag conflicts, return:
/// `'y'` (un-orphan this), `'Y'` (un-orphan all remaining), or `'a'` (abort).
/// `None` → use interactive stdin.
#[allow(dead_code)] // used in tests and internal entry-point variants
pub fn run_in(root: &Path, silent: bool, accept_fn: Option<&dyn Fn(&str) -> char>) -> Result<()> {
    run_in_with_options(root, silent, accept_fn, false)
}

fn run_in_with_options(
    root: &Path,
    silent: bool,
    accept_fn: Option<&dyn Fn(&str) -> char>,
    cli_mark_new_as_orphans: bool,
) -> Result<()> {
    if !silent {
        println!(
            "  {} {}",
            "Scanning".cyan().bold(),
            "repository tree...".dark_grey()
        );
    }

    let fs_paths = tree::scan(root)?;
    let graph_path = tangle::graph_path(root);
    let graph_content = fs::read_to_string(&graph_path)?;
    let mut doc = graph::parse(&graph_content)?;
    let resolved_orphan_conflicts = resolve_orphan_link_conflicts(&mut doc, silent, accept_fn)?;
    let converted_folder_orphans = 0usize;
    let converted_link_subtrees =
        resolve_orphaned_link_subtree_conflicts(&mut doc, silent, accept_fn)?;
    let collapsed_subtree_roots = graph::collapsed_subtree_roots(&doc);
    let g = graph::to_graph(&doc)?;
    let cfg = load_config(root)?;
    let d = diff::compute(&fs_paths, &g);
    let mark_new_as_orphans = cfg.mark_new_as_orphans || cli_mark_new_as_orphans;

    // --- deleted files ---
    let deletion_summary = handle_deletions(&mut doc, &g, &d.missing, &cfg, silent, accept_fn)?;

    // --- new files → orphan nodes ---
    // Process additions after deletions so orphan tags from removed nodes cannot
    // end up "floating" onto newly inserted nodes.
    let subtree_roots = collapsed_subtree_roots.clone();
    let mut marked_orphans = 0usize;
    let new_count = d.untracked.len();
    for path in &d.untracked {
        graph::add_node(&mut doc, path);

        if !mark_new_as_orphans {
            continue;
        }

        let covered_by_subtree = subtree_roots
            .iter()
            .any(|root| is_descendant_of_subtree_root(path, root));
        if covered_by_subtree {
            continue;
        }

        if graph::mark_orphan(&mut doc, path) {
            marked_orphans += 1;
        }
    }

    // --- lint graph file ---
    let lint_report = graph::lint(&mut doc);

    let reordered_edge_nodes = graph::sort_edges_by_kind(&mut doc);

    if !silent {
        if new_count > 0 {
            println!(
                "  {} {} {}",
                "Added".green().bold(),
                new_count.to_string().green().bold(),
                format!("new node{}", plural(new_count)).green()
            );
        } else {
            println!("  {}", "No new nodes found.".dark_grey());
        }
    }

    if !silent {
        let resolved = deletion_summary.resolved_count();
        if resolved > 0 {
            println!(
                "  {} {} {}",
                "Resolved".green().bold(),
                resolved.to_string().green().bold(),
                format!("stale node{}", plural(resolved)).green()
            );
        }
        if deletion_summary.preserved_missing > 0 {
            let count = deletion_summary.preserved_missing;
            println!(
                "  {} {} {}",
                "Marked".yellow().bold(),
                count.to_string().yellow().bold(),
                format!("missing node{} as [missing]", plural(count)).yellow()
            );
        }
        if deletion_summary.skipped_missing > 0 {
            let count = deletion_summary.skipped_missing;
            println!(
                "  {} {} {}",
                "Skipped".dark_yellow().bold(),
                count.to_string().dark_yellow().bold(),
                format!("missing node{}", plural(count)).dark_yellow()
            );
        }
        if lint_report.removed_floating_orphan_tags > 0 {
            let count = lint_report.removed_floating_orphan_tags;
            println!(
                "  {} {} {}",
                "Linted".blue().bold(),
                count.to_string().blue().bold(),
                format!("floating tag{}", plural(count)).blue()
            );
        }
        if lint_report.removed_extra_blank_lines > 0 {
            let count = lint_report.removed_extra_blank_lines;
            println!(
                "  {} {} {}",
                "Linted".blue().bold(),
                count.to_string().blue().bold(),
                format!("extra blank line{}", plural(count)).blue()
            );
        }
        if lint_report.normalized_tag_indentation > 0 {
            let count = lint_report.normalized_tag_indentation;
            println!(
                "  {} {} {}",
                "Linted".blue().bold(),
                count.to_string().blue().bold(),
                format!("normalized tag indentation{}", plural(count)).blue()
            );
        }
        if lint_report.normalized_edge_indentation > 0 {
            let count = lint_report.normalized_edge_indentation;
            println!(
                "  {} {} {}",
                "Linted".blue().bold(),
                count.to_string().blue().bold(),
                format!("normalized edge indentation{}", plural(count)).blue()
            );
        }
        if resolved_orphan_conflicts > 0 {
            let count = resolved_orphan_conflicts;
            println!(
                "  {} {} {}",
                "Resolved".green().bold(),
                count.to_string().green().bold(),
                format!("orphan link conflict{}", plural(count)).green()
            );
        }
        if converted_folder_orphans > 0 {
            let count = converted_folder_orphans;
            println!(
                "  {} {} {}",
                "Converted".cyan().bold(),
                count.to_string().cyan().bold(),
                format!("folder [orphan] tag{} to [orphan bundle]", plural(count)).cyan()
            );
        }
        if converted_link_subtrees > 0 {
            let count = converted_link_subtrees;
            println!(
                "  {} {} {}",
                "Converted".cyan().bold(),
                count.to_string().cyan().bold(),
                format!("orphaned [bundle] tag{}", plural(count)).cyan()
            );
        }
        if marked_orphans > 0 {
            println!(
                "  {} {} {}",
                "Marked".cyan().bold(),
                marked_orphans.to_string().cyan().bold(),
                format!("new node{} as [orphan]", plural(marked_orphans)).cyan()
            );
        }
        if reordered_edge_nodes > 0 {
            println!(
                "  {} {} {}",
                "Reordered".blue().bold(),
                reordered_edge_nodes.to_string().blue().bold(),
                format!(
                    "node edge block{} to (--, ->, <-)",
                    plural(reordered_edge_nodes)
                )
                .blue()
            );
        }
        if new_count == 0
            && !deletion_summary.changed()
            && !lint_report.changed()
            && resolved_orphan_conflicts == 0
            && converted_folder_orphans == 0
            && converted_link_subtrees == 0
            && reordered_edge_nodes == 0
        {
            println!("  {}", "No changes detected.".dark_grey());
        }
    }

    let graph_changed = new_count > 0
        || deletion_summary.changed()
        || lint_report.changed()
        || resolved_orphan_conflicts > 0
        || converted_folder_orphans > 0
        || converted_link_subtrees > 0
        || reordered_edge_nodes > 0;

    // Write graph.tngl only when something changed.
    if graph_changed {
        fs::write(&graph_path, graph::serialize(&doc))?;
    }

    // Keep layout.tngl aligned with graph.tngl for additions and removals.
    if graph_changed {
        let layout_path = tangle::layout_path(root);
        if layout_path.exists() {
            let existing = fs::read_to_string(&layout_path)?;
            let final_graph = graph::to_graph(&doc)?;
            let paths: Vec<String> = final_graph.nodes.iter().map(|n| n.path.clone()).collect();
            let updated = layout::reconcile(&existing, &paths);
            if updated != existing {
                fs::write(&layout_path, updated)?;
            }
        }
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Deletion handling
// ---------------------------------------------------------------------------

/// Apply the on_delete strategy to every missing node.
///
/// Returns a summary of what changed.
fn handle_deletions(
    doc: &mut graph::Document,
    g: &Graph,
    missing: &[String],
    cfg: &Config,
    silent: bool,
    accept_fn: Option<&dyn Fn(&str) -> char>,
) -> Result<DeletionSummary> {
    let mut summary = DeletionSummary::default();

    for path in missing {
        // Internal control paths should never be graph nodes.
        if is_internal_node(path) {
            graph::remove_edges_targeting(doc, path);
            graph::remove_node(doc, path);
            summary.removed_internal += 1;
            continue;
        }

        // A node is "involved" if it has outgoing edges OR if other nodes point to it.
        // Nodes with no connections at all are silently removed regardless of on_delete.
        if !has_graph_connections(g, path) {
            graph::remove_node(doc, path);
            summary.removed_unconnected += 1;
            continue;
        }

        // Nodes with edges: apply the configured strategy.
        let action = match cfg.on_delete {
            OnDelete::Delete => 'd',
            OnDelete::Preserve => 'p',
            OnDelete::Prompt => {
                let node = g.get(path).expect("path in graph");
                if !silent {
                    print_deleted_node(path, node);
                }
                match accept_fn {
                    Some(f) => f(path),
                    None => prompt_delete_action()?,
                }
            }
        };

        match action {
            'd' => {
                // Remove all incoming edges first, then the node itself.
                graph::remove_edges_targeting(doc, path);
                graph::remove_node(doc, path);
                summary.removed_connected += 1;
            }
            'p' => {
                graph::mark_missing(doc, path);
                summary.preserved_missing += 1;
            }
            _ => {
                // 's' or unknown → skip (leave as-is)
                summary.skipped_missing += 1;
            }
        }
    }

    Ok(summary)
}

// ---------------------------------------------------------------------------
// Interactive prompt
// ---------------------------------------------------------------------------

fn print_deleted_node(path: &str, node: &Node) {
    println!("  {} (deleted)", path);
    for edge in &node.edges {
        let arrow = match edge.kind {
            EdgeKind::Directed => "->",
            EdgeKind::Incoming => "<-",
            EdgeKind::Undirected => "--",
        };
        if edge.label.is_empty() {
            println!("    {} {} :", arrow, edge.target);
        } else {
            println!("    {} {} : {}", arrow, edge.target, edge.label);
        }
    }
}

fn prompt_delete_action() -> Result<char> {
    println!("\n  What should happen to these edges?");
    print!("  [d] delete  [p] preserve  [s] skip for now: ");
    io::stdout().flush()?;
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;
    Ok(match input.trim() {
        "d" => 'd',
        "p" => 'p',
        _ => 's',
    })
}

fn prompt_orphan_conflict_action() -> Result<char> {
    loop {
        print!("  [y] yes (un-orphan this)  [Y] yes to all  [a] abort update: ");
        io::stdout().flush()?;
        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        let action = match input.trim() {
            "y" => Some('y'),
            "Y" => Some('Y'),
            "a" => Some('a'),
            _ => None,
        };
        if let Some(action) = action {
            return Ok(action);
        }
        println!("  Please choose y, Y, or a.");
    }
}

fn prompt_link_subtree_conflict_action() -> Result<char> {
    loop {
        print!("  [y] convert this to [orphan bundle]  [Y] convert all  [a] abort update: ");
        io::stdout().flush()?;
        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        let action = match input.trim() {
            "y" => Some('y'),
            "Y" => Some('Y'),
            "a" => Some('a'),
            _ => None,
        };
        if let Some(action) = action {
            return Ok(action);
        }
        println!("  Please choose y, Y, or a.");
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// True if `path` has any outgoing edges OR is the target of any incoming edge.
fn has_graph_connections(g: &Graph, path: &str) -> bool {
    // Outgoing
    if g.get(path).map(|n| !n.edges.is_empty()).unwrap_or(false) {
        return true;
    }
    // Incoming
    g.nodes
        .iter()
        .any(|n| n.edges.iter().any(|e| e.target == path))
}

fn load_config(root: &Path) -> Result<Config> {
    let path = tangle::config_path(root);
    if path.exists() {
        let content = fs::read_to_string(&path)?;
        config::parse(&content)
    } else {
        Ok(Config::default())
    }
}

fn plural(n: usize) -> &'static str {
    if n == 1 { "" } else { "s" }
}

fn is_internal_node(path: &str) -> bool {
    matches!(path, ".tnglignore" | "tngl")
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct OrphanLinkConflict {
    path: String,
    kind: graph::OrphanMarkerKind,
    incoming: usize,
    outgoing: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct LinkSubtreeOrphanConflict {
    path: String,
    incoming: usize,
    outgoing: usize,
}

fn detect_orphan_link_conflicts(doc: &graph::Document, g: &Graph) -> Vec<OrphanLinkConflict> {
    let mut conflicts = Vec::new();
    for (path, kind) in graph::explicit_orphan_markers(doc) {
        let Some(node) = g.get(&path) else {
            continue;
        };
        let outgoing = node.edges.len();
        let incoming = g
            .nodes
            .iter()
            .flat_map(|n| n.edges.iter())
            .filter(|e| e.target == path)
            .count();
        if incoming + outgoing == 0 {
            continue;
        }
        conflicts.push(OrphanLinkConflict {
            path,
            kind,
            incoming,
            outgoing,
        });
    }
    conflicts
}

fn resolve_orphan_link_conflicts(
    doc: &mut graph::Document,
    silent: bool,
    accept_fn: Option<&dyn Fn(&str) -> char>,
) -> Result<usize> {
    let g = graph::to_graph(doc)?;
    let conflicts = detect_orphan_link_conflicts(doc, &g);
    if conflicts.is_empty() {
        return Ok(0);
    }

    if silent {
        let mut details: Vec<String> = conflicts
            .iter()
            .map(|c| {
                format!(
                    "{} [{}]",
                    c.path,
                    match c.kind {
                        graph::OrphanMarkerKind::Orphan => "orphan",
                        graph::OrphanMarkerKind::OrphanSubtree => "orphan bundle",
                    }
                )
            })
            .collect();
        details.sort();
        bail!(
            "linked orphan conflict: {}. Run `tngl update` (non-silent) and un-orphan linked nodes, or edit graph tags manually.",
            details.join(", ")
        );
    }

    println!(
        "  {} {}",
        "Conflict".red().bold(),
        "Orphan-tagged node(s) now have link(s).".red()
    );
    println!("  Orphan tags are only valid on isolated nodes.");
    println!("  Resolve these before update can continue.");

    let mut resolved = 0usize;
    let mut yes_to_all = false;
    for conflict in conflicts {
        let tag = match conflict.kind {
            graph::OrphanMarkerKind::Orphan => "[orphan]",
            graph::OrphanMarkerKind::OrphanSubtree => "[orphan bundle]",
        };
        println!(
            "\n  {} is tagged {} but has {} incoming and {} outgoing link{}.",
            conflict.path,
            tag,
            conflict.incoming,
            conflict.outgoing,
            plural(conflict.incoming + conflict.outgoing)
        );
        let action = if yes_to_all {
            'y'
        } else {
            match accept_fn {
                Some(f) => f(&conflict.path),
                None => prompt_orphan_conflict_action()?,
            }
        };
        match action {
            'y' | 'u' => {
                let removed = match conflict.kind {
                    graph::OrphanMarkerKind::Orphan => graph::unmark_orphan(doc, &conflict.path),
                    graph::OrphanMarkerKind::OrphanSubtree => {
                        graph::unmark_orphan_subtree(doc, &conflict.path)
                    }
                };
                if !removed {
                    bail!(
                        "failed to remove orphan marker for '{}' while resolving conflict",
                        conflict.path
                    );
                }
                resolved += 1;
            }
            'Y' => {
                let removed = match conflict.kind {
                    graph::OrphanMarkerKind::Orphan => graph::unmark_orphan(doc, &conflict.path),
                    graph::OrphanMarkerKind::OrphanSubtree => {
                        graph::unmark_orphan_subtree(doc, &conflict.path)
                    }
                };
                if !removed {
                    bail!(
                        "failed to remove orphan marker for '{}' while resolving conflict",
                        conflict.path
                    );
                }
                yes_to_all = true;
                resolved += 1;
            }
            _ => {
                bail!(
                    "update aborted: orphan-link conflict for '{}' was not resolved",
                    conflict.path
                );
            }
        }
    }

    Ok(resolved)
}

fn detect_orphaned_link_subtree_conflicts(
    doc: &graph::Document,
    g: &Graph,
) -> Vec<LinkSubtreeOrphanConflict> {
    let mut conflicts = Vec::new();
    for path in graph::explicit_link_subtree_markers(doc) {
        let Some(node) = g.get(&path) else {
            continue;
        };
        let outgoing = node.edges.len();
        let incoming = g
            .nodes
            .iter()
            .flat_map(|n| n.edges.iter())
            .filter(|e| e.target == path)
            .count();
        if incoming + outgoing == 0 {
            conflicts.push(LinkSubtreeOrphanConflict {
                path,
                incoming,
                outgoing,
            });
        }
    }
    conflicts
}

fn resolve_orphaned_link_subtree_conflicts(
    doc: &mut graph::Document,
    silent: bool,
    accept_fn: Option<&dyn Fn(&str) -> char>,
) -> Result<usize> {
    let g = graph::to_graph(doc)?;
    let conflicts = detect_orphaned_link_subtree_conflicts(doc, &g);
    if conflicts.is_empty() {
        return Ok(0);
    }

    if silent {
        let mut details: Vec<String> = conflicts.iter().map(|c| c.path.clone()).collect();
        details.sort();
        bail!(
            "orphaned bundle conflict: {}. Run `tngl update` (non-silent) to convert to [orphan bundle], or edit tags manually.",
            details.join(", ")
        );
    }

    println!(
        "  {} {}",
        "Conflict".red().bold(),
        "Orphaned [bundle] folder(s) detected.".red()
    );
    println!("  [bundle] is only valid for linked folders.");
    println!("  Convert orphaned ones to [orphan bundle] or abort.");

    let mut converted = 0usize;
    let mut yes_to_all = false;
    for conflict in conflicts {
        println!(
            "\n  {} is tagged [bundle] but has {} incoming and {} outgoing links.",
            conflict.path, conflict.incoming, conflict.outgoing
        );
        let action = if yes_to_all {
            'y'
        } else {
            match accept_fn {
                Some(f) => f(&conflict.path),
                None => prompt_link_subtree_conflict_action()?,
            }
        };
        match action {
            'y' => {
                if !graph::convert_link_subtree_to_orphan_subtree(doc, &conflict.path) {
                    bail!(
                        "failed to convert [bundle] to [orphan bundle] for '{}'",
                        conflict.path
                    );
                }
                converted += 1;
            }
            'Y' => {
                if !graph::convert_link_subtree_to_orphan_subtree(doc, &conflict.path) {
                    bail!(
                        "failed to convert [bundle] to [orphan bundle] for '{}'",
                        conflict.path
                    );
                }
                yes_to_all = true;
                converted += 1;
            }
            _ => {
                bail!(
                    "update aborted: orphaned [bundle] conflict for '{}' was not resolved",
                    conflict.path
                );
            }
        }
    }

    Ok(converted)
}

fn is_descendant_of_subtree_root(path: &str, root: &str) -> bool {
    path != root && path.starts_with(root)
}

#[derive(Debug, Default, Clone, Copy)]
struct DeletionSummary {
    removed_internal: usize,
    removed_unconnected: usize,
    removed_connected: usize,
    preserved_missing: usize,
    skipped_missing: usize,
}

impl DeletionSummary {
    fn resolved_count(&self) -> usize {
        self.removed_internal + self.removed_unconnected + self.removed_connected
    }

    fn changed(&self) -> bool {
        self.resolved_count() > 0 || self.preserved_missing > 0
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::Cell;
    use std::fs as sfs;
    use tempfile::TempDir;

    /// Create a minimal initialised repo in a tmpdir.
    fn init_repo(files: &[&str], graph_contents: &str, config_contents: &str) -> TempDir {
        let dir = TempDir::new().unwrap();
        sfs::create_dir_all(dir.path().join("tangle")).unwrap();
        sfs::write(dir.path().join("tangle/graph.tngl"), graph_contents).unwrap();
        sfs::write(dir.path().join("tangle/config.tngl"), config_contents).unwrap();
        for f in files {
            let p = dir.path().join(f);
            if let Some(parent) = p.parent() {
                sfs::create_dir_all(parent).unwrap();
            }
            sfs::write(p, "").unwrap();
        }
        dir
    }

    fn graph_content(dir: &TempDir) -> String {
        sfs::read_to_string(dir.path().join("tangle/graph.tngl")).unwrap()
    }

    #[test]
    fn adds_new_files_as_orphans() {
        let dir = init_repo(&["a.rs", "b.rs"], "a.rs\n", "on_delete: delete\n");
        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        assert!(content.contains("b.rs"));
        // b.rs should have no edges
        assert!(!content.contains("-> b.rs"));
    }

    #[test]
    fn adds_new_folders_before_files() {
        let dir = init_repo(
            &["existing.rs", "src/main.rs", "new.rs"],
            "existing.rs\n",
            "on_delete: delete\n",
        );
        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        let idx_folder = content.find("src/\n").unwrap();
        let idx_file = content.find("new.rs\n").unwrap();
        assert!(
            idx_folder < idx_file,
            "folders should be listed before files"
        );
    }

    #[test]
    fn inserts_new_subtree_before_root_files() {
        let dir = init_repo(
            &["src/a/file1.rs", "root.rs", "src/b/file2.rs"],
            "src/\n\n    src/a/\n\n        src/a/file1.rs\n\nroot.rs\n",
            "on_delete: delete\n",
        );
        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        let idx_root = content.find("root.rs\n").unwrap();
        let idx_new_sub = content.find("    src/b/\n").unwrap();
        assert!(
            idx_new_sub < idx_root,
            "new subtree should be inserted with its folder block before root files: {content}"
        );
    }

    #[test]
    fn orphan_tagged_deleted_node_does_not_tag_new_node() {
        let dir = init_repo(
            &["base.rs", "new.rs"],
            "[orphan]\nold.rs\n\nbase.rs\n",
            "on_delete: delete\n",
        );
        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        assert!(
            !content.contains("[orphan]"),
            "floating orphan tag must be removed: {content}"
        );
    }

    #[test]
    fn update_lints_floating_orphan_tags() {
        let dir = init_repo(&[], "[orphan]\n", "on_delete: delete\n");
        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        assert_eq!(content, "");
    }

    #[test]
    fn silent_adds_without_printing() {
        // Just verify it doesn't panic in silent mode
        let dir = init_repo(&["a.rs", "b.rs"], "a.rs\n", "on_delete: delete\n");
        run_in(dir.path(), true, None).unwrap();
    }

    #[test]
    fn delete_mode_removes_orphan_missing_node() {
        // a.rs is in graph but not on disk (orphan — no edges)
        let dir = init_repo(&["b.rs"], "a.rs\n\nb.rs\n", "on_delete: delete\n");
        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        assert!(!content.contains("a.rs"));
        assert!(content.contains("b.rs"));
    }

    #[test]
    fn delete_mode_removes_node_and_edges() {
        // b.rs is in graph with edges, missing from disk
        let dir = init_repo(
            &["a.rs"],
            "a.rs\n    -> b.rs : uses\n\nb.rs\n    -> a.rs : back\n",
            "on_delete: delete\n",
        );
        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        assert!(
            !content.contains("b.rs"),
            "b.rs node should be gone: {}",
            content
        );
        // Also the incoming edge a.rs -> b.rs should be gone
        assert!(
            !content.contains("-> b.rs"),
            "incoming edge should be gone: {}",
            content
        );
    }

    #[test]
    fn preserve_mode_marks_missing_node() {
        let dir = init_repo(
            &["a.rs"],
            "a.rs\n    -> b.rs : uses\n\nb.rs\n",
            "on_delete: preserve\n",
        );
        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        assert!(
            content.contains("# [missing] b.rs"),
            "expected missing marker: {}",
            content
        );
        // Edge is preserved
        assert!(content.contains("-> b.rs : uses"));
    }

    #[test]
    fn prompt_mode_delete_via_accept_fn() {
        let dir = init_repo(
            &["a.rs"],
            "a.rs\n    -> b.rs : uses\n\nb.rs\n",
            "on_delete: prompt\n",
        );
        run_in(dir.path(), true, Some(&|_path| 'd')).unwrap();
        let content = graph_content(&dir);
        assert!(!content.contains("b.rs"), "b.rs should be deleted");
    }

    #[test]
    fn prompt_mode_preserve_via_accept_fn() {
        let dir = init_repo(
            &["a.rs"],
            "a.rs\n    -> b.rs : uses\n\nb.rs\n",
            "on_delete: prompt\n",
        );
        run_in(dir.path(), true, Some(&|_path| 'p')).unwrap();
        let content = graph_content(&dir);
        assert!(content.contains("# [missing] b.rs"));
    }

    #[test]
    fn prompt_mode_skip_via_accept_fn() {
        let dir = init_repo(
            &["a.rs"],
            "a.rs\n    -> b.rs : uses\n\nb.rs\n",
            "on_delete: prompt\n",
        );
        run_in(dir.path(), true, Some(&|_path| 's')).unwrap();
        // Nothing changes
        let content = graph_content(&dir);
        assert!(content.contains("b.rs\n"));
        assert!(!content.contains("# [missing]"));
    }

    #[test]
    fn orphan_deleted_node_silently_removed_regardless_of_on_delete() {
        // b.rs is in graph with no edges, missing from disk — removed even in preserve mode
        let dir = init_repo(&["a.rs"], "a.rs\n\nb.rs\n", "on_delete: preserve\n");
        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        // b.rs (orphan) is silently gone
        assert!(
            !content.contains("\nb.rs\n") && !content.starts_with("b.rs\n"),
            "orphan missing node should be removed: {}",
            content
        );
    }

    #[test]
    fn internal_nodes_removed_without_prompt_logic() {
        let dir = init_repo(
            &["a.rs"],
            "a.rs\n    -> .tnglignore : temp\n\n.tnglignore\n",
            "on_delete: preserve\n",
        );

        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        assert!(!content.contains(".tnglignore"));
        assert!(!content.contains("-> .tnglignore"));
    }

    #[test]
    fn no_write_when_no_changes() {
        let graph_text = "a.rs\n";
        let dir = init_repo(&["a.rs"], graph_text, "on_delete: delete\n");
        // Record mtime before
        let before = sfs::metadata(dir.path().join("tangle/graph.tngl"))
            .unwrap()
            .modified()
            .unwrap();
        run_in(dir.path(), true, None).unwrap();
        let after = sfs::metadata(dir.path().join("tangle/graph.tngl"))
            .unwrap()
            .modified()
            .unwrap();
        assert_eq!(before, after, "file should not have been rewritten");
    }

    #[test]
    fn lint_collapses_extra_blank_lines_in_graph() {
        let dir = init_repo(&["a.rs", "b.rs"], "a.rs\n\n\nb.rs\n", "on_delete: delete\n");
        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        assert_eq!(content, "a.rs\n\nb.rs\n");
    }

    #[test]
    fn updates_layout_tngl_for_new_files() {
        let dir = init_repo(&["a.rs", "b.rs"], "a.rs\n", "on_delete: delete\n");
        // Create a layout.tngl with just a.rs
        sfs::write(
            dir.path().join("tangle/layout.tngl"),
            format!(
                "{}\na.rs                                     x:80   y:80\n",
                layout::HEADER
            ),
        )
        .unwrap();
        run_in(dir.path(), true, None).unwrap();
        let layout_content = sfs::read_to_string(dir.path().join("tangle/layout.tngl")).unwrap();
        assert!(
            layout_content.contains("b.rs"),
            "layout should include new node b.rs"
        );
    }

    #[test]
    fn removes_layout_entries_for_removed_nodes() {
        let dir = init_repo(&["a.rs"], "a.rs\n\nb.rs\n", "on_delete: delete\n");
        sfs::write(
            dir.path().join("tangle/layout.tngl"),
            format!(
                "{}\na.rs                                     x:80   y:80\nb.rs                                     x:240  y:80\n",
                layout::HEADER
            ),
        )
        .unwrap();

        run_in(dir.path(), true, None).unwrap();

        let layout_content = sfs::read_to_string(dir.path().join("tangle/layout.tngl")).unwrap();
        assert!(layout_content.contains("a.rs"));
        assert!(!layout_content.contains("b.rs"));
    }

    #[test]
    fn ignored_child_removal_preserves_blank_line_spacing() {
        let dir = init_repo(
            &["src/a/file1.rs", "src/b/file2.rs"],
            "src/\n\n    src/a/\n\n        src/a/file1.rs\n\n    src/b/\n\n        src/b/file2.rs\n",
            "on_delete: delete\n",
        );
        sfs::write(dir.path().join(".tnglignore"), "src/a/file1.rs\n").unwrap();
        let scanned = tree::scan(dir.path()).unwrap();
        assert!(
            !scanned.contains(&"src/a/file1.rs".to_string()),
            "scanner should ignore child via .tnglignore: {scanned:?}"
        );

        run_in(dir.path(), true, None).unwrap();

        let content = graph_content(&dir);
        assert!(
            !content.contains("src/a/file1.rs"),
            "child should be removed: {content}"
        );
        let folder_a_pos = content
            .find("    src/a/\n")
            .expect("src/a/ folder should remain");
        let after_a = &content[folder_a_pos + "    src/a/\n".len()..];
        assert!(
            after_a.starts_with('\n'),
            "expected a blank separator between sibling folders: {content}"
        );
        assert!(
            after_a.contains("    src/b/\n"),
            "expected sibling folder to remain after spacing separator: {content}"
        );
    }

    #[test]
    fn does_not_create_layout_if_absent() {
        let dir = init_repo(&["a.rs", "b.rs"], "a.rs\n", "on_delete: delete\n");
        // No layout.tngl pre-exists
        run_in(dir.path(), true, None).unwrap();
        assert!(!dir.path().join("tangle/layout.tngl").exists());
    }

    #[test]
    fn uses_default_config_when_config_missing() {
        let dir = TempDir::new().unwrap();
        sfs::create_dir_all(dir.path().join("tangle")).unwrap();
        sfs::write(dir.path().join("tangle/graph.tngl"), "a.rs\n").unwrap();
        sfs::write(dir.path().join("a.rs"), "").unwrap();
        // No config.tngl — should use defaults (on_delete: prompt)
        // No deletions, so prompt is never triggered.
        run_in(dir.path(), true, None).unwrap();
    }

    #[test]
    fn does_not_auto_mirror_directed_edges() {
        let dir = init_repo(
            &["a.rs", "b.rs"],
            "a.rs\n    -> b.rs : uses\n\nb.rs\n",
            "on_delete: delete\n",
        );
        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        assert!(content.contains("a.rs\n    -> b.rs : uses"));
        assert!(!content.contains("b.rs\n    <- a.rs : uses"));
    }

    #[test]
    fn does_not_auto_mirror_incoming_edges() {
        let dir = init_repo(
            &["a.rs", "b.rs"],
            "a.rs\n    <- b.rs : consumed by\n\nb.rs\n",
            "on_delete: delete\n",
        );
        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        assert!(content.contains("a.rs\n    <- b.rs : consumed by"));
        assert!(!content.contains("b.rs\n    -> a.rs : consumed by"));
    }

    #[test]
    fn does_not_auto_mirror_undirected_edges() {
        let dir = init_repo(
            &["a.rs", "b.rs"],
            "a.rs\n    -- b.rs : peers\n\nb.rs\n",
            "on_delete: delete\n",
        );
        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        assert!(content.contains("a.rs\n    -- b.rs : peers"));
        assert!(!content.contains("b.rs\n    -- a.rs : peers"));
    }

    #[test]
    fn does_not_insert_nested_mirror_edges() {
        let dir = init_repo(
            &["src/main.rs", "src/commands/mod.rs"],
            "src/\n\n    src/commands/\n\n        src/commands/mod.rs\n            -> src/main.rs : dispatches\n\n    src/main.rs\n",
            "on_delete: delete\n",
        );
        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        assert!(
            !content.contains("    src/main.rs\n        <- src/commands/mod.rs : dispatches\n")
        );
    }

    #[test]
    fn keeps_existing_edges_without_adding_mirrors() {
        let dir = init_repo(
            &["a.rs", "b.rs", "c.rs", "d.rs"],
            "a.rs\n    -> b.rs : uses\n\nb.rs\n\nc.rs\n    <- d.rs : consumed by\n\nd.rs\n",
            "on_delete: delete\n",
        );
        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        assert!(content.contains("a.rs\n    -> b.rs : uses"));
        assert!(!content.contains("b.rs\n    <- a.rs : uses"));
        assert!(content.contains("c.rs\n    <- d.rs : consumed by"));
        assert!(!content.contains("d.rs\n    -> c.rs : consumed by"));
    }

    #[test]
    fn update_orders_edges_undir_then_out_then_in() {
        let dir = init_repo(
            &["a.rs", "b.rs", "c.rs", "d.rs"],
            "a.rs\n    <- d.rs : in\n    -> b.rs : out\n    -- c.rs : undir\n\nb.rs\n\nc.rs\n\nd.rs\n",
            "on_delete: delete\n",
        );
        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        let block = "a.rs\n    -- c.rs : undir\n    -> b.rs : out\n    <- d.rs : in\n";
        assert!(
            content.contains(block),
            "expected sorted edge block, got: {content}"
        );
    }

    #[test]
    fn mirrored_edges_are_not_duplicated() {
        let dir = init_repo(
            &["a.rs", "b.rs"],
            "a.rs\n    -> b.rs : uses\n\nb.rs\n    <- a.rs :\n",
            "on_delete: delete\n",
        );
        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        assert_eq!(content.matches("    <- a.rs :").count(), 1);
    }

    #[test]
    fn existing_mirror_with_different_label_is_kept() {
        let dir = init_repo(
            &["a.rs", "b.rs"],
            "a.rs\n    -> b.rs : producer\n\nb.rs\n    <- a.rs : consumer\n",
            "on_delete: delete\n",
        );
        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        assert!(content.contains("a.rs\n    -> b.rs : producer"));
        assert!(content.contains("b.rs\n    <- a.rs : consumer"));
    }

    #[test]
    fn orphan_subtree_keeps_children_on_update() {
        let dir = init_repo(
            &["src/lib.rs", "src/a.rs"],
            "[orphan bundle]\nsrc/\n\n    src/lib.rs\n\n    src/a.rs\n",
            "on_delete: delete\n",
        );
        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        assert!(content.contains("[orphan bundle]\nsrc/\n"));
        assert!(content.contains("src/lib.rs"));
        assert!(content.contains("src/a.rs"));
    }

    #[test]
    fn orphan_subtree_children_remain_when_tag_removed() {
        let dir = init_repo(
            &["src/lib.rs", "src/a.rs"],
            "[orphan bundle]\nsrc/\n",
            "on_delete: delete\n",
        );
        run_in(dir.path(), true, None).unwrap();
        let with_tag = graph_content(&dir);
        assert!(with_tag.contains("src/lib.rs"));
        assert!(with_tag.contains("src/a.rs"));

        sfs::write(dir.path().join("tangle/graph.tngl"), "src/\n").unwrap();
        run_in(dir.path(), true, None).unwrap();
        let without_tag = graph_content(&dir);
        assert!(without_tag.contains("src/lib.rs"));
        assert!(without_tag.contains("src/a.rs"));
    }

    #[test]
    fn mark_new_as_orphans_tags_files_and_new_folders() {
        let dir = init_repo(
            &["src/new.rs", "src/sub/leaf.rs", "standalone.rs"],
            "src/\n",
            "on_delete: delete\nmark_new_as_orphans: true\n",
        );
        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        assert!(content.contains("[orphan]\nstandalone.rs"));
        assert!(content.contains("[orphan]\n    src/sub/"));
        assert!(content.contains("[orphan]\n        src/sub/leaf.rs"));
    }

    #[test]
    fn cli_mark_new_as_orphans_overrides_default_config_false() {
        let dir = init_repo(
            &["new.rs"],
            "",
            "on_delete: delete\nmark_new_as_orphans: false\n",
        );
        run_in_with_options(dir.path(), true, None, true).unwrap();
        let content = graph_content(&dir);
        assert!(content.contains("[orphan]\nnew.rs"));
    }

    #[test]
    fn linked_orphan_conflict_fails_in_silent_mode() {
        let dir = init_repo(
            &["a.rs", "b.rs"],
            "[orphan]\na.rs\n    -> b.rs : uses\n\nb.rs\n",
            "on_delete: delete\n",
        );
        let err = run_in(dir.path(), true, None).unwrap_err().to_string();
        assert!(err.contains("linked orphan conflict"));
    }

    #[test]
    fn linked_orphan_can_be_unmarked_and_update_continues() {
        let dir = init_repo(
            &["a.rs", "b.rs"],
            "[orphan]\na.rs\n    -> b.rs : uses\n\nb.rs\n",
            "on_delete: delete\n",
        );
        run_in_with_options(dir.path(), false, Some(&|_path| 'y'), false).unwrap();
        let content = graph_content(&dir);
        assert!(!content.contains("[orphan]"));
        assert!(content.contains("a.rs\n    -> b.rs : uses"));
        assert!(!content.contains("b.rs\n    <- a.rs : uses"));
    }

    #[test]
    fn linked_orphan_yes_to_all_resolves_multiple_conflicts_once() {
        let dir = init_repo(
            &["a.rs", "b.rs", "c.rs"],
            "[orphan]\na.rs\n    -> c.rs : uses\n\n[orphan]\nb.rs\n    -> c.rs : uses\n\nc.rs\n",
            "on_delete: delete\n",
        );
        let calls = Cell::new(0usize);
        run_in_with_options(
            dir.path(),
            false,
            Some(&|_path| {
                let n = calls.get();
                calls.set(n + 1);
                if n == 0 { 'Y' } else { 'a' }
            }),
            false,
        )
        .unwrap();
        assert_eq!(calls.get(), 1, "yes-to-all should prompt only once");
        let content = graph_content(&dir);
        assert!(!content.contains("[orphan]"));
    }

    #[test]
    fn folder_orphan_with_children_stays_as_orphan() {
        let dir = init_repo(
            &["src/lib.rs"],
            "[orphan]\nsrc/\n\n    src/lib.rs\n",
            "on_delete: delete\n",
        );
        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        assert!(content.contains("[orphan]\nsrc/\n"));
        assert!(!content.contains("[orphan bundle]\nsrc/\n"));
    }

    #[test]
    fn folder_orphan_with_children_no_longer_auto_converts() {
        let dir = init_repo(
            &["src/lib.rs"],
            "[orphan]\nsrc/\n\n    src/lib.rs\n",
            "on_delete: delete\n",
        );
        run_in_with_options(dir.path(), false, Some(&|_path| 'y'), false).unwrap();
        let content = graph_content(&dir);
        assert!(content.contains("[orphan]\nsrc/\n"));
        assert!(!content.contains("[orphan bundle]\nsrc/\n"));
    }

    #[test]
    fn folder_orphan_prompt_callback_not_used_anymore() {
        let dir = init_repo(
            &["src/a.rs", "docs/arch.md"],
            "[orphan]\nsrc/\n\n    src/a.rs\n\n[orphan]\ndocs/\n\n    docs/arch.md\n",
            "on_delete: delete\n",
        );
        let calls = Cell::new(0usize);
        run_in_with_options(
            dir.path(),
            false,
            Some(&|_path| {
                let n = calls.get();
                calls.set(n + 1);
                if n == 0 { 'Y' } else { 'a' }
            }),
            false,
        )
        .unwrap();
        assert_eq!(
            calls.get(),
            0,
            "folder-orphan conversion prompt should no longer be used"
        );
        let content = graph_content(&dir);
        assert_eq!(content.matches("[orphan bundle]").count(), 0);
        assert_eq!(content.matches("[orphan]").count(), 2);
    }

    #[test]
    fn empty_folder_orphan_tag_remains_valid() {
        let dir = TempDir::new().unwrap();
        sfs::create_dir_all(dir.path().join("tangle")).unwrap();
        sfs::write(dir.path().join("tangle/graph.tngl"), "[orphan]\nempty/\n").unwrap();
        sfs::write(dir.path().join("tangle/config.tngl"), "on_delete: delete\n").unwrap();
        sfs::create_dir_all(dir.path().join("empty")).unwrap();

        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        assert!(content.contains("[orphan]\nempty/\n"));
    }

    #[test]
    fn orphaned_link_subtree_conflict_fails_in_silent_mode() {
        let dir = init_repo(
            &["src/lib.rs"],
            "[bundle]\nsrc/\n\n    src/lib.rs\n",
            "on_delete: delete\n",
        );
        let err = run_in(dir.path(), true, None).unwrap_err().to_string();
        assert!(err.contains("orphaned bundle conflict"));
    }

    #[test]
    fn orphaned_link_subtree_can_be_converted_and_update_continues() {
        let dir = init_repo(
            &["src/lib.rs"],
            "[bundle]\nsrc/\n\n    src/lib.rs\n",
            "on_delete: delete\n",
        );
        run_in_with_options(dir.path(), false, Some(&|_path| 'y'), false).unwrap();
        let content = graph_content(&dir);
        assert!(content.contains("[orphan bundle]\nsrc/\n"));
        assert!(!content.contains("[bundle]"));
    }

    #[test]
    fn orphaned_link_subtree_yes_to_all_converts_multiple_once() {
        let dir = init_repo(
            &["src/a.rs", "docs/arch.md"],
            "[bundle]\nsrc/\n\n    src/a.rs\n\n[bundle]\ndocs/\n\n    docs/arch.md\n",
            "on_delete: delete\n",
        );
        let calls = Cell::new(0usize);
        run_in_with_options(
            dir.path(),
            false,
            Some(&|_path| {
                let n = calls.get();
                calls.set(n + 1);
                if n == 0 { 'Y' } else { 'a' }
            }),
            false,
        )
        .unwrap();
        assert_eq!(calls.get(), 1, "yes-to-all should prompt only once");
        let content = graph_content(&dir);
        assert_eq!(content.matches("[bundle]").count(), 0);
        assert_eq!(content.matches("[orphan bundle]").count(), 2);
    }

    #[test]
    fn linked_folder_with_link_subtree_does_not_conflict_and_keeps_children() {
        let dir = init_repo(
            &["src/lib.rs", "consumer.rs"],
            "[bundle]\nsrc/\n    <- consumer.rs : used by\n\n    src/lib.rs\n\nconsumer.rs\n",
            "on_delete: delete\n",
        );
        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        assert!(content.contains("[bundle]\nsrc/\n    <- consumer.rs : used by"));
        assert!(content.contains("src/lib.rs"));
        assert!(!content.contains("consumer.rs\n    -> src/ : used by"));
    }

    #[test]
    fn removing_link_subtree_does_not_introduce_extra_blank_spacing() {
        let dir = init_repo(
            &["src/lib.rs", "consumer.rs"],
            "[bundle]\nsrc/\n    <- consumer.rs : used by\n\nconsumer.rs\n    -> src/ : used by\n",
            "on_delete: delete\n",
        );

        // Remove the collapse tag manually, then update to expand subtree children.
        sfs::write(
            dir.path().join("tangle/graph.tngl"),
            "src/\n    <- consumer.rs : used by\n\nconsumer.rs\n    -> src/ : used by\n",
        )
        .unwrap();
        run_in(dir.path(), true, None).unwrap();

        let content = graph_content(&dir);
        let marker = "    <- consumer.rs : used by\n";
        let idx = content.find(marker).expect("expected parent edge");
        let after = &content[idx + marker.len()..];
        assert!(
            !after.starts_with('\n'),
            "unexpected blank line before first child: {content}"
        );
    }

    #[test]
    fn collapsing_subtree_keeps_following_comment_lines() {
        let dir = init_repo(
            &["src/lib.rs", "consumer.rs"],
            "[bundle]\nsrc/\n    <- consumer.rs : used by\n\n    src/lib.rs\n# keep this comment\n\nconsumer.rs\n    -> src/ : used by\n",
            "on_delete: delete\n",
        );

        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        assert!(
            content.contains("# keep this comment"),
            "comment should survive collapse/lint: {content}"
        );
    }

    #[test]
    fn update_keeps_orphan_subtree_tag_for_empty_folder_on_disk() {
        let dir = TempDir::new().unwrap();
        sfs::create_dir_all(dir.path().join("tangle")).unwrap();
        sfs::write(
            dir.path().join("tangle/graph.tngl"),
            "[orphan bundle]\nempty/\n",
        )
        .unwrap();
        sfs::write(dir.path().join("tangle/config.tngl"), "on_delete: delete\n").unwrap();
        sfs::create_dir_all(dir.path().join("empty")).unwrap();

        run_in(dir.path(), true, None).unwrap();
        let content = graph_content(&dir);
        assert_eq!(content, "[orphan bundle]\nempty/\n");
    }

    #[test]
    fn preview_does_not_report_bundle_as_pending_collapse_change() {
        let dir = init_repo(
            &["src/lib.rs", "consumer.rs"],
            "[bundle]\nsrc/\n    <- consumer.rs : used by\n\n    src/lib.rs\n\nconsumer.rs\n    -> src/ : used by\n",
            "on_delete: delete\n",
        );
        let preview = preview_in(dir.path()).unwrap();
        assert_eq!(preview.collapsed_subtree_nodes, 0);
        assert!(!preview.has_any_change());
    }

    #[test]
    fn preview_reports_pending_lint_cleanup() {
        let dir = init_repo(&[], "[orphan]\n", "on_delete: delete\n");
        let preview = preview_in(dir.path()).unwrap();
        assert_eq!(preview.lint_removed_floating_tags, 1);
        assert!(preview.has_any_change());
    }

    #[test]
    fn preview_reports_folder_orphan_scope_paths() {
        let dir = init_repo(
            &["src/lib.rs"],
            "[orphan]\nsrc/\n\n    src/lib.rs\n",
            "on_delete: delete\n",
        );
        let preview = preview_in(dir.path()).unwrap();
        assert_eq!(preview.folder_orphan_scope_conflicts, 0);
        assert!(preview.folder_orphan_scope_paths.is_empty());
    }
}
