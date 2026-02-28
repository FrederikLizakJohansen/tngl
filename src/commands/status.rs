//! `tngl status` — diff the filesystem against graph.tngl (read-only).

use std::collections::{HashMap, HashSet};
use std::path::Path;

use anyhow::Result;
use crossterm::style::Stylize;

use crate::commands::update;
use crate::graph::model::{EdgeKind, Graph};
use crate::parser::{config, graph};
use crate::scanner::{diff, tree};
use crate::tangle;

// ---------------------------------------------------------------------------
// Public entry points
// ---------------------------------------------------------------------------

pub fn run() -> Result<()> {
    let root = tangle::find_root()?;
    run_in(&root)
}

pub fn run_in(root: &Path) -> Result<()> {
    let report = compute(root)?;
    print_report(&report);
    Ok(())
}

// ---------------------------------------------------------------------------
// Computation (testable, no I/O)
// ---------------------------------------------------------------------------

/// A complete status report for the repository.
#[derive(Debug, Default)]
pub struct StatusReport {
    /// Nodes present in the filesystem but not declared in the graph.
    pub untracked: Vec<String>,
    /// Nodes declared in the graph but absent from the filesystem, with their
    /// total graph link count (incoming + outgoing edges).
    pub missing: Vec<(String, usize)>,
    /// For missing nodes, whether the path still exists on disk (usually excluded by ignore rules).
    pub missing_exists_on_disk: HashMap<String, bool>,
    /// Edges pointing to nodes not declared in the graph: (source, arrow, target, label).
    pub dangling_edges: Vec<(String, &'static str, String, String)>,
    /// Edges with empty labels/comments: (source, arrow, target).
    pub uncommented_edges: Vec<(String, &'static str, String)>,
    /// Isolated nodes not marked as intentional (`[orphan]` or `[orphan bundle]`).
    pub unattended_orphans: Vec<String>,
    /// All declared graph node paths (used for diagnostics/hints).
    pub graph_nodes: Vec<String>,
    /// Non-mutating preview of changes `tngl update` would apply to graph.tngl.
    pub update_preview: Option<update::UpdatePreview>,
}

impl StatusReport {
    #[allow(dead_code)] // used by tests; retained for caller compatibility
    pub fn has_issues(&self) -> bool {
        !self.untracked.is_empty() || !self.missing.is_empty() || !self.dangling_edges.is_empty()
    }

    pub fn is_clean(&self) -> bool {
        self.untracked.is_empty()
            && self.missing.is_empty()
            && self.dangling_edges.is_empty()
            && self.uncommented_edges.is_empty()
            && self.unattended_orphans.is_empty()
    }
}

/// Compute the status report for the repo rooted at `root`.
pub fn compute(root: &Path) -> Result<StatusReport> {
    let fs_paths = tree::scan(root)?;
    let content = std::fs::read_to_string(tangle::graph_path(root))?;
    let doc = graph::parse(&content)?;
    let g = graph::to_graph(&doc)?;
    let cfg = load_config(root)?;
    let intentional = graph::intentional_orphans(&doc);
    let mut report = compute_from_with_intentional_and_options(
        &fs_paths,
        &g,
        &intentional,
        cfg.warn_uncommented_edges,
    );
    report.missing_exists_on_disk = report
        .missing
        .iter()
        .map(|(path, _)| (path.clone(), node_exists_on_disk(root, path)))
        .collect();
    report.update_preview = Some(update::preview_in(root)?);
    Ok(report)
}

/// Pure computation from already-loaded data (for testing).
#[allow(dead_code)] // used by tests
pub fn compute_from(fs_paths: &[String], g: &Graph) -> StatusReport {
    compute_from_with_intentional_and_options(fs_paths, g, &HashSet::new(), true)
}

/// Pure computation with explicit intentional orphan markers.
#[allow(dead_code)] // used by tests
pub fn compute_from_with_intentional(
    fs_paths: &[String],
    g: &Graph,
    intentional_orphans: &HashSet<String>,
) -> StatusReport {
    compute_from_with_intentional_and_options(fs_paths, g, intentional_orphans, true)
}

/// Pure computation with explicit intentional orphan markers and optional edge-comment warnings.
pub fn compute_from_with_intentional_and_options(
    fs_paths: &[String],
    g: &Graph,
    intentional_orphans: &HashSet<String>,
    warn_uncommented_edges: bool,
) -> StatusReport {
    let d = diff::compute(fs_paths, g);
    let fs_set: HashSet<&str> = fs_paths.iter().map(String::as_str).collect();

    let missing: Vec<(String, usize)> = d
        .missing
        .into_iter()
        .map(|path| {
            let outgoing = g.get(&path).map(|n| n.edges.len()).unwrap_or(0);
            let incoming = g
                .nodes
                .iter()
                .flat_map(|n| n.edges.iter())
                .filter(|e| e.target == path)
                .count();
            (path, outgoing + incoming)
        })
        .collect();

    let dangling_edges = g
        .dangling_edges()
        .into_iter()
        .map(|(src, e)| {
            let arrow = match e.kind {
                EdgeKind::Directed => "->",
                EdgeKind::Incoming => "<-",
                EdgeKind::Undirected => "--",
            };
            (src.path.clone(), arrow, e.target.clone(), e.label.clone())
        })
        .collect();

    let uncommented_edges = if warn_uncommented_edges {
        g.nodes
            .iter()
            .flat_map(|n| {
                n.edges.iter().filter_map(move |e| {
                    if !e.label.trim().is_empty() {
                        return None;
                    }
                    let arrow = match e.kind {
                        EdgeKind::Directed => "->",
                        EdgeKind::Incoming => "<-",
                        EdgeKind::Undirected => "--",
                    };
                    Some((n.path.clone(), arrow, e.target.clone()))
                })
            })
            .collect()
    } else {
        Vec::new()
    };

    let unattended_orphans = g
        .orphans()
        .filter(|n| fs_set.contains(n.path.as_str()))
        .filter(|n| !intentional_orphans.contains(&n.path))
        .map(|n| n.path.clone())
        .collect();

    StatusReport {
        untracked: d.untracked,
        missing,
        missing_exists_on_disk: HashMap::new(),
        dangling_edges,
        uncommented_edges,
        unattended_orphans,
        graph_nodes: g.nodes.iter().map(|n| n.path.clone()).collect(),
        update_preview: None,
    }
}

// ---------------------------------------------------------------------------
// Output
// ---------------------------------------------------------------------------

fn print_report(r: &StatusReport) {
    if !r.untracked.is_empty() {
        println!("\n  {}", "Untracked nodes (not in graph):".yellow().bold());
        for path in &r.untracked {
            println!("    {}", path);
        }
    }

    if !r.missing.is_empty() {
        println!(
            "\n  {}",
            "Declared nodes not in scan (ignored or missing):"
                .red()
                .bold()
        );
        for (path, edge_count) in &r.missing {
            let reason = match r.missing_exists_on_disk.get(path).copied() {
                Some(true) => "excluded by ignore rules",
                Some(false) => "not found on disk",
                None => "not in scan",
            };
            if *edge_count > 0 {
                println!(
                    "    {}  [{} linked edge{}, {}]",
                    path,
                    edge_count,
                    if *edge_count == 1 { "" } else { "s" },
                    reason
                );
            } else {
                println!("    {}  [{}]", path, reason);
            }
        }
    }

    if !r.dangling_edges.is_empty() {
        println!(
            "\n  {}",
            "Dangling edges (target not in graph):".magenta().bold()
        );
        let mut hint_cache: HashMap<String, Option<String>> = HashMap::new();
        for (src, arrow, target, label) in &r.dangling_edges {
            if label.is_empty() {
                println!("    {} {} {} :", src, arrow, target);
            } else {
                println!("    {} {} {} : {}", src, arrow, target, label);
            }
            let hint = hint_cache
                .entry(target.clone())
                .or_insert_with(|| closest_node_hint(target, &r.graph_nodes));
            if let Some(hint) = hint {
                println!(
                    "      {}",
                    format!("hint: did you mean `{}`?", hint).dark_grey()
                );
            }
        }
    }

    if !r.uncommented_edges.is_empty() {
        println!(
            "\n  {}",
            "Uncommented edges (empty labels):".yellow().bold()
        );
        for (src, arrow, target) in &r.uncommented_edges {
            println!("    {} {} {} :", src, arrow, target);
        }
        println!(
            "    {}",
            "(set `warn_uncommented_edges: false` in tangle/config.tngl to silence)".dark_grey()
        );
    }

    if !r.unattended_orphans.is_empty() {
        println!(
            "\n  {}",
            "Unattended orphans (isolated nodes):".cyan().bold()
        );
        for path in &r.unattended_orphans {
            println!("    {}", path);
        }
    }

    let mut update_preview_printed = false;
    if let Some(preview) = &r.update_preview {
        if preview.has_any_change() {
            update_preview_printed = true;
            println!("\n  {}", "Graph file can be updated:".blue().bold());
            if preview.orphan_link_conflicts > 0 {
                println!(
                    "    {}",
                    format!(
                        "{} orphan-link conflict{}",
                        preview.orphan_link_conflicts,
                        if preview.orphan_link_conflicts == 1 {
                            ""
                        } else {
                            "s"
                        }
                    )
                );
            }
            if preview.folder_orphan_scope_conflicts > 0 {
                let mut example_paths = preview.folder_orphan_scope_paths.clone();
                example_paths.sort();
                let examples = example_paths
                    .iter()
                    .take(3)
                    .cloned()
                    .collect::<Vec<_>>()
                    .join(", ");
                println!(
                    "    {}",
                    format!(
                        "{} folder [orphan] scope conflict{}{}",
                        preview.folder_orphan_scope_conflicts,
                        if preview.folder_orphan_scope_conflicts == 1 {
                            ""
                        } else {
                            "s"
                        },
                        if examples.is_empty() {
                            String::new()
                        } else {
                            format!(" ({})", examples)
                        }
                    )
                );
                let unattended_inside: Vec<&String> = r
                    .unattended_orphans
                    .iter()
                    .filter(|path| {
                        preview.folder_orphan_scope_paths.iter().any(|root| {
                            path.as_str() == root.as_str()
                                || (path.as_str() != root.as_str() && path.starts_with(root))
                        })
                    })
                    .collect();
                if !unattended_inside.is_empty() {
                    println!(
                        "    {}",
                        "Unattended orphan nodes exist inside those folder(s).".dark_grey()
                    );
                    println!(
                        "    {}",
                        "If you update without adding a tangle, you'll be asked to convert to [orphan bundle]."
                            .dark_grey()
                    );
                }
            }
            if preview.orphaned_bundle_conflicts > 0 {
                println!(
                    "    {}",
                    format!(
                        "{} orphaned [bundle] conflict{}",
                        preview.orphaned_bundle_conflicts,
                        if preview.orphaned_bundle_conflicts == 1 {
                            ""
                        } else {
                            "s"
                        }
                    )
                );
            }
            if preview.lint_removed_floating_tags > 0 {
                println!(
                    "    {}",
                    format!(
                        "{} floating tag{} can be removed",
                        preview.lint_removed_floating_tags,
                        if preview.lint_removed_floating_tags == 1 {
                            ""
                        } else {
                            "s"
                        }
                    )
                );
            }
            if preview.lint_removed_extra_blank_lines > 0 {
                println!(
                    "    {}",
                    format!(
                        "{} extra blank line{} can be collapsed",
                        preview.lint_removed_extra_blank_lines,
                        if preview.lint_removed_extra_blank_lines == 1 {
                            ""
                        } else {
                            "s"
                        }
                    )
                );
            }
            if preview.lint_normalized_tag_indentation > 0 {
                println!(
                    "    {}",
                    format!(
                        "{} tag indentation{} can be normalized",
                        preview.lint_normalized_tag_indentation,
                        if preview.lint_normalized_tag_indentation == 1 {
                            ""
                        } else {
                            "s"
                        }
                    )
                );
            }
            if preview.lint_normalized_edge_indentation > 0 {
                println!(
                    "    {}",
                    format!(
                        "{} edge indentation{} can be normalized",
                        preview.lint_normalized_edge_indentation,
                        if preview.lint_normalized_edge_indentation == 1 {
                            ""
                        } else {
                            "s"
                        }
                    )
                );
            }
            if preview.mirrored_edges > 0 {
                println!(
                    "    {}",
                    format!(
                        "{} mirrored edge{} can be synced",
                        preview.mirrored_edges,
                        if preview.mirrored_edges == 1 { "" } else { "s" }
                    )
                );
            }
            if preview.reordered_edge_nodes > 0 {
                println!(
                    "    {}",
                    format!(
                        "{} node edge block{} can be reordered (--, ->, <-)",
                        preview.reordered_edge_nodes,
                        if preview.reordered_edge_nodes == 1 {
                            ""
                        } else {
                            "s"
                        }
                    )
                );
            }
            println!("    {}", "Run `tngl update` to apply.".dark_grey());
        }
    }

    if r.is_clean() && !update_preview_printed {
        println!("\n  {}", "Graph is clean.".green());
    }
}

fn node_exists_on_disk(root: &Path, node_path: &str) -> bool {
    let rel = node_path.trim_end_matches('/');
    if rel.is_empty() {
        return false;
    }
    root.join(rel).exists()
}

fn load_config(root: &Path) -> Result<config::Config> {
    let path = tangle::config_path(root);
    if path.exists() {
        let content = std::fs::read_to_string(path)?;
        config::parse(&content)
    } else {
        Ok(config::Config::default())
    }
}

fn closest_node_hint(target: &str, candidates: &[String]) -> Option<String> {
    let mut best: Option<(&str, usize)> = None;
    let target_lc = target.to_ascii_lowercase();
    for candidate in candidates {
        if candidate == target {
            continue;
        }
        let d = levenshtein(&target_lc, &candidate.to_ascii_lowercase());
        match best {
            None => best = Some((candidate.as_str(), d)),
            Some((best_cand, best_d)) => {
                if d < best_d || (d == best_d && candidate.as_str() < best_cand) {
                    best = Some((candidate.as_str(), d));
                }
            }
        }
    }

    let (candidate, dist) = best?;
    let max_len = target.chars().count().max(candidate.chars().count());
    let threshold = if max_len <= 4 {
        1
    } else if max_len <= 10 {
        2
    } else {
        3
    };
    if dist <= threshold {
        Some(candidate.to_string())
    } else {
        None
    }
}

fn levenshtein(a: &str, b: &str) -> usize {
    let a_chars: Vec<char> = a.chars().collect();
    let b_chars: Vec<char> = b.chars().collect();
    if a_chars.is_empty() {
        return b_chars.len();
    }
    if b_chars.is_empty() {
        return a_chars.len();
    }

    let mut prev: Vec<usize> = (0..=b_chars.len()).collect();
    let mut curr: Vec<usize> = vec![0; b_chars.len() + 1];

    for (i, &ca) in a_chars.iter().enumerate() {
        curr[0] = i + 1;
        for (j, &cb) in b_chars.iter().enumerate() {
            let cost = if ca == cb { 0 } else { 1 };
            curr[j + 1] = (prev[j + 1] + 1).min(curr[j] + 1).min(prev[j] + cost);
        }
        std::mem::swap(&mut prev, &mut curr);
    }
    prev[b_chars.len()]
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::model::{Edge, EdgeKind, Node};

    fn graph_from_edges(nodes: &[(&str, &[(&str, &str)])]) -> Graph {
        let mut g = Graph::new();
        for (path, edges) in nodes {
            let mut node = Node::new(*path);
            for (target, label) in *edges {
                node.edges.push(Edge {
                    target: target.to_string(),
                    kind: EdgeKind::Directed,
                    label: label.to_string(),
                });
            }
            g.add_node(node);
        }
        g
    }

    fn fs(paths: &[&str]) -> Vec<String> {
        paths.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn clean_when_in_sync() {
        let g = graph_from_edges(&[("a.rs", &[]), ("b.rs", &[])]);
        let report = compute_from(&fs(&["a.rs", "b.rs"]), &g);
        assert!(!report.has_issues());
    }

    #[test]
    fn detects_untracked_files() {
        let g = graph_from_edges(&[("a.rs", &[])]);
        let report = compute_from(&fs(&["a.rs", "new.rs"]), &g);
        assert_eq!(report.untracked, vec!["new.rs"]);
    }

    #[test]
    fn detects_missing_files() {
        let g = graph_from_edges(&[("a.rs", &[]), ("b.rs", &[])]);
        let report = compute_from(&fs(&["a.rs"]), &g);
        assert_eq!(report.missing.len(), 1);
        assert_eq!(report.missing[0].0, "b.rs");
        assert_eq!(report.missing[0].1, 0); // no edges
    }

    #[test]
    fn missing_file_reports_dangling_edge_count() {
        let g = graph_from_edges(&[
            ("a.rs", &[("b.rs", "uses"), ("b.rs", "also uses")]),
            ("b.rs", &[]),
        ]);
        let report = compute_from(&fs(&["a.rs"]), &g);
        let (_, edge_count) = report.missing.iter().find(|(p, _)| p == "b.rs").unwrap();
        assert_eq!(*edge_count, 2); // two incoming links from a.rs
    }

    #[test]
    fn detects_dangling_edges() {
        // a.rs has an edge to phantom.rs which is not declared as a node
        let g = graph_from_edges(&[("a.rs", &[("phantom.rs", "uses")])]);
        let report = compute_from(&fs(&["a.rs"]), &g);
        assert_eq!(report.dangling_edges.len(), 1);
        let (src, _, target, label) = &report.dangling_edges[0];
        assert_eq!(src, "a.rs");
        assert_eq!(target, "phantom.rs");
        assert_eq!(label, "uses");
    }

    #[test]
    fn detects_orphan_nodes() {
        let g = graph_from_edges(&[("a.rs", &[("b.rs", "uses")]), ("b.rs", &[]), ("c.rs", &[])]);
        let report = compute_from(&fs(&["a.rs", "b.rs", "c.rs"]), &g);
        // b.rs is targeted, so only c.rs is isolated.
        assert!(!report.unattended_orphans.contains(&"b.rs".to_string()));
        assert!(report.unattended_orphans.contains(&"c.rs".to_string()));
    }

    #[test]
    fn no_issues_with_only_orphans() {
        let g = graph_from_edges(&[("a.rs", &[])]);
        let report = compute_from(&fs(&["a.rs"]), &g);
        // Orphans are informational — not counted as issues
        assert!(!report.has_issues());
        assert_eq!(report.unattended_orphans, vec!["a.rs"]);
        assert!(!report.is_clean());
    }

    #[test]
    fn missing_isolated_node_not_duplicated_as_orphan() {
        let g = graph_from_edges(&[("stale.rs", &[])]);
        let report = compute_from(&fs(&[]), &g);
        assert_eq!(report.missing.len(), 1);
        assert!(report.unattended_orphans.is_empty());
    }

    #[test]
    fn compute_from_does_not_guess_missing_disk_state() {
        let g = graph_from_edges(&[("stale.rs", &[])]);
        let report = compute_from(&fs(&[]), &g);
        assert!(report.missing_exists_on_disk.is_empty());
    }

    #[test]
    fn intentional_orphan_is_not_reported_as_unattended() {
        let g = graph_from_edges(&[("a.rs", &[])]);
        let intentional: HashSet<String> = ["a.rs".to_string()].into_iter().collect();
        let report = compute_from_with_intentional(&fs(&["a.rs"]), &g, &intentional);
        assert!(report.unattended_orphans.is_empty());
        assert!(report.is_clean());
    }

    #[test]
    fn integration_with_real_files() {
        use std::fs as sfs;
        use tempfile::TempDir;

        let dir = TempDir::new().unwrap();
        // Set up tangle/graph.tngl
        sfs::create_dir_all(dir.path().join("tangle")).unwrap();
        sfs::write(
            dir.path().join("tangle/graph.tngl"),
            "a.rs\n    -> b.rs : uses\n\nb.rs\n\nold.rs\n",
        )
        .unwrap();
        // Only a.rs and b.rs exist on disk; old.rs is missing; new.rs is untracked
        sfs::write(dir.path().join("a.rs"), "").unwrap();
        sfs::write(dir.path().join("b.rs"), "").unwrap();
        sfs::write(dir.path().join("new.rs"), "").unwrap();

        let report = compute(dir.path()).unwrap();
        assert!(report.untracked.contains(&"new.rs".to_string()));
        assert!(report.missing.iter().any(|(p, _)| p == "old.rs"));
    }

    #[test]
    fn warns_on_uncommented_edges_by_default() {
        let g = graph_from_edges(&[("a.rs", &[("b.rs", "")]), ("b.rs", &[])]);
        let report = compute_from(&fs(&["a.rs", "b.rs"]), &g);
        assert_eq!(report.uncommented_edges.len(), 1);
        assert_eq!(
            report.uncommented_edges[0],
            ("a.rs".to_string(), "->", "b.rs".to_string())
        );
        assert!(!report.is_clean());
    }

    #[test]
    fn can_disable_uncommented_edge_warnings() {
        let g = graph_from_edges(&[("a.rs", &[("b.rs", "")]), ("b.rs", &[])]);
        let report = compute_from_with_intentional_and_options(
            &fs(&["a.rs", "b.rs"]),
            &g,
            &HashSet::new(),
            false,
        );
        assert!(report.uncommented_edges.is_empty());
        assert!(report.is_clean());
    }

    #[test]
    fn compute_respects_config_for_uncommented_edges() {
        use std::fs as sfs;
        use tempfile::TempDir;

        let dir = TempDir::new().unwrap();
        sfs::create_dir_all(dir.path().join("tangle")).unwrap();
        sfs::write(
            dir.path().join("tangle/graph.tngl"),
            "a.rs\n    -> b.rs :\n\nb.rs\n",
        )
        .unwrap();
        sfs::write(
            dir.path().join("tangle/config.tngl"),
            "warn_uncommented_edges: false\n",
        )
        .unwrap();
        sfs::write(dir.path().join("a.rs"), "").unwrap();
        sfs::write(dir.path().join("b.rs"), "").unwrap();

        let report = compute(dir.path()).unwrap();
        assert!(report.uncommented_edges.is_empty());
        assert!(report.is_clean());
    }

    #[test]
    fn compute_reports_untracked_descendants_for_orphan_subtree() {
        use std::fs as sfs;
        use tempfile::TempDir;

        let dir = TempDir::new().unwrap();
        sfs::create_dir_all(dir.path().join("tangle")).unwrap();
        sfs::write(
            dir.path().join("tangle/graph.tngl"),
            "[orphan bundle]\nsrc/\n",
        )
        .unwrap();
        sfs::create_dir_all(dir.path().join("src")).unwrap();
        sfs::write(dir.path().join("src/lib.rs"), "").unwrap();

        let report = compute(dir.path()).unwrap();
        assert!(report.untracked.contains(&"src/lib.rs".to_string()));
    }

    #[test]
    fn compute_reports_untracked_descendants_for_link_subtree() {
        use std::fs as sfs;
        use tempfile::TempDir;

        let dir = TempDir::new().unwrap();
        sfs::create_dir_all(dir.path().join("tangle")).unwrap();
        sfs::write(dir.path().join("tangle/graph.tngl"), "[bundle]\nsrc/\n").unwrap();
        sfs::create_dir_all(dir.path().join("src")).unwrap();
        sfs::write(dir.path().join("src/lib.rs"), "").unwrap();

        let report = compute(dir.path()).unwrap();
        assert!(report.untracked.contains(&"src/lib.rs".to_string()));
    }

    #[test]
    fn compute_does_not_report_bundle_collapse_as_pending_update() {
        use std::fs as sfs;
        use tempfile::TempDir;

        let dir = TempDir::new().unwrap();
        sfs::create_dir_all(dir.path().join("tangle")).unwrap();
        sfs::write(
            dir.path().join("tangle/graph.tngl"),
            "[bundle]\nsrc/\n    <- consumer.rs : used by\n\n    src/lib.rs\n\nconsumer.rs\n    -> src/ : used by\n",
        )
        .unwrap();
        sfs::create_dir_all(dir.path().join("src")).unwrap();
        sfs::write(dir.path().join("src/lib.rs"), "").unwrap();
        sfs::write(dir.path().join("consumer.rs"), "").unwrap();

        let report = compute(dir.path()).unwrap();
        let preview = report.update_preview.expect("preview should be present");
        assert_eq!(preview.collapsed_subtree_nodes, 0);
        assert!(!preview.has_any_change());
    }

    #[test]
    fn compute_does_not_report_folder_orphan_scope_conflict() {
        use std::fs as sfs;
        use tempfile::TempDir;

        let dir = TempDir::new().unwrap();
        sfs::create_dir_all(dir.path().join("tangle")).unwrap();
        sfs::write(
            dir.path().join("tangle/graph.tngl"),
            "[orphan]\nsrc/\n\n    src/lib.rs\n",
        )
        .unwrap();
        sfs::create_dir_all(dir.path().join("src")).unwrap();
        sfs::write(dir.path().join("src/lib.rs"), "").unwrap();

        let report = compute(dir.path()).unwrap();
        let preview = report.update_preview.expect("preview should be present");
        assert_eq!(preview.folder_orphan_scope_conflicts, 0);
        assert!(preview.folder_orphan_scope_paths.is_empty());
    }

    #[test]
    fn dangling_hint_suggests_close_match() {
        let candidates = vec!["src/parser/graph.rs".to_string(), "src/main.rs".to_string()];
        let hint = closest_node_hint("src/parser/grap.rs", &candidates);
        assert_eq!(hint, Some("src/parser/graph.rs".to_string()));
    }

    #[test]
    fn dangling_hint_ignores_distant_match() {
        let candidates = vec!["src/main.rs".to_string(), "docs/spec.md".to_string()];
        let hint = closest_node_hint("totally/unrelated/path.rs", &candidates);
        assert!(hint.is_none());
    }
}
