//! `tngl inspect` — query the graph for specific conditions.

use std::collections::{HashMap, HashSet};
use std::fs;
use std::io::{self, Write};
use std::path::Path;

use anyhow::Result;

use crate::graph::model::{EdgeKind, Graph, Node};
use crate::parser::graph;
use crate::tangle;

// ---------------------------------------------------------------------------
// Public entry points
// ---------------------------------------------------------------------------

pub fn run_orphans() -> Result<()> {
    let g = load_graph_from_root()?;
    let orphans: Vec<&Node> = g.orphans().collect();
    if orphans.is_empty() {
        println!("  No isolated orphan nodes.");
    } else {
        for node in orphans {
            println!("  {}", node.path);
        }
    }
    Ok(())
}

pub fn run_dangling() -> Result<()> {
    let g = load_graph_from_root()?;
    let dangling = g.dangling_edges();
    if dangling.is_empty() {
        println!("  No dangling edges.");
    } else {
        for (src, edge) in dangling {
            print_edge(&src.path, edge);
        }
    }
    Ok(())
}

pub fn run_edges(node_path: &str) -> Result<()> {
    let g = load_graph_from_root()?;
    match g.get(node_path) {
        None => {
            println!("  Node not found: {}", node_path);
        }
        Some(node) if node.is_orphan() => {
            println!("  {} has no edges.", node_path);
        }
        Some(node) => {
            for edge in &node.edges {
                print_edge(node_path, edge);
            }
        }
    }
    Ok(())
}

pub fn run_unreachable() -> Result<()> {
    let g = load_graph_from_root()?;
    let unreachable = unreachable_nodes(&g);
    if unreachable.is_empty() {
        println!("  All nodes are reachable from at least one other node.");
    } else {
        for node in unreachable {
            println!("  {}", node.path);
        }
    }
    Ok(())
}

pub fn run_comment_mismatches() -> Result<()> {
    let g = load_graph_from_root()?;
    let mismatches = comment_mismatches(&g);
    if mismatches.is_empty() {
        println!("  No mirrored edge comment mismatches.");
    } else {
        for mismatch in &mismatches {
            print_mismatch(mismatch);
        }
    }
    Ok(())
}

pub fn run_reconcile_comment_mismatches() -> Result<()> {
    let root = tangle::find_root()?;
    let graph_path = tangle::graph_path(&root);
    let content = fs::read_to_string(&graph_path)?;
    let mut doc = graph::parse(&content)?;
    let g = graph::to_graph(&doc)?;
    let mismatches = comment_mismatches(&g);

    if mismatches.is_empty() {
        println!("  No mirrored edge comment mismatches.");
        return Ok(());
    }

    let mut changed_entries = 0usize;
    let mut resolved_pairs = 0usize;

    for (idx, mismatch) in mismatches.iter().enumerate() {
        println!("\n  Mismatch {}/{}:", idx + 1, mismatches.len());
        print_mismatch(mismatch);

        loop {
            print!("  Keep label from [1] first, [2] second, [s] skip, [q] quit: ");
            io::stdout().flush()?;
            let mut input = String::new();
            io::stdin().read_line(&mut input)?;
            match input.trim() {
                "1" => {
                    let changed = apply_mismatch_label(&mut doc, mismatch, &mismatch.forward.label);
                    changed_entries += changed;
                    resolved_pairs += usize::from(changed > 0);
                    break;
                }
                "2" => {
                    let changed = apply_mismatch_label(&mut doc, mismatch, &mismatch.reverse.label);
                    changed_entries += changed;
                    resolved_pairs += usize::from(changed > 0);
                    break;
                }
                "s" => break,
                "q" => {
                    if changed_entries > 0 {
                        fs::write(&graph_path, graph::serialize(&doc))?;
                        println!(
                            "\n  Updated {} edge entries across {} mismatch pair{}.",
                            changed_entries,
                            resolved_pairs,
                            if resolved_pairs == 1 { "" } else { "s" }
                        );
                    } else {
                        println!("\n  No changes written.");
                    }
                    return Ok(());
                }
                _ => {
                    println!("  Please choose 1, 2, s, or q.");
                }
            }
        }
    }

    if changed_entries > 0 {
        fs::write(&graph_path, graph::serialize(&doc))?;
        println!(
            "\n  Updated {} edge entries across {} mismatch pair{}.",
            changed_entries,
            resolved_pairs,
            if resolved_pairs == 1 { "" } else { "s" }
        );
    } else {
        println!("\n  No changes written.");
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Pure logic (testable without filesystem)
// ---------------------------------------------------------------------------

/// Nodes that are not the target of any edge in the graph.
///
/// A node is "unreachable" if no other node has an edge pointing to it.
/// This is distinct from "orphan" (which means the node has no incoming and no
/// outgoing edges).
pub fn unreachable_nodes(g: &Graph) -> Vec<&Node> {
    let targets: HashSet<&str> = g
        .nodes
        .iter()
        .flat_map(|n| {
            n.edges.iter().map(move |e| match e.kind {
                EdgeKind::Directed | EdgeKind::Undirected => e.target.as_str(),
                EdgeKind::Incoming => n.path.as_str(),
            })
        })
        .collect();

    g.nodes
        .iter()
        .filter(|n| !targets.contains(n.path.as_str()))
        .collect()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EdgeRef {
    pub source: String,
    pub target: String,
    pub kind: EdgeKind,
    pub label: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CommentMismatch {
    pub forward: EdgeRef,
    pub reverse: EdgeRef,
}

/// Directed `<-> incoming` mirrored edge pairs where labels disagree.
pub fn comment_mismatches(g: &Graph) -> Vec<CommentMismatch> {
    let mut incoming_by_pair: HashMap<(String, String), String> = HashMap::new();

    for node in &g.nodes {
        for edge in &node.edges {
            if edge.kind != EdgeKind::Incoming {
                continue;
            }
            // node <- edge.target  is the mirror of  edge.target -> node
            incoming_by_pair
                .entry((edge.target.clone(), node.path.clone()))
                .or_insert_with(|| edge.label.clone());
        }
    }

    let mut mismatches = Vec::new();
    for node in &g.nodes {
        for edge in &node.edges {
            if edge.kind != EdgeKind::Directed {
                continue;
            }
            let key = (node.path.clone(), edge.target.clone());
            let Some(incoming_label) = incoming_by_pair.get(&key) else {
                continue;
            };
            if edge.label == *incoming_label {
                continue;
            }

            mismatches.push(CommentMismatch {
                forward: EdgeRef {
                    source: node.path.clone(),
                    target: edge.target.clone(),
                    kind: EdgeKind::Directed,
                    label: edge.label.clone(),
                },
                reverse: EdgeRef {
                    source: edge.target.clone(),
                    target: node.path.clone(),
                    kind: EdgeKind::Incoming,
                    label: incoming_label.clone(),
                },
            });
        }
    }

    mismatches.sort_by(|a, b| {
        a.forward
            .source
            .cmp(&b.forward.source)
            .then(a.forward.target.cmp(&b.forward.target))
    });
    mismatches
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn load_graph_from_root() -> Result<Graph> {
    let root = tangle::find_root()?;
    load_graph(&root)
}

pub fn load_graph(root: &Path) -> Result<Graph> {
    let content = std::fs::read_to_string(tangle::graph_path(root))?;
    let doc = graph::parse(&content)?;
    graph::to_graph(&doc)
}

fn print_edge(source: &str, edge: &crate::graph::model::Edge) {
    let arrow = match edge.kind {
        EdgeKind::Directed => "->",
        EdgeKind::Incoming => "<-",
        EdgeKind::Undirected => "--",
    };
    if edge.label.is_empty() {
        println!("  {} {} {} :", source, arrow, edge.target);
    } else {
        println!("  {} {} {} : {}", source, arrow, edge.target, edge.label);
    }
}

fn print_mismatch(mismatch: &CommentMismatch) {
    println!(
        "  {} -> {} : {}",
        mismatch.forward.source,
        mismatch.forward.target,
        display_label(&mismatch.forward.label)
    );
    println!(
        "  {} <- {} : {}",
        mismatch.reverse.source,
        mismatch.reverse.target,
        display_label(&mismatch.reverse.label)
    );
}

fn display_label(label: &str) -> String {
    if label.is_empty() {
        "<empty>".to_string()
    } else {
        label.to_string()
    }
}

fn apply_mismatch_label(
    doc: &mut graph::Document,
    mismatch: &CommentMismatch,
    chosen: &str,
) -> usize {
    let mut changed = 0;
    changed += graph::replace_edge_label(
        doc,
        &mismatch.forward.source,
        &mismatch.forward.target,
        mismatch.forward.kind.clone(),
        &mismatch.forward.label,
        chosen,
    );
    changed += graph::replace_edge_label(
        doc,
        &mismatch.reverse.source,
        &mismatch.reverse.target,
        mismatch.reverse.kind.clone(),
        &mismatch.reverse.label,
        chosen,
    );
    changed
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::model::{Edge, Node};

    fn make_graph(spec: &[(&str, &[(&str, &str)])]) -> Graph {
        let mut g = Graph::new();
        for (path, edges) in spec {
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

    // -----------------------------------------------------------------------
    // orphans
    // -----------------------------------------------------------------------

    #[test]
    fn orphans_returns_only_isolated_nodes() {
        let g = make_graph(&[("a.rs", &[("b.rs", "uses")]), ("b.rs", &[]), ("c.rs", &[])]);
        let orphans: Vec<&str> = g.orphans().map(|n| n.path.as_str()).collect();
        assert!(!orphans.contains(&"b.rs"));
        assert!(orphans.contains(&"c.rs"));
        assert!(!orphans.contains(&"a.rs"));
    }

    #[test]
    fn orphans_empty_when_all_have_edges() {
        let g = make_graph(&[("a.rs", &[("b.rs", "")]), ("b.rs", &[("a.rs", "")])]);
        let orphans: Vec<_> = g.orphans().collect();
        assert!(orphans.is_empty());
    }

    // -----------------------------------------------------------------------
    // dangling edges
    // -----------------------------------------------------------------------

    #[test]
    fn dangling_edges_detected() {
        // a.rs -> phantom.rs, but phantom.rs is not a node
        let g = make_graph(&[("a.rs", &[("phantom.rs", "uses")])]);
        let dangling = g.dangling_edges();
        assert_eq!(dangling.len(), 1);
        assert_eq!(dangling[0].0.path, "a.rs");
        assert_eq!(dangling[0].1.target, "phantom.rs");
    }

    #[test]
    fn no_dangling_when_all_targets_present() {
        let g = make_graph(&[("a.rs", &[("b.rs", "uses")]), ("b.rs", &[])]);
        assert!(g.dangling_edges().is_empty());
    }

    // -----------------------------------------------------------------------
    // edges for node
    // -----------------------------------------------------------------------

    #[test]
    fn edges_for_node_found() {
        let g = make_graph(&[("a.rs", &[("b.rs", "uses"), ("c.rs", "tests")])]);
        let node = g.get("a.rs").unwrap();
        assert_eq!(node.edges.len(), 2);
        assert_eq!(node.edges[0].target, "b.rs");
        assert_eq!(node.edges[1].target, "c.rs");
    }

    #[test]
    fn edges_for_node_not_found() {
        let g = make_graph(&[("a.rs", &[])]);
        assert!(g.get("z.rs").is_none());
    }

    // -----------------------------------------------------------------------
    // unreachable nodes
    // -----------------------------------------------------------------------

    #[test]
    fn unreachable_when_no_incoming_edges() {
        // a.rs -> b.rs; c.rs has no incoming edges
        let g = make_graph(&[("a.rs", &[("b.rs", "")]), ("b.rs", &[]), ("c.rs", &[])]);
        let unreachable: Vec<&str> = unreachable_nodes(&g)
            .iter()
            .map(|n| n.path.as_str())
            .collect();
        // a.rs and c.rs have no incoming edges; b.rs does.
        assert!(unreachable.contains(&"a.rs"));
        assert!(unreachable.contains(&"c.rs"));
        assert!(!unreachable.contains(&"b.rs"));
    }

    #[test]
    fn all_reachable_in_cycle() {
        // a -> b -> a
        let g = make_graph(&[("a.rs", &[("b.rs", "")]), ("b.rs", &[("a.rs", "")])]);
        assert!(unreachable_nodes(&g).is_empty());
    }

    #[test]
    fn all_unreachable_when_graph_empty() {
        let g = Graph::new();
        assert!(unreachable_nodes(&g).is_empty());
    }

    #[test]
    fn isolated_orphan_is_also_unreachable() {
        // An isolated orphan (no edges in either direction) is unreachable.
        let g = make_graph(&[("a.rs", &[("b.rs", "")]), ("b.rs", &[]), ("c.rs", &[])]);
        let unreachable: Vec<&str> = unreachable_nodes(&g)
            .iter()
            .map(|n| n.path.as_str())
            .collect();
        assert!(unreachable.contains(&"c.rs"));
    }

    #[test]
    fn undirected_edge_makes_both_ends_reachable() {
        let mut g = Graph::new();
        let mut a = Node::new("a.rs");
        a.edges.push(Edge {
            target: "b.rs".into(),
            kind: EdgeKind::Undirected,
            label: String::new(),
        });
        g.add_node(a);
        g.add_node(Node::new("b.rs"));
        let unreachable: Vec<&str> = unreachable_nodes(&g)
            .iter()
            .map(|n| n.path.as_str())
            .collect();
        // b.rs is a target of the undirected edge → reachable
        assert!(!unreachable.contains(&"b.rs"));
        // a.rs is not targeted by anyone → unreachable
        assert!(unreachable.contains(&"a.rs"));
    }

    #[test]
    fn incoming_edge_marks_current_node_reachable() {
        let mut g = Graph::new();
        let mut a = Node::new("a.rs");
        a.edges.push(Edge {
            target: "b.rs".into(),
            kind: EdgeKind::Incoming, // a <- b (b -> a)
            label: String::new(),
        });
        g.add_node(a);
        g.add_node(Node::new("b.rs"));
        let unreachable: Vec<&str> = unreachable_nodes(&g)
            .iter()
            .map(|n| n.path.as_str())
            .collect();
        assert!(!unreachable.contains(&"a.rs"));
    }

    // -----------------------------------------------------------------------
    // mirrored comment mismatches
    // -----------------------------------------------------------------------

    #[test]
    fn comment_mismatches_detects_disagreement() {
        let mut g = Graph::new();
        let mut a = Node::new("a.rs");
        a.edges.push(Edge {
            target: "b.rs".into(),
            kind: EdgeKind::Directed,
            label: "producer".into(),
        });
        let mut b = Node::new("b.rs");
        b.edges.push(Edge {
            target: "a.rs".into(),
            kind: EdgeKind::Incoming,
            label: "consumer".into(),
        });
        g.add_node(a);
        g.add_node(b);

        let mismatches = comment_mismatches(&g);
        assert_eq!(mismatches.len(), 1);
        assert_eq!(mismatches[0].forward.source, "a.rs");
        assert_eq!(mismatches[0].forward.target, "b.rs");
    }

    #[test]
    fn comment_mismatches_ignores_matching_labels() {
        let mut g = Graph::new();
        let mut a = Node::new("a.rs");
        a.edges.push(Edge {
            target: "b.rs".into(),
            kind: EdgeKind::Directed,
            label: "sync".into(),
        });
        let mut b = Node::new("b.rs");
        b.edges.push(Edge {
            target: "a.rs".into(),
            kind: EdgeKind::Incoming,
            label: "sync".into(),
        });
        g.add_node(a);
        g.add_node(b);

        assert!(comment_mismatches(&g).is_empty());
    }

    #[test]
    fn apply_mismatch_label_sets_both_sides() {
        let mut doc = graph::parse("a.rs\n    -> b.rs : one\n\nb.rs\n    <- a.rs : two\n").unwrap();
        let mismatch = CommentMismatch {
            forward: EdgeRef {
                source: "a.rs".into(),
                target: "b.rs".into(),
                kind: EdgeKind::Directed,
                label: "one".into(),
            },
            reverse: EdgeRef {
                source: "b.rs".into(),
                target: "a.rs".into(),
                kind: EdgeKind::Incoming,
                label: "two".into(),
            },
        };

        let changed = apply_mismatch_label(&mut doc, &mismatch, "one");
        assert_eq!(changed, 2);
        let out = graph::serialize(&doc);
        assert!(out.contains("a.rs\n    -> b.rs : one"));
        assert!(out.contains("b.rs\n    <- a.rs : one"));
    }

    // -----------------------------------------------------------------------
    // Integration: load from real files
    // -----------------------------------------------------------------------

    #[test]
    fn integration_load_and_inspect() {
        use std::fs as sfs;
        use tempfile::TempDir;

        let dir = TempDir::new().unwrap();
        sfs::create_dir_all(dir.path().join("tangle")).unwrap();
        sfs::write(
            dir.path().join("tangle/graph.tngl"),
            "a.rs\n    -> b.rs : uses\n\nb.rs\n\nc.rs\n",
        )
        .unwrap();

        let g = load_graph(dir.path()).unwrap();
        assert_eq!(g.nodes.len(), 3);

        let orphans: Vec<_> = g.orphans().collect();
        // only c.rs is fully isolated
        assert_eq!(orphans.len(), 1);
        assert_eq!(orphans[0].path, "c.rs");

        let unreachable = unreachable_nodes(&g);
        // a.rs and c.rs have no incoming edges
        assert_eq!(unreachable.len(), 2);
    }
}
