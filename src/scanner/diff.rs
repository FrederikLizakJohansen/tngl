//! Diff between the live filesystem and a parsed graph.

use std::collections::HashSet;

use crate::graph::model::Graph;

/// Result of diffing the filesystem against `graph.tngl`.
#[derive(Debug, Default)]
pub struct GraphDiff {
    /// Paths present in the filesystem but not declared as nodes in the graph.
    pub untracked: Vec<String>,
    /// Paths declared as nodes in the graph but absent from the filesystem.
    pub missing: Vec<String>,
}

impl GraphDiff {
    #[allow(dead_code)] // used in tests; available for future command use
    pub fn is_clean(&self) -> bool {
        self.untracked.is_empty() && self.missing.is_empty()
    }
}

/// Compute the diff between `fs_paths` (from the scanner) and the graph.
///
/// Both output lists are sorted for deterministic output.
pub fn compute(fs_paths: &[String], graph: &Graph) -> GraphDiff {
    let fs_set: HashSet<&str> = fs_paths.iter().map(String::as_str).collect();
    let graph_set: HashSet<&str> = graph.nodes.iter().map(|n| n.path.as_str()).collect();

    let mut untracked: Vec<String> = fs_paths
        .iter()
        .filter(|p| !graph_set.contains(p.as_str()))
        .cloned()
        .collect();
    untracked.sort_by(|a, b| compare_hierarchical(a, b));

    let mut missing: Vec<String> = graph
        .nodes
        .iter()
        .map(|n| n.path.as_str())
        .filter(|p| !fs_set.contains(*p))
        .map(String::from)
        .collect();
    missing.sort_by(|a, b| compare_hierarchical(a, b));

    GraphDiff { untracked, missing }
}

fn compare_hierarchical(a: &str, b: &str) -> std::cmp::Ordering {
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
    use crate::graph::model::Node;

    fn graph_from(paths: &[&str]) -> Graph {
        let mut g = Graph::new();
        for &p in paths {
            g.add_node(Node::new(p));
        }
        g
    }

    fn fs(paths: &[&str]) -> Vec<String> {
        paths.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn clean_when_identical() {
        let g = graph_from(&["a.rs", "b.rs"]);
        let diff = compute(&fs(&["a.rs", "b.rs"]), &g);
        assert!(diff.is_clean());
    }

    #[test]
    fn untracked_detected() {
        let g = graph_from(&["a.rs"]);
        let diff = compute(&fs(&["a.rs", "b.rs"]), &g);
        assert_eq!(diff.untracked, vec!["b.rs"]);
        assert!(diff.missing.is_empty());
    }

    #[test]
    fn missing_detected() {
        let g = graph_from(&["a.rs", "b.rs"]);
        let diff = compute(&fs(&["a.rs"]), &g);
        assert_eq!(diff.missing, vec!["b.rs"]);
        assert!(diff.untracked.is_empty());
    }

    #[test]
    fn both_untracked_and_missing() {
        let g = graph_from(&["a.rs", "old.rs"]);
        let diff = compute(&fs(&["a.rs", "new.rs"]), &g);
        assert_eq!(diff.untracked, vec!["new.rs"]);
        assert_eq!(diff.missing, vec!["old.rs"]);
    }

    #[test]
    fn empty_filesystem_empty_graph() {
        let g = Graph::new();
        let diff = compute(&fs(&[]), &g);
        assert!(diff.is_clean());
    }

    #[test]
    fn output_is_sorted() {
        let g = graph_from(&["a.rs"]);
        let diff = compute(&fs(&["z.rs", "m.rs", "a.rs"]), &g);
        let mut sorted = diff.untracked.clone();
        sorted.sort_by(|a, b| compare_hierarchical(a, b));
        assert_eq!(diff.untracked, sorted);
    }

    #[test]
    fn output_prioritizes_folders_before_files() {
        let g = graph_from(&[]);
        let diff = compute(&fs(&["b.rs", "a/", "a/file.rs"]), &g);
        let first_file_idx = diff
            .untracked
            .iter()
            .position(|p| !p.ends_with('/'))
            .unwrap();
        let last_folder_idx = diff
            .untracked
            .iter()
            .rposition(|p| p.ends_with('/'))
            .unwrap();
        assert!(last_folder_idx < first_file_idx);
    }

    #[test]
    fn output_keeps_children_grouped_before_root_files() {
        let g = graph_from(&[]);
        let diff = compute(&fs(&["src/a/file.rs", "root.rs", "src/"]), &g);
        let idx_src = diff.untracked.iter().position(|p| p == "src/").unwrap();
        let idx_child = diff
            .untracked
            .iter()
            .position(|p| p == "src/a/file.rs")
            .unwrap();
        let idx_root = diff.untracked.iter().position(|p| p == "root.rs").unwrap();
        assert!(idx_src < idx_child);
        assert!(idx_child < idx_root);
    }
}
