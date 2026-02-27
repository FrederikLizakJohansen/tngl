use std::collections::HashSet;

/// Whether an edge is directed (`->`, `<-`) or undirected (`--`).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EdgeKind {
    /// `source -> target`
    Directed,
    /// `source <- target`
    Incoming,
    /// `source -- target`
    Undirected,
}

/// A single relationship between two nodes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Edge {
    /// The target node path.
    pub target: String,
    pub kind: EdgeKind,
    /// The human-authored label (empty string = no label, but colon was present).
    pub label: String,
}

/// A node in the graph — a file or folder path.
///
/// Folder paths end with `/`. Nodes may have zero edges (orphans).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    /// Path relative to repo root. Folders end with `/`.
    pub path: String,
    pub edges: Vec<Edge>,
}

impl Node {
    pub fn new(path: impl Into<String>) -> Self {
        Self {
            path: path.into(),
            edges: Vec::new(),
        }
    }

    /// True when this node has no outgoing edges.
    ///
    /// Note: global orphan reporting (`Graph::orphans`) also checks for
    /// incoming edges, so a pure target node is not reported as an orphan.
    pub fn is_orphan(&self) -> bool {
        self.edges.is_empty()
    }

    #[allow(dead_code)] // used by TUI (not yet implemented)
    pub fn is_folder(&self) -> bool {
        self.path.ends_with('/')
    }
}

/// The full graph: an ordered collection of nodes.
///
/// Order is preserved exactly as it appears in `graph.tngl` for round-trip safety.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Graph {
    pub nodes: Vec<Node>,
}

impl Graph {
    pub fn new() -> Self {
        Self::default()
    }

    /// Find a node by path, returning a reference.
    pub fn get(&self, path: &str) -> Option<&Node> {
        self.nodes.iter().find(|n| n.path == path)
    }

    /// Find a node by path, returning a mutable reference.
    pub fn get_mut(&mut self, path: &str) -> Option<&mut Node> {
        self.nodes.iter_mut().find(|n| n.path == path)
    }

    /// Check whether a path is present in the graph.
    pub fn contains(&self, path: &str) -> bool {
        self.nodes.iter().any(|n| n.path == path)
    }

    /// Add a node. Panics if the path is already present.
    pub fn add_node(&mut self, node: Node) {
        assert!(!self.contains(&node.path), "duplicate node: {}", node.path);
        self.nodes.push(node);
    }

    /// All isolated nodes (no incoming and no outgoing edges).
    pub fn orphans(&self) -> impl Iterator<Item = &Node> {
        let targeted: HashSet<&str> = self
            .nodes
            .iter()
            .flat_map(|n| n.edges.iter().map(|e| e.target.as_str()))
            .collect();

        self.nodes
            .iter()
            .filter(move |n| n.edges.is_empty() && !targeted.contains(n.path.as_str()))
    }

    /// All edges pointing to a path that is not in the graph.
    pub fn dangling_edges(&self) -> Vec<(&Node, &Edge)> {
        self.nodes
            .iter()
            .flat_map(|n| n.edges.iter().map(move |e| (n, e)))
            .filter(|(_, e)| !self.contains(&e.target))
            .collect()
    }
}
