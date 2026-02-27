//! `tngl list` — print all edges in graph order.

use anyhow::Result;

use crate::graph::model::{EdgeKind, Graph};
use crate::parser::graph;
use crate::tangle;

pub fn run() -> Result<()> {
    let root = tangle::find_root()?;
    let content = std::fs::read_to_string(tangle::graph_path(&root))?;
    let doc = graph::parse(&content)?;
    let g = graph::to_graph(&doc)?;

    let lines = list_edges(&g);
    if lines.is_empty() {
        println!("  No edges.");
    } else {
        for line in lines {
            println!("  {}", line);
        }
    }
    Ok(())
}

fn list_edges(g: &Graph) -> Vec<String> {
    let mut lines = Vec::new();
    for node in &g.nodes {
        for edge in &node.edges {
            let arrow = match edge.kind {
                EdgeKind::Directed => "->",
                EdgeKind::Incoming => "<-",
                EdgeKind::Undirected => "--",
            };
            if edge.label.is_empty() {
                lines.push(format!("{} {} {} :", node.path, arrow, edge.target));
            } else {
                lines.push(format!(
                    "{} {} {} : {}",
                    node.path, arrow, edge.target, edge.label
                ));
            }
        }
    }
    lines
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::model::{Edge, Node};

    #[test]
    fn list_edges_preserves_graph_order() {
        let mut g = Graph::new();
        let mut a = Node::new("a.rs");
        a.edges.push(Edge {
            target: "b.rs".into(),
            kind: EdgeKind::Directed,
            label: "uses".into(),
        });
        a.edges.push(Edge {
            target: "c.rs".into(),
            kind: EdgeKind::Undirected,
            label: String::new(),
        });
        a.edges.push(Edge {
            target: "d.rs".into(),
            kind: EdgeKind::Incoming,
            label: String::new(),
        });
        let b = Node::new("b.rs");
        g.add_node(a);
        g.add_node(b);

        let lines = list_edges(&g);
        assert_eq!(
            lines,
            vec![
                "a.rs -> b.rs : uses".to_string(),
                "a.rs -- c.rs :".to_string(),
                "a.rs <- d.rs :".to_string(),
            ]
        );
    }

    #[test]
    fn list_edges_empty_when_no_edges() {
        let mut g = Graph::new();
        g.add_node(Node::new("a.rs"));
        assert!(list_edges(&g).is_empty());
    }
}
