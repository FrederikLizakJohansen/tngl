//! `tngl mark-orphans` — mark unattended orphans as intentional.

use std::fs;
use std::path::Path;

use anyhow::Result;
use crossterm::style::Stylize;

use crate::parser::graph;
use crate::tangle;

pub fn run() -> Result<()> {
    let root = tangle::find_root()?;
    run_in(&root)
}

pub fn run_in(root: &Path) -> Result<()> {
    let graph_path = tangle::graph_path(root);
    let content = fs::read_to_string(&graph_path)?;
    let mut doc = graph::parse(&content)?;
    let g = graph::to_graph(&doc)?;

    let intentional = graph::intentional_orphans(&doc);
    let unattended: Vec<String> = g
        .orphans()
        .filter(|n| !intentional.contains(&n.path))
        .map(|n| n.path.clone())
        .collect();

    if unattended.is_empty() {
        println!("  {}", "No unattended orphans found.".dark_grey());
        return Ok(());
    }

    let mut changed = 0usize;
    for path in &unattended {
        if graph::mark_orphan(&mut doc, path) {
            changed += 1;
        }
    }

    if changed > 0 {
        fs::write(&graph_path, graph::serialize(&doc))?;
    }

    println!(
        "  {} {} {}",
        "Marked".green().bold(),
        changed.to_string().green().bold(),
        format!(
            "orphan{} as intentional.",
            if changed == 1 { "" } else { "s" }
        )
        .green()
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs as sfs;
    use tempfile::TempDir;

    fn init_repo(graph_contents: &str) -> TempDir {
        let dir = TempDir::new().unwrap();
        sfs::create_dir_all(dir.path().join("tangle")).unwrap();
        sfs::write(dir.path().join("tangle/graph.tngl"), graph_contents).unwrap();
        dir
    }

    fn graph_content(dir: &TempDir) -> String {
        sfs::read_to_string(dir.path().join("tangle/graph.tngl")).unwrap()
    }

    #[test]
    fn marks_unattended_orphans_only() {
        let dir = init_repo(
            "\
a.rs
    -> b.rs : uses

b.rs

c.rs
",
        );

        run_in(dir.path()).unwrap();
        let content = graph_content(&dir);
        // b.rs is targeted, so not an orphan under isolated definition.
        assert!(!content.contains("\n[orphan]\nb.rs\n"));
        // c.rs is isolated and should be marked.
        assert!(content.contains("\n[orphan]\nc.rs\n") || content.starts_with("[orphan]\nc.rs\n"));
    }

    #[test]
    fn does_not_duplicate_existing_markers() {
        let dir = init_repo(
            "\
[orphan]
a.rs
",
        );
        run_in(dir.path()).unwrap();
        let content = graph_content(&dir);
        assert_eq!(content.matches("[orphan]").count(), 1);
    }
}
