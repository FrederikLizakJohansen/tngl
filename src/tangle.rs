//! Paths and common operations for the `tangle/` directory.

use std::path::{Path, PathBuf};

use anyhow::{Result, bail};

/// Walk upward from `start` to find the directory containing `tangle/graph.tngl`.
pub fn find_root_from(start: &Path) -> Result<PathBuf> {
    let mut dir = start;
    loop {
        if dir.join("tangle").join("graph.tngl").exists() {
            return Ok(dir.to_path_buf());
        }
        match dir.parent() {
            Some(parent) => dir = parent,
            None => bail!("no tngl graph found — run `tngl init` to initialise this directory"),
        }
    }
}

/// Walk upward from the current working directory to find the repo root.
pub fn find_root() -> Result<PathBuf> {
    let cwd = std::env::current_dir()?;
    find_root_from(&cwd)
}

pub fn tangle_dir(root: &Path) -> PathBuf {
    root.join("tangle")
}

pub fn graph_path(root: &Path) -> PathBuf {
    root.join("tangle").join("graph.tngl")
}

pub fn config_path(root: &Path) -> PathBuf {
    root.join("tangle").join("config.tngl")
}

pub fn layout_path(root: &Path) -> PathBuf {
    root.join("tangle").join("layout.tngl")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn find_root_from_direct() {
        let dir = TempDir::new().unwrap();
        fs::create_dir_all(dir.path().join("tangle")).unwrap();
        fs::write(dir.path().join("tangle/graph.tngl"), "").unwrap();
        let root = find_root_from(dir.path()).unwrap();
        assert_eq!(root, dir.path());
    }

    #[test]
    fn find_root_from_subdir() {
        let dir = TempDir::new().unwrap();
        fs::create_dir_all(dir.path().join("tangle")).unwrap();
        fs::write(dir.path().join("tangle/graph.tngl"), "").unwrap();
        fs::create_dir_all(dir.path().join("src/deep")).unwrap();
        let root = find_root_from(&dir.path().join("src/deep")).unwrap();
        assert_eq!(root, dir.path());
    }

    #[test]
    fn find_root_fails_without_init() {
        let dir = TempDir::new().unwrap();
        assert!(find_root_from(dir.path()).is_err());
    }
}
