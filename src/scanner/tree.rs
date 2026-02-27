//! Filesystem scanner with `.gitignore` and `.tnglignore` support.
//!
//! Uses the `ignore` crate (same engine as ripgrep) for correct gitignore semantics.
//! Always skips internal tngl files. Returns paths relative to the root, with
//! folders ending in `/`.

use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use ignore::WalkBuilder;

/// Scan the repository tree rooted at `root`.
///
/// Returns all non-ignored files and directories as paths relative to `root`.
/// Directory paths end with `/`. Internal tngl paths are excluded.
///
/// Respects (in priority order):
/// 1. `.git/` — always ignored
/// 2. `tangle/` — always ignored
/// 3. `.tnglignore` — always ignored as a graph node
/// 4. `tngl` wrapper script — always ignored as a graph node
/// 5. `.gitignore` — if present at `root`
/// 6. `.tnglignore` patterns — if present at `root`
pub fn scan(root: &Path) -> Result<Vec<String>> {
    let mut builder = WalkBuilder::new(root);
    builder
        .hidden(false) // include dotfiles
        .git_ignore(true)
        .git_global(false)
        .git_exclude(false)
        .ignore(false) // don't pick up .ignore files automatically
        .require_git(false)
        .add_custom_ignore_filename(".tnglignore");

    let mut entries: Vec<String> = Vec::new();

    for result in builder.build() {
        let entry = result.with_context(|| "error walking file tree")?;
        let path = entry.path();

        // Skip the root itself.
        if path == root {
            continue;
        }

        let rel = relative_to(root, path)?;

        // Always skip internal control paths.
        if is_excluded(&rel) {
            continue;
        }

        let node_path = if path.is_dir() {
            format!("{}/", rel)
        } else {
            rel
        };

        entries.push(node_path);
    }

    // Sort for deterministic, hierarchical output:
    // folders appear before files at each level and descendants stay grouped.
    entries.sort_by(|a, b| compare_hierarchical(a, b));
    Ok(entries)
}

/// Return the path of `target` relative to `base`, as a forward-slash string.
fn relative_to(base: &Path, target: &Path) -> Result<String> {
    let rel: PathBuf = target
        .strip_prefix(base)
        .with_context(|| format!("{:?} is not under {:?}", target, base))?
        .into();

    // Always use forward slashes, even on Windows.
    let s = rel
        .components()
        .map(|c| c.as_os_str().to_string_lossy().into_owned())
        .collect::<Vec<_>>()
        .join("/");

    Ok(s)
}

/// True if this path should be unconditionally excluded from scanning.
fn is_excluded(rel: &str) -> bool {
    let first_component = rel.split('/').next().unwrap_or("");
    matches!(first_component, ".git" | "tangle" | ".tnglignore" | "tngl")
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
            return a_kind.cmp(&b_kind); // directory before file
        }
        return a_comps[i].cmp(b_comps[i]);
    }

    match a_comps.len().cmp(&b_comps.len()) {
        Ordering::Equal => a_is_dir.cmp(&b_is_dir).reverse(), // dir before file
        other => other,                                       // ancestor before descendant
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
    // 0 = directory component, 1 = file component
    if idx < len.saturating_sub(1) {
        0
    } else if is_dir {
        0
    } else {
        1
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    fn make_tree(root: &Path, files: &[&str]) {
        for f in files {
            let p = root.join(f);
            if let Some(parent) = p.parent() {
                fs::create_dir_all(parent).unwrap();
            }
            if f.ends_with('/') {
                fs::create_dir_all(&p).unwrap();
            } else {
                fs::write(&p, "").unwrap();
            }
        }
    }

    fn tmpdir() -> tempfile::TempDir {
        tempfile::TempDir::new().unwrap()
    }

    // tempfile is a dev-dependency; if it isn't present these tests will simply
    // not compile. We add it below. The tests use a throwaway temp directory.

    #[test]
    fn basic_scan() {
        let dir = tmpdir();
        make_tree(dir.path(), &["src/main.rs", "README.md"]);
        let entries = scan(dir.path()).unwrap();
        assert!(entries.contains(&"README.md".to_string()));
        assert!(entries.contains(&"src/main.rs".to_string()));
        assert!(entries.contains(&"src/".to_string()));
    }

    #[test]
    fn folders_end_with_slash() {
        let dir = tmpdir();
        make_tree(dir.path(), &["src/main.rs"]);
        let entries = scan(dir.path()).unwrap();
        assert!(entries.iter().any(|e| e == "src/"));
    }

    #[test]
    fn git_dir_excluded() {
        let dir = tmpdir();
        make_tree(dir.path(), &[".git/HEAD", "src/main.rs"]);
        let entries = scan(dir.path()).unwrap();
        assert!(!entries.iter().any(|e| e.starts_with(".git")));
    }

    #[test]
    fn tangle_dir_excluded() {
        let dir = tmpdir();
        make_tree(dir.path(), &["tangle/graph.tngl", "src/main.rs"]);
        let entries = scan(dir.path()).unwrap();
        assert!(!entries.iter().any(|e| e.starts_with("tangle")));
    }

    #[test]
    fn gitignore_respected() {
        let dir = tmpdir();
        make_tree(dir.path(), &["src/main.rs", "dist/bundle.js"]);
        fs::write(dir.path().join(".gitignore"), "dist/\n").unwrap();
        let entries = scan(dir.path()).unwrap();
        assert!(!entries.iter().any(|e| e.starts_with("dist")));
        assert!(entries.contains(&"src/main.rs".to_string()));
    }

    #[test]
    fn tnglignore_respected() {
        let dir = tmpdir();
        make_tree(dir.path(), &["src/main.rs", "coverage/lcov.info"]);
        fs::write(dir.path().join(".tnglignore"), "coverage/\n").unwrap();
        let entries = scan(dir.path()).unwrap();
        assert!(!entries.iter().any(|e| e.starts_with("coverage")));
        assert!(entries.contains(&"src/main.rs".to_string()));
    }

    #[test]
    fn tnglignore_glob_pattern() {
        let dir = tmpdir();
        make_tree(dir.path(), &["build.log", "src/main.rs"]);
        fs::write(dir.path().join(".tnglignore"), "*.log\n").unwrap();
        let entries = scan(dir.path()).unwrap();
        assert!(!entries.iter().any(|e| e.ends_with(".log")));
    }

    #[test]
    fn dotfiles_included_by_default() {
        let dir = tmpdir();
        make_tree(dir.path(), &[".env", "src/main.rs"]);
        let entries = scan(dir.path()).unwrap();
        assert!(entries.contains(&".env".to_string()));
    }

    #[test]
    fn internal_tngl_files_excluded() {
        let dir = tmpdir();
        make_tree(dir.path(), &[".tnglignore", "tngl", "src/main.rs"]);
        let entries = scan(dir.path()).unwrap();
        assert!(!entries.contains(&".tnglignore".to_string()));
        assert!(!entries.contains(&"tngl".to_string()));
        assert!(entries.contains(&"src/main.rs".to_string()));
    }

    #[test]
    fn sorted_output() {
        let dir = tmpdir();
        make_tree(dir.path(), &["z.rs", "a.rs", "m.rs"]);
        let entries = scan(dir.path()).unwrap();
        let mut sorted = entries.clone();
        sorted.sort_by(|a, b| compare_hierarchical(a, b));
        assert_eq!(entries, sorted);
    }

    #[test]
    fn folders_prioritized_before_files() {
        let dir = tmpdir();
        make_tree(dir.path(), &["z.rs", "a/", "a/file.rs"]);
        let entries = scan(dir.path()).unwrap();
        let first_file_idx = entries.iter().position(|e| !e.ends_with('/')).unwrap();
        let last_folder_idx = entries.iter().rposition(|e| e.ends_with('/')).unwrap();
        assert!(last_folder_idx < first_file_idx);
    }

    #[test]
    fn children_grouped_with_folder_before_root_files() {
        let dir = tmpdir();
        make_tree(dir.path(), &["src/a/file.rs", "root.rs"]);
        let entries = scan(dir.path()).unwrap();
        let idx_src = entries.iter().position(|e| e == "src/").unwrap();
        let idx_child = entries.iter().position(|e| e == "src/a/file.rs").unwrap();
        let idx_root = entries.iter().position(|e| e == "root.rs").unwrap();
        assert!(idx_src < idx_child);
        assert!(idx_child < idx_root);
    }

    #[test]
    fn empty_tree() {
        let dir = tmpdir();
        let entries = scan(dir.path()).unwrap();
        assert!(entries.is_empty());
    }
}
