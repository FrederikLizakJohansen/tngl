//! `tngl init` — initialise tngl in the current directory.

use std::fs;
use std::io::{self, Write};
use std::path::Path;

use anyhow::{Result, bail};
use crossterm::style::Stylize;

use crate::graph::model::{Graph, Node};
use crate::parser::{config, graph};
use crate::scanner::tree;
use crate::tangle;

// ---------------------------------------------------------------------------
// Public entry points
// ---------------------------------------------------------------------------

/// Entry point called from `main`.
pub fn run() -> Result<()> {
    let root = std::env::current_dir()?;
    run_in(&root, None)
}

/// Run init inside `root`.
///
/// `accept_hooks`:
/// - `None`        → prompt the user interactively
/// - `Some(true)`  → install git hooks without prompting
/// - `Some(false)` → skip git hooks without prompting
pub fn run_in(root: &Path, accept_hooks: Option<bool>) -> Result<()> {
    let tangle_dir = tangle::tangle_dir(root);

    if tangle_dir.join("graph.tngl").exists() {
        bail!("tngl is already initialised (tangle/graph.tngl exists). Run `tngl update` instead.");
    }

    fs::create_dir_all(&tangle_dir)?;

    // Scan
    println!(
        "  {} {}",
        "Scanning".cyan().bold(),
        "repository tree...".dark_grey()
    );
    let paths = tree::scan(root)?;
    let file_count = paths.iter().filter(|p| !p.ends_with('/')).count();
    let dir_count = paths.iter().filter(|p| p.ends_with('/')).count();
    println!(
        "  {} {} files and {} folders",
        "Found".green().bold(),
        file_count.to_string().green(),
        dir_count.to_string().green()
    );

    // graph.tngl
    let mut g = Graph::new();
    for path in &paths {
        g.add_node(Node::new(path.as_str()));
    }
    let graph_contents = graph::serialize(&graph::from_graph(&g));
    fs::write(tangle_dir.join("graph.tngl"), &graph_contents)?;
    println!("  {} tangle/graph.tngl", "Created".green().bold());

    // config.tngl
    fs::write(tangle_dir.join("config.tngl"), config::DEFAULT_CONTENTS)?;
    println!("  {} tangle/config.tngl", "Created".green().bold());

    // .tnglignore
    let tnglignore_path = root.join(".tnglignore");
    if !tnglignore_path.exists() {
        fs::write(&tnglignore_path, TNGLIGNORE_DEFAULTS)?;
        println!("  {} .tnglignore", "Created".green().bold());
    }

    // wrapper script
    let wrapper_path = root.join("tngl");
    if !wrapper_path.exists() {
        fs::write(&wrapper_path, WRAPPER_SCRIPT)?;
        set_executable(&wrapper_path)?;
        println!("  {} tngl (wrapper script)", "Created".green().bold());
    }

    // Git hooks
    let install = match accept_hooks {
        Some(v) => v,
        None => prompt_yes_no("  Install git hooks? [y/n] ")?,
    };
    if install {
        install_git_hooks(root)?;
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Git hooks
// ---------------------------------------------------------------------------

fn install_git_hooks(root: &Path) -> Result<()> {
    let hooks_dir = root.join(".git/hooks");
    if !hooks_dir.exists() {
        bail!(".git/hooks/ not found — is this a git repository?");
    }
    for hook in &["post-merge", "post-checkout"] {
        let path = hooks_dir.join(hook);
        fs::write(&path, GIT_HOOK_CONTENT)?;
        set_executable(&path)?;
    }
    println!(
        "  {} git hooks (post-merge, post-checkout)",
        "Installed".green().bold()
    );
    Ok(())
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn prompt_yes_no(prompt: &str) -> Result<bool> {
    print!("{}", prompt);
    io::stdout().flush()?;
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;
    Ok(matches!(
        input.trim().to_ascii_lowercase().as_str(),
        "y" | "yes"
    ))
}

#[cfg(unix)]
fn set_executable(path: &Path) -> Result<()> {
    use std::os::unix::fs::PermissionsExt;
    let mut perms = fs::metadata(path)?.permissions();
    perms.set_mode(0o755);
    fs::set_permissions(path, perms)?;
    Ok(())
}

#[cfg(not(unix))]
fn set_executable(_path: &Path) -> Result<()> {
    Ok(())
}

// ---------------------------------------------------------------------------
// Static file contents
// ---------------------------------------------------------------------------

const TNGLIGNORE_DEFAULTS: &str = "\
# .tnglignore
node_modules/
dist/
*.log
coverage/
";

const WRAPPER_SCRIPT: &str = "\
#!/bin/sh
# tngl wrapper — https://github.com/yourname/tngl
SCRIPT_DIR=\"$(CDPATH= cd -- \"$(dirname -- \"$0\")\" && pwd)\"
SELF=\"$SCRIPT_DIR/tngl\"
TNGL_BIN=\"$(command -v tngl 2>/dev/null || true)\"
if [ -n \"$TNGL_BIN\" ] && [ \"$TNGL_BIN\" != \"$SELF\" ] && [ \"$TNGL_BIN\" != \"$0\" ]; then
  exec \"$TNGL_BIN\" \"$@\"
elif [ -f \"$SCRIPT_DIR/Cargo.toml\" ] && command -v cargo >/dev/null 2>&1 && grep -q 'name = \"tngl\"' \"$SCRIPT_DIR/Cargo.toml\"; then
  exec cargo run --manifest-path \"$SCRIPT_DIR/Cargo.toml\" --quiet -- \"$@\"
else
  echo \"tngl is not installed.\"
  echo \"Install it with: cargo install tngl\"
  echo \"Or visit: https://github.com/yourname/tngl\"
  exit 1
fi
";

const GIT_HOOK_CONTENT: &str = "#!/bin/sh\ntngl update --silent\n";

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn setup(files: &[&str]) -> TempDir {
        let dir = TempDir::new().unwrap();
        for f in files {
            let p = dir.path().join(f);
            if let Some(parent) = p.parent() {
                fs::create_dir_all(parent).unwrap();
            }
            fs::write(p, "").unwrap();
        }
        dir
    }

    #[test]
    fn creates_tangle_directory() {
        let dir = setup(&["src/main.rs"]);
        run_in(dir.path(), Some(false)).unwrap();
        assert!(dir.path().join("tangle").is_dir());
    }

    #[test]
    fn creates_graph_tngl() {
        let dir = setup(&["src/main.rs", "README.md"]);
        run_in(dir.path(), Some(false)).unwrap();
        let content = fs::read_to_string(dir.path().join("tangle/graph.tngl")).unwrap();
        assert!(content.contains("src/main.rs"));
        assert!(content.contains("README.md"));
    }

    #[test]
    fn graph_tngl_all_nodes_are_orphans() {
        let dir = setup(&["a.rs", "b.rs"]);
        run_in(dir.path(), Some(false)).unwrap();
        let content = fs::read_to_string(dir.path().join("tangle/graph.tngl")).unwrap();
        // No edge lines (no -> or -- after init)
        assert!(!content.contains("->"));
        assert!(!content.contains("--"));
    }

    #[test]
    fn creates_config_tngl() {
        let dir = setup(&[]);
        run_in(dir.path(), Some(false)).unwrap();
        let content = fs::read_to_string(dir.path().join("tangle/config.tngl")).unwrap();
        assert!(content.contains("on_delete: prompt"));
    }

    #[test]
    fn creates_tnglignore() {
        let dir = setup(&[]);
        run_in(dir.path(), Some(false)).unwrap();
        assert!(dir.path().join(".tnglignore").exists());
        let content = fs::read_to_string(dir.path().join(".tnglignore")).unwrap();
        assert!(content.contains("node_modules/"));
    }

    #[test]
    fn does_not_overwrite_existing_tnglignore() {
        let dir = setup(&[]);
        fs::write(dir.path().join(".tnglignore"), "my_custom_rule/\n").unwrap();
        run_in(dir.path(), Some(false)).unwrap();
        let content = fs::read_to_string(dir.path().join(".tnglignore")).unwrap();
        // Custom content preserved, defaults NOT added
        assert!(content.contains("my_custom_rule/"));
        assert!(!content.contains("node_modules/"));
    }

    #[test]
    fn creates_wrapper_script() {
        let dir = setup(&[]);
        run_in(dir.path(), Some(false)).unwrap();
        assert!(dir.path().join("tngl").exists());
        let content = fs::read_to_string(dir.path().join("tngl")).unwrap();
        assert!(content.contains("#!/bin/sh"));
        assert!(content.contains("SCRIPT_DIR="));
        assert!(content.contains("SELF=\"$SCRIPT_DIR/tngl\""));
        assert!(content.contains("TNGL_BIN=\"$(command -v tngl 2>/dev/null || true)\""));
        assert!(content.contains("[ \"$TNGL_BIN\" != \"$SELF\" ]"));
        assert!(
            content.contains("cargo run --manifest-path \"$SCRIPT_DIR/Cargo.toml\" --quiet --")
        );
        assert!(content.contains("cargo install tngl"));
    }

    #[test]
    fn does_not_overwrite_existing_wrapper() {
        let dir = setup(&[]);
        fs::write(dir.path().join("tngl"), "custom content").unwrap();
        run_in(dir.path(), Some(false)).unwrap();
        let content = fs::read_to_string(dir.path().join("tngl")).unwrap();
        assert_eq!(content, "custom content");
    }

    #[test]
    fn error_if_already_initialised() {
        let dir = setup(&[]);
        run_in(dir.path(), Some(false)).unwrap();
        assert!(run_in(dir.path(), Some(false)).is_err());
    }

    #[test]
    fn installs_git_hooks_when_accepted() {
        let dir = setup(&[]);
        fs::create_dir_all(dir.path().join(".git/hooks")).unwrap();
        run_in(dir.path(), Some(true)).unwrap();
        assert!(dir.path().join(".git/hooks/post-merge").exists());
        assert!(dir.path().join(".git/hooks/post-checkout").exists());
    }

    #[test]
    fn skips_git_hooks_when_declined() {
        let dir = setup(&[]);
        run_in(dir.path(), Some(false)).unwrap();
        assert!(!dir.path().join(".git/hooks/post-merge").exists());
    }

    #[test]
    fn tangle_dir_excluded_from_graph() {
        let dir = setup(&["src/main.rs"]);
        // Pre-create tangle/ with a file — scanner must skip it
        fs::create_dir_all(dir.path().join("tangle")).unwrap();
        fs::write(dir.path().join("tangle/old.tngl"), "").unwrap();
        // Should fail because graph.tngl already exists (we need to re-run after removing)
        // Actually: old.tngl exists but graph.tngl does not, so init should proceed.
        // But the tangle dir exists. Our check is for graph.tngl specifically.
        run_in(dir.path(), Some(false)).unwrap();
        let content = fs::read_to_string(dir.path().join("tangle/graph.tngl")).unwrap();
        assert!(!content.contains("old.tngl"));
        assert!(!content.contains("graph.tngl"));
    }
}
