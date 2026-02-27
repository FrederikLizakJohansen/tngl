//! `tngl edit` — open `tangle/graph.tngl` in an editor.

use std::process::{Command, Stdio};

use anyhow::{Context, Result, bail};

use crate::parser::config;
use crate::tangle;

pub fn run() -> Result<()> {
    let root = tangle::find_root()?;
    let graph_path = tangle::graph_path(&root);
    let editor = resolve_editor(load_config_editor(&root)?);

    let mut parts = editor.split_whitespace();
    let program = parts
        .next()
        .ok_or_else(|| anyhow::anyhow!("no editor configured for `tngl edit`"))?;
    let args: Vec<String> = parts.map(ToString::to_string).collect();

    let status = Command::new(program)
        .args(&args)
        .arg(&graph_path)
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .status()
        .with_context(|| {
            format!(
                "failed to launch editor {:?} for {}",
                editor,
                graph_path.display()
            )
        })?;

    if !status.success() {
        bail!("editor exited with status {}", status);
    }

    Ok(())
}

fn load_config_editor(root: &std::path::Path) -> Result<Option<String>> {
    let path = tangle::config_path(root);
    if !path.exists() {
        return Ok(None);
    }
    let content = std::fs::read_to_string(path)?;
    let cfg = config::parse(&content)?;
    Ok(cfg.editor)
}

fn resolve_editor(config_editor: Option<String>) -> String {
    if let Some(e) = config_editor.filter(|v| !v.trim().is_empty()) {
        return e;
    }
    if let Some(e) = std::env::var("TNGL_EDITOR")
        .ok()
        .filter(|v| !v.trim().is_empty())
    {
        return e;
    }
    if let Some(e) = std::env::var("VISUAL")
        .ok()
        .filter(|v| !v.trim().is_empty())
    {
        return e;
    }
    if let Some(e) = std::env::var("EDITOR")
        .ok()
        .filter(|v| !v.trim().is_empty())
    {
        return e;
    }
    if cfg!(windows) {
        "notepad".to_string()
    } else {
        "vi".to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn config_editor_takes_priority() {
        let picked = resolve_editor(Some("nvim".to_string()));
        assert_eq!(picked, "nvim");
    }
}
