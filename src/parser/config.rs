//! Parser for `tangle/config.tngl`.
//!
//! The format is line-oriented key-value pairs (`key: value`) with `#` comments.

use anyhow::{Result, bail};

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum OnDelete {
    #[default]
    Prompt,
    Delete,
    Preserve,
}

#[derive(Debug, Clone)]
pub struct Config {
    pub on_delete: OnDelete,
    pub show_orphans: bool,
    pub git_hooks: bool,
    pub editor: Option<String>,
    pub warn_uncommented_edges: bool,
    pub mark_new_as_orphans: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            on_delete: OnDelete::default(),
            show_orphans: true,
            git_hooks: true,
            editor: None,
            warn_uncommented_edges: true,
            mark_new_as_orphans: false,
        }
    }
}

// ---------------------------------------------------------------------------
// Default file content
// ---------------------------------------------------------------------------

pub const DEFAULT_CONTENTS: &str = "\
# tngl configuration
# Edit manually or run: tngl setup

# What to do with edges when a file is deleted
# Options: prompt | delete | preserve
on_delete: prompt

# Whether to show orphan nodes in the default TUI view
show_orphans: true

# Auto-update graph.tngl on git operations (requires hook install)
git_hooks: true

# Preferred editor for `tngl edit` (falls back to $VISUAL, then $EDITOR)
# editor: nvim

# Warn in `tngl status` when an edge has an empty label/comment
warn_uncommented_edges: true

# On `tngl update`, tag new files as [orphan] and new folders as [orphan bundle]
mark_new_as_orphans: false
";

// ---------------------------------------------------------------------------
// Parsing
// ---------------------------------------------------------------------------

/// Parse `config.tngl` text. Unknown keys are silently ignored for forward
/// compatibility. Returns `Config::default()` for any missing keys.
pub fn parse(input: &str) -> Result<Config> {
    let mut config = Config::default();

    for (line_num, line) in input.lines().enumerate() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }

        let (key, value) = trimmed.split_once(':').ok_or_else(|| {
            anyhow::anyhow!("config line {}: expected 'key: value'", line_num + 1)
        })?;
        let key = key.trim();
        let value = value.trim();

        match key {
            "on_delete" => {
                config.on_delete = match value {
                    "prompt" => OnDelete::Prompt,
                    "delete" => OnDelete::Delete,
                    "preserve" => OnDelete::Preserve,
                    other => bail!(
                        "config line {}: unknown on_delete value {:?}",
                        line_num + 1,
                        other
                    ),
                };
            }
            "show_orphans" => {
                config.show_orphans = parse_bool(value, line_num + 1)?;
            }
            "git_hooks" => {
                config.git_hooks = parse_bool(value, line_num + 1)?;
            }
            "editor" => {
                config.editor = parse_optional_string(value);
            }
            "warn_uncommented_edges" => {
                config.warn_uncommented_edges = parse_bool(value, line_num + 1)?;
            }
            "mark_new_as_orphans" => {
                config.mark_new_as_orphans = parse_bool(value, line_num + 1)?;
            }
            _ => {
                // Unknown keys are silently ignored.
            }
        }
    }

    Ok(config)
}

fn parse_bool(value: &str, line_num: usize) -> Result<bool> {
    match value {
        "true" => Ok(true),
        "false" => Ok(false),
        other => bail!(
            "config line {}: expected 'true' or 'false', got {:?}",
            line_num,
            other
        ),
    }
}

fn parse_optional_string(value: &str) -> Option<String> {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        None
    } else {
        Some(trimmed.to_string())
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn defaults_on_empty_input() {
        let cfg = parse("").unwrap();
        assert_eq!(cfg.on_delete, OnDelete::Prompt);
        assert!(cfg.show_orphans);
        assert!(cfg.git_hooks);
        assert_eq!(cfg.editor, None);
        assert!(cfg.warn_uncommented_edges);
        assert!(!cfg.mark_new_as_orphans);
    }

    #[test]
    fn parses_default_contents() {
        let cfg = parse(DEFAULT_CONTENTS).unwrap();
        assert_eq!(cfg.on_delete, OnDelete::Prompt);
        assert!(cfg.show_orphans);
        assert!(cfg.git_hooks);
        assert_eq!(cfg.editor, None);
        assert!(cfg.warn_uncommented_edges);
        assert!(!cfg.mark_new_as_orphans);
    }

    #[test]
    fn parses_all_on_delete_variants() {
        let cfg = parse("on_delete: delete\n").unwrap();
        assert_eq!(cfg.on_delete, OnDelete::Delete);

        let cfg = parse("on_delete: preserve\n").unwrap();
        assert_eq!(cfg.on_delete, OnDelete::Preserve);
    }

    #[test]
    fn parses_false_values() {
        let cfg = parse("show_orphans: false\ngit_hooks: false\n").unwrap();
        assert!(!cfg.show_orphans);
        assert!(!cfg.git_hooks);
    }

    #[test]
    fn parses_warn_uncommented_edges_false() {
        let cfg = parse("warn_uncommented_edges: false\n").unwrap();
        assert!(!cfg.warn_uncommented_edges);
    }

    #[test]
    fn parses_mark_new_as_orphans_true() {
        let cfg = parse("mark_new_as_orphans: true\n").unwrap();
        assert!(cfg.mark_new_as_orphans);
    }

    #[test]
    fn parses_editor() {
        let cfg = parse("editor: nvim\n").unwrap();
        assert_eq!(cfg.editor, Some("nvim".to_string()));
    }

    #[test]
    fn parses_empty_editor_as_none() {
        let cfg = parse("editor:   \n").unwrap();
        assert_eq!(cfg.editor, None);
    }

    #[test]
    fn ignores_comments_and_blanks() {
        let cfg = parse("# comment\n\non_delete: delete\n# another\n").unwrap();
        assert_eq!(cfg.on_delete, OnDelete::Delete);
    }

    #[test]
    fn ignores_unknown_keys() {
        let cfg = parse("on_delete: delete\nunknown_key: foo\n").unwrap();
        assert_eq!(cfg.on_delete, OnDelete::Delete);
    }

    #[test]
    fn error_on_bad_on_delete() {
        assert!(parse("on_delete: maybe\n").is_err());
    }

    #[test]
    fn error_on_bad_bool() {
        assert!(parse("show_orphans: yes\n").is_err());
    }

    #[test]
    fn error_on_malformed_line() {
        assert!(parse("no_colon_here\n").is_err());
    }
}
