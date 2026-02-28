# tngl Specification

`tngl` is a repo-native graph for code relationships.

This spec describes current behavior in this repository. It intentionally omits roadmap content so it stays aligned with the implementation.

## Core Model

- Node: a file or folder path in the repository.
- Edge: a human-authored relationship between nodes.
  - `->` directed edge
  - `<-` incoming edge (stored on the current node)
  - `--` undirected edge
- Graph file: `tangle/graph.tngl`.
- Config file: `tangle/config.tngl`.

## graph.tngl Format

The format is line-oriented and hand-editable.

- A node is a path on a non-indented line.
- Folder nodes end with `/`.
- Edges are indented under a node.
- Edge syntax: `<arrow> <target> : <label>`.
  - The colon is required; label may be empty.
- `# ...` are comments.
- Blank lines are allowed.

Example:

```tngl
src/main.rs
    -> src/tui/canvas.rs : starts app state
    -- README.md : described in

src/tui/canvas.rs

README.md
```

## Tag Markers

Tag markers are bracketed tags on a line above a node.

- `[orphan]`: node is intentionally orphaned.
- `[bundle]`: folder is treated as a collapsed subtree in the TUI.
- `[orphan bundle]`: folder is intentionally orphaned and collapsed.

Markers apply to the next node block and are normalized during update/lint passes.

## Repository Files

`tngl init` creates:

- `tangle/graph.tngl`
- `tangle/config.tngl`
- `.tnglignore` (if missing)
- `./tngl` wrapper script (if missing)

## CLI Surface

Current commands:

- `tngl init`
- `tngl update [--silent] [--mark-new-as-orphans]`
- `tngl status`
- `tngl inspect --orphans|--dangling|--edges <node>|--unreachable|--comment-mismatches|--reconcile-comments`
- `tngl mark-orphans`
- `tngl list`
- `tngl edit`
- `tngl view [--demo]`
- `tngl setup`
- `tngl open` (currently returns a not-implemented error)

## update Semantics

`update` scans the working tree, respects ignore rules, and reconciles `graph.tngl`.

- New files/folders are added as nodes.
- Deleted nodes are handled via `on_delete`:
  - `prompt`
  - `delete`
  - `preserve` (keeps a `# [missing] ...` marker)
- Optional orphan tagging of new nodes is controlled by config or CLI flag.
- Marker/format linting is applied during reconciliation (floating/invalid tags, indentation, spacing).

## status Semantics

`status` is read-only and reports graph drift and quality checks:

- untracked filesystem nodes
- missing graph nodes
- dangling edges
- orphan counts
- optional warnings for blank edge comments

## TUI Semantics (view/setup)

The TUI is the primary interactive editor.

- Tree + details layout for navigation and editing.
- Edge operations include create, delete, and reroute flows.
- Folder collapse and bundle controls are supported.
- Setup panel edits selected config values and writes changes immediately.

Refer to in-app help (`?`) for the active key map.

## Config Keys

Supported keys in `tangle/config.tngl`:

- `on_delete: prompt|delete|preserve`
- `show_orphans: true|false`
- `auto_reveal_links: true|false`
- `git_hooks: true|false`
- `editor: <command>`
- `warn_uncommented_edges: true|false`
- `mark_new_as_orphans: true|false`

Unknown keys are ignored by the parser for forward compatibility.

## Current Source Layout

```text
src/
  main.rs
  commands/
  graph/
  parser/
  scanner/
  tui/
tangle/
  graph.tngl
  config.tngl
```
