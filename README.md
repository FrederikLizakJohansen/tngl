
      ████████╗███╗  ██╗ ██████╗ ██╗
         ██╔══╝████╗ ██║██╔════╝ ██║
         ██║   ██╔██╗██║██║  ███╗██║
         ██║   ██║╚████║██║   ██║██║
         ██║   ██║ ╚███║╚██████╔╝███████╗
         ╚═╝   ╚═╝  ╚══╝ ╚═════╝ ╚══════╝

        relational graph for your repository

> Some generative AI was used while building this project to test how fast and reliable it could be for a first pass at a useful software tool. If you think this makes the tool illegit you can [kiss my piss], this MIT licensed so do with it what you want.

`tngl` is a repo-native, human-authored graph for code relationships.

- Nodes are files/folders.
- Edges are tangles between nodes.
- The graph lives in `tangle/graph.tngl` and is designed to be edited by humans.
- An integrated TUI makes it easy and intuitive to keep track of file dependencies.
<img width="1274" height="804" alt="image" src="https://github.com/user-attachments/assets/3f98672a-1f05-4602-87a7-765fe84ac628" />

## Install

From crates.io (recommended):

```bash
cargo install tngl
```

Upgrade to latest published version:

```bash
cargo install tngl --force
```

From source (dev/local):

```bash
git clone git@github.com:FrederikLizakJohansen/tngl.git
cd tngl
cargo install --path .
```

Without global install:

```bash
cargo run -- <command>
```

## Quick Start

Inside the repo you want to map:

```bash
tngl init
tngl view
tngl status
tngl inspect --orphans
```

`init` also creates a local `./tngl` wrapper script in that repo, so collaborators can run `./tngl ...`.

## Core Files

- `tangle/graph.tngl`: graph source of truth (nodes + edges + tags)
- `tangle/config.tngl`: runtime/config behavior
- `.tnglignore`: extra scan ignores for `tngl update`

## CLI Reference

```bash
tngl init
tngl update [--silent]
tngl status
tngl inspect --orphans|--dangling|--edges <node>|--unreachable|--comment-mismatches|--reconcile-comments
tngl mark-orphans
tngl list
tngl edit
tngl view [--demo]
tngl setup
tngl open
```

### What Each Command Does

- `init`: bootstrap `tangle/`, scan filesystem, write graph/config/ignore/wrapper.
- `update`: reconcile graph against filesystem (`on_delete` strategy applies).
- `status`: read-only drift and quality report.
- `inspect`: targeted graph audits.
- `mark-orphans`: marks unattended isolated nodes as intentional in graph tags.
- `list`: print all tangles in graph order.
- `edit`: open `tangle/graph.tngl` in configured editor.
- `view`: interactive TUI (main editing UX).
- `setup`: opens settings panel directly.
- `open`: reserved for static HTML export, currently not implemented.

### When To Run `tngl update`

You still use `update` when the repository structure changed outside TUI editing, for example:

- files/folders were added, moved, or deleted
- you merged/pulled a branch with path changes
- you want reconciliation/lint cleanup applied to `graph.tngl`

If you only edited relationships in `tngl view`, those writes are already persisted directly to `graph.tngl`.

## TUI Guide (Full)

Launch:

```bash
tngl view
```

Demo mode (no repo required, in-memory edits only):

```bash
tngl view --demo
```

Settings only:

```bash
tngl setup
```

### Layout

The UI has three major areas:

- `TREE` (left): filesystem tree with connection markers and folder fold/bundle state.
- `DETAILS` (right): focused node info + connected tangles + action row.
- Bottom status/description bar: selected node summary + context-sensitive key hints.

Mode badges appear in title/status when active:

- `CREATE` (green)
- `DELETE` (red)
- `MOVE` (blue)

### Visual Language

Connection markers:

- `▶` out
- `◀` in
- `◆` undirected/reference

Folder markers in tree:

- `▾` expanded
- `▸` collapsed
- `▣` bundled/locked

Node/path cues:

- Focused node path is highlighted.
- In detail mode, selected edge is highlighted in both tree route and details list.

### Contexts and Flows

#### 1) Tree Context (default)

Main navigation/edit context.

- Move focus: `j/k` or `↑/↓`
- Open details for focused node: `Enter`
- Fold/unfold focused folder: `z`
- Bundle/unbundle focused folder: `b`
- Start create flow from focused node: `c`
- Start edge delete picker: `d`
- Cycle line overlay filter (`off/all/in/out/ref`): `f`
- Toggle orphan visibility: `o`
- Add node: `n`
- Edit node path: `e`
- Delete node (confirm): `D`
- Next node: `Tab`
- Zoom in/out: `+` / `-`
- Help: `?`
- Setup popup: `s`
- Quit: `q`

#### 2) Details Context

Entered with `Enter` from tree.

- Navigate tangle list: `↑/↓`
- `Enter`: no action in details mode
- Delete tangle at cursor: `d`
- Start move flow: `m`
- Start create flow: `c`
- Edit selected edge label: `l`
- Toggle edge kind directed/undirected: `t`
- Reverse directed edge direction: `r`
- Delete focused node (confirm): `D`
- Toggle orphan visibility: `o`
- Cycle line overlay filter: `f`
- Back to tree: `Esc` or `Backspace`

#### 3) CREATE Flow

Start from tree (`c`) or details (`c`).

- Pick target node: `↑/↓`
- Change edge type (`in/out/ref`): `←/→`
- Fold/unfold folders while selecting: `z`
- Confirm target/type and open comment popup: `Enter`
- Cancel CREATE: `Esc` or `Backspace`

Important CREATE guard:

- While creating from a folder/source path, you cannot collapse that source folder or its parents.

#### 4) Comment Popup (during CREATE)

After choosing source/target/type:

- Type label/comment text directly
- Move caret: `←/→`
- Delete character: `Backspace`
- Save edge: `Enter`
- Go back to CREATE selection: `Esc`

#### 5) DELETE Flow

From tree, press `d`.

- Cycle candidate tangles: `j/k` or `↑/↓`
- Confirm deletion prompt: `Enter`
- Cancel delete mode: `Esc` or `Backspace`

#### 6) MOVE Flow (Edge Reroute)

In details mode, hover an edge and press `m`.

- This grabs the opposite endpoint relative to the details anchor node.
- Move focus to the replacement node with `↑/↓`.
- Change tangle type with `←/→`.
- Apply move with `Enter`.
- Cancel with `Esc` or `Backspace`.
- Moving onto the other endpoint is blocked (no self-loop moves).

#### 7) Bundle Flow

From tree on a folder, press `b`.

- Bundling locks/collapses folder (`[bundle]` behavior).
- If child links exist, you get a warning/confirm prompt before pruning those links.
- Unbundle with `b` again.

#### 8) Confirm Prompts

Used for destructive actions (delete edge/node, bundle-prune).

- Confirm: `y` or `Enter`
- Cancel: `n`, `Esc`, or `Backspace`

### TUI Behavior Worth Knowing

- Changes made in TUI are written back to `tangle/graph.tngl`.
- On exit, graph formatting/normalization is also applied.
- If `auto_reveal_links` is enabled, collapsed folders that hide currently relevant links can open transiently while focused, then close again when focus changes.

## `graph.tngl` Format

Basic structure:

```tngl
src/main.rs
    -> src/tui/canvas.rs : starts app
    -- README.md : documented in
```

Rules:

- Node line: non-indented path.
- Edge line: indented under source node.
- Edge operators: `->`, `<-`, `--`.
- `:` is required after target (label can be empty).
- Folder nodes end with `/`.
- Comments start with `#`.

Tags supported in graph:

- `[orphan]`
- `[bundle]`
- `[orphan bundle]`

## Settings (`tangle/config.tngl`)

Supported keys:

- `on_delete: prompt|delete|preserve`
- `show_orphans: true|false`
- `auto_reveal_links: true|false`
- `git_hooks: true|false`
- `editor: <command>`
- `warn_uncommented_edges: true|false`

`setup` popup currently exposes these toggles directly:

- reveal linked paths (`auto_reveal_links`)
- git hook updates (`git_hooks`)
- warn blank comments (`warn_uncommented_edges`)

## Typical Workflows

### Map a repo from scratch

```bash
tngl init
tngl view
# add/edit tangles interactively
tngl status
```

### Audit graph quality

```bash
tngl status
tngl inspect --dangling
tngl inspect --comment-mismatches
```

### Keep things current after file churn

```bash
tngl update
tngl status
```

## Current Limitations

- `tngl open` is not implemented yet.
- This is terminal-first; no web UI export path currently ships.
