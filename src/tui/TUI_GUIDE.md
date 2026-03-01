# TUI Guide

`tngl view` is the primary interactive editor for `tangle/graph.tngl`.
This guide documents the current keymap, flows, and behavior.

## Launch

```bash
tngl view
```

Demo mode (no repo required, in-memory only):

```bash
tngl view --demo
```

Open settings directly:

```bash
tngl setup
```

## Layout

The UI has three major areas:

- `TREE` (left): filesystem tree with connection markers and folder fold/bundle state.
- `DETAILS` (right): focused node info, connected tangles, and edge actions.
- Bottom status/hint bar: selected node summary and context-sensitive key hints.

Mode badges appear in title/status when active:

- `CREATE` (green)
- `DELETE` (red)
- `MOVE` (blue)

## Visual Language

Connection markers:

- `▶` out
- `◀` in
- `◆` undirected/reference

Folder markers:

- `▾` expanded
- `▸` collapsed
- `▣` bundled/locked

Node/path cues:

- Focused node path is highlighted.
- In details mode, selected edge is highlighted in both tree route and details list.

## Contexts And Flows

### 1. Tree Context (default)

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

### 2. Details Context

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

### 3. CREATE Flow

Start from tree (`c`) or details (`c`).

- Pick target node: `↑/↓`
- Change edge type (`in/out/ref`): `←/→`
- Fold/unfold folders while selecting: `z`
- Confirm target/type and open comment popup: `Enter`
- Cancel CREATE: `Esc` or `Backspace`

Important CREATE guard:

- While creating from a folder/source path, you cannot collapse that source folder or its parents.

### 4. Comment Popup (during CREATE)

After choosing source/target/type:

- Type label/comment text directly
- Move caret: `←/→`
- Delete character: `Backspace`
- Save edge: `Enter`
- Go back to CREATE selection: `Esc`

### 5. DELETE Flow

From tree, press `d`.

- Cycle candidate tangles: `j/k` or `↑/↓`
- Confirm deletion prompt: `Enter`
- Cancel delete mode: `Esc` or `Backspace`

### 6. MOVE Flow (Edge reroute)

In details mode, choose an edge and press `m`.

- Grabs the opposite endpoint relative to the details anchor node.
- Move focus to replacement node with `↑/↓`.
- Change tangle type with `←/→`.
- Apply move with `Enter`.
- Cancel with `Esc` or `Backspace`.
- Moving onto the other endpoint is blocked (no self-loop moves).

### 7. Bundle Flow

From tree on a folder, press `b`.

- Bundling locks/collapses folder (`[bundle]` behavior).
- If child links exist, a warning/confirm prompt appears before pruning links.
- Unbundle with `b` again.

### 8. Confirm Prompts

Used for destructive actions (delete edge/node, bundle-prune).

- Confirm: `y` or `Enter`
- Cancel: `n`, `Esc`, or `Backspace`

## Behavior Notes

- Changes made in TUI are written back to `tangle/graph.tngl`.
- On exit, graph formatting/normalization is applied.
- If `auto_reveal_links` is enabled, collapsed folders hiding relevant links can open transiently while focused, then close again when focus changes.
