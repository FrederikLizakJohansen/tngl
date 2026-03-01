<pre>
████████╗███╗  ██╗ ██████╗ ██╗ ○ ──╮◀ ╮
   ██╔══╝████╗ ██║██╔════╝ ██║ ◀ ──╯  │
   ██║   ██╔██╗██║██║  ███╗██║ ○ ─────╯○ ─╮
   ██║   ██║╚████║██║   ██║██║            │
   ██║   ██║ ╚███║╚██████╔╝███████╗○ ─────╯
   ╚═╝   ╚═╝  ╚══╝ ╚═════╝ ╚══════╝
</pre>

`tngl` is a repo-native, human-authored graph for understanding how files and folders relate.

A `tngl` graph is simple and durable:

- Nodes are files/folders.
- Edges are tangles (relationships between nodes).
- The source of truth is `tangle/graph.tngl`.
- The TUI is optimized for fast maintenance as projects evolve.

<figure>
  <img width="1274" height="804" alt="tngl TUI showing tree, details, and colored relationship routes" src="https://github.com/user-attachments/assets/3f98672a-1f05-4602-87a7-765fe84ac628" />
  <figcaption><em>tngl view: interactive tree + details workflow for building and maintaining relationship maps.</em></figcaption>
</figure>

## Why tngl

Most repos explain what files exist, but not how outputs depend on each other.
`tngl` makes those dependencies explicit in a format that is both human-editable and tool-aware.

It is especially useful when:

- onboarding collaborators into an unfamiliar repo,
- handing over project snapshots,
- auditing drift between filesystem state and documented relationships.

## Install

From crates.io (recommended):

```bash
cargo install tngl
```

Upgrade:

```bash
cargo install tngl --force
```

From source:

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
tngl open
```

`tngl init` also creates a local `./tngl` wrapper script so collaborators can run `./tngl ...` directly in that repo.

## Command Reference

| Command | Purpose |
| --- | --- |
| `tngl init` | Bootstrap `tangle/`, scan repo, write graph/config/ignore/wrapper. |
| `tngl update [--silent]` | Reconcile graph against filesystem changes. |
| `tngl status` | Read-only drift and graph-quality report. |
| `tngl inspect ...` | Targeted graph audits (orphans, dangling, edges, mismatch checks). |
| `tngl mark-orphans` | Mark unattended isolated nodes as intentional in graph tags. |
| `tngl list` | Print all tangles in graph order. |
| `tngl edit` | Open `tangle/graph.tngl` in your configured editor. |
| `tngl view [--demo]` | Launch interactive TUI editor. |
| `tngl setup` | Open settings panel directly. |
| `tngl open` | Export static HTML map and attempt browser open. |

## Static HTML Map (`tngl open`)

```bash
tngl open
```

What it does:

- writes `tangle/map.html`,
- attempts to open it in your default browser,
- keeps export read-only,
- supports click and `↑/↓` navigation in the page details view.

If browser auto-open is unavailable in the current environment, the HTML export is still generated.

## Core Files

| File | Role |
| --- | --- |
| `tangle/graph.tngl` | Graph source of truth (nodes, edges, tags). |
| `tangle/config.tngl` | Runtime behavior and preferences. |
| `.tnglignore` | Extra scan ignores used by `tngl update` and `tngl open`. |

## `graph.tngl` Format

Minimal example:

```tngl
src/main.rs
    -> src/tui/canvas.rs : starts app
    -- README.md : documented in
```

Rules:

- node line: non-indented path,
- edge line: indented under source node,
- edge operators: `->`, `<-`, `--`,
- `:` required after target (label may be empty),
- folder nodes end with `/`,
- comments start with `#`.

Supported tags:

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

`setup` currently exposes:

- reveal linked paths (`auto_reveal_links`)
- git hook updates (`git_hooks`)
- warn blank comments (`warn_uncommented_edges`)

## TUI Guide

The full TUI guide (layout, keymaps, CREATE/DELETE/MOVE flows, and behavior notes) is now maintained in:

- [`src/tui/TUI_GUIDE.md`](src/tui/TUI_GUIDE.md)

## Current Limitations

- Static export is read-only (`tngl open` writes `tangle/map.html`).
