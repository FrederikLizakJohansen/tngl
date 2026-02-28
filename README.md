# tngl

`tngl` is a repo-native, human-authored graph for code relationships.

- Nodes are files/folders.
- Links are tangles:
  - Directed link: `->`
  - Undirected link: `--`

## Install

From source:

```bash
git clone git@github.com:FrederikLizakJohansen/tngl.git
cd tngl
cargo install --path .
```

If you don't want to install globally, run directly with Cargo:

```bash
cargo run -- <command>
```

## Run

Inside any repo you want to tngl-ize:

```bash
tngl init
tngl status
tngl edit
tngl update
tngl inspect --orphans
tngl list
```

After `tngl init`, a local `./tngl` wrapper script is also created, so you can run:

```bash
./tngl status
```

## Command surface (v1)

```bash
tngl init
tngl update [--silent] [--mark-new-as-orphans]
tngl status
tngl inspect --orphans|--dangling|--edges <node>|--unreachable|--comment-mismatches|--reconcile-comments
tngl mark-orphans
tngl list
tngl edit
```

`view` and `setup` are available.  
`open` currently returns a not-implemented message.

## Example Graph

The live graph for this repository is tracked in:

- `tangle/graph.tngl`

Use the TUI to explore it:

```bash
tngl view
```
