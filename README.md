# tngl

A repo-native, human-authored graph for code relationships.

Nodes are files/folders.  
Links are tangles: directed (`->`) and undirected (`--`).

Even this repo is tngl'd:

```tngl
src/
    -> src/main.rs : crate root hosts CLI entrypoint

src/commands/mod.rs
    -- src/main.rs : command registry consumed by CLI
    -> src/commands/update.rs : re-export update
```
