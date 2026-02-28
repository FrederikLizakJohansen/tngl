mod commands;
mod graph;
mod parser;
mod scanner;
mod tangle;
mod tui;

use anyhow::Result;
use clap::{ArgGroup, Parser, Subcommand};

#[derive(Parser)]
#[command(
    name = "tngl",
    about = "A human-authored, repo-native relational graph tool"
)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Initialise tngl in the current directory
    Init,
    /// Re-scan the file tree and reconcile against graph.tngl
    Update {
        /// Suppress output unless conflicts require user input
        #[arg(long)]
        silent: bool,
        /// Tag new files and folders as [orphan]
        #[arg(long)]
        mark_new_as_orphans: bool,
    },
    /// Diff the current filesystem against graph.tngl (read-only)
    Status,
    /// Query the graph for specific conditions
    #[command(
        group(
            ArgGroup::new("inspect_query")
                .args([
                    "orphans",
                    "dangling",
                    "edges",
                    "unreachable",
                    "comment_mismatches",
                    "reconcile_comments",
                ])
                .multiple(false)
        )
    )]
    Inspect {
        /// List all isolated nodes (no incoming or outgoing edges)
        #[arg(long)]
        orphans: bool,
        /// List edges pointing to missing files
        #[arg(long)]
        dangling: bool,
        /// Show all edges for a specific node
        #[arg(long)]
        edges: Option<String>,
        /// List nodes not reachable from any other node
        #[arg(long)]
        unreachable: bool,
        /// List mirrored directed/incoming edges with different labels
        #[arg(long)]
        comment_mismatches: bool,
        /// Interactively reconcile mirrored edge label mismatches
        #[arg(long)]
        reconcile_comments: bool,
    },
    /// Mark all unattended orphan nodes as intentional
    MarkOrphans,
    /// List all edges in graph order
    List,
    /// Open tangle/graph.tngl in your editor
    Edit,
    /// Open the interactive TUI canvas
    View {
        /// Launch with a built-in sample graph (no repo required)
        #[arg(long)]
        demo: bool,
    },
    /// Reserved for static HTML export (currently not implemented)
    Open,
    /// Open the TUI settings panel
    Setup,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Command::Init => commands::init::run(),
        Command::Update {
            silent,
            mark_new_as_orphans,
        } => commands::update::run(silent, mark_new_as_orphans),
        Command::Status => commands::status::run(),
        Command::Inspect {
            orphans,
            dangling,
            edges,
            unreachable,
            comment_mismatches,
            reconcile_comments,
        } => {
            if orphans {
                commands::inspect::run_orphans()
            } else if dangling {
                commands::inspect::run_dangling()
            } else if let Some(node) = edges {
                commands::inspect::run_edges(&node)
            } else if unreachable {
                commands::inspect::run_unreachable()
            } else if comment_mismatches {
                commands::inspect::run_comment_mismatches()
            } else if reconcile_comments {
                commands::inspect::run_reconcile_comment_mismatches()
            } else {
                eprintln!(
                    "Specify one of: --orphans, --dangling, --edges <node>, --unreachable, --comment-mismatches, --reconcile-comments"
                );
                Ok(())
            }
        }
        Command::MarkOrphans => commands::intentionalize_orphans::run(),
        Command::List => commands::list::run(),
        Command::Edit => commands::edit::run(),
        Command::View { demo } => commands::view::run(demo),
        Command::Open => commands::open::run(),
        Command::Setup => commands::view::run_setup(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::error::ErrorKind;

    #[test]
    fn inspect_rejects_multiple_query_flags() {
        let parsed = Cli::try_parse_from(["tngl", "inspect", "--orphans", "--dangling"]);
        assert!(
            parsed.is_err(),
            "inspect flags should be mutually exclusive"
        );
        let err = parsed.err().expect("expected clap parse error");
        assert_eq!(err.kind(), ErrorKind::ArgumentConflict);
    }

    #[test]
    fn inspect_accepts_single_query_flag() {
        let cli = Cli::try_parse_from(["tngl", "inspect", "--orphans"])
            .expect("single inspect flag should parse");
        match cli.command {
            Command::Inspect { orphans, .. } => assert!(orphans),
            _ => panic!("expected inspect command"),
        }
    }
}
