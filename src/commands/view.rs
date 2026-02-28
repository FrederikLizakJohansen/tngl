use anyhow::Result;

use crate::tui::canvas;

pub fn run(demo: bool) -> Result<()> {
    canvas::run(demo, false)
}

pub fn run_setup() -> Result<()> {
    canvas::run(false, true)
}
