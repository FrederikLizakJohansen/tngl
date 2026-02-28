use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    Move(Direction),
    MoveRightOrEditLabel,
    Activate,
    NextNode,
    ZoomIn,
    ZoomOut,
    Quit,
    ToggleHelp,
    StartConnect,
    StartDetach,
    DeleteEdge,
    ToggleEdgeDirection,
    ReverseEdgeDirection,
    AddNode,
    EditNode,
    DeleteNode,
    ToggleBundle,
    ToggleOrphans,
    CycleConnectionFilter,
    ToggleCollapse,
    OpenSettings,
    SubmitText,
    Cancel,
    Backspace,
    InputChar(char),
    Noop,
}

pub fn action_for_key(key: KeyEvent, text_mode: bool) -> Action {
    if text_mode {
        return match key.code {
            KeyCode::Enter => Action::SubmitText,
            KeyCode::Esc => Action::Cancel,
            KeyCode::Backspace => Action::Backspace,
            KeyCode::Left => Action::Move(Direction::Left),
            KeyCode::Right => Action::Move(Direction::Right),
            KeyCode::Char(c) => Action::InputChar(c),
            _ => Action::Noop,
        };
    }

    match key.code {
        KeyCode::Up => Action::Move(Direction::Up),
        KeyCode::Down => Action::Move(Direction::Down),
        KeyCode::Left => Action::Move(Direction::Left),
        KeyCode::Right => Action::Move(Direction::Right),
        KeyCode::Enter => Action::Activate,
        KeyCode::Tab => Action::NextNode,
        KeyCode::Esc => Action::Cancel,
        KeyCode::Backspace => Action::Cancel,
        KeyCode::Char('+') => Action::ZoomIn,
        KeyCode::Char('=') if key.modifiers.contains(KeyModifiers::SHIFT) => Action::ZoomIn,
        KeyCode::Char('-') => Action::ZoomOut,
        KeyCode::Char('?') => Action::ToggleHelp,
        KeyCode::Char('q') => Action::Quit,
        KeyCode::Char('h') => Action::Move(Direction::Left),
        KeyCode::Char('j') => Action::Move(Direction::Down),
        KeyCode::Char('k') => Action::Move(Direction::Up),
        KeyCode::Char('l') => Action::MoveRightOrEditLabel,
        KeyCode::Char('c') => Action::StartConnect,
        KeyCode::Char('m') => Action::StartDetach,
        KeyCode::Char('d') => Action::DeleteEdge,
        KeyCode::Char('t') => Action::ToggleEdgeDirection,
        KeyCode::Char('r') => Action::ReverseEdgeDirection,
        KeyCode::Char('n') => Action::AddNode,
        KeyCode::Char('e') => Action::EditNode,
        KeyCode::Char('D') => Action::DeleteNode,
        KeyCode::Char('b') => Action::ToggleBundle,
        KeyCode::Char('o') => Action::ToggleOrphans,
        KeyCode::Char('f') => Action::CycleConnectionFilter,
        KeyCode::Char('z') => Action::ToggleCollapse,
        KeyCode::Char('s') => Action::OpenSettings,
        _ => Action::Noop,
    }
}
