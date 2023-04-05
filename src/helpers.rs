use ropey::Rope;
use tower_lsp::lsp_types::*;

pub fn find_word(rope: &Rope, position: Position) -> ropey::Result<Option<(usize, String)>> {
    let char_idx = position_to_char(rope, position)?;
    let char = rope.get_char(char_idx)
        .expect("char_idx should not be out of range since position_to_char guarantees");

    if char.is_alphanumeric() {
        let start = 'start: loop {
            let mut i = char_idx;
            for c in rope.chars_at(char_idx).reversed() {
                if !c.is_alphanumeric() && c != '_' {
                    break 'start i;
                }
                i -= 1;
            }
            break i;
        };
        let end = 'end: loop {
            let mut i = char_idx;
            for c in rope.chars_at(char_idx) {
                if !c.is_alphanumeric() && c != '_' {
                    break 'end i;
                }
                i += 1;
            }
            break i;
        };

        Ok(Some((start, rope.slice(start..end).to_string())))
    } else {
        Ok(None)
    }
}

pub fn position_to_char(rope: &Rope, position: Position) -> ropey::Result<usize> {
    let line_start = rope.try_line_to_char(usize::try_from(position.line).unwrap())?;
    let char = line_start + usize::try_from(position.character).unwrap();

    // ensure resulting character is in bounds
    rope.try_char_to_byte(char)?;
    Ok(char)
}

pub fn char_to_position(rope: &Rope, idx: usize) -> ropey::Result<Position> {
    let line = rope.try_char_to_line(idx)?;
    let line_start = rope.line_to_char(line);
    let character = idx - line_start;

    let line = u32::try_from(line).unwrap();
    let character = u32::try_from(character).unwrap();

    Ok(Position {
        line,
        character
    })
}

pub fn uri_to_string(uri: &Url) -> String {
    uri
        .to_file_path()
        .expect("Invalid text document URI")
        .into_os_string()
        .into_string()
        .expect("Invalid text document URI")
}

pub fn string_to_uri(s: &str) -> Url {
    // strip first and last chars because circom is stupid
    let fixed = {
        let mut chars = s.chars();
        chars.next();
        chars.next_back();

        chars.as_str()
    };

    Url::from_file_path(fixed).expect("string is valid uri")
}

pub fn simple_hover(message: String) -> Hover {
    return Hover {
        contents: HoverContents::Scalar(MarkedString::String(message)),
        range: None
    };
}
