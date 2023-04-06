use ropey::Rope;
use itertools::Itertools;
use tower_lsp::lsp_types::*;

enum CommentParserState {
    Outside,
    MaybeInside,
    JustEntered,
    Inside,
    MaybeOutside
}

pub fn read_comment(content: &Rope, start: usize) -> Option<String> {
    read_multi_singleline_comment(content, start)
        .or(read_multiline_comment(content, start))
}

// start is the char index where the keyword starts.
// example: if comment is produced for a function 'Main', the index would be the index of the
// character 'M'
pub fn read_multiline_comment(content: &Rope, start: usize) -> Option<String> {
    let mut current_idx = start;
    let mut current_state = CommentParserState::Outside;
    let mut start_idx = 0;
    let mut end_idx = 0;

    let mut iter = content.chars_at(start).reversed();
    while let Some(c) = iter.next() {
        match current_state {
            CommentParserState::Outside => match c {
                '/' => current_state = CommentParserState::MaybeInside,
                _ => ()
            },
            CommentParserState::MaybeInside => match c {
                '*' => {
                    current_state = CommentParserState::JustEntered;
                    end_idx = current_idx;
                },
                '/' => (),
                _ => current_state = CommentParserState::Outside
            },
            CommentParserState::JustEntered => match c {
                '*' => end_idx = current_idx,
                _ => current_state = CommentParserState::Inside
            }
            CommentParserState::Inside => match c {
                '*' => {
                    current_state = CommentParserState::MaybeOutside;
                    start_idx = current_idx;
                }
                _ => ()
            },
            CommentParserState::MaybeOutside => match c {
                '/' => {
                    let result = content
                        .slice(start_idx..end_idx-1)
                        .to_string()
                        .lines()
                        .map(|x| x.trim_matches(|c: char| c.is_whitespace() || c == '*'))
                        .intersperse("\n")
                        .collect::<String>()
                        .trim()
                        .to_string();

                    return if !result.is_empty() {
                        Some(result)
                    } else {
                        None
                    }
                },
                '*' => (),
                _ => current_state = CommentParserState::Inside
            }
        }

        current_idx -= 1;
    }

    None
}

pub fn read_multi_singleline_comment(content: &Rope, start: usize) -> Option<String> {
    let mut current_line_idx = content.try_char_to_line(start).expect("char start index should be valid");
    let mut first_comment_line = 0;
    let mut last_comment_line = 0;
    let mut entered = false;

    while current_line_idx > 0 {
        current_line_idx -= 1;

        let line = content.line(current_line_idx);
        // not converting to &str because it might fail (due to the structue of rope), 
        // and allocating for each line is wasteful
        let is_line_comment = {
            if line.len_chars() < 2 {
                false 
            } else {
                line.char(0) == '/' && line.char(1) == '/'
            }
        };

        match (entered, is_line_comment) {
            (false, true) => {
                entered = true;
                last_comment_line = current_line_idx;
            },
            (true, false) => {
                first_comment_line = current_line_idx;
                break;
            },
            _ => ()
        }
    }

    if entered {
        let start = content.line_to_char(first_comment_line);
        let end = content.line_to_char(last_comment_line + 1);
        let result = content
            .slice(start..end)
            .to_string()
            .lines()
            .map(|x| x.trim_matches(|c: char| c.is_whitespace() || c == '/'))
            .intersperse("\n")
            .collect::<String>()
            .trim()
            .to_string();

        Some(result)
    } else {
        None
    }
}

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
