use std::{borrow::Cow, str::CharIndices};

#[derive(Debug)]
pub struct InvalidCharacterEscape(pub char, pub usize);

pub fn unescape_str(s: &str) -> Result<Cow<str>, InvalidCharacterEscape> {
    fn escape_inner(c: &str, i: &mut CharIndices<'_>) -> Result<String, InvalidCharacterEscape> {
        let mut buffer = c.to_owned();
        let mut in_escape = true;

        for (index, c) in i {
            match (in_escape, c) {
                (false, '\\') => {
                    in_escape = true;
                    continue;
                }
                (false, c) => buffer.push(c),
                (true, '\\') => buffer.push('\\'),
                (true, 'n') => buffer.push('\n'),
                (true, '0') => buffer.push('\0'),
                (true, '"') => buffer.push('"'),
                (true, '\'') => buffer.push('\''),
                (true, 'r') => buffer.push('\r'),
                (true, 't') => buffer.push('\t'),
                (true, c) => Err(InvalidCharacterEscape(c, index))?,
            }

            in_escape = false;
        }

        Ok(buffer)
    }

    let mut char_indicies = s.char_indices();
    for (index, c) in &mut char_indicies {
        let scanned = &s[..index];
        if c == '\\' {
            return Ok(Cow::Owned(escape_inner(scanned, &mut char_indicies)?));
        }
    }

    Ok(Cow::Borrowed(s))
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use super::unescape_str;

    #[test]
    fn no_unescapes() {
        let s = "Hello, this string should have no characters that need unescaping.";
        let u = unescape_str(s).unwrap();

        assert!(matches!(u, Cow::Borrowed(_)));
        assert_eq!(s, u);
    }
}
