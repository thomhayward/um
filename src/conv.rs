
const WORD_LEN: usize = std::mem::size_of::<u32>();

#[derive(Debug)]
pub struct InvalidProgram;

/// Converts a byte slice to a program.
///
/// Returns `None` if the byte slice is not a multiple of 4 bytes in length.
pub fn bytes_to_program(bytes: &[u8]) -> Result<Vec<u32>, InvalidProgram> {
    if bytes.len().rem_euclid(WORD_LEN) != 0 {
        return Err(InvalidProgram);
    }

    Ok(bytes
        .chunks_exact(WORD_LEN)
        .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
        .collect())
}
