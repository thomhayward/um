// Copyright (C) 2025 Thom Hayward.
//
// This program is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation, version 3.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program. If not, see <https://www.gnu.org/licenses/>.
//
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
