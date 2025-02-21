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
use std::path::{Path, PathBuf};

fn main() {
    let mut output = PathBuf::from("./a.um");

    let mut program = Vec::new();
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-o" | "--out" => {
                output = PathBuf::from(args.next().expect("expected output path"));
            }
            _ => {
                let path = Path::new(&arg);
                program.extend_from_slice(&match load_program(path) {
                    Ok(p) => p,
                    Err(error) => {
                        eprintln!("{error}");
                        std::process::exit(1);
                    }
                });
            }
        }
    }

    // Convert the program to bytes.
    let bytes: Vec<_> = program
        .into_iter()
        .flat_map(|word| word.to_be_bytes())
        .collect();

    std::fs::write(&output, bytes).unwrap();
}

fn load_program(path: &Path) -> std::io::Result<Vec<u32>> {
    match path.extension().map(|ext| ext.as_encoded_bytes()) {
        Some(b"uasm") | Some(b"asm") => {
            let source = std::fs::read_to_string(path)?;
            let program = um::asm::assemble(&source);
            Ok(program)
        }
        _ => {
            let program = std::fs::read(path)?;
            Ok(um::bytes_to_program(&program).unwrap())
        }
    }
}
