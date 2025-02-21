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
use std::{path::Path, time::Instant};
use um::Um;

fn main() {
    let mut program = Vec::new();
    let mut time = false;

    for arg in std::env::args().skip(1) {
        if arg == "--time" {
            time = true;
            continue;
        }

        let path = Path::new(&arg);
        program.extend_from_slice(&match load_program(path) {
            Ok(p) => p,
            Err(error) => {
                eprintln!("{error}");
                std::process::exit(1);
            }
        });
    }

    let start = Instant::now();
    Um::new(program)
        .stdout(&mut std::io::stdout())
        .stdin(&mut std::io::stdin())
        .run();

    if time {
        eprintln!("{:?}", start.elapsed());
    }
}

#[cfg(feature = "asm")]
fn load_program(path: &Path) -> std::io::Result<Vec<u32>> {
    match path.extension().map(|ext| ext.as_encoded_bytes()) {
        // In an ideal world we would just add `#[cfg(feature = "asm")]` here.
        // Unfortunately this leads some wierd code generation fuckery which
        // makes the version without the 'asm' feature ~1-2 seconds slower
        // when running the sandmark program.
        Some(b"uasm") | Some(b"asm") => {
            let source = std::fs::read_to_string(path)?;
            Ok(um::asm::assemble(&source))
        }
        _ => {
            let program = std::fs::read(path)?;
            Ok(um::conv::bytes_to_program(&program).unwrap())
        }
    }
}

#[cfg(not(feature = "asm"))]
fn load_program(path: &Path) -> std::io::Result<Vec<u32>> {
    let program = std::fs::read(path)?;
    Ok(um::conv::bytes_to_program(&program).unwrap())
}
