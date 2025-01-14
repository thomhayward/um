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

fn load_program(path: &Path) -> std::io::Result<Vec<u32>> {
    match path.extension().map(|ext| ext.as_encoded_bytes()) {
        Some(b"uasm") | Some(b"asm") => {
            let source = std::fs::read_to_string(path)?;
            Ok(um::asm::assemble(&source))
        }
        _ => {
            let program = std::fs::read(path)?;
            Ok(um::bytes_to_program(&program).unwrap())
        }
    }
}
