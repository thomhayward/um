#!/usr/bin/env bash
cargo clean && cargo build --release
hyperfine 'target/release/um --time files/sandmark.umz'
