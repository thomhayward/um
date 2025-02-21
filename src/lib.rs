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
use smallvec::SmallVec;
use std::io::{Read, Write};

pub mod asm;
pub mod conv;
pub mod ops;
pub mod reg;

use ops::Operation;
use reg::{Page, Register};

const SMALLVEC_SIZE: usize = 24;

/// Lossless conversion to `usize`.
///
/// This should only be implemented on types which can be losslessly
/// cast to a `usize`.
trait IntoIndex: Sized + Copy {
    fn into_index(self) -> usize;
}

macro_rules! impl_into_index {
    ($t:ty) => {
        impl IntoIndex for $t {
            fn into_index(self) -> usize {
                self as usize
            }
        }
    };
}

#[cfg(target_pointer_width = "16")]
compile_error!("16 bit architectures are unsupported");

// usize *may* be 16 bits, so only implement if it is 32 or 64 bits.
#[cfg(any(target_pointer_width = "64", target_pointer_width = "32"))]
impl_into_index!(u32);

#[derive(Default)]
pub struct Um<'a> {
    pub program_counter: u32,
    registers: Page,
    /// Program memory, modelled as a `Vec` of `SmallVec`.
    ///
    /// Memory allocations greater than `SMALLVEC_SIZE` will incur a memory
    /// indirection penalty for every memory access within that block.
    memory: Vec<SmallVec<[u32; SMALLVEC_SIZE]>>,
    free_blocks: Vec<u32>,
    /// Partially decoded operations cache.
    ops: Vec<Operation>,
    stdin: Option<&'a mut dyn Read>,
    stdout: Option<&'a mut dyn Write>,
}

impl<'a> Um<'a> {
    /// Initialise a Universal Machine with the specified program scroll.
    pub fn new(program: Vec<u32>) -> Self {
        let ops = ops::decode(&program);
        Self {
            memory: vec![program.into()],
            ops,
            ..Default::default()
        }
    }

    /// Sets the output for the universal machine.
    pub fn stdout<T: Write>(mut self, stdout: &'a mut T) -> Self {
        self.stdout.replace(stdout);
        self
    }

    /// Sets the input for the universal machine.
    pub fn stdin<T: Read>(mut self, stdin: &'a mut T) -> Self {
        self.stdin.replace(stdin);
        self
    }

    /// Begins the spin-cycle of the universal machine.
    #[inline(never)]
    pub fn run(mut self) -> Self {
        loop {
            // println!(
            //     "{:?}, pc: {:08x}, r: {:08x?}",
            //     self.ops[self.program_counter as usize], self.program_counter, self.registers
            // );
            match self.ops[self.program_counter as usize] {
                Operation::ConditionalMove { a, b, c } => self.conditional_move(a, b, c),
                Operation::ArrayIndex { a, b, c } => self.array_index(a, b, c),
                Operation::ArrayAmendment { a, b, c } => self.array_amendment(a, b, c),
                Operation::Addition { a, b, c } => self.addition(a, b, c),
                Operation::Multiplication { a, b, c } => self.multiplication(a, b, c),
                Operation::Division { a, b, c } => self.division(a, b, c),
                Operation::NotAnd { a, b, c } => self.not_and(a, b, c),
                Operation::Halt => break,
                Operation::Allocation { b, c } => self.allocation(b, c),
                Operation::Abandonment { c } => self.abandonment(c),
                Operation::Output { c } => self.output(c),
                Operation::Input { c } => self.input(c),
                Operation::LoadProgram { b, c } => {
                    self.load_program(b, c);
                    continue;
                }
                Operation::Orthography { a, value } => self.orthography(a, value),
                Operation::IllegalInstruction => self.illegal_instruction(),
            }
            self.program_counter += 1;
        }

        self
    }

    // Un-commenting step() slows down the sandmark benchmark by ~3-5 seconds, even
    // though it has *no* interaction with the code path in Um::run().
    //
    // /// Steps one instruction.
    // #[inline(never)]
    // pub fn step(&mut self) -> bool {
    //     match self.ops[self.program_counter as usize] {
    //         Operation::ConditionalMove { a, b, c } => self.conditional_move(a, b, c),
    //         Operation::ArrayIndex { a, b, c } => self.array_index(a, b, c),
    //         Operation::ArrayAmendment { a, b, c } => self.array_amendment(a, b, c),
    //         Operation::Addition { a, b, c } => self.addition(a, b, c),
    //         Operation::Multiplication { a, b, c } => self.multiplication(a, b, c),
    //         Operation::Division { a, b, c } => self.division(a, b, c),
    //         Operation::NotAnd { a, b, c } => self.not_and(a, b, c),
    //         Operation::Halt => return false,
    //         Operation::Allocation { b, c } => self.allocation(b, c),
    //         Operation::Abandonment { c } => self.abandonment(c),
    //         Operation::Output { c } => self.output(c),
    //         Operation::Input { c } => self.input(c),
    //         Operation::LoadProgram { b, c } => {
    //             self.load_program(b, c);
    //             return true;
    //         }
    //         Operation::Orthography { a, value } => self.orthography(a, value),
    //         Operation::IllegalInstruction => self.illegal_instruction(),
    //     }
    //     self.program_counter += 1;
    //     true
    // }

    /// Loads the value from the specified register.
    fn load_register(&self, register: Register) -> u32 {
        self.registers[register]
    }

    /// Saves a value to the specified register.
    fn save_register(&mut self, register: Register, value: u32) {
        self.registers[register] = value;
    }

    fn conditional_move(&mut self, a: Register, b: Register, c: Register) {
        if self.load_register(c) != 0 {
            self.save_register(a, self.load_register(b));
        }
    }

    fn array_index(&mut self, a: Register, b: Register, c: Register) {
        let block = self.load_register(b);
        let offset = self.load_register(c);
        self.save_register(a, self.load_memory(block, offset));
    }

    fn array_amendment(&mut self, a: Register, b: Register, c: Register) {
        let block = self.load_register(a);
        let offset = self.load_register(b);
        let value = self.load_register(c);
        self.store_memory(block, offset, value);
    }

    fn addition(&mut self, a: Register, b: Register, c: Register) {
        self.save_register(a, self.load_register(b).wrapping_add(self.load_register(c)));
    }

    fn multiplication(&mut self, a: Register, b: Register, c: Register) {
        self.save_register(a, self.load_register(b).wrapping_mul(self.load_register(c)));
    }

    fn division(&mut self, a: Register, b: Register, c: Register) {
        self.save_register(a, self.load_register(b).wrapping_div(self.load_register(c)));
    }

    fn not_and(&mut self, a: Register, b: Register, c: Register) {
        self.save_register(a, !(self.load_register(b) & self.load_register(c)));
    }

    fn allocation(&mut self, b: Register, c: Register) {
        let length = self.load_register(c);
        let index = self.allocate_memory(length);
        self.save_register(b, index);
    }

    fn abandonment(&mut self, c: Register) {
        let block = self.load_register(c);
        self.free_memory(block);
    }

    fn output(&mut self, c: Register) {
        let value = self.load_register(c);
        if let Some(stdout) = self.stdout.as_mut() {
            let buffer = [(value & 0xff) as u8];
            stdout.write_all(&buffer).unwrap();
        }
    }

    fn input(&mut self, c: Register) {
        if let Some(stdin) = self.stdin.as_mut() {
            let mut buffer = vec![0];
            match stdin.read_exact(&mut buffer) {
                Ok(()) => self.save_register(c, buffer[0] as u32),
                Err(_) => self.save_register(c, u32::MAX),
            }
        } else {
            self.save_register(c, u32::MAX);
        }
    }

    fn load_program(&mut self, b: Register, c: Register) {
        let block = self.load_register(b);

        // Source array is always copied to array[0], but there
        // is no point copying array[0] to array[0].
        if block != 0 {
            let duplicated = self.duplicate_memory(block);
            let ops = ops::decode(duplicated);
            self.ops = ops;
        }

        self.program_counter = self.load_register(c);
    }

    fn orthography(&mut self, a: Register, value: u32) {
        self.save_register(a, value);
    }

    #[cold]
    #[inline(never)]
    fn illegal_instruction(&self) -> ! {
        panic!(
            "illegal instruction: {:08x}, pc: {:08x}, r: {:08x?}",
            self.memory[0][self.program_counter.into_index()],
            self.program_counter,
            self.registers
        )
    }

    fn load_memory(&self, block: u32, offset: u32) -> u32 {
        let block = block.into_index();
        let offset = offset.into_index();
        assert!(block < self.memory.len() && offset < self.memory[block].len());
        self.memory[block][offset]
    }

    fn store_memory(&mut self, block: u32, offset: u32, value: u32) {
        let block = block.into_index();
        let offset = offset.into_index();
        assert!(block < self.memory.len() && offset < self.memory[block].len());
        self.memory[block][offset] = value
    }

    /// Duplicates a block of memory.
    ///
    /// The block is copied to the first block of memory.
    fn duplicate_memory(&mut self, block: u32) -> &[u32] {
        let block = block.into_index();
        assert!(block < self.memory.len());
        self.memory[0] = self.memory[block].clone();
        &self.memory[0]
    }

    /// Allocates a block of memory of the specified length.
    fn allocate_memory(&mut self, length: u32) -> u32 {
        if let Some(index) = self.free_blocks.pop() {
            self.memory[index.into_index()] = Self::new_block(length.into_index());
            index
        } else {
            self.memory.push(Self::new_block(length.into_index()));
            (self.memory.len() - 1) as u32
        }
    }

    /// Frees a block of memory.
    fn free_memory(&mut self, block: u32) {
        assert!(block.into_index() < self.memory.len());
        self.free_blocks.push(block);
        self.memory[block.into_index()] = Self::new_block(0);
    }

    /// Creates a new block of memory.
    ///
    /// The block is initialised with `len` zeroes.
    fn new_block(len: usize) -> SmallVec<[u32; SMALLVEC_SIZE]> {
        smallvec::smallvec![0; len]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic]
    fn empty_program() {
        Um::new(vec![]).run();
    }

    #[test]
    fn just_halt() {
        Um::new(vec![0x70000000]).run();
    }

    #[test]
    fn hello_world() {
        let program = asm::assemble(include_str!("../files/hello-world.asm"));
        let mut buffer = Vec::new();
        Um::new(program).stdout(&mut buffer).run();
        assert_eq!(&buffer, b"Hello, world!\n");
    }

    #[test]
    fn cat() {
        let program = asm::assemble(include_str!("../files/cat.asm"));
        let input = include_bytes!("lib.rs");

        let mut reader = std::io::Cursor::new(input);
        let mut buffer = Vec::new();

        Um::new(program)
            .stdin(&mut reader)
            .stdout(&mut buffer)
            .run();

        assert_eq!(&buffer, &input);
    }
}
