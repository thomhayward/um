use smallvec::SmallVec;
use std::{
    io::{Read, Write},
    ops,
};

pub mod asm;
pub mod str;

/// A reference to a register of the UM-32.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum Register {
    #[default]
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "r{}", *self as u8)
    }
}

impl Register {
    /// Encodes the register as the 'a' parameter of an encoded
    /// instruction (bits 6..=8).
    fn encode_a(self) -> u32 {
        ((self as u32) & 0x7) << 6
    }

    /// Encodes the register as the 'b' parameter of an encoded
    /// instruction (bits 3..=5).
    fn encode_b(self) -> u32 {
        ((self as u32) & 0x7) << 3
    }

    /// Encodes the register as the 'c' parameter of an encoded
    /// instruction (bits 0..=2).
    fn encode_c(self) -> u32 {
        (self as u32) & 0x7
    }

    /// Encodes the register as the 'a' parameter of an `Orthography`
    /// operation.
    ///
    /// This is *only* valid for `Orthography` operations.
    fn encode_a_ortho(self) -> u32 {
        ((self as u32) & 0x7) << 25
    }

    fn from_u8(index: u8) -> Self {
        match index {
            0 => Register::R0,
            1 => Register::R1,
            2 => Register::R2,
            3 => Register::R3,
            4 => Register::R4,
            5 => Register::R5,
            6 => Register::R6,
            7 => Register::R7,
            _ => unreachable!(),
        }
    }
}

/// A set of registers.
#[derive(Debug, Default)]
struct Page([u32; 8]);

impl ops::Index<Register> for Page {
    type Output = u32;
    #[inline(always)]
    fn index(&self, index: Register) -> &Self::Output {
        &self.0[index as usize]
    }
}

impl ops::IndexMut<Register> for Page {
    #[inline(always)]
    fn index_mut(&mut self, index: Register) -> &mut Self::Output {
        &mut self.0[index as usize]
    }
}

impl From<[u32; 8]> for Page {
    fn from(value: [u32; 8]) -> Self {
        Self(value)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Operation {
    /// Operator #0. Conditional Move.
    ///
    /// The register A receives the value in register B,
    /// unless the register C contains 0.
    ConditionalMove {
        a: Register,
        b: Register,
        c: Register,
    },
    /// Operator #1: Array Index.
    ///
    /// The register A receives the value stored at offset
    /// in register C in the array identified by B.
    ArrayIndex {
        a: Register,
        b: Register,
        c: Register,
    },
    /// Operator #2. Array Amendment.
    ///
    /// The array identified by A is amended at the offset
    /// in register B to store the value in register C.
    ArrayAmendment {
        a: Register,
        b: Register,
        c: Register,
    },
    /// Operator #3. Addition.
    ///
    /// The register A receives the value in register B plus
    /// the value in register C, modulo 2^32.
    Addition {
        a: Register,
        b: Register,
        c: Register,
    },
    /// Operator #4. Multiplication.
    ///
    /// The register A receives the value in register B times
    /// the value in register C, modulo 2^32.
    Multiplication {
        a: Register,
        b: Register,
        c: Register,
    },
    /// Operator #5. Division.
    ///
    /// The register A receives the value in register B
    /// divided by the value in register C, if any, where
    /// each quantity is treated as an unsigned 32 bit number.
    Division {
        a: Register,
        b: Register,
        c: Register,
    },
    /// Operator #6. Not-And.
    ///
    /// Each bit in the register A receives the 1 bit if
    /// either register B or register C has a 0 bit in that
    /// position.  Otherwise the bit in register A receives
    /// the 0 bit.
    NotAnd {
        a: Register,
        b: Register,
        c: Register,
    },
    /// Operator #7. Halt.
    ///
    /// The universal machine stops computation.
    Halt,
    /// Operator #8. Allocation.
    ///
    /// A new array is created with a capacity of platters
    /// commensurate to the value in the register C. This
    /// new array is initialized entirely with platters
    /// holding the value 0. A bit pattern not consisting of
    /// exclusively the 0 bit, and that identifies no other
    /// active allocated array, is placed in the B register.
    Allocation {
        b: Register,
        c: Register,
    },
    /// Operator #9. Abandonment.
    ///
    /// The array identified by the register C is abandoned.
    /// Future allocations may then reuse that identifier.
    Abandonment {
        c: Register,
    },
    /// Operator #10. Output.
    ///
    /// The value in the register C is displayed on the console
    /// immediately. Only values between and including 0 and 255
    /// are allowed.
    Output {
        c: Register,
    },
    /// Operator #11. Input.
    ///
    /// The universal machine waits for input on the console.
    /// When input arrives, the register C is loaded with the
    /// input, which must be between and including 0 and 255.
    /// If the end of input has been signaled, then the
    /// register C is endowed with a uniform value pattern
    /// where every place is pregnant with the 1 bit.
    Input {
        c: Register,
    },
    /// Operator #12. Load Program.
    ///
    /// The array identified by the B register is duplicated
    /// and the duplicate shall replace the '0' array,
    /// regardless of size. The execution finger is placed
    /// to indicate the platter of this array that is
    /// described by the offset given in C, where the value
    /// 0 denotes the first platter, 1 the second, et
    /// cetera.
    ///
    /// The '0' array shall be the most sublime choice for
    /// loading, and shall be handled with the utmost
    /// velocity.
    LoadProgram {
        b: Register,
        c: Register,
    },
    /// Operator #13. Orthography.
    ///
    /// The value indicated is loaded into the register A
    /// forthwith.
    Orthography {
        a: Register,
        value: u32,
    },
    IllegalInstruction,
}

impl From<u32> for Operation {
    fn from(value: u32) -> Self {
        let a = Register::from_u8(((value >> 6) & 0x07) as u8);
        let b = Register::from_u8(((value >> 3) & 0x07) as u8);
        let c = Register::from_u8((value & 0x07) as u8);
        match value & 0xf0000000 {
            0x00000000 => Self::ConditionalMove { a, b, c },
            0x10000000 => Self::ArrayIndex { a, b, c },
            0x20000000 => Self::ArrayAmendment { a, b, c },
            0x30000000 => Self::Addition { a, b, c },
            0x40000000 => Self::Multiplication { a, b, c },
            0x50000000 => Self::Division { a, b, c },
            0x60000000 => Self::NotAnd { a, b, c },
            0x70000000 => Self::Halt,
            0x80000000 => Self::Allocation { b, c },
            0x90000000 => Self::Abandonment { c },
            0xa0000000 => Self::Output { c },
            0xb0000000 => Self::Input { c },
            0xc0000000 => Self::LoadProgram { b, c },
            0xd0000000 => {
                let a = Register::from_u8(((value >> 25) & 0x07) as u8);
                let value = value & 0x01ffffff;
                Self::Orthography { a, value }
            }
            _ => Self::IllegalInstruction,
        }
    }
}

fn decode_ops(ops: &[u32]) -> Vec<Operation> {
    ops.iter()
        .map(|&encoded| Operation::from(encoded))
        .collect()
}

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
        let ops = decode_ops(&program);
        Self {
            memory: vec![program.into()],
            ops,
            ..Default::default()
        }
    }

    /// Initialise a Universal Machine with a program read from a legacy
    /// unsigned 8-bit character scroll.
    pub fn from_bytes(program: impl AsRef<[u8]> + 'a) -> Self {
        fn inner<'a>(bytes: &[u8]) -> Um<'a> {
            let mut program = Vec::with_capacity(bytes.len().div_ceil(std::mem::size_of::<u32>()));

            // Split the program into platters.
            let mut chunks = bytes.chunks_exact(std::mem::size_of::<u32>());
            for word in &mut chunks {
                program.push(u32::from_be_bytes(unsafe {
                    // SAFETY: The `chunks_exact` iterator will *always* emit
                    // a slice of the correct length.
                    word.try_into().unwrap_unchecked()
                }));
            }

            if !chunks.remainder().is_empty() {
                eprintln!(
                    "WARNING: program may be corrupt; {} bytes remain after platter conversion.",
                    chunks.remainder().len()
                );
            }

            Um::new(program)
        }

        inner(program.as_ref())
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
            let ops = decode_ops(duplicated);
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
            index as u32
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
