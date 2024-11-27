#[cfg(feature = "smallvec")]
use smallvec::SmallVec;
#[cfg(feature = "timing")]
use std::time::Instant;
use um::{Operation, Parameter, Platter};

#[cfg(feature = "smallvec")]
const SMALLVEC_SIZE: usize = 24;

fn main() {
    let mut program = Vec::new();
    for arg in std::env::args().skip(1) {
        let p = std::fs::read(arg).unwrap();
        program.extend_from_slice(&p);
    }

    Um::from_bytes(program)
        .stdout(std::io::stdout())
        .stdin(std::io::stdin())
        .run();
}

#[derive(Default)]
pub struct Um {
    program_counter: Platter,
    registers: [Platter; 8],
    #[cfg(feature = "smallvec")]
    memory: Vec<SmallVec<[Platter; SMALLVEC_SIZE]>>,
    #[cfg(not(feature = "smallvec"))]
    memory: Vec<Vec<Platter>>,
    #[cfg(feature = "reclaim-memory")]
    free_blocks: Vec<Platter>,
    ops: Vec<Operation>,
    stdin: Option<Box<dyn std::io::Read>>,
    stdout: Option<Box<dyn std::io::Write>>,
}

impl Um {
    /// Initialise a Universal Machine with the specified program scroll.
    pub fn new(program: Vec<Platter>) -> Self {
        let ops = um::decode_ops(&program);
        Self {
            memory: vec![program.into()],
            ops,
            ..Default::default()
        }
    }

    /// Initialise a Universal Machine with a program read from a legacy
    /// unsigned 8-bit character scroll.
    pub fn from_bytes(program: impl AsRef<[u8]>) -> Self {
        let bytes = program.as_ref();
        let mut program = Vec::with_capacity(bytes.len().div_ceil(size_of::<Platter>()));

        // Split the program into platters.
        let mut chunks = bytes.chunks_exact(size_of::<Platter>());
        for word in &mut chunks {
            program.push(Platter::from_be_bytes([word[0], word[1], word[2], word[3]]));
        }

        if !chunks.remainder().is_empty() {
            eprintln!(
                "WARNING: program may be corrupt; {} bytes remain after platter conversion.",
                chunks.remainder().len()
            );
        }

        Self::new(program)
    }

    /// Sets the output for the universal machine.
    pub fn stdout(mut self, stdout: impl std::io::Write + 'static) -> Self {
        self.stdout.replace(Box::new(stdout));
        self
    }

    /// Sets the input for the universal machine.
    pub fn stdin(mut self, stdin: impl std::io::Read + 'static) -> Self {
        self.stdin.replace(Box::new(stdin));
        self
    }

    /// Begins the spin-cycle of the universal machine.
    pub fn run(mut self) {
        #[cfg(feature = "timing")]
        let start = Instant::now();

        loop {
            match self.ops[self.program_counter as usize] {
                // Operator #0. Conditional Move.
                //
                // The register A receives the value in register B,
                // unless the register C contains 0.
                Operation::ConditionalMove { a, b, c } => {
                    if self.load_register(c) != 0 {
                        self.save_register(a, self.load_register(b));
                    }
                }

                // Operator #1: Array Index.
                //
                // The register A receives the value stored at offset
                // in register C in the array identified by B.
                Operation::ArrayIndex { a, b, c } => {
                    let block = self.load_register(b);
                    let offset = self.load_register(c);
                    self.save_register(a, self.load_memory(block, offset));
                }

                // Operator #2. Array Amendment.
                //
                // The array identified by A is amended at the offset
                // in register B to store the value in register C.
                Operation::ArrayAmendment { a, b, c } => {
                    let block = self.load_register(a);
                    let offset = self.load_register(b);
                    let value = self.load_register(c);
                    self.store_memory(block, offset, value);
                }

                // Operator #3. Addition.
                //
                // The register A receives the value in register B plus
                // the value in register C, modulo 2^32.
                Operation::Addition { a, b, c } => {
                    self.save_register(
                        a,
                        self.load_register(b).wrapping_add(self.load_register(c)),
                    );
                }

                // Operator #4. Multiplication.
                //
                // The register A receives the value in register B times
                // the value in register C, modulo 2^32.
                Operation::Multiplication { a, b, c } => {
                    self.save_register(
                        a,
                        self.load_register(b).wrapping_mul(self.load_register(c)),
                    );
                }

                // Operator #5. Division.
                //
                // The register A receives the value in register B
                // divided by the value in register C, if any, where
                // each quantity is treated as an unsigned 32 bit number.
                Operation::Division { a, b, c } => {
                    self.save_register(
                        a,
                        self.load_register(b).wrapping_div(self.load_register(c)),
                    );
                }

                // Operator #6. Not-And.
                //
                // Each bit in the register A receives the 1 bit if
                // either register B or register C has a 0 bit in that
                // position.  Otherwise the bit in register A receives
                // the 0 bit.
                Operation::NotAnd { a, b, c } => {
                    self.save_register(a, !(self.load_register(b) & self.load_register(c)));
                }

                // Operator #7. Halt.
                //
                // The universal machine stops computation.
                Operation::Halt => break,

                // Operator #8. Allocation.
                //
                // A new array is created with a capacity of platters
                // commensurate to the value in the register C. This
                // new array is initialized entirely with platters
                // holding the value 0. A bit pattern not consisting of
                // exclusively the 0 bit, and that identifies no other
                // active allocated array, is placed in the B register.
                Operation::Allocation { b, c } => {
                    let length = self.load_register(c);
                    let index = self.allocate_memory(length);
                    self.save_register(b, index);
                }

                // Operator #9. Abandonment.
                //
                // The array identified by the register C is abandoned.
                // Future allocations may then reuse that identifier.
                Operation::Abandonment { c } => {
                    let block = self.load_register(c);
                    self.free_memory(block);
                }

                // Operator #10. Output.
                //
                // The value in the register C is displayed on the console
                // immediately. Only values between and including 0 and 255
                // are allowed.
                Operation::Output { c } => {
                    let value = self.load_register(c);
                    if let Some(stdout) = self.stdout.as_mut() {
                        let buffer = [(value & 0xff) as u8];
                        stdout.write_all(&buffer).unwrap();
                    }
                }

                // Operator #11. Input.
                //
                // The universal machine waits for input on the console.
                // When input arrives, the register C is loaded with the
                // input, which must be between and including 0 and 255.
                // If the end of input has been signaled, then the
                // register C is endowed with a uniform value pattern
                // where every place is pregnant with the 1 bit.
                Operation::Input { c } => {
                    if let Some(stdin) = self.stdin.as_mut() {
                        let mut buffer = vec![0];
                        match stdin.read_exact(&mut buffer) {
                            Ok(()) => self.save_register(c, buffer[0] as u32),
                            Err(_) => self.save_register(c, 0xff),
                        }
                    } else {
                        self.save_register(c, 0xff);
                    }
                }

                // Operator #12. Load Program.
                //
                // The array identified by the B register is duplicated
                // and the duplicate shall replace the '0' array,
                // regardless of size. The execution finger is placed
                // to indicate the platter of this array that is
                // described by the offset given in C, where the value
                // 0 denotes the first platter, 1 the second, et
                // cetera.
                //
                // The '0' array shall be the most sublime choice for
                // loading, and shall be handled with the utmost
                // velocity.
                Operation::LoadProgram { b, c } => {
                    let block = self.load_register(b);

                    // Source array is always copied to array[0], but there
                    // is no point copying array[0] to array[0].
                    if block != 0 {
                        let duplicated = self.duplicate_memory(block);
                        let ops = um::decode_ops(&duplicated);
                        self.ops = ops;
                    }

                    self.program_counter = self.load_register(c);
                    continue;
                }

                // Operator #13. Orthography.
                //
                // The value indicated is loaded into the register A
                // forthwith.
                Operation::Orthography { a, value } => {
                    self.save_register(a, value);
                }

                Operation::IllegalInstruction => self.panic(),
            }

            self.program_counter += 1;
        }

        #[cfg(feature = "timing")]
        eprintln!("um complete: {:?}", start.elapsed());
    }

    /// Loads the value from the specified register.
    fn load_register(&self, index: Parameter) -> Platter {
        assert!(index < 8, "register index out of bounds");
        self.registers[index as usize]
    }

    /// Saves a value to the specified register.
    fn save_register(&mut self, index: Parameter, value: Platter) {
        assert!(index < 8, "register index out of bounds");
        self.registers[index as usize] = value;
    }

    fn load_memory(&self, block: Platter, offset: Platter) -> Platter {
        assert!(
            (block as usize) < self.memory.len()
                && (offset as usize) < self.memory[block as usize].len()
        );
        self.memory[block as usize][offset as usize]
    }

    fn store_memory(&mut self, block: Platter, offset: Platter, value: Platter) {
        assert!(
            (block as usize) < self.memory.len()
                && (offset as usize) < self.memory[block as usize].len()
        );
        self.memory[block as usize][offset as usize] = value;
    }

    fn duplicate_memory(&mut self, block: Platter) -> &[Platter] {
        assert!((block as usize) < self.memory.len());
        self.memory[0] = self.memory[block as usize].clone();
        &self.memory[0]
    }

    #[cfg(not(feature = "reclaim-memory"))]
    fn allocate_memory(&mut self, length: Platter) -> Platter {
        self.memory.push(Self::new_block(length as usize));
        (self.memory.len() - 1) as Platter
    }

    #[cfg(feature = "reclaim-memory")]
    fn allocate_memory(&mut self, length: Platter) -> Platter {
        if let Some(index) = self.free_blocks.pop() {
            self.memory[index as usize] = Self::new_block(length as usize);
            index as Platter
        } else {
            self.memory.push(Self::new_block(length as usize));
            (self.memory.len() - 1) as Platter
        }
    }

    fn free_memory(&mut self, block: Platter) {
        assert!((block as usize) < self.memory.len());
        #[cfg(feature = "reclaim-memory")]
        {
            self.free_blocks.push(block);
            self.memory[block as usize] = Self::new_block(0);
        }
    }

    #[cold]
    #[inline(never)]
    fn panic(&self) -> ! {
        panic!(
            "universal machine failure: instruction: {:08x}, program_counter: {:08x}, registers: {:08x?}",
            self.memory[0][self.program_counter as usize], self.program_counter, self.registers
        )
    }

    #[cfg(feature = "smallvec")]
    fn new_block(len: usize) -> SmallVec<[Platter; SMALLVEC_SIZE]> {
        smallvec::smallvec![0; len]
    }

    #[cfg(not(feature = "smallvec"))]
    fn new_block(len: usize) -> Vec<Platter> {
        vec![0; len]
    }
}
