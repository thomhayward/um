use smallvec::SmallVec;
use std::io::{Read, Write};
#[cfg(feature = "timing")]
use std::time::Instant;
use um::{Operation, Parameter, Platter};

const SMALLVEC_SIZE: usize = 24;

fn main() {
    let mut program = Vec::new();
    for arg in std::env::args().skip(1) {
        let p = std::fs::read(arg).unwrap();
        program.extend_from_slice(&p);
    }

    Um::from_bytes(program)
        .stdout(&mut std::io::stdout())
        .stdin(&mut std::io::stdin())
        .run();
}

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
impl_into_index!(Platter);
impl_into_index!(Parameter);

#[derive(Default)]
pub struct Um<'a> {
    program_counter: Platter,
    registers: [Platter; 8],
    memory: Vec<SmallVec<[Platter; SMALLVEC_SIZE]>>,
    #[cfg(feature = "reclaim-memory")]
    free_blocks: Vec<Platter>,
    ops: Vec<Operation>,
    stdin: Option<&'a mut dyn Read>,
    stdout: Option<&'a mut dyn Write>,
}

impl<'a> Um<'a> {
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
    pub fn run(mut self) -> Self {
        #[cfg(feature = "timing")]
        let start = Instant::now();

        loop {
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

        #[cfg(feature = "timing")]
        eprintln!("um complete: {:?}", start.elapsed());

        self
    }

    /// Loads the value from the specified register.
    fn load_register(&self, index: Parameter) -> Platter {
        assert!(index < 8, "register index out of bounds");
        self.registers[index.into_index()]
    }

    /// Saves a value to the specified register.
    fn save_register(&mut self, index: Parameter, value: Platter) {
        assert!(index < 8, "register index out of bounds");
        self.registers[index.into_index()] = value;
    }

    pub fn conditional_move(&mut self, a: Parameter, b: Parameter, c: Parameter) {
        if self.load_register(c) != 0 {
            self.save_register(a, self.load_register(b));
        }
    }

    pub fn array_index(&mut self, a: Parameter, b: Parameter, c: Parameter) {
        let block = self.load_register(b);
        let offset = self.load_register(c);
        self.save_register(a, self.load_memory(block, offset));
    }

    pub fn array_amendment(&mut self, a: Parameter, b: Parameter, c: Parameter) {
        let block = self.load_register(a);
        let offset = self.load_register(b);
        let value = self.load_register(c);
        self.store_memory(block, offset, value);
    }

    pub fn addition(&mut self, a: Parameter, b: Parameter, c: Parameter) {
        self.save_register(a, self.load_register(b).wrapping_add(self.load_register(c)));
    }

    pub fn multiplication(&mut self, a: Parameter, b: Parameter, c: Parameter) {
        self.save_register(a, self.load_register(b).wrapping_mul(self.load_register(c)));
    }

    pub fn division(&mut self, a: Parameter, b: Parameter, c: Parameter) {
        self.save_register(a, self.load_register(b).wrapping_div(self.load_register(c)));
    }

    pub fn not_and(&mut self, a: Parameter, b: Parameter, c: Parameter) {
        self.save_register(a, !(self.load_register(b) & self.load_register(c)));
    }

    pub fn allocation(&mut self, b: Parameter, c: Parameter) {
        let length = self.load_register(c);
        let index = self.allocate_memory(length);
        self.save_register(b, index);
    }

    pub fn abandonment(&mut self, c: Parameter) {
        let block = self.load_register(c);
        self.free_memory(block);
    }

    pub fn output(&mut self, c: Parameter) {
        let value = self.load_register(c);
        if let Some(stdout) = self.stdout.as_mut() {
            let buffer = [(value & 0xff) as u8];
            stdout.write_all(&buffer).unwrap();
        }
    }

    pub fn input(&mut self, c: Parameter) {
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

    pub fn load_program(&mut self, b: Parameter, c: Parameter) {
        let block = self.load_register(b);

        // Source array is always copied to array[0], but there
        // is no point copying array[0] to array[0].
        if block != 0 {
            let duplicated = self.duplicate_memory(block);
            let ops = um::decode_ops(duplicated);
            self.ops = ops;
        }

        self.program_counter = self.load_register(c);
    }

    pub fn orthography(&mut self, a: Parameter, value: Platter) {
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

    fn load_memory(&self, block: Platter, offset: Platter) -> Platter {
        let block = block.into_index();
        let offset = offset.into_index();
        assert!(block < self.memory.len() && offset < self.memory[block].len());
        self.memory[block][offset]
    }

    fn store_memory(&mut self, block: Platter, offset: Platter, value: Platter) {
        let block = block.into_index();
        let offset = offset.into_index();
        assert!(block < self.memory.len() && offset < self.memory[block].len());
        self.memory[block][offset] = value
    }

    fn duplicate_memory(&mut self, block: Platter) -> &[Platter] {
        let block = block.into_index();
        assert!(block < self.memory.len());
        self.memory[0] = self.memory[block].clone();
        &self.memory[0]
    }

    #[cfg(not(feature = "reclaim-memory"))]
    fn allocate_memory(&mut self, length: Platter) -> Platter {
        self.memory.push(Self::new_block(length.into_index()));
        (self.memory.len() - 1) as Platter
    }

    #[cfg(feature = "reclaim-memory")]
    fn allocate_memory(&mut self, length: Platter) -> Platter {
        if let Some(index) = self.free_blocks.pop() {
            self.memory[index.into_index()] = Self::new_block(length.into_index());
            index as Platter
        } else {
            self.memory.push(Self::new_block(length.into_index()));
            (self.memory.len() - 1) as Platter
        }
    }

    fn free_memory(&mut self, block: Platter) {
        assert!(block.into_index() < self.memory.len());
        #[cfg(feature = "reclaim-memory")]
        {
            self.free_blocks.push(block);
            self.memory[block.into_index()] = Self::new_block(0);
        }
    }

    fn new_block(len: usize) -> SmallVec<[Platter; SMALLVEC_SIZE]> {
        smallvec::smallvec![0; len]
    }
}
