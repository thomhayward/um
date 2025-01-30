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
    pub fn encode_a(self) -> u32 {
        ((self as u32) & 0x7) << 6
    }

    /// Encodes the register as the 'b' parameter of an encoded
    /// instruction (bits 3..=5).
    pub fn encode_b(self) -> u32 {
        ((self as u32) & 0x7) << 3
    }

    /// Encodes the register as the 'c' parameter of an encoded
    /// instruction (bits 0..=2).
    pub fn encode_c(self) -> u32 {
        (self as u32) & 0x7
    }

    /// Encodes the register as the 'a' parameter of an `Orthography`
    /// operation.
    ///
    /// This is *only* valid for `Orthography` operations.
    pub fn encode_a_ortho(self) -> u32 {
        ((self as u32) & 0x7) << 25
    }

    pub fn from_u8(index: u8) -> Self {
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
