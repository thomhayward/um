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
use crate::reg::Register;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operation {
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

pub fn decode(ops: &[u32]) -> Vec<Operation> {
    ops.iter()
        .map(|&encoded| Operation::from(encoded))
        .collect()
}
