pub type Platter = u32;
pub type Parameter = u8;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operation {
    ConditionalMove {
        a: Parameter,
        b: Parameter,
        c: Parameter,
    },
    ArrayIndex {
        a: Parameter,
        b: Parameter,
        c: Parameter,
    },
    ArrayAmendment {
        a: Parameter,
        b: Parameter,
        c: Parameter,
    },
    Addition {
        a: Parameter,
        b: Parameter,
        c: Parameter,
    },
    Multiplication {
        a: Parameter,
        b: Parameter,
        c: Parameter,
    },
    Division {
        a: Parameter,
        b: Parameter,
        c: Parameter,
    },
    NotAnd {
        a: Parameter,
        b: Parameter,
        c: Parameter,
    },
    Halt,
    Allocation {
        b: Parameter,
        c: Parameter,
    },
    Abandonment {
        c: Parameter,
    },
    Output {
        c: Parameter,
    },
    Input {
        c: Parameter,
    },
    LoadProgram {
        b: Parameter,
        c: Parameter,
    },
    Orthography {
        a: Parameter,
        value: u32,
    },
    IllegalInstruction,
}

impl From<Platter> for Operation {
    #[inline]
    fn from(value: Platter) -> Self {
        let a = ((value >> 6) & 0x07) as Parameter;
        let b = ((value >> 3) & 0x07) as Parameter;
        let c = (value & 0x07) as Parameter;

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
                let a = ((value >> 25) & 0x07) as Parameter;
                let value = value & 0x01ffffff;
                Self::Orthography { a, value }
            }
            _ => Self::IllegalInstruction,
        }
    }
}

#[inline]
pub fn decode_ops(ops: &[Platter]) -> Vec<Operation> {
    ops.iter()
        .map(|&encoded| Operation::from(encoded))
        .collect()
}
