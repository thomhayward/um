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
mod lexer;
mod parse;

use crate::Register;
use lexer::Token;
use parse::{Instruction, Node, NodeType, PragmaType};
use std::collections::HashMap;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
enum Section {
    Text,
    Data,
}

pub fn assemble<'s>(source: &'s str) -> Vec<u32> {
    let parsed = parse::parse("", source).unwrap();

    let mut sections: HashMap<Section, Vec<&Node<'s>>> = HashMap::new();
    let mut offsets: HashMap<Section, usize> = HashMap::new();
    let mut label_locations: HashMap<&'s str, (Section, usize)> = HashMap::new();
    for node in parsed.nodes().iter() {
        match node.entity {
            NodeType::Pragma(_) => {
                let loc = *offsets
                    .entry(Section::Data)
                    .and_modify(|loc| *loc += node.size())
                    .or_default();

                sections
                    .entry(Section::Data)
                    .and_modify(|section| section.push(node))
                    .or_insert(vec![node]);

                for label in &node.labels {
                    label_locations.insert(label, (Section::Data, loc));
                }
            }
            NodeType::Instruction(_) => {
                let loc = *offsets
                    .entry(Section::Text)
                    .and_modify(|loc| *loc += node.size())
                    .or_default();

                sections
                    .entry(Section::Text)
                    .and_modify(|section| section.push(node))
                    .or_insert(vec![node]);

                for label in &node.labels {
                    label_locations.insert(label, (Section::Text, loc));
                }
            }
            _ => {}
        }
    }

    let text = sections.remove(&Section::Text).unwrap();
    let data_offset = text.len();

    let mut program = vec![];
    for node in text.into_iter() {
        let NodeType::Instruction(instruction) = &node.entity else {
            panic!("invalid node in .text section");
        };

        let encoded = match instruction {
            Instruction::ConditionalMove {
                destination,
                source,
                condition,
            } => encode_standard(0x00, destination, source, condition),
            Instruction::Load {
                destination,
                address,
            } => {
                let parse::Location { block, offset } = address;
                encode_standard(0x01, destination, block, offset)
            }
            Instruction::Store { source, address } => {
                let parse::Location { block, offset } = address;
                encode_standard(0x02, block, offset, source)
            }
            Instruction::Add { destination, a, b } => encode_standard(0x03, destination, a, b),
            Instruction::AddAssign { destination, a } => {
                encode_standard(0x03, destination, destination, a)
            }
            Instruction::AddSelf { destination } => {
                encode_standard(0x03, destination, destination, destination)
            }
            Instruction::Mul { destination, a, b } => encode_standard(0x04, destination, a, b),
            Instruction::MulAssign { destination, a } => {
                encode_standard(0x04, destination, destination, a)
            }
            Instruction::MulSelf { destination } => {
                encode_standard(0x04, destination, destination, destination)
            }
            Instruction::Div { destination, a, b } => encode_standard(0x05, destination, a, b),
            Instruction::DivAssign { destination, a } => {
                encode_standard(0x05, destination, destination, a)
            }
            Instruction::DivSelf { destination } => {
                encode_standard(0x05, destination, destination, destination)
            }
            Instruction::Nand { destination, a, b } => encode_standard(0x06, destination, a, b),
            Instruction::NandAssign { destination, a } => {
                encode_standard(0x06, destination, destination, a)
            }
            Instruction::NandSelf { destination } => {
                encode_standard(0x06, destination, destination, destination)
            }
            Instruction::Halt => encode_standard(
                0x07,
                &Default::default(),
                &Default::default(),
                &Default::default(),
            ),
            Instruction::Alloc {
                destination,
                length,
            } => encode_standard(0x08, &Register::default(), destination, length),
            Instruction::Free { block } => {
                encode_standard(0x09, &Register::default(), &Register::default(), block)
            }
            Instruction::Out { source } => {
                encode_standard(0x0a, &Default::default(), &Default::default(), source)
            }
            Instruction::In { destination } => {
                encode_standard(0x0b, &Default::default(), &Default::default(), destination)
            }
            Instruction::Jmp { location } => {
                let parse::Location { block, offset } = location;
                encode_standard(0x0c, &Register::default(), block, offset)
            }
            Instruction::Address {
                destination,
                reference,
            } => {
                // lookup reference
                let Some((section, offset)) = label_locations.get(reference.label) else {
                    panic!("failed to resolve {}", reference.label);
                };

                let value = match section {
                    Section::Text => *offset,
                    Section::Data => data_offset + *offset,
                };

                0xd0000000 | destination.encode_a_ortho() | encode_literal(value as u32)
            }
            Instruction::LiteralMove {
                destination,
                literal,
            } => 0xd0000000 | destination.encode_a_ortho() | encode_literal(*literal),
        };

        program.push(encoded);
    }

    if let Some(data) = sections.remove(&Section::Data) {
        for node in data.into_iter() {
            let NodeType::Pragma(pragma) = &node.entity else {
                panic!("invalid node in .data section. {node:?}");
            };

            let encoded = match &pragma.payload {
                PragmaType::WideString { value } => {
                    for byte in value.as_bytes() {
                        program.push(*byte as u32);
                    }
                    Some(0) // terminating byte.
                }
                PragmaType::U32 { value } => Some(*value),
            };

            if let Some(encoded) = encoded {
                program.push(encoded);
            }
        }
    }

    program
}

fn encode_literal(value: u32) -> u32 {
    const LITERAL_MAX: u32 = 0x1ffffff;
    assert!(value <= LITERAL_MAX, "literal value exceeds available bits. value: {value} (0x{value:x}), max: {LITERAL_MAX} (0x{LITERAL_MAX:x})");
    value
}

fn encode_standard(op: u32, a: &Register, b: &Register, c: &Register) -> u32 {
    (op << 28) | a.encode_a() | b.encode_b() | c.encode_c()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Operation, Register::*};

    #[test]
    fn wide_str() {
        // Embed a wide string and get a reference to it.
        let program = assemble(
            r#"
                adr r0, msg
                msg: .wstr "Hello"
            "#,
        );

        let ops = crate::ops::decode(&program);
        assert_eq!(ops[0], Operation::Orthography { a: R0, value: 1 });

        let mut platters = program.into_iter().skip(1);
        assert_eq!(platters.next(), Some('H' as u32));
        assert_eq!(platters.next(), Some('e' as u32));
        assert_eq!(platters.next(), Some('l' as u32));
        assert_eq!(platters.next(), Some('l' as u32));
        assert_eq!(platters.next(), Some('o' as u32));
        assert_eq!(platters.next(), Some(0));
        assert_eq!(platters.next(), None);
    }

    #[test]
    fn addresses() {
        let program = assemble(
            r#"
                halt
            start:
                ldr r2, [r0, r1]
                str r2, [r0, r1]
                adr r3, start
                halt
            "#,
        );

        let mut ops = crate::ops::decode(&program).into_iter();

        assert_eq!(ops.next(), Some(Operation::Halt));
        assert_eq!(
            ops.next(),
            Some(Operation::ArrayIndex {
                a: R2,
                b: R0,
                c: R1
            })
        );
        assert_eq!(
            ops.next(),
            Some(Operation::ArrayAmendment {
                a: R0,
                b: R1,
                c: R2
            })
        );
        assert_eq!(ops.next(), Some(Operation::Orthography { a: R3, value: 1 }));
        assert_eq!(ops.next(), Some(Operation::Halt));
        assert_eq!(ops.next(), None);
    }

    #[test]
    fn load_store() {
        let state = crate::Um::new(assemble(
            r#"
                adr r1, loc
                ldr r2, [r0, r1]
                mov r3, 56
                str r3, [r0, r1]
                halt
            loc:.u32 42
            "#,
        ))
        .run();
        assert_eq!(state.registers[R2], 42);
        assert_eq!(state.memory[0][5], 56);
    }

    #[test]
    fn addition() {
        let state = crate::Um::new(assemble(
            r#"
                mov r0, 42
                mov r1, 64
                mov r2, 8192

                add r3, r0, r1 ; r3 = r0 + r1 = 106
                add r1, r2     ; r1 = r1 + r2 = 8256
                add r0         ; r0 = r0 + r0 = 84
                
                halt
            "#,
        ))
        .run();

        assert_eq!(state.registers[R0], 84);
        assert_eq!(state.registers[R1], 8256);
        assert_eq!(state.registers[R2], 8192);
        assert_eq!(state.registers[R3], 106);
    }

    #[test]
    fn alloc() {
        let state = crate::Um::new(assemble(
            r#"
                ; Allocate 1000 bytes.
                mov   r0, 1000
                alloc r1, r0
                halt
            "#,
        ))
        .run();
        assert_eq!(state.registers[R0], 1000);
        assert_ne!(state.registers[R1], 0);
        assert_eq!(state.memory[state.registers[R1] as usize].len(), 1000);
    }

    #[test]
    fn free() {
        let state = crate::Um::new(assemble(
            r#"
                ; Allocate 1000 bytes.
                mov   r0, 1000
                alloc r1, r0
                free  r1
                halt
            "#,
        ))
        .run();
        assert_eq!(state.registers[R0], 1000);
        assert_ne!(state.registers[R1], 0);
        assert_eq!(
            state.memory[state.registers[R1] as usize].len(),
            0,
            "memory not free'd"
        );
    }
}
