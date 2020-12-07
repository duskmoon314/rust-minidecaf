use crate::parser::ast::*;

pub mod command;
pub mod irst;
pub use command::IRStatement;
pub use irst::{IRFunction, IRProgram};

pub fn ir(program: &Program) -> IRProgram {
    irst::ir_program(program)
}
