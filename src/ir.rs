use crate::parser::ast::*;

pub mod command;
pub mod ir;
pub use command::IRStatement;
pub use ir::{IRFunction, IRProgram};

pub fn ir(program: &Program) -> IRProgram {
    ir::ir_program(program)
}
