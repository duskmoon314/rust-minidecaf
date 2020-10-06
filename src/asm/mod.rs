use std::io::Result;
use std::io::Write;

use crate::ir::*;

pub fn write_asm(ir_program: &IRProgram, w: &mut impl Write) -> Result<()> {
    let ir_function = &ir_program.function;
    writeln!(w, "    .text")?;
    writeln!(w, "    .global {}", ir_function.name)?;
    writeln!(w, "{}:", ir_function.name)?;
    for s in &ir_function.statement {
        writeln!(w, "# {:?}", s)?;
        match s {
            IRStatement::Push(int32) => {
                writeln!(w, "    addi  sp, sp, -4")?;
                writeln!(w, "    li    t1, {}", int32)?;
                writeln!(w, "    sw    t1, 0(sp)")?;
            }
            IRStatement::Return => {
                writeln!(w, "    lw    a0, 0(sp)")?;
                writeln!(w, "    addi  sp, sp, 4")?;
                writeln!(w, "    jr    ra")?;
            }
            IRStatement::Neg | IRStatement::Not | IRStatement::LogicalNot => {
                let op = match s {
                    IRStatement::Neg => "neg",
                    IRStatement::Not => "not",
                    IRStatement::LogicalNot => "seqz",
                    _ => panic!("Expecting unary operator"),
                };
                writeln!(w, "    lw    t1, 0(sp)")?;
                writeln!(w, "    {}   t1, t1", op)?;
                writeln!(w, "    sw    t1, 0(sp)")?;
            }
            IRStatement::Add
            | IRStatement::Sub
            | IRStatement::Mul
            | IRStatement::Div
            | IRStatement::Rem => {
                let op = match s {
                    IRStatement::Add => "add",
                    IRStatement::Sub => "sub",
                    IRStatement::Mul => "mul",
                    IRStatement::Div => "div",
                    IRStatement::Rem => "rem",
                    _ => panic!("Expecting binary operator"),
                };
                writeln!(w, "    lw    t2, 0(sp)")?;
                writeln!(w, "    lw    t1, 4(sp)")?;
                writeln!(w, "    addi  sp, sp, 4")?;
                writeln!(w, "    {}   t1, t1, t2", op)?;
                writeln!(w, "    sw    t1, 0(sp)")?;
            }
            _ => (),
        }
    }
    Ok(())
}
