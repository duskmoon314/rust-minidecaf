use std::cmp::Ordering;
use std::io::Result;
use std::io::Write;

use crate::ir::*;

pub fn asm_program(ir_program: &IRProgram, w: &mut impl Write) -> Result<()> {
    writeln!(w, "    .text")?;
    writeln!(w, "    j     main")?;
    for func in &ir_program.functions {
        asm_function(func, w)?;
    }
    Ok(())
}

#[allow(unreachable_patterns)]
pub fn asm_function(ir_function: &IRFunction, w: &mut impl Write) -> Result<()> {
    writeln!(w, "\n    .global {}", ir_function.name)?;
    writeln!(w, "{}:", ir_function.name)?;

    writeln!(w, "{}_prologue:", ir_function.name)?;
    let frame_size: u32 = (ir_function.var_max - ir_function.param_cnt) * 4 + 8;
    writeln!(w, "    addi  sp, sp, -{}", frame_size)?;
    writeln!(w, "    sw    ra, {}(sp)", frame_size - 4)?;
    writeln!(w, "    sw    fp, {}(sp)", frame_size - 8)?;
    writeln!(w, "    addi  fp, sp, {}", frame_size)?;

    for s in &ir_function.statements {
        writeln!(w, "# {:?}", s)?;
        match s {
            IRStatement::Comment(_) => {
                // Use {:?} write comment already
            }
            IRStatement::Push(int32) => {
                writeln!(w, "    addi  sp, sp, -4")?;
                writeln!(w, "    li    t1, {}", int32)?;
                writeln!(w, "    sw    t1, 0(sp)")?;
            }
            IRStatement::Return => {
                writeln!(w, "    lw    a0, 0(sp)")?;
                writeln!(w, "    addi  sp, sp, 4")?;
                writeln!(w, "    j     {}_epilogue", ir_function.name)?;
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
            | IRStatement::Rem
            | IRStatement::GT
            | IRStatement::LT => {
                let op = match s {
                    IRStatement::Add => "add",
                    IRStatement::Sub => "sub",
                    IRStatement::Mul => "mul",
                    IRStatement::Div => "div",
                    IRStatement::Rem => "rem",
                    IRStatement::GT => "sgt",
                    IRStatement::LT => "slt",
                    _ => panic!("Expecting binary operator"),
                };
                writeln!(w, "    lw    t2, 0(sp)")?;
                writeln!(w, "    lw    t1, 4(sp)")?;
                writeln!(w, "    addi  sp, sp, 4")?;
                writeln!(w, "    {}   t1, t1, t2", op)?;
                writeln!(w, "    sw    t1, 0(sp)")?;
            }
            IRStatement::GE | IRStatement::LE => {
                let op = match s {
                    IRStatement::GE => "slt",
                    IRStatement::LE => "sgt",
                    _ => panic!("Expecting binary operator"),
                };
                writeln!(w, "    lw    t2, 0(sp)")?;
                writeln!(w, "    lw    t1, 4(sp)")?;
                writeln!(w, "    addi  sp, sp, 4")?;
                writeln!(w, "    {}   t1, t1, t2", op)?;
                writeln!(w, "    xori  t1, t1, 1")?;
                writeln!(w, "    sw    t1, 0(sp)")?;
            }
            IRStatement::EQ | IRStatement::NEQ => {
                let op = match s {
                    IRStatement::EQ => "seqz",
                    IRStatement::NEQ => "snez",
                    _ => panic!("Expecting binary operator"),
                };
                writeln!(w, "    lw    t2, 0(sp)")?;
                writeln!(w, "    lw    t1, 4(sp)")?;
                writeln!(w, "    addi  sp, sp, 4")?;
                writeln!(w, "    sub   t1, t1, t2")?;
                writeln!(w, "    {}  t1, t1", op)?;
                writeln!(w, "    sw    t1, 0(sp)")?;
            }
            IRStatement::LogicalAnd => {
                writeln!(w, "    lw    t2, 0(sp)")?;
                writeln!(w, "    lw    t1, 4(sp)")?;
                writeln!(w, "    addi  sp, sp, 4")?;
                writeln!(w, "    snez  t1, t1")?;
                writeln!(w, "    snez  t2, t2")?;
                writeln!(w, "    and   t1, t1, t2")?;
                writeln!(w, "    sw    t1, 0(sp)")?;
            }
            IRStatement::LogicalOr => {
                writeln!(w, "    lw    t2, 0(sp)")?;
                writeln!(w, "    lw    t1, 4(sp)")?;
                writeln!(w, "    addi  sp, sp, 4")?;
                writeln!(w, "    or    t1, t1, t2")?;
                writeln!(w, "    snez  t1, t1")?;
                writeln!(w, "    sw    t1, 0(sp)")?;
            }
            IRStatement::FrameAddr(idx) => {
                writeln!(w, "    li    t2, {}", idx)?;
                writeln!(w, "    slli  t2, t2, 2")?;
                match idx.cmp(&ir_function.param_cnt) {
                    Ordering::Less => {
                        writeln!(w, "    add   t1, fp, t2")?;
                        writeln!(w, "    addi  sp, sp, -4")?;
                        writeln!(w, "    sw    t1, 0(sp)")?;
                    }
                    _ => {
                        writeln!(w, "    addi  t2, t2, 12")?;
                        writeln!(w, "    sub   t1, fp, t2")?;
                        writeln!(w, "    addi  sp, sp, -4")?;
                        writeln!(w, "    sw    t1, 0(sp)")?;
                    }
                }
            }
            IRStatement::Load => {
                writeln!(w, "    lw    t1, 0(sp)")?;
                writeln!(w, "    lw    t1, 0(t1)")?;
                writeln!(w, "    sw    t1, 0(sp)")?;
            }
            IRStatement::Store => {
                writeln!(w, "    lw    t1, 4(sp)")?;
                writeln!(w, "    lw    t2, 0(sp)")?;
                writeln!(w, "    addi  sp, sp, 4")?;
                writeln!(w, "    sw    t1, 0(t2)")?;
            }
            IRStatement::Pop => {
                writeln!(w, "    addi  sp, sp, 4")?;
            }
            IRStatement::Label(l) => {
                writeln!(w, "{}:", l)?;
            }
            IRStatement::Br(l) => {
                writeln!(w, "    j     {}", l)?;
            }
            IRStatement::Beqz(l) | IRStatement::Bnez(l) => {
                let op = match s {
                    IRStatement::Beqz(_) => "beqz",
                    IRStatement::Bnez(_) => "bnez",
                    _ => panic!("Expecting branch operation"),
                };
                writeln!(w, "    lw    t1, 0(sp)")?;
                writeln!(w, "    addi  sp, sp, 4")?;
                writeln!(w, "    {}  t1, {}", op, l)?;
            }
            IRStatement::Call(func_name, params) => {
                writeln!(w, "    jal   {}", func_name)?;
                for _i in 1..*params {
                    writeln!(w, "    addi  sp, sp, 4")?;
                }
                if *params == 0 {
                    writeln!(w, "    addi  sp, sp, -4")?;
                }
                writeln!(w, "    sw    a0, 0(sp)")?;
            }
            _ => (),
        }
    }

    writeln!(w, "{}_epilogue:", ir_function.name)?;
    writeln!(w, "    lw    fp, {}(sp)", frame_size - 8)?;
    writeln!(w, "    lw    ra, {}(sp)", frame_size - 4)?;
    writeln!(w, "    addi  sp, sp, {}", frame_size)?;
    writeln!(w, "    ret")?;
    Ok(())
}
