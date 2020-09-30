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
        }
    }
    Ok(())
}
