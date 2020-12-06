use std::io::{Result, Write};

use crate::ir::*;

pub fn asm_program(ir_program: &IRProgram, w: &mut impl Write) -> Result<()> {
    // code
    writeln!(w, "\n    .text")?;
    writeln!(w, "    j     main")?;
    asm_function(&ir_program.function, w)?;
    Ok(())
}

fn asm_function(ir_function: &IRFunction, w: &mut impl Write) -> Result<()> {
    writeln!(w, "\n    .global {}", ir_function.name)?;
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
