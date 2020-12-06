extern crate nom;
extern crate peek_nth;

use chrono::prelude::*;
use std::fs::{File, OpenOptions};
use std::io::prelude::*;

pub mod asm;
pub mod ir;
pub mod lexer;
pub mod parser;

pub struct Config<'a> {
    pub input: &'a str,
    pub log_file: &'a str,
    pub lexer: bool,
    pub parser: bool,
    pub ir: bool,
}

pub fn run(config: Config, output: &mut impl std::io::Write) -> std::io::Result<()> {
    let local_date: DateTime<Local> = Local::now();
    let log_path = format!("{}.log", config.log_file);

    if config.lexer | config.parser | config.ir {
        let mut file = File::create(&log_path)?;
        writeln!(
            file,
            "Minidecaf in Rust\nAuthor: duskmoon\nLog Date: {}",
            local_date.format("%Y-%m-%d %H:%M:%S").to_string()
        )?;
        writeln!(file, "original code:\n{}", config.input)?;
    }

    let tokens = lexer::lexer(config.input);
    if config.lexer {
        let mut file = OpenOptions::new().append(true).open(&log_path)?;
        writeln!(file, "\nLexer output:\n{:?}", tokens)?;
    }

    let program = parser::parser(tokens);
    if config.parser {
        let mut file = OpenOptions::new().append(true).open(&log_path)?;
        writeln!(file, "\nParser output:\n{:?}", program)?;
    }

    let ir_code = ir::ir(&program);
    if config.ir {
        let mut file = OpenOptions::new().append(true).open(&log_path)?;
        writeln!(file, "\nIR output:\n{:?}", ir_code)?;
    }

    let _asm_code = asm::asm_program(&ir_code, output);

    Ok(())
}
