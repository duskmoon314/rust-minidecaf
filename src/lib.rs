extern crate nom;
extern crate peek_nth;

use chrono::prelude::*;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::prelude::*;

pub mod asm;
pub mod ir;
pub mod lexer;
pub mod parser;

pub struct LogOpts {
    pub log_file: String,
    pub lexer: bool,
    pub parser: bool,
    pub ir: bool,
}

pub fn run(_input: &str, _output: &mut impl std::io::Write, log: LogOpts) -> std::io::Result<()> {
    // Trying Generator
    //
    // let mut test_lex_generator = lexer::LexGenerator::new(_input);
    // loop {
    //     let tk = test_lex_generator.next().unwrap();
    //     match tk {
    //         lexer::token::Token::Symbol(lexer::token::Symbol::EOF) => {
    //             println!("EOF");
    //             break;
    //         }
    //         _ => println!("{:?}", tk),
    //     }
    // }

    let local_date: DateTime<Local> = Local::now();
    let path = format!(
        "{}{}.log",
        log.log_file,
        &local_date.format("%Y-%m-%d").to_string()
    );
    if log.lexer | log.parser | log.ir {
        let mut file = File::create(&path)?;
        writeln!(
            file,
            "Minidecaf in Rust\nAuthor: duskmoon\nLog Date: {}",
            local_date.format("%Y-%m-%d %H:%M:%S").to_string()
        )?;
        writeln!(file, "original code:\n{}", _input)?;
    }

    let tokens = lexer::lexer(_input);
    if log.lexer {
        let mut file = OpenOptions::new().append(true).open(&path)?;
        writeln!(file, "\nLexer output:\n{:?}", tokens)?;
    }

    let program = parser::parser(tokens);
    if log.parser {
        let mut file = OpenOptions::new().append(true).open(&path)?;
        writeln!(file, "\nParser output:\n{:?}", program)?;
    }

    let ir_code = ir::ir(&program);
    if log.ir {
        let mut file = OpenOptions::new().append(true).open(&path)?;
        writeln!(file, "\nIR output:\n{:?}", ir_code)?;
    }
    let _asm_code = asm::asm_program(&ir_code, _output);
    Ok(())
}
