/*
 * Symbol
 * ( Lparen
 * ) Rparen
 * [ Lbrack
 * ] Rbrack
 * { Lbrace
 * } Rbrace
 * ; Semicolon
 * , Comma
**/

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Symbol {
    Lparen,
    Rparen,
    Lbrack,
    Rbrack,
    Lbrace,
    Rbrace,
    Semicolon,
    Comma,
    EOF,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Keyword {
    Return,
    For,
    If,
    Else,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Type {
    Int,
    Double,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Symbol(Symbol),
    Keyword(Keyword),
    Type(Type),
    Identifier(String),
    Integer(i32),
}
