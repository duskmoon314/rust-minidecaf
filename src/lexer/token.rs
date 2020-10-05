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

/*
 * Operator
 * + Plus
 * - Minus
 * * Asterisk
 * / Slash
 * \ BackSlash
 * && And
 * & BitwiseAnd
 * || Or
 * | BitwiseOr
 * ! Not
 * ~ BitwiseNot
 */

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {
    Plus,
    Minus,
    Asterisk,
    Slash,
    BackSlash,
    And,
    BitwiseAnd,
    Or,
    BitwiseOr,
    Not,
    BitwiseNot,
}

impl Operator {
    pub fn is_unary(&self) -> bool {
        match self {
            Operator::Minus | Operator::Not | Operator::BitwiseNot => true,
            _ => false,
        }
    }
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
    Operator(Operator),
    Keyword(Keyword),
    Type(Type),
    Identifier(String),
    Integer(i32),
}
