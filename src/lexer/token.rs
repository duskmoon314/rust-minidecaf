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
 * != NEQ
 * ! Not
 * ~ BitwiseNot
 * % Percent
 * == EQ
 * < LT
 * > GT
 * <= LE
 * >= GE
 * = Assign
 * ? Condition
 * : Colon
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
    Percent,
    EQ,
    NEQ,
    LT,
    GT,
    LE,
    GE,
    Assign,
    Condition,
    Colon,
}

impl Operator {
    pub fn is_unary(&self) -> bool {
        match self {
            Operator::Minus | Operator::Not | Operator::BitwiseNot => true,
            _ => false,
        }
    }
    pub fn is_multiplicative(&self) -> bool {
        match self {
            Operator::Asterisk | Operator::Slash | Operator::Percent => true,
            _ => false,
        }
    }
    pub fn is_additive(&self) -> bool {
        match self {
            Operator::Plus | Operator::Minus => true,
            _ => false,
        }
    }
    pub fn is_relational(&self) -> bool {
        match self {
            Operator::LT | Operator::LE | Operator::GT | Operator::GE => true,
            _ => false,
        }
    }
    pub fn is_equality(&self) -> bool {
        match self {
            Operator::EQ | Operator::NEQ => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Keyword {
    Return,
    If,
    Else,
    For,
    While,
    Do,
    Break,
    Continue,
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
