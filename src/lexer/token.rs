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
        matches!(self, Operator::Minus | Operator::Not | Operator::BitwiseNot)
    }
    pub fn is_multiplicative(&self) -> bool {
        matches!(
            self,
            Operator::Asterisk | Operator::Slash | Operator::Percent
        )
    }
    pub fn is_additive(&self) -> bool {
        matches!(self, Operator::Plus | Operator::Minus)
    }
    pub fn is_relational(&self) -> bool {
        matches!(
            self,
            Operator::LT | Operator::LE | Operator::GT | Operator::GE
        )
    }
    pub fn is_equality(&self) -> bool {
        matches!(self, Operator::EQ | Operator::NEQ)
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
