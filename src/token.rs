#[derive(Clone,Hash, Eq, PartialEq, Debug, Copy)]
pub enum Keyword {

    // types
    Str,
    Integer,
    
    // function
    If,
    Loop,
    Break,

    // fudges
    Print,
}

#[derive(Clone,Hash, Eq, PartialEq, Debug, Copy)]
pub enum Identifier {
    Variable,
}

#[derive(Clone,Hash, Eq, PartialEq, Debug, Copy)]
pub enum Constant {
    Integer,
    Str,
}

// Serve to group other tokens together mostly, for processing in conjunction
// with eg. if-statements or grouping function args together
#[derive(Clone,Hash, Eq, PartialEq, Debug, Copy)]
pub enum SpecialChar {
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    LParen,
    RParen,
    Semicolon,
}
#[derive(Clone,Hash, Eq, PartialEq, Debug, Copy)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mult,
    Div,

    LShift,
    RShift,
    
    Greater,
    Less,
    Equals,

    Or,
    And,
    Xor,
    
}
#[derive(Clone,Hash, Eq, PartialEq, Debug, Copy)]
pub enum AssignmentOperator{
    Assign,
}

#[derive(Clone,Hash, Eq, PartialEq, Debug, Copy)]
pub enum UnaryOperator {
    Not
}


// Main token representation
#[derive(Clone,Hash, Eq, PartialEq, Debug, Copy)]
pub enum Token {
    Keyword(Keyword),
    Identifier(Identifier),
    Constant(Constant),
    SpecialChar(SpecialChar),
    BinaryOperator(BinaryOperator),
    UnaryOperator(UnaryOperator),
    AssignmentOperator(AssignmentOperator)
}

