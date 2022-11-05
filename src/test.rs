use crate::{*, parser::{TokenData, Tokenizer, Parser}, token::Token, asm_emitter::AsmCtx};
use crate::{asm_emitter};
fn tokenize_example () ->  Vec<TokenData>{
    let src : String = 
    "
    int i = 0;
    if(i == 0) {
        int a = 5;
        int b = 9;
        a = b | 9 + 2;
        print a;
    }
    loop {
        print \"hello\";
        if(1 == 5 - 4) {
            break;
        }
    }".to_string();
    let mut testtokenizer = parser::Tokenizer {};
    let tokendata = testtokenizer.tokenize(src).unwrap();
    tokendata
}

#[test] 
fn tokenize_test() {
    let tokendata = tokenize_example();
    println!("{:?}", tokendata[3].token);

    assert!(tokendata[5].token == token::Token::Keyword(token::Keyword::If));
    assert!(tokendata[6].token == token::Token::SpecialChar(token::SpecialChar::LParen));
    assert!(tokendata[7].token == token::Token::Identifier(token::Identifier::Variable));
    assert!(tokendata[8].token == token::Token::BinaryOperator(token::BinaryOperator::Equals));

    assert!(tokendata[12].token == token::Token::Keyword(token::Keyword::Integer));

}
#[test]
fn ast_test() {
    println!("lol");
    let tokens = tokenize_example();
    for (i, tkn) in tokens.iter().enumerate() {
        println!("{:?}: {:?}", i, tkn.token);
    }
    let tokenizer : Tokenizer = Tokenizer {  };
    let parser = Parser {};
    let a = parser.generate_ast(tokens);
    println!("{:#?}", a);
    assert!(true);
}

#[test]
fn emissions_test() {
    println!("lol");
    let tokens = tokenize_example();
    for (i, tkn) in tokens.iter().enumerate() {
        println!("{:?}: {:?}", i, tkn.token);
    }
    let tokenizer : Tokenizer = Tokenizer {  };
    let parser = Parser {};
    let a = parser.generate_ast(tokens);
    let mut asm_ctx = asm_emitter::AsmCtx::new(a);
    let asm = asm_ctx.emit();
}

