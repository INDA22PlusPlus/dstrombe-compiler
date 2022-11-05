mod parser;
mod token;
mod test;
mod asm_emitter;
use crate::{parser::{TokenData, Tokenizer, Parser}, token::Token, asm_emitter::AsmCtx};

fn tokenize () ->  Vec<TokenData>{
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
fn main() {
    let tokens = tokenize();
    let tokenizer : Tokenizer = Tokenizer {  };
    let parser = Parser {};
    let a = parser.generate_ast(tokens);
    let mut asm_ctx = asm_emitter::AsmCtx::new(a);
    let asm = asm_ctx.emit();
}


