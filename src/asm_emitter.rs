
/* */
use crate::token::{self, Identifier, BinaryOperator};
use crate::token::{Token, AssignmentOperator::*, BinaryOperator::*, UnaryOperator::*, Keyword::*, SpecialChar::*, Constant::*, Identifier::*};
use crate::parser::{*, GrammarType, Grammar, SyntaxNode};
#[derive(Clone)]
pub struct ScopeFrame {
    pub symbol_table : Vec<String>
}
impl ScopeFrame {
    fn new() -> Self {
        ScopeFrame { symbol_table : Vec::new() }
    }
}
#[derive(Clone)]

pub struct AsmCtx {
    pub label_idx : i32,
    
    pub scope_stack : Vec<ScopeFrame>,
    pub data_section : String,
    pub master_node : SyntaxNode,
}

impl AsmCtx {
    pub fn new(master_node : SyntaxNode) -> Self {
        AsmCtx { label_idx: 0, scope_stack: Vec::new(), data_section : String::new(), master_node : master_node.clone() }
    }
    fn generate_unique_label(&mut self) -> String {
        self.label_idx+=1;
        let s = self.label_idx.to_string();
        s
    }
    fn register_string(&mut self, label : String, val : String) {
        // adds crlf before each string 
        self.data_section.push_str(&format!("\n{}\tdb\t0Ah, 0Dh'{}'\n", label, val))
    }
    // todo: make this less hacky
    pub fn emit(&mut self) {
        let text_section = emit_scope(self.master_node.clone(), self).split('\n')
        .into_iter()
        .map(|x| x.trim_start().to_string())
        .collect::<Vec<String>>().join("\n");
        println!("SECTION .data");
        println!("{}", self.data_section);
        println!("SECTION .text");
        println!("global _start");
        println!("_start:");

        println!("{}", text_section);
        println!("
call quit
quit:
mov     ebx, 0
mov     eax, 1
int     80h
ret
        "); // linux syscall: exit(0)
    }
}
fn emit_if(node : SyntaxNode, stack_context : &mut AsmCtx) -> String {
    let cond_expr = emit_expr(node.children[0].clone(), stack_context);
    let inner_scope = emit_scope(node.children[1].clone(), stack_context);
    let jne_label = stack_context.generate_unique_label();
    let codeblock = format!("
        ; if statement
        {}
        pop eax
        cmp eax, 0
        jne skip_{}
        {}
        skip_{}:
    ", cond_expr,  jne_label, inner_scope, jne_label);

    codeblock
} 

fn emit_expr(node : SyntaxNode, stack_context : &mut AsmCtx) -> String {
    let mut code: String = String::new();

    // if there are operators involved; 2 operands
    if node.children.len() > 1 {
        // TODO: lh may become an expr in the future
        let lh = emit_value(node.children[0].clone(), stack_context);
        let rh =  emit_expr(node.children[2].clone(), stack_context);
        match node.children[1].clone().children[0].clone().data.unwrap().token {
            token::Token::BinaryOperator(op) => {
                code.push_str(&format!("
                            {}
                            {}
                            ; expression with 2 operands
                            pop eax ; lh
                            pop ebx ; rh
                        ", lh, rh));
                match op {
                    BinaryOperator::Add => {
                        code.push_str("
                            add eax, ebx
                        ");
                    }
                    BinaryOperator::Sub => {
                        code.push_str("
                            sub eax, ebx
                        ");
                    }
                    BinaryOperator::Mult => {
                        code.push_str("
                            imul eax, ebx
                        ");
                    }
                    BinaryOperator::Div => {
                        code.push_str("
                            idiv eax, ebx
                        ");
                    }
                    BinaryOperator::RShift => {
                        code.push_str("
                            shr eax, ebx
                        ");
                    }
                    BinaryOperator::LShift => {
                        code.push_str("
                            shl eax, ebx
                        ");
                    }
                    BinaryOperator::Greater => {
                        code.push_str("
                            sub eax, ebx
                        ");
                    }
                    BinaryOperator::Less => {
                        code.push_str("
                            sub eax, ebx
                        ");
                    }
                    BinaryOperator::Equals => {
                        code.push_str("
                            xor eax, ebx
                        ");
                    }
                    BinaryOperator::And => {
                        code.push_str("
                            and eax, ebx
                        ");
                    }
                    BinaryOperator::Or => {
                        code.push_str("
                            or eax, ebx
                        ");
                    }
                    BinaryOperator::Xor => {
                        code.push_str("
                            xor eax, ebx
                        ");
                    }

                }

            }
            _ => panic!("Not a binary operator")
        }
        code.push_str("push eax");
        
    }
    // unary expression; a value
    else {
        code = emit_value(node.children[0].clone(), stack_context);

    }
    code
}
fn emit_value(node : SyntaxNode, stack_context : &mut AsmCtx) -> String {
    let mut code = String::new();
    let innerstr = node.children[0].clone().data.expect(&format!("value is none {:?}", node.children[0].grammar));

    match node.children[0].grammar {
        GrammarType::Token(Token::Constant(token::Constant::Str)) => {
            let str_label = format!("string{}", stack_context.generate_unique_label());
            stack_context.register_string(str_label.clone(), innerstr.inner_data);
            code.push_str(&format!("
            ; value - string literal
            mov eax, {}
            ", str_label)); // yes this will allow integers that are too big.
            // lol strings are not gonna happen
        }
        GrammarType::Token(Token::Constant(token::Constant::Integer)) => {
            code.push_str(&format!("
            ; value - integer constant
            push dword {}
            ", innerstr.inner_data)); // yes this will allow integers that are too big.
            //stack_context.register_dword(format!("dword{}", stack_context.generate_unique_label()), innerstr.inner_data);
        }
        GrammarType::Token(Token::Identifier(Identifier::Variable)) => {
            let idx_of_variable_in_ctx = stack_context.scope_stack[stack_context.scope_stack.len() - 1].symbol_table.iter().position(
                |x : &String| x == &innerstr.inner_data
            ).expect("variable does not exist or is not in scope");
            let addr = idx_of_variable_in_ctx;
            code.push_str(&format!("
            ; value - reference to variable 
            push dword ptr [ebp - {} * 4] ; dereferencing the variable. ebp = top of the stack frame, 32bit dwords are 4 bytes
            ", addr)); 
        }
        _ => panic!("lol")
    }
    code
}
fn emit_loop(node : SyntaxNode, stack_context : &mut AsmCtx) -> String {
    let mut code: String = String::new();
    let loop_label = stack_context.generate_unique_label();
    let inner_scope = emit_scope(node.children[0].clone(), stack_context);
    code.push_str(&format!("
    ; loop
        begin_loop{}:
        {}
        jmp loop{}
    ", loop_label, inner_scope, loop_label));
    code
}
fn emit_reference(node : SyntaxNode, stack_context : &mut AsmCtx) -> String {
    let mut code: String = String::new();
    match node.children[0].grammar {
        GrammarType::Token(tkn) => {
            match tkn {
                token::Token::Keyword(kw) => {
                    
                    // variable declaration
                    let varname = node.children[1].clone().data.unwrap().inner_data;
                    let l = stack_context.scope_stack.len();

                    // uhh, yeah strings and integers are the same right???
                    // they should be differentiated, but at the same time they are both 4 byte values
                    // just that the string is a pointer

                    // register the varname in our stack context
                    stack_context.scope_stack[l - 1].symbol_table.push(varname.clone());

                    // offset to var
                    let offset = stack_context.scope_stack.len();
                    code.push_str(&format!("
                    ; variable declaration for {} that returns a reference (offset 0)
                    push dword 0 ; zero initialized
                    push dword 0 ; the reference to the value above is just a an offset of length 0
                    ", varname))
                }
                token::Token::Identifier(id) => {
                    if id != token::Identifier::Variable {
                        panic!("not a variable!");
                    }
                    let innerstr = node.children[0].clone().data.unwrap();
                    let idx_of_variable_in_ctx = stack_context.scope_stack[stack_context.scope_stack.len() - 1].symbol_table.iter().position(
                        |x : &String| x == &innerstr.inner_data
                    ).expect(&format!("variable does not exist or is not in scope! {:?}", stack_context.scope_stack[stack_context.scope_stack.len() - 1].symbol_table));
                    let addr = (stack_context.scope_stack.len() - 1) - idx_of_variable_in_ctx; 
                    code.push_str(&format!("
                    ; reference to the variable {}
                    push {}
                    ", innerstr.inner_data, addr)); 
                }
                _ =>  panic!("invalid reference: {:?}", node.grammar)
            }
            
        }
        _ =>  panic!("invalid reference, {:?}", node.children[0].clone().children[0].grammar)
    }
    code
}
fn emit_assign(node : SyntaxNode, stack_context : &mut AsmCtx) -> String {
    let mut code: String = String::new();
    let reference = emit_reference(node.children[0].clone(), stack_context); // the offset
    let expr = emit_expr(node.children[1].clone(), stack_context);
    code.push_str(&format!("
    ; assignment
    {}
    {}
    pop eax ; the value of expr 
    pop ebx ; the offset pointing to the stack var
    mov [ebp - ebx * 4], eax ; move the value in eax to the address pointed to by ebp - ebx * 4
", reference, expr)); 
    code
    
}

fn emit_statement(node : SyntaxNode, stack_context : &mut AsmCtx) -> String{
    let code = match node.children[0].grammar {
        GrammarType::If => {
            emit_if(node.children[0].clone(), stack_context)
        }
        GrammarType::Loop => {
            emit_loop(node.children[0].clone(), stack_context)
        }
        GrammarType::Assignment => {
            emit_assign(node.children[0].clone(), stack_context)
        }
        GrammarType::Print => {
            "".to_owned()
            //emit_print(node, stack_context)
        }
        GrammarType::Break => {
            "".to_owned()
            //emit_break(node, stack_context)
        }
        _ => panic!("not a statement: {:?}", node.grammar)
    };
    code
}



pub fn emit_scope(node : SyntaxNode, stack_context : &mut AsmCtx) -> String {
    stack_context.scope_stack.push(ScopeFrame::new());
    let mut code: String = String::new();
    for n in node.children {
        let stmt = emit_statement(n, stack_context);
        code.push_str(&stmt);
    }
    
    // clean up the scope context
    code.push_str("
        ; exiting scope - cleaning local variables");
    for variable in &stack_context.scope_stack[stack_context.scope_stack.len() - 1].symbol_table {
        code.push_str(&format!("
        pop ebx ; {} going out of scope
        ", variable));
    }
    stack_context.scope_stack.remove(stack_context.scope_stack.len() - 1);

    code
}
