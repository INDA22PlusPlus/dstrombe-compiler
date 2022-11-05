use lazy_static::lazy_static;
use std::collections::HashMap;
use std::cmp::Ordering;
use crate::token;
use crate::token::{Token, AssignmentOperator::*, BinaryOperator::*, UnaryOperator::*, Keyword::*, SpecialChar::*, Constant::*, Identifier::*};
pub struct Parser {

}
pub struct Tokenizer {

}

#[derive(Clone)]
enum ParseRule {
    Str(String),
    AnyStr,
    AnyInt
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TokenData {
    pub token : Token,
    pub src_length : usize,
    pub inner_data : String,
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum FreqRule {
    Any,
    Once,
    Exactly(usize),
    OneOf
}
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum GrammarType {
    Token(Token),
    Print,
    Expr,
    Scope,
    Assignment,
    Reference,
    Statement,
    If,
    Break,
    Loop,
    BinaryOperator,
    Value,
}
#[derive(Clone, Copy, PartialEq)]
struct GrammarRule {
    pub required : GrammarType,
    pub occurs : FreqRule,
}
#[derive(Clone)]
pub struct Grammar {
    grammar_type : GrammarType,
    grammar_pattern : Vec<GrammarRule>,
    return_pattern : Vec<GrammarRule>,
}
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct SyntaxNode {
    pub grammar : GrammarType,
    pub data : Option<TokenData>,
    pub children : Vec<SyntaxNode>,
}
// operator precedence logic. 
/* 
impl Ord for SyntaxNode {
    fn cmp(&self, other : &Self) -> Ordering {
        let idx_in_syntax_ruleset = 
    }
}

impl PartialOrd for SyntaxNode {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}*/

lazy_static! {
    // defines the patterns that will be matched on to determine if a sequence of chars constitutes a given Token.
    // This is needed as some sequences may parse into multiple different valid tokens, 
    // eg "== 1337" could be "assign, assign, constant(1337)" or "equals constant(1337)"
    static ref TOKEN_RULES: Vec<(Token, Vec<ParseRule>)> = {
        let mut m = Vec::new();
        m.push((Token::BinaryOperator(Add), vec![ParseRule::Str("+".to_owned())]));
        m.push((Token::BinaryOperator(Sub), vec![ParseRule::Str("-".to_owned())]));
        m.push((Token::BinaryOperator(Mult), vec![ParseRule::Str("*".to_owned())]));
        m.push((Token::BinaryOperator(Div), vec![ParseRule::Str("/".to_owned())]));
        m.push((Token::BinaryOperator(Equals), vec![ParseRule::Str("==".to_owned())]));

        m.push((Token::AssignmentOperator(Assign), vec![ParseRule::Str("=".to_owned())]));


        m.push((Token::BinaryOperator(Or), vec![ParseRule::Str("|".to_owned())]));
        m.push((Token::BinaryOperator(And), vec![ParseRule::Str("&".to_owned())]));

        m.push((Token::UnaryOperator(Not), vec![ParseRule::Str("!".to_owned())]));

        m.push((Token::Keyword(Print), vec![ParseRule::Str("print ".to_owned())]));
        m.push((Token::Keyword(Loop), vec![ParseRule::Str("loop ".to_owned())]));
        m.push((Token::Keyword(token::Keyword::Str), vec![ParseRule::Str("string ".to_owned())]));
        m.push((Token::Keyword(token::Keyword::Integer), vec![ParseRule::Str("int ".to_owned())]));
        m.push((Token::Keyword(If), vec![ParseRule::Str("if".to_owned())]));
        m.push((Token::Keyword(Break), vec![ParseRule::Str("break".to_owned())]));

        m.push((Token::SpecialChar(LBrace), vec![ParseRule::Str("{".to_owned())]));
        m.push((Token::SpecialChar(RBrace), vec![ParseRule::Str("}".to_owned())]));
        m.push((Token::SpecialChar(LParen), vec![ParseRule::Str("(".to_owned())]));
        m.push((Token::SpecialChar(RParen), vec![ParseRule::Str(")".to_owned())]));
        m.push((Token::SpecialChar(Semicolon), vec![ParseRule::Str(";".to_owned())]));

        m.push((Token::Constant(token::Constant::Str), vec![ParseRule::Str("\"".to_owned()), ParseRule::AnyStr, ParseRule::Str("\"".to_owned())]));
        m.push((Token::Identifier(Variable), vec![ParseRule::AnyStr]));
        m.push((Token::Constant(token::Constant::Integer), vec![ParseRule::AnyInt]));

        //m.reverse();
        m
    };
    static ref SYNTAX_RULES : Vec<Grammar> = {
        let mut m = Vec::new();
        m.push(Grammar {
            grammar_type : GrammarType::Scope, 
            grammar_pattern : vec![
                GrammarRule {required : GrammarType::Token(Token::SpecialChar(LBrace)), occurs : FreqRule::Once}, 
                GrammarRule {required : GrammarType::Statement, occurs : FreqRule::Any}, 
                GrammarRule {required : GrammarType::Token(Token::SpecialChar(RBrace)), occurs : FreqRule::Once}
            ],
            return_pattern : vec! [
                GrammarRule {required : GrammarType::Statement, occurs : FreqRule::Any}, 
            ],
            
        });
        m.push(Grammar { grammar_type : GrammarType::If, // or GrammarType::Token(Token::Keyword(If))
            grammar_pattern : vec![
                GrammarRule {required : GrammarType::Token(Token::Keyword(If)), occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::Token(Token::SpecialChar(LParen)), occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::Expr, occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::Token(Token::SpecialChar(RParen)), occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::Scope, occurs : FreqRule::Once}
            ],
            return_pattern : vec![
                GrammarRule {required : GrammarType::Expr, occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::Scope, occurs : FreqRule::Once}
            ],
            
        });
        m.push(Grammar {
            grammar_type : GrammarType::Loop, 
            grammar_pattern : vec![
                GrammarRule {required : GrammarType::Token(Token::Keyword(Loop)), occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::Scope, occurs : FreqRule::Once}
            ],
            return_pattern : vec![
                GrammarRule {required : GrammarType::Scope, occurs : FreqRule::Once}
            ],
            
        });

        m.push(Grammar {
            grammar_type : GrammarType::Statement, 
            grammar_pattern : vec![
                GrammarRule {required : GrammarType::Assignment, occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::Print, occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::If, occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::Loop, occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::Break, occurs : FreqRule::OneOf},

            ],
            return_pattern : vec![
                GrammarRule {required : GrammarType::Assignment, occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::Print, occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::If, occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::Loop, occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::Break, occurs : FreqRule::OneOf},
            ],
        
        });

        m.push(Grammar {
            grammar_type : GrammarType::Assignment, 
            grammar_pattern : vec![
                GrammarRule {required : GrammarType::Reference, occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::Token(Token::AssignmentOperator(Assign)), occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::Expr, occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::Token(Token::SpecialChar(Semicolon)), occurs : FreqRule::Once},

            ],
            return_pattern : vec![
                GrammarRule {required : GrammarType::Reference, occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::Expr, occurs : FreqRule::Once},
            ],
        });
        
        m.push(Grammar {
            grammar_type : GrammarType::Print, 
            grammar_pattern : vec![
                GrammarRule {required : GrammarType::Token(Token::Keyword(Print)), occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::Expr, occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::Token(Token::SpecialChar(Semicolon)), occurs : FreqRule::Once},

            ],
            return_pattern : vec![
                GrammarRule {required : GrammarType::Expr, occurs : FreqRule::Once},
            ], 
        });
        m.push(Grammar {
            grammar_type : GrammarType::Break, 
            grammar_pattern : vec![
                GrammarRule {required : GrammarType::Token(Token::Keyword(Break)), occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::Token(Token::SpecialChar(Semicolon)), occurs : FreqRule::Once},

            ],
            return_pattern : vec![
            ], 
        });
        /*
        m.push(Grammar {
            grammar_type : GrammarType::Expr, 
            grammar_pattern : vec![
                GrammarRule {required : GrammarType::Expr, occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::BinaryOperator, occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::Expr, occurs : FreqRule::Once},
            ],
            return_pattern : vec![
                GrammarRule {required : GrammarType::Expr, occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::BinaryOperator, occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::Expr, occurs : FreqRule::Once},
            ]
        });
         */
        // add rule "BinaryOperation"
        m.push(Grammar {
            grammar_type : GrammarType::Expr, 
            grammar_pattern : vec![
                GrammarRule {required : GrammarType::Value, occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::BinaryOperator, occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::Expr, occurs : FreqRule::Once},
            ],
            return_pattern : vec![
                GrammarRule {required : GrammarType::Value, occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::BinaryOperator, occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::Expr, occurs : FreqRule::Once},
            ]
        });

        m.push(Grammar {
            grammar_type : GrammarType::Expr, 
            grammar_pattern : vec![
                GrammarRule {required : GrammarType::Value, occurs : FreqRule::Once},
            ],
            return_pattern : vec![
                GrammarRule {required : GrammarType::Value, occurs : FreqRule::Once},
            ]
        });
        
        m.push(Grammar {
            grammar_type : GrammarType::Reference, 
            grammar_pattern : vec![
                GrammarRule {required : GrammarType::Token(Token::Identifier(Variable)), occurs : FreqRule::Once}
            ],
            return_pattern : vec![
                GrammarRule {required : GrammarType::Token(Token::Identifier(Variable)), occurs : FreqRule::Once}
            ],
        });
        m.push(Grammar {
            grammar_type : GrammarType::Reference, 
            grammar_pattern : vec![
                GrammarRule {required : GrammarType::Token(Token::Keyword(token::Keyword::Integer)), occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::Token(Token::Identifier(Variable)), occurs : FreqRule::Once}
            ],
            return_pattern : vec![
                GrammarRule {required : GrammarType::Token(Token::Keyword(token::Keyword::Integer)), occurs : FreqRule::Once},
                GrammarRule {required : GrammarType::Token(Token::Identifier(Variable)), occurs : FreqRule::Once}
            ],
        });

        m.push(Grammar {
            grammar_type : GrammarType::BinaryOperator, 
            grammar_pattern : vec![
                GrammarRule {required : GrammarType::Token(Token::BinaryOperator(Equals)), occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::Token(Token::BinaryOperator(Or)), occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::Token(Token::BinaryOperator(And)), occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::Token(Token::BinaryOperator(Add)), occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::Token(Token::BinaryOperator(Sub)), occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::Token(Token::BinaryOperator(Div)), occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::Token(Token::BinaryOperator(Mult)), occurs : FreqRule::OneOf},

            ],
            return_pattern : vec![
                GrammarRule {required : GrammarType::Token(Token::BinaryOperator(Equals)), occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::Token(Token::BinaryOperator(Or)), occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::Token(Token::BinaryOperator(And)), occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::Token(Token::BinaryOperator(Add)), occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::Token(Token::BinaryOperator(Sub)), occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::Token(Token::BinaryOperator(Div)), occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::Token(Token::BinaryOperator(Mult)), occurs : FreqRule::OneOf},
            ],
        });
        m.push(Grammar {
            grammar_type : GrammarType::Value, 
            grammar_pattern : vec![
                GrammarRule {required : GrammarType::Token(Token::Identifier(Variable)), occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::Token(Token::Constant(token::Constant::Integer)), occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::Token(Token::Constant(token::Constant::Str)), occurs : FreqRule::OneOf},
            ],
            return_pattern : vec![
                GrammarRule {required : GrammarType::Token(Token::Identifier(Variable)), occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::Token(Token::Constant(token::Constant::Integer)), occurs : FreqRule::OneOf},
                GrammarRule {required : GrammarType::Token(Token::Constant(token::Constant::Str)), occurs : FreqRule::OneOf},
            ]
        });
        
        m
    };

}
impl Parser {
    
    pub fn parse() {
        // IOU: parser :)
    }
    // returns: (tokens consumed, root of the parsed syntax branch)
    fn parse_ahead(&self, grammar : Grammar, tokens : Vec<TokenData>, tptr : i32) -> Option<(usize, SyntaxNode)>{
        
        // These two variables track what token is currently being parsed,
        // as well as the rules for parsing said token
        let mut grammar_to_check = grammar.grammar_type;
        let mut rules : Vec<GrammarRule> = grammar.clone().grammar_pattern; 

        // what rule are we currently parsing? how many chars have parsed so far in that rule?
        let mut rule_idx = 0;
        let mut matches = 0; 
        
        let mut root_syntax_node : SyntaxNode = SyntaxNode { 
            grammar: grammar_to_check,
            children: Vec::new(), 
            data : None,
        };

        let mut unsorted_syntax_nodes = Vec::new(); // sort it into a tree later. dont add immediately
        
        let mut has_matched_oneof = false;
        
        let mut i = 0;
        while rule_idx < rules.len() {

            // usize : amt of tokens consumed in parsing the node
            let mut matched_node : Option<(usize, SyntaxNode)> = None;
            match rules[rule_idx].required {
                GrammarType::Token(token_type) => {
                    if token_type == tokens[i].token {
                        let node_to_add = SyntaxNode { 
                            grammar: GrammarType::Token(tokens[i].token), 
                            children: Vec::new(), 
                            data : Some(tokens[i].clone())
                        };
                        matched_node = Some((1, node_to_add));
                        
                    }
                    else {
                    }
                    //println!("got {:?} at {:?}", tokens[i].token, i as i32 + tptr)

                }
                _ => {
                    // we need to recursively check if the token at the root of
                    // the rule matches 

                    // the reason we do not use a hashmap is that there can be many rules
                    // for any one grammarrule, Expr has 2 for example.
                    let vec_of_rules : Vec<Grammar> = SYNTAX_RULES.clone().into_iter().filter(
                        |x : &Grammar |
                        // filter by matching rules
                        x.grammar_type == rules[rule_idx].required &&
                        // check so that we don't recurse into a rule whose first requirement
                        // is the same as the grammar we are checking, as this would cause SO
                        !(i == 0 && x.grammar_type == grammar_to_check) 
                    ).collect();
                    let mut potential_nodes = Vec::new();
                    for (matching_rulepair) in vec_of_rules {
                        // println!("{:?}, {:?}",  grammar_to_check, matching_rulepair.grammar_type);

                        // continue parsing the tokens from where we have not yet parsed
                        //println!("{:?}", grammar_to_check);

                        let parsed = self.parse_ahead(matching_rulepair, tokens[i..tokens.len()].to_vec(), tptr + i as i32);
                        if(parsed.is_some()) {
                            let child_node = parsed.unwrap();
                            //println!("got at i={:?}: {:?} (called from grammar_to_check: {:?})", child_node.0, child_node.1.grammar, grammar_to_check);
                            potential_nodes.push(child_node);
                        }

                    }

                    // TODO: sort the list by precedence, eg operator precedence
                    // do this by implementing partialord, check each child node against a list recursively
                    // eg children : {expr, binop, expr}, check expr, binop and then expr against a list
                    // if equal, descend one level and check the grandchildren recursively
                    // potential_nodes.sort();

                    // in the meantime:
                    if potential_nodes.len() > 0 {
                        matched_node = Some(potential_nodes[0].clone());
                    }
                }
            }
            if(matched_node.is_some()) {
            //println!("grammar_to_check: {:?} matched: {:?}, rules[rule_idx].occurs: {:?}", grammar_to_check, matched_node.clone().unwrap().1.grammar, rules[rule_idx].occurs);
        }
        else {
            //println!("grammar_to_check: {:?} matched: {:?}, {:?}, rules[rule_idx].occurs: {:?}", grammar_to_check, "None", rules[rule_idx].required, rules[rule_idx].occurs);

        }
            // TODO: turn this into a subfunction
            match rules[rule_idx].occurs {
                FreqRule::Any => {
                    match(matched_node) {
                        Some(size_node_tuple) => {

                            unsorted_syntax_nodes.push(size_node_tuple.1);
                            i += size_node_tuple.0; // increment by amt of consumed tokens
                        }
                        None => {
                            rule_idx += 1; //so, always advance when we stop matching
                            // but we don't advance the token ptr (i), ie don't consume it
                        }
                    }

                }
                FreqRule::Once => {
                    match(matched_node) {
                        
                        Some(size_node_tuple) => {

                            unsorted_syntax_nodes.push(size_node_tuple.1);
                            i += size_node_tuple.0; // matched, now keep going
                            rule_idx += 1;
                        }
                        None => {
                            return None; 
                        }
                    }
                }
                FreqRule::OneOf => {

                    match(matched_node) {
                        Some(size_node_tuple) => {
                            if !has_matched_oneof {
                                unsorted_syntax_nodes.push(size_node_tuple.1);
                                i += size_node_tuple.0; // matched, now keep going
                                rule_idx += 1;
                                has_matched_oneof = true;
                            }
                            else {
                                
                                rule_idx = rules.len() + 1;
                            }
                            
                        }
                        None => {
                            // this is fine as long as it there are oneof rules ahead that it hasn't matched on 
                            // or if it already has matched a oneof rule
                            let is_fine = has_matched_oneof || (rule_idx < grammar.clone().grammar_pattern.len() && grammar.clone().grammar_pattern[rule_idx+1..].to_vec()
                            .into_iter()
                            .filter(
                                |x: &GrammarRule|
                                x.occurs == FreqRule::OneOf
                            ).collect::<Vec<GrammarRule>>().len() > 0);
                            if !is_fine {
                                //println!("it was not fine.");
                                return None;
                            }
                            else {
                                //println!("it's fine: {:?} has_matched_oneof: {:?}", grammar.clone().return_pattern[rule_idx].required, has_matched_oneof);
                                rule_idx += 1;
                            }
                        }
                        
                        
                    }
                }
                FreqRule::Exactly(_) => todo!()
            }


            
        }
        // step 3: arrange the syntaxnodes we parsed into a branch that adheres to Grammar.return_pattern.
        let mut j : usize = 0;
        let rp = grammar.clone().return_pattern;
        while j < rp.len() {
            let grammar_rule = rp[j];
            // find the first idx of a node whose grammar matches grammar_rule
            let matching_idx = unsorted_syntax_nodes.iter().position(|stx_node| {
                stx_node.grammar == grammar_rule.required
            });
            match matching_idx {
                Some(idx) => {
                    // quickref: swap_remove removes an item at idx and returns it
                    let node = unsorted_syntax_nodes.swap_remove(idx);
                    root_syntax_node.children.push(node);
                    match grammar_rule.occurs {
                        FreqRule::Any => { 

                        }
                        FreqRule::Once => { 
                            j += 1;
                        }
                        FreqRule::OneOf => { 
                            j += 1;
                        }
                        _ => panic!("not implemented")
                    }
                }
                None => {
                    j += 1;
                    // must be a OneOf rule. Do nothing ig. Unless?
                }
            } 
            
        }
        //println!("returned at i = {:?}: {:?}", i, grammar_to_check);
        Some((i, root_syntax_node))
        
    }
    /* 
    fn parse_ahead(&self, rulepair : &(GrammarType, Vec<GrammarRule>), tokens : Vec<TokenData>) -> Option<SyntaxNode> { 
        // These two variables track what token is currently being parsed,
        // as well as the rules for parsing said token
        let mut grammar_to_check = &rulepair.0;
        let mut rules : Vec<GrammarRule> = rulepair.1.to_vec(); 
        
        

    }
    */
    pub fn generate_ast(&self, tokens_to_parse : Vec<TokenData>) -> SyntaxNode {
        
        // we will have an enclosing master scope, which requires braces
        let mut tokens = vec![TokenData {
            token : Token::SpecialChar(LBrace),
            src_length : 0,
            inner_data : String::new(),
        }];
        tokens.append(&mut tokens_to_parse.clone());
        tokens.push(TokenData {
            token : Token::SpecialChar(RBrace),
            src_length : 0,
            inner_data : String::new(),
        });
        // find the grammar rule for Scope
        let scope_grammar = SYNTAX_RULES[SYNTAX_RULES.iter().position(
            |rule| rule.grammar_type == GrammarType::Scope
        ).unwrap()].clone();
        let mut parse_result = self.parse_ahead(scope_grammar, tokens, 0);
        
        parse_result.unwrap().1
    }

    

}

impl Tokenizer {
    // implements the rules for parsing tokens, returns Some(length, Token) if it could parse the token
    fn parse_ahead(&self, rulepair : &(Token, Vec<ParseRule>), src : String) -> Option<TokenData> {
        let chars : Vec<char> = src.chars().collect();
       
        // These two variables track what token is currently being parsed,
        // as well as the rules for parsing said token
        let mut token_to_parse = &rulepair.0;
        let mut rules : Vec<ParseRule> = rulepair.1.to_vec(); 

        // what rule are we currently parsing? how many chars have parsed so far in that rule?
        let mut rule_word_idx = 0;
        let mut rule_char_idx = 0;
        
        let mut previous_char = '\0';
        let mut outdata = String::new();
        let mut end_of_whitespace = false;
        
        // TODO remove fudge 
        let mut fudge = 0;
        for (curr_idx,c) in chars.iter().enumerate() {
            let mut advance_to_next_rule : bool = false;
            // ignore leading whitespace
            let is_not_whitespace = (*c != ' ' && *c != '\t' && *c != '\n');
            if  end_of_whitespace || is_not_whitespace {
                //println!("{}", c);
                // stop ignoring whitespaces upon encountering our first non-whitespace
                end_of_whitespace = (end_of_whitespace || is_not_whitespace);

                // logic for when we are not parsing strings
                match &rules[rule_word_idx] {
                    ParseRule::Str(string) => {
                        if &c.to_string() != &string[rule_char_idx..rule_char_idx+1] {
                            // unexpected char, does not match the token we are checking!
                            //println!("{}, {}, {}", &c.to_string(), &string[rule_char_idx..rule_char_idx+1], rule_char_idx);
                            return None;
                        }
                        advance_to_next_rule = (rule_char_idx == (string.len() - 1));
                    }
                    ParseRule::AnyStr => {
                        if previous_char == '\\' {
                            outdata.push(match c {
                                'n' => {
                                    '\n'
                                }
                                '0' => {
                                    '\0'
                                }
                                '\'' => {
                                    '\''
                                }
                                '\\' => {
                                    '\\'
                                }
                                _ => {
                                    *c
                                }
                                
                            });
                        }
                        else {
                            // if you don't escape special chars then you only get A-z 
                            match c {
                                'A'..='z' => {
                                    outdata.push(*c);
                                }
                                _ => {
                                    // does not constitute a valid anystr bc it is empty
                                    if(rule_char_idx == 0) {
                                        return None;
                                    }
                                }
                                
                                
                            }
                            match(chars[curr_idx + 1]) {
                                'A'..='z' => {
                                    
                                }
                                _ => {
                                    advance_to_next_rule = true;
                                }
                            }
                        }

                    }
                    ParseRule::AnyInt => {
                        match c {
                            '0'..='9' => {
                                outdata.push(*c);
                            }
                            _ => {
                                // does not constitute a valid anyint bc it is empty
                                if(rule_char_idx == 0) {
                                    return None;
                                }
                            }
                            
                            
                        }
                        match(chars[curr_idx + 1]) {
                            '0'..='9' => {
                                
                            }
                            _ => {
                                advance_to_next_rule = true;
                            }
                        }
                    }
                }
                rule_char_idx += 1;
                previous_char = *c;
                if(advance_to_next_rule) {
                    rule_char_idx = 0;
                    rule_word_idx += 1;
                    if (rule_word_idx == rules.len()) {
                        // valid parse, return
                        // TODO: replace curr_idx bc we should count wildcards -- AnyStr/AnyInt only once
                        // or maybe not, because we use this return size to calculate how many chars to
                        // skip
                        let token_data = TokenData {
                            token : token_to_parse.clone(),
                            src_length : (curr_idx + 1 - fudge) as usize,
                            inner_data : outdata
                        };
                        return Some(token_data)
                    }
                }
            }
            else {

            }
            
        }
        // we will never get here
        None 
    }
    
    // consumes the parser by taking self 
    pub fn tokenize(self, src : String) -> Result<Vec<TokenData>, String> {
        
        let mut tokens : Vec<TokenData> = Vec::new();
        let chars : Vec<char> = src.chars().collect();
       
        // store related chars 
        let mut group_buf : Vec<char>;

        let mut skip_chars = 0; // skip these many chars -- if we parse the keyword 'loop" we skip 4
        for (i, c) in chars.into_iter().enumerate() {
            // skip chars if a token has been parsed recently
            if skip_chars > 0 {
                skip_chars -= 1;
                continue;
            }
            //println!("-------- {}", i);

            // check every token against the sequence of chars immediately following this char
            let mut matching : Option<TokenData> = None;
            for rule in TOKEN_RULES.iter() {
                //println!("{:?}", rule.0);
                let a = self.parse_ahead(rule, (&src[i.. src.len()]).to_string());
                match a {
                    None => {
                        continue;
                    }
                    Some(out_tokendata) => {
                        matching = Some(out_tokendata);
                        break;
                    }
                }
            }
            if matching.is_none() {
                return Err(format!("Invalid token at index {}: at ...{}", i, &src[i.. src.len()]));
            }
            else {
                // we are interested in the longest token rule that matches, that way == will match
                // instead of 2x = 
                //matches.sort_by(|a, b| (b.src_length).cmp(&a.src_length));
                //let longest_match = matches[0].clone();
                let a = matching.unwrap();
                skip_chars = a.src_length - 1;
                tokens.push(a);
            }
            
        }
        Ok(tokens)
    }
    
}

