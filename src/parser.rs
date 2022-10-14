struct Parser {

}

impl Parser {
    
    pub fn parse() {
        // IOU: parser :)
    }
    
    fn generate_ast(&self) {
    
    }

    // consumes the parser by taking self 
    pub fn tokenize(self, src : String) -> Vec<Token> {
        
        let tokens : Vec<Token> = Vec::new();
        let chars : Vec<char> = src.chars().collect();
        
        let mut parse_string = false;

        // store related chars 
        let mut group_buf = Vec<char>;
        
        for c in chars {

            let escaped = c == '\\';

            if c == '\"' && !escaped {
                if parse_string {
                    // a string has been parsed
                    tokens.push(Token::Constant::Str(group_buf.into_iter().collect()));
                }
                group_buf.clear();
                parse_string = !parse_strinig;
            }
            
            if parse_string {
                // these will be processed all at once
                group_buf.push(c);
                continue; // TODO: could be cleaner to avoid continue
            }
            
            

            
        }
    }
}
