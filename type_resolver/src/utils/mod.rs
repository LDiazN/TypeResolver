/*
    Utility functions and structures
*/

pub mod tokenizer {

    pub struct Tokenizer {
        to_parse : String,
        position : usize
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum Token {
        ParOpen,        // open parenthesis 
        ParClosed,      // closed parenthesis
        Id(String),     // a name
        Arrow,          // -> 
        Invalid(char)         // invalid symbol
    }

    impl Tokenizer {
        pub fn new(s : String) -> Tokenizer {
            Tokenizer {
                to_parse : s,
                position : 0
            }
        }
    }

    impl Iterator for Tokenizer {
        type Item = Token;

        fn next(&mut self) -> Option<Token>{
            
            // if at the end of the string, just return
            if self.position == self.to_parse.len() {
                return None
            }

            // current slice
            let curr_str = &self.to_parse[self.position..];

            // try to trim whistespaces
            for c in curr_str.chars() {
                if c.is_whitespace() {
                    self.position += 1
                }
                else {
                    break
                }
            }

            // if at the end of the string, just return
            if self.position == self.to_parse.len() {
                return None
            }

            let curr_str = &self.to_parse[self.position..];

            // check if str starts with parenthesis or functional apply
            if curr_str.starts_with("(") {
                self.position +=1;
                return Some(Token::ParOpen)
            }
            else if curr_str.starts_with(")") {
                self.position += 1;
                return Some(Token::ParClosed)
            }
            else if curr_str.starts_with("->") {
                self.position += 2;
                return Some(Token::Arrow)
            }
            
            // try to parse name
            let lo = self.position;
            for c in curr_str.chars() {
                // if alphanum, then is part of our current word
                if c.is_alphanumeric() {
                    self.position +=1
                }
                else if c.is_whitespace() || c == '-' || c == '(' || c == ')' {
                    break
                }
                else {
                    self.position = self.to_parse.len();
                    return Some(Token::Invalid(c))
                }
            }
            
            // Return a string with the current position
            Some(Token::Id(self.to_parse[lo..self.position].to_string()))
        }
    }
}