/*
    Utility functions and structures
*/

pub mod tokenizer {
    #[derive(Debug, PartialEq, Clone)]
    pub enum Token {
        ParOpen,        // open parenthesis 
        ParClosed,      // closed parenthesis
        Id(String),     // a name
        Arrow,          // -> 
        Invalid
    }

    impl Token {
        pub fn new(s : &String) -> Token {
            match s.as_str() {
                "("  => Token::ParOpen,
                ")"  => Token::ParClosed,
                "->" => Token::Arrow,
                s    => {
                    // only create token if alphanumeric
                    if s.chars().all(|a| a.is_alphanumeric()) {
                        Token::Id(s.to_string())
                    }
                    else {
                        Token::Invalid
                    }
                }
            }
        }
    }
}