/*
    Utility functions and structures
*/

/// Mod with logic to  string into some token
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
        /// Create a new token from the given string
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

/// Structure tu create a new name that's not currently in use
pub struct NameGenerator {
    stored_names : Vec<String>
}

impl NameGenerator {

    /// Create a new Name generator object froma  list of unusable strings
    pub fn new(buffer : Vec<String>) -> NameGenerator {
        NameGenerator{
            stored_names : buffer
        }
    }

    /// Return a valid string that is sure to be valid,
    /// generated from an initial string
    pub fn valid_name(&self, name : &String) -> String {
        let original_name = name.clone();
        let mut possible_name = name.clone();
        let mut counter = 0;

        while self.stored_names.contains(&possible_name) {
            possible_name = format!("{}{}", original_name, counter);
            counter+=1
        }

        possible_name
    }
}
