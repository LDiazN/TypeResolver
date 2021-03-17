/*
    core logic
*/

// Rust imports
use std::collections::HashMap;

// internal imports
use crate::utils::*;

// an identifier
pub type Name = String;
// stream of elements representing some expression
pub type RawExpr = Vec<String>;
// stream of elements representing some type expression
pub type RawType = Vec<String>;
// map from names to type names, so we can give a type to some constant atomic expression
pub type VarTypes = HashMap<Name, Type>;

// --- Expression Types ---
#[derive(Debug, PartialEq)]
pub struct AtomExpr {
    name : Name
}

#[derive(Debug, PartialEq)]
pub struct FuncApply {
    function    : Expr,
    arg         : Expr,
}

#[derive(Debug, PartialEq)]
pub struct ParentExpression {
    expr : Expr
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Atom(AtomExpr),
    Apply(Box<FuncApply>)
}

// --- Type types ---

#[derive(Debug, PartialEq, Clone)]
pub struct ConstType {
    name : Name
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarType {
    name : Name
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    l_type : Type,
    r_type : Type,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Const(ConstType),      // T
    Var(VarType),          // t
    Func(Box<Function>),   // a -> b
    ParType(Box<Type>)              // (a)
}

// --- Wrapper Type ---
pub struct TypeManager {
    name_to_type : VarTypes
}

#[derive(Debug, PartialEq)]
pub enum TypeError {
    InvalidSyntax
}

impl TypeManager {

    /// Create a new manager
    pub fn new() -> TypeManager {
        TypeManager{
            name_to_type : VarTypes::new()
        }
    }

    pub fn get_type(&self, expr : &Expr) -> Result<Type, TypeError> {
        unimplemented!()
    }

    pub fn add_type(&mut self, typename : &Name, my_type : Type) {
        *self.name_to_type
            .entry(typename.clone())
            .or_insert(my_type) = my_type.clone();
    }

    pub fn add_atm_expr(&mut self, name : Name, typename : Type) -> Result<(), TypeError> {
        unimplemented!()
    }
}

impl Expr {
    pub fn new(manager : &TypeManager, expr : RawExpr) -> Result<Expr, TypeError> {
        unimplemented!()
    }
}

impl Type {

    /// Try to parse tokens from a token vector if possible
    /// ## Params
    /// * `tokens` - Tokens to turn into expression 
    /// ---
    /// ## Return
    /// A type if succesfull parsing, an error otherwise
    pub fn new(rawtype : RawType) -> Result<Type, TypeError> {
        let to_parse = rawtype.join(" ");
        let tokens = tokenizer::Tokenizer::new(to_parse).collect();

        Type::parse_type_helper(&tokens, 0, tokens.len() - 1)
    }

    pub fn display(&self) -> String {
        format!("{:?}", self)
    }


    fn parse_type_helper(tokens : &Vec<tokenizer::Token>, lo : usize, hi : usize) -> Result<Type, TypeError> {
        // check consistent syntax
        if tokens.len() == 0  || lo > hi{
            panic!("Should not be parsing 0-length expression")
        }

        let mut lo = lo;

        // Try to get first expression
        let l_val = match &tokens[lo] {
            tokenizer::Token::ParOpen => {
                // If parenthesis expression, then 
                let last_pos = Type::find_closing_par(tokens, lo)?;

                // compute expression
                let new_type = Type::parse_type_helper(tokens, lo+1, last_pos-1)?;

                lo = last_pos + 1;
                Type::ParType(Box::new(new_type)) 
            },
            tokenizer::Token::Id(name) => { lo += 1; Type::create_atm_type(name.clone()) },
            _ => return Err(TypeError::InvalidSyntax)
        };

        // check if we reach end of stream
        if lo > hi {
            return Ok(l_val)
        }

        // if we have something else to parse, then it should be a function 
        if let tokenizer::Token::Arrow = &tokens[lo] {

            // Create new function 
            let f = Function{
                        l_type : l_val,
                        r_type : Type::parse_type_helper(tokens, lo+1, hi)?
                    };
            
            Ok( Type::Func( Box::new(f)) )
        }
        else {
            // If next token is not an arrow, then raise syntax error
            Err(TypeError::InvalidSyntax)
        }
    }

    fn find_closing_par(tokens : &Vec<tokenizer::Token>, start_par_pos : usize) -> Result<usize, TypeError> {
        let mut parity_counter = 0;

        for i in start_par_pos..tokens.len(){
            match tokens[i] {
                tokenizer::Token::ParOpen   => parity_counter += 1,
                tokenizer::Token::ParClosed => parity_counter -= 1,
                _                           => {}
            };

            if parity_counter == 0 {
                return Ok(i)
            }
        };

        Err(TypeError::InvalidSyntax)
    }

    fn create_atm_type(name : String) -> Type {
        let c = name.chars().next().unwrap();

        if c.is_lowercase() {
            Type::Var( VarType{ name } ) // if lowercase return a variable type
        }
        else {
            Type::Const( ConstType{ name } ) // if uppercase return a constant type
        }
    }
}

impl TypeError {
    pub fn display(&self) -> String {
        format!("{:?}", self)
    }
}