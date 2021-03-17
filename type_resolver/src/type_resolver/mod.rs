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
    Atom(Name),
    Apply(Box<FuncApply>),
    ParExpr(Box<Expr>)
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
    ParType(Box<Type>)     // (a)
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
}

impl Expr {
    pub fn new(expr : RawExpr) -> Result<Expr, TypeError> {

        // build token stream
        let expr : Vec<tokenizer::Token> = expr.iter().map(tokenizer::Token::new).collect();

        if expr.len() == 0 {
            return Err(TypeError::InvalidSyntax)
        }

        Expr::parse_expr_helper(&expr, 0,expr.len() - 1)
    }

    fn parse_expr_helper(tokens : &Vec<tokenizer::Token>, lo : usize, hi : usize) -> Result<Expr, TypeError> {
         // check consistent syntax
         if tokens.len() == 0  || lo > hi{
            panic!("Should not be parsing 0-length expression")
        }

        let mut hi = hi;

        // Try to get first expression
        let r_val = match &tokens[hi] {

            tokenizer::Token::ParOpen => {
                // If parenthesis expression, then 
                let last_pos = Expr::find_openning_par(tokens, lo, hi)?;

                // compute expression
                let new_expr = Expr::parse_expr_helper(tokens, lo+1, last_pos-1)?;

                // move next token one to left
                hi = last_pos - 1;

                Expr::ParExpr(Box::new(new_expr)) 
            },

            tokenizer::Token::Id(name) => { hi -= 1; Expr::Atom(name.clone()) },

            _ => return Err(TypeError::InvalidSyntax)
        };

        // eveything done
        if lo > hi {
            return Ok(r_val)
        }

        // Parse left argument of function application
        let l_val = Expr::parse_expr_helper(tokens, lo, hi)?;

        // create function to return 
        let f = FuncApply{function : l_val, arg : r_val};

        Ok( Expr::Apply( Box::new(f) ) )
    }

    /// Find position of openning parenthesis that matches the one in position "end_position",
    /// up to position "start_pos"
    fn find_openning_par(tokens : &Vec<tokenizer::Token>,start_pos : usize, end_pos : usize) -> Result<usize, TypeError> {

        let mut parity = 0;

        for i in (start_pos..end_pos+1).rev() {
            match &tokens[i] {
                tokenizer::Token::ParClosed => parity += 1,
                tokenizer::Token::ParOpen   => parity -=1,
                _                           => {} 
            };

            if parity == 0 {
                return Ok(i)
            }
        };

        Err(TypeError::InvalidSyntax)
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

        let tokens : Vec<tokenizer::Token> = rawtype.iter().map(tokenizer::Token::new).collect();

        if tokens.len() == 0 {
            return Err(TypeError::InvalidSyntax)
        }

        Type::parse_type_helper(&tokens, 0, tokens.len() - 1)
    }

    /// Print a human-readable representation of this Type object
    pub fn display(&self) -> String {
        match self {
            Type::Const(s)   => format!(" {} ", s.name),
            Type::Var(s)     => format!(" {} ", s.name),
            Type::ParType(s) => format!("({})", s.display()),
            Type::Func(f)    => format!("{} -> {}", f.l_type.display(), f.r_type.display())
        }
    }

    /// Utility function to parse expresion from a list of tokens, considering only
    /// tokens in range [lo, hi]
    fn parse_type_helper(tokens : &Vec<tokenizer::Token>, lo : usize, hi : usize) -> Result<Type, TypeError> {
        // check consistent syntax
        if tokens.len() == 0  || lo > hi{
            return Err(TypeError::InvalidSyntax)
        }

        let mut lo = lo;

        // Try to get first expression
        let l_val = match &tokens[lo] {
            tokenizer::Token::ParOpen => {
                // If parenthesis expression, then 
                let last_pos = Type::find_closing_par(tokens, lo, hi)?;

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

    /// Find closing parenthesis that matches the one in position "start_pos" up to position "end_pos"
    fn find_closing_par(tokens : &Vec<tokenizer::Token>, start_pos : usize, end_pos : usize) -> Result<usize, TypeError> {
        
        let mut parity_counter = 0;

        for i in start_pos..end_pos+1{
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

    /// utility function to create an atomic type, could be variant or constant
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