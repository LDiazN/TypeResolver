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
// Type representing a substitution
type Subst = HashMap<Name, Type>;

// --- Expression Types ---

#[derive(Debug, PartialEq, Clone)]
pub struct FuncApply {
    function    : Expr,
    arg         : Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ParentExpression {
    expr : Expr
}

#[derive(Debug, PartialEq, Clone)]
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
    InvalidSyntax,
    InvalidType,
    UndefinedSymbol(Name),
    UnmatchingTypes(Type, Type) // expected, found
}

impl TypeManager {

    /// Create a new manager
    pub fn new() -> TypeManager {
        TypeManager{
            name_to_type : VarTypes::new()
        }
    }

    pub fn get_type(&self, expr : &Expr) -> Result<Type, TypeError> {

        // if atomic or expression, return type directly. 
        // If function, oh no
        let function = match expr {
            Expr::Atom(name) => {

                // If name is defined, return its type, otherwise raise an error
                match self.name_to_type.get(name) {
                    None    => return Err(TypeError::UndefinedSymbol(name.clone())),
                    Some(t) => return Ok(t.clone())
                };
            },
            Expr::ParExpr(e) => return self.get_type(e), // if parenthized type, just return type of internal type

            Expr::Apply(f)   => {(**f).clone()}          // if function, get a copy of this function
            
        };

        // get sub expresions
        let (l_expr, r_expr) = (function.function, function.arg);

        // get type for both sides of function application
        let l_type = self.get_type(&l_expr)?;

        let r_type = self.get_type(&r_expr)?;

        // check if type of left side is function and matches right side type
        let (ll_type, lr_type) = match l_type {
            Type::Func(f) => (f.l_type, f.r_type),
            _             => return Err(TypeError::InvalidType)
        };

        // Try to bind variables in ll_type to patterns in r_type
        let bindings = Type::type_matcher(&ll_type, &r_type)?;

        // Try to replace those bindings in lr_type
        let new_type = Type::replace(&lr_type, &bindings);

        Ok(new_type)
    }


    pub fn add_type(&mut self, typename : &Name, my_type : Type) {
        *self.name_to_type
            .entry(typename.clone())
            .or_insert(my_type) = my_type.clone();
    }

}

impl Expr {

    /// Try tu parse an expression from list of strings such that each string 
    /// is a possible valid token
    /// ## Params
    /// * `expr` - Expression to parse
    /// ---
    /// ## Return
    /// An expression if parse succeeded, or an error otherwise
    pub fn new(expr : RawExpr) -> Result<Expr, TypeError> {

        // build token stream
        let expr : Vec<tokenizer::Token> = expr.iter().map(tokenizer::Token::new).collect();

        if expr.len() == 0 {
            return Err(TypeError::InvalidSyntax)
        }

        Expr::parse_expr_helper(&expr, 0,expr.len() - 1)
    }

    /// helper function required to parse an expression from an array of tokens 'tokens'
    /// from position 'lo' to position 'hi' into a type if possible
    fn parse_expr_helper(tokens : &Vec<tokenizer::Token>, lo : usize, hi : usize) -> Result<Expr, TypeError> {
         // check consistent syntax
         if tokens.len() == 0  || lo > hi{
            return Err(TypeError::InvalidSyntax)
        }

        let mut hi = hi;
        let mut lo = lo;
        // Try to get first expression
        let r_val = match &tokens[hi] {
            
            tokenizer::Token::ParClosed => {
                // If parenthesis expression, then 
                let last_pos = Expr::find_openning_par(tokens, lo, hi)?;
                
                // compute expression
                let new_expr = Expr::parse_expr_helper(tokens, last_pos+1, hi-1)?;

                // move next token one to left
                hi = last_pos - 1;
                
                
                Expr::ParExpr(Box::new(new_expr)) 
            },
            
            // TODO los nombres tienen que existir para que sea valido
            tokenizer::Token::Id(name) => { 
                if hi == lo {
                    lo += 1; // sendo pqc
                }
                else {
                    hi -= 1; 
                }
                Expr::Atom(name.clone()) 
            },
            
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

    /// Return every name in t1 related to its corresponding matching sub type in t2.
    /// Aka returns a list of bindings from names to patterns
    /// ## Params 
    /// * `t1` - Type to be matched by type t2
    /// * `t2` - Type that should match type 1
    /// ---
    /// ## Return 
    /// A substitution object defining bindings from name to expression, or 
    /// a type error in case types doesn't even match
    pub fn type_matcher(t1 : &Type, t2 : &Type) -> Result<Subst, TypeError> {

        let mut bindings = HashMap::new();

        Type::type_matcher_helper(t1, t2,&mut bindings)?;

        Ok(bindings)
    }


    /// Perform a textual substitution for every variable type in t as 
    /// specified by the given substitution object "bindings"
    pub fn replace(t : &Type, bindings : &Subst ) -> Type {

        // try to get a function
        let func = match t {
            Type::Var(var) => {
                let bind = bindings.get(&var.name);
                if bind.is_none() {
                    return t.clone()
                }

                return bind.unwrap().clone()
            },
            Type::ParType(t) => return Type::replace(t, bindings),
            Type::Const(_)   => return t.clone(),
            Type::Func(f)    => *f.clone()
        };

        // Get both sides of function
        let (l_type, r_type) = (func.l_type, func.r_type);

        // if left side is function, should be parenthized
        let new_l_type = match Type::replace(&l_type, bindings) {
            Type::Func(f) => Type::ParType( Box::new(Type::Func(f)) ),
            t             => t
        };
        
        // Return a new function with both sides changed
        let new_f = Function{
            l_type : new_l_type,
            r_type : Type::replace(&r_type, bindings)
        };

        // return a new function type
        Type::Func(Box::new(new_f))
    } 

    fn type_matcher_helper(t1 : &Type, t2 : &Type, bindings : &mut Subst) -> Result<(), TypeError>{
        let type_err = Err( TypeError::UnmatchingTypes(t1.clone(), t2.clone()) );

        // get function in f1
        let l_func = match t1 {
            Type::Var(var) => {

                // If already exists, check if equal. If equal, return ok, else, raise type error
                let existent = bindings.get(&var.name);
                if existent.is_some() && existent.unwrap()!=t2 {
                    return type_err
                }

                // if does not exists, insert it 
                bindings.insert(var.name.clone(), t2.clone());
                return Ok(());
            },
            Type::Const(k1) => {
                if let Type::Const(k2) = t2 {
                    // if constant, t2 should be a constant
                    if k1 == k2 { 
                        return Ok(()) 
                    } else {
                        return type_err 
                    }
                }
                else {
                    // if t2 not constant, raise type error
                    return type_err
                } 
            },
            Type::ParType(t) => return Type::type_matcher_helper(&t, t2, bindings),
            Type::Func(f)  => f
        };

        // If left side is a function, right side should be a function as well
        let r_func = match t2 {
            Type::Var(_)     => return type_err,
            Type::Const(_)   => return type_err,
            Type::ParType(t) => return Type::type_matcher_helper( &Type::Func(Box::new(*l_func.clone()) ), t, bindings),
            Type::Func(f)    => f,
        };

        // left side from left func, and right side for left func
        let (ll_type, rl_type) = (l_func.l_type.clone(), l_func.r_type.clone());
        // left side from right func, and right side for right func
        let (lr_type, rr_type) = (r_func.l_type.clone(), r_func.r_type.clone());

        // deconstruct pattern and find bindings in both sides
        Type::type_matcher_helper(&ll_type, &lr_type, bindings)?;
        Type::type_matcher_helper(&rl_type, &rr_type, bindings)?;

        Ok(())
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
        match self {
            TypeError::InvalidSyntax => "Invalid Syntax".to_string(),
            TypeError::InvalidType   => "Invalid Type".to_string(),
            TypeError::UnmatchingTypes(t1, t2) => format!("Tipos no compatibles.\n   Esperaba: {}\n   Encontré: {}", t1.display(), t2.display()),
            TypeError::UndefinedSymbol(s) => format!("No conozco este símbolo: {}", s)
        }
    }
}