/*
  Driver code for our logic
*/

// Rust imports
use std::io;
use std::io::Write;

// Internal imports
use crate::type_resolver::*;

/// Program state
pub struct Program {
    manager : TypeManager,
    running : bool
}

/// possible user actions
pub enum Action {
    Exit,
    Def(Name, RawType),
    Type(RawExpr)
}

/// Possible parsing or input errors
#[derive(Debug)]
pub enum ProgramError {
    NotEnoughArgs,
    InvalidAction(String)
}

impl Program {

	// Crea un programa nuevo listo para correr
	pub fn new() -> Program {
		Program {
				manager: TypeManager::new(),
				running: true
		}
	}

	/// Tells if this program should run
	pub fn should_run(&self) -> bool {
		self.running
	}

	/// Run one iteration for this program
	pub fn run(&mut self) {
		// Command buffer: store user input in this line
		let mut line = String::new();

		print!(">> "); // print prompt
		// flush so the print! doesn't mess up the execution order with read_line
		io::stdout().flush().expect("Couldn't flush stdout"); 

		// Read a single line
		if let Err(_) = io::stdin().read_line(&mut line) { panic!("Error leyendo input D:") }
		
		// parse a line and do its corresponding action if Result is ok, 
		// otherwise print an error
		let _ = Program::parse(line)
				.and_then(
					|a| {
						self.do_action(a);
						Ok(())
					}
				)
				.or_else(
					|e| {
					println!("[ERROR] {}", e.display());
					Err(e)
					}
				);
	}

	/// Perform the given action, and handle errors if some
	/// ## Params
	/// * `action` - Action to perform
	pub fn do_action(&mut self, action : Action) {

		let res : Result<(), TypeError> = match action {
			Action::Exit => { self.running = false; return },
			Action::Def(name, raw_type) => self.do_def(name, raw_type),
			Action::Type(raw_expr)      => self.do_type(raw_expr)
		};

		// print error if necessary
		let _ = res.or_else(|e| {
						println!("[TYPE ERROR] {}", e.display());
						Err(e)
					});
	}

	/// Perform a type definition action
	fn do_def(&mut self, name : Name, rawtype : RawType) -> Result<(), TypeError> {
		
		// get type, if there was an error, return such error
		let actual_type = Type::new(rawtype)?;

		let display = actual_type.display();

		// if everything went ok, print it, otherwise just return the error
		self.manager.add_type(&name, actual_type);
		println!("añadido '{}' con tipo {}", name, display);

		Ok(())
	}

	/// perform a "type" operation, printing the actual type for this expression if any
	fn do_type(&mut self, rawexpr : RawExpr) -> Result<(), TypeError> {
		
		let expr  = Expr::new(rawexpr)?;
		
		// get type for this expression and then print it if correct
		self.manager.get_type(&expr)
					.and_then( |t| { println!("{}", t.display()); Ok(()) } )
	}

	/// Get next action from user input
	fn parse(input: String) -> Result<Action, ProgramError>{

		// replace substrings for cpu friendly ones
		let input = input.replace( "(", " ( ");
		let input = input.replace( ")", " ) ");
		let input = input.replace( "->", " -> ");
		let mut input = input.split_whitespace();


		// Try to Parse verb from input
		let action = match input.next() {
			None  => return Err( ProgramError::NotEnoughArgs ),
			Some(s) => s.to_lowercase()
		};

		// parse the next possible action
		match action.as_str() {
			"salir"     => Ok(Action::Exit),
			"def"       => Program::parse_def(input),
			"tipo"      => Program::parse_type(input),
			s           => Err(ProgramError::InvalidAction(s.to_string()))
		}
	}

	/// Parse properties from token iterator and create a new definition action
	fn parse_def<'a, I>(input:  I) -> Result<Action, ProgramError> 
	where 
		I: Iterator<Item=&'a str>
	{
		let mut input = input; // honestly i don't get why this is necessary for this function to type check but ok

		// parse name, if not found, then not enough arguments error is raised
		let name = match &mut input.next() {
			None    => return Err( ProgramError::NotEnoughArgs ),
			Some(e) => e.to_string()
		};

		// Create a typelist from the rest of our tokens
		let types : RawType = input.map(|s| s.to_string()).collect();

		Ok(Action::Def(name, types))
	}

	/// Parse properties from token iterator and create a new definition action
	fn parse_type<'a, I>(input:  I) -> Result<Action, ProgramError> 
	where 
		I: Iterator<Item=&'a str>
	{
		// Create a raw type expression from remaining elements
		let types : RawExpr = input.map(|s| s.to_string()).collect();

		// no empty type allowed
		if types.is_empty() {
		return Err(ProgramError::NotEnoughArgs)
		}

		Ok(Action::Type(types))
	}

}

impl ProgramError {
	pub fn display(&self) -> String {
		format!("{:?}", self)
	} 
}