mod driver;
mod type_resolver;
mod utils;

fn main() {
    let mut program = driver::Program::new();

    println!("Bienvenido no a ghci, sino al resolvedor de tipos\n\n");

    while program.should_run() {
        program.run()
    }
}
