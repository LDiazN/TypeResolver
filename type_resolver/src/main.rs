mod driver;
mod type_resolver;
mod utils;
mod test_suite;

fn main() {
    let mut program = driver::Program::new();

    println!("Bienvenido no a ghci, sino al resolvedor de tipos de Luis\n");

    println!("      - Powered by Rust 😎 ⚙️\n");

    while program.should_run() {
        program.run()
    }
}
