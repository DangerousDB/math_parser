fn main() {
    let input = std::env::args().skip(1).collect::<Vec<_>>().join(" ");
    match math_parser::expr(&input) {
        Some(v) => println!("{v}"),
        None => {
            eprintln!("error: invalid expression");
            std::process::exit(1);
        }
    }
}
