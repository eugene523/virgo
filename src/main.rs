mod lexer;

use std::fs;

fn main() {
    let file_path = "C:\\code\\virgo\\test\\test.v";

    let src = fs::read_to_string(file_path).expect("Can't read file.");
    let mut lex = lexer::Lexer::new();
    lex.tokenize(&src);
    lex.print_tokens();
}
