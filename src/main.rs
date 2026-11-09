// heavily (with emphasis) based on https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

#[derive(Debug)]
enum Token {
    Value(f64),
    Operation(String),
    Addition,
    Subtraction,
    Multiplication,
    Division,
    LeftParenthesis,
    RightParenthesis,
    Factorial,
    Power,
    Unit,
    Eof,
}
#[derive(Debug)]
struct Lexer {
    tokens: Vec<Token>,
}

fn matches_simple_operation(c: char) -> Option<Token> {
    match c {
        '+' => Some(Token::Addition),
        '-' => Some(Token::Subtraction),
        '*' => Some(Token::Multiplication),
        '/' => Some(Token::Division),
        '(' => Some(Token::LeftParenthesis),
        ')' => Some(Token::RightParenthesis),
        '!' => Some(Token::Factorial),
        '^' => Some(Token::Power),
        _ => None,
    }
}

impl Lexer {
    fn new(input: &str) -> Lexer {
        let mut chars = input.chars().peekable();
        let mut tokens = Vec::new();

        while let Some(&c) = chars.peek() {
            if c.is_ascii_whitespace() {
                // if char is whitespace skip
                chars.next();
            } else if let Some(matching_token) = matches_simple_operation(c) {
                chars.next();
                tokens.push(matching_token);
            } else if c.is_ascii_digit() || c == '.' {
                let mut number = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_ascii_digit() || ch == '.' {
                        number.push(ch);
                        chars.next();
                    } else {
                        break;
                    }
                }
                // check if we can turn number into float
                if let Some(float) = number.parse().ok() {
                    tokens.push(Token::Value(float));
                } else {
                    // if we cant then parsing has failed
                    tokens.clear();
                    return Lexer { tokens };
                }
            }
            // if char is non numeric we are attempting to make a word
            else if c.is_ascii_alphabetic() {
                let mut word = String::new();
                while let Some(&ch) = chars.peek() {
                    // consume chars to make word
                    if ch.is_ascii_alphabetic() {
                        word.push(ch);
                        chars.next();
                    } else {
                        // word has finished
                        break;
                    }
                }
                // add the word to token list
                tokens.push(Token::Operation(word));
            }
        }

        tokens.reverse();
        Lexer { tokens }
    }

    fn next(&mut self) -> Token {
        self.tokens.pop().unwrap_or(Token::Eof)
    }

    fn peek(&mut self) -> &Token {
        self.tokens.last().unwrap_or(&Token::Eof)
    }
}

fn expr(input: &str) -> Option<f64> {
    let mut lexer = Lexer::new(input);
    expr_bp(&mut lexer, 0)
}

fn expr_bp(lexer: &mut Lexer, min_bp: u8) -> Option<f64> {
    let mut result = None;
    if let Some(lhs) = match lexer.next() {
        Token::Value(it) => Some(it),
        _ => None,
    } {
        loop {
            if let Some(op) = match lexer.next() {
                Token::Eof => break,
                Token::Operation(operation) => Some(operation),
                Token::Addition => Some("+".to_owned()),
                Token::Subtraction => Some("-".to_owned()),
                Token::Multiplication => Some("*".to_owned()),
                Token::Division => Some("/".to_owned()),
                Token::LeftParenthesis => Some("(".to_owned()),
                Token::RightParenthesis => Some(")".to_owned()),
                Token::Factorial => Some("!".to_owned()),
                Token::Power => Some("^".to_owned()),
                _ => None,
            } {
                let (l_bp, r_bp) = infix_binding_power(&op);
                if l_bp < min_bp {
                    break;
                }

                lexer.next();
                if let Some(rhs) = expr_bp(lexer, r_bp) {
                    result = match op.as_str() {
                        "+" => Some(lhs + rhs),
                        _ => None,
                    }
                }
            }
        }
    }

    result
}

fn infix_binding_power(op: &str) -> (u8, u8) {
    match op {
        "+" | "-" => (1, 2),
        "*" | "/" => (3, 4),
        _ => panic!("bad op: {:?}", op),
    }
}

#[test]
fn tests() {
    if let Some(s) = expr("1") {
        print!("{s}");
    }
}

fn main() {
    dbg!(Lexer::new("5+3-10(sqrt(3))"));
    dbg!(Lexer::new("5.32"));
    dbg!(Lexer::new("5.32.3"));
    dbg!(Lexer::new("5.32*3/3"));
    dbg!(expr("4"));
    if let Some(s) = expr("1") {
        print!("{s}");
    }
}
