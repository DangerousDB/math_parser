// heavily (with emphasis) based on https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

use core::f64;

#[derive(Debug, Clone, PartialEq)]
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

fn match_functions(func: &str, rhs: f64) -> Option<f64> {
    let res = match func {
        "sqrt" => f64::sqrt(rhs),
        _ => return None,
    };
    Some(res)
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
            } else {
                tokens.clear();
                return Lexer { tokens };
            }
        }
        tokens.reverse();
        Lexer { tokens }
    }

    fn next(&mut self) -> Token {
        self.tokens.pop().unwrap_or(Token::Eof)
    }

    fn peek(&self) -> Token {
        self.tokens.last().cloned().unwrap_or(Token::Eof)
    }
}

fn expr(input: &str) -> Option<f64> {
    let mut lexer = Lexer::new(input);
    let res = expr_bp(&mut lexer, 0);
    if lexer.peek() == Token::Eof {
        res
    } else {
        None
    }
}

fn expr_bp(lexer: &mut Lexer, min_bp: u8) -> Option<f64> {
    let mut lhs = match lexer.next() {
        Token::Value(val) => val,
        Token::LeftParenthesis => {
            let lhs = expr_bp(lexer, 0)?;
            if lexer.next() == Token::RightParenthesis {
                lhs
            } else {
                return None;
            }
        }
        op => {
            let ((), r_bp) = prefix_binding_power(&op)?;
            let rhs = expr_bp(lexer, r_bp)?;
            let rhs = eval_prefix(&op, rhs)?;
            rhs
        }
    };

    loop {
        let op = lexer.peek();
        if matches!(op, Token::Eof | Token::RightParenthesis) {
            break;
        }
        if let Some((l_bp, ())) = postfix_binding_power(&op) {
            if l_bp < min_bp {
                break;
            }
            lexer.next();
            lhs = eval_postfix(&op, lhs)?;
            continue;
        }
        if matches!(op, Token::LeftParenthesis | Token::Operation(_)) {
            let (l_bp, r_bp) = infix_binding_power(&Token::Multiplication)?;
            if l_bp < min_bp {
                break;
            }
            let rhs = expr_bp(lexer, r_bp)?;
            lhs = eval_infix(&Token::Multiplication, lhs, rhs)?;
            continue;
        }
        if let Some((l_bp, r_bp)) = infix_binding_power(&op) {
            if l_bp < min_bp {
                break;
            }
            lexer.next();
            let rhs = expr_bp(lexer, r_bp)?;
            lhs = eval_infix(&op, lhs, rhs)?;
            continue;
        }
        return None;
    }
    Some(lhs)
}

fn prefix_binding_power(op: &Token) -> Option<((), u8)> {
    let res = match op {
        Token::Addition | Token::Subtraction => ((), 5),
        Token::Operation(_) => ((), 5),
        _ => return None,
    };
    Some(res)
}

fn eval_prefix(op: &Token, rhs: f64) -> Option<f64> {
    let res = match op {
        Token::Addition => rhs,
        Token::Subtraction => -rhs,
        Token::Operation(func) => match_functions(func, rhs)?,
        _ => return None,
    };
    Some(res)
}

fn infix_binding_power(op: &Token) -> Option<(u8, u8)> {
    let res = match op {
        Token::Addition | Token::Subtraction => (1, 2),
        Token::Multiplication | Token::Division => (3, 4),
        Token::LeftParenthesis => (3, 4),
        _ => return None,
    };
    Some(res)
}

fn eval_infix(op: &Token, lhs: f64, rhs: f64) -> Option<f64> {
    let res = match op {
        Token::Addition => lhs + rhs,
        Token::Subtraction => lhs - rhs,
        Token::Multiplication => lhs * rhs,
        Token::Division => lhs / rhs,
        _ => return None,
    };
    Some(res)
}

fn postfix_binding_power(op: &Token) -> Option<(u8, ())> {
    let res = match op {
        Token::Factorial => (7, ()),
        _ => return None,
    };
    return Some(res);
}

fn eval_postfix(op: &Token, lhs: f64) -> Option<f64> {
    let res = match op {
        Token::Factorial => libm::tgamma(lhs + 1.0),
        _ => return None,
    };
    Some(res)
}

#[test]
fn tests() {
    if let Some(s) = expr("1") {
        print!("{s}");
    }
}

fn main() {
    dbg!(Lexer::new("4"));
    dbg!(Lexer::new("5.32"));
    dbg!(Lexer::new("5(10)"));
    dbg!(Lexer::new("5.32.3"));
    dbg!(Lexer::new("sqrt(9)"));
    dbg!(Lexer::new("5.32*3/3"));
    dbg!(Lexer::new("5+3-10(sqrt(3))"));
    dbg!(expr("4"));
    dbg!(expr("5.32"));
    dbg!(expr("5(10)"));
    dbg!(expr("5.32.3"));
    dbg!(expr("sqrt(9)"));
    dbg!(expr("5.32*3/3"));
    dbg!(expr("5+3-10(sqrt(3))"));
}
