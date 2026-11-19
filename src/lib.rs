use core::f64;

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Value(Value),
    Operation(String),
    Addition,
    Subtraction,
    Multiplication,
    Division,
    LeftParenthesis,
    RightParenthesis,
    Factorial,
    Power,
    Eof,
}

#[derive(Debug)]
struct Lexer {
    tokens: Vec<Token>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Value {
    num: f64,
    unit: Option<UnitType>,
}

#[derive(Debug, Clone, PartialEq)]
enum UnitType {
    Distance(String),
    Mass(String),
    Time(String),
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
        // roots
        "sqrt" => rhs.sqrt(),
        "cbrt" => rhs.cbrt(),
        // trigonometric
        "sin" => rhs.sin(),
        "cos" => rhs.cos(),
        "tan" => rhs.tan(),
        "sec" => 1.0 / rhs.cos(),
        "csc" => 1.0 / rhs.sin(),
        "cot" => 1.0 / rhs.tan(),
        // inverse trigonometric
        "asin" | "arcsin" => rhs.asin(),
        "acos" | "arccos" => rhs.acos(),
        "atan" | "arctan" => rhs.atan(),
        // hyperbolic
        "sinh" => rhs.sinh(),
        "cosh" => rhs.cosh(),
        "tanh" => rhs.tanh(),
        "asinh" | "arsinh" => rhs.asinh(),
        "acosh" | "arcosh" => rhs.acosh(),
        "atanh" | "artanh" => rhs.atanh(),
        // exponential & logarithmic
        // (the lexer only builds words from letters, so log10/log2 are spelled lg/lb)
        "exp" => rhs.exp(),
        "ln" => rhs.ln(),
        "log" | "lg" => rhs.log10(),
        "lb" => rhs.log2(),
        // rounding & sign
        "abs" => rhs.abs(),
        "floor" => rhs.floor(),
        "ceil" => rhs.ceil(),
        "round" => rhs.round(),
        "trunc" => rhs.trunc(),
        "sign" | "sgn" => {
            if rhs == 0.0 {
                0.0
            } else {
                rhs.signum()
            }
        }
        // special
        "gamma" => libm::tgamma(rhs),
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
                if let Ok(float) = number.parse() {
                    tokens.push(Token::Value(Value {
                        num: float,
                        unit: None,
                    }));
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

pub fn expr(input: &str) -> Option<Value> {
    let mut lexer = Lexer::new(input);
    let res = expr_bp(&mut lexer, 0);
    if lexer.peek() == Token::Eof {
        res
    } else {
        None
    }
}

fn expr_bp(lexer: &mut Lexer, min_bp: u8) -> Option<Value> {
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

            eval_prefix(&op, rhs)?
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

fn eval_prefix(op: &Token, rhs: Value) -> Option<Value> {
    let rhs_num = rhs.num;
    let res = match op {
        Token::Addition => rhs_num,
        Token::Subtraction => -rhs_num,
        Token::Operation(func) => match_functions(func, rhs_num)?,
        _ => return None,
    };
    Some(Value {
        num: res,
        unit: rhs.unit,
    })
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

// TODO: ADD UNIT TYPE CHECKING HERE
fn eval_infix(op: &Token, lhs: Value, rhs: Value) -> Option<Value> {
    let lhs_num = lhs.num;
    let rhs_num = rhs.num;
    let res = match op {
        Token::Addition => lhs_num + rhs_num,
        Token::Subtraction => lhs_num - rhs_num,
        Token::Multiplication => lhs_num * rhs_num,
        Token::Division => lhs_num / rhs_num,
        _ => return None,
    };
    Some(Value {
        num: res,
        unit: lhs.unit,
    })
}

fn postfix_binding_power(op: &Token) -> Option<(u8, ())> {
    let res = match op {
        Token::Factorial => (7, ()),
        _ => return None,
    };
    Some(res)
}

fn eval_postfix(op: &Token, lhs: Value) -> Option<Value> {
    let lhs_num = lhs.num;
    let res = match op {
        Token::Factorial => libm::tgamma(lhs_num + 1.0),
        _ => return None,
    };
    Some(Value {
        num: res,
        unit: lhs.unit,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::f64::consts::{E, PI};

    fn check(cases: &[(&str, f64)]) {
        for &(input, expected) in cases {
            let got = expr(input).unwrap_or_else(|| panic!("`{input}` failed to parse"));
            let num = got.num;
            assert!(
                (num - expected).abs() < 1e-9,
                "`{input}`: expected {expected}, got {num}"
            );
        }
    }

    #[test]
    fn numbers() {
        check(&[("5", 5.0), ("5.32", 5.32), (".5", 0.5), ("  7  ", 7.0)]);
    }

    #[test]
    fn arithmetic() {
        check(&[
            ("1+2", 3.0),
            ("5-8", -3.0),
            ("2*3+4", 10.0),
            ("2+3*4", 14.0),
            ("10/4", 2.5),
            ("5.32*3/3", 5.32),
            ("(2+3)*4", 20.0),
            ("1 + 2 * 3", 7.0),
        ]);
    }

    #[test]
    fn unary_and_factorial() {
        check(&[
            ("-3", -3.0),
            ("+3", 3.0),
            ("--3", 3.0),
            ("5!", 120.0),
            ("0!", 1.0),
            ("3!+1", 7.0),
        ]);
    }

    #[test]
    fn implicit_multiplication() {
        check(&[
            ("5(10)", 50.0),
            ("2sqrt(16)", 8.0),
            ("5+3-10(sqrt(4))", -12.0),
        ]);
    }

    #[test]
    fn roots() {
        check(&[
            ("sqrt25", 5.0),
            ("sqrt(25)", 5.0),
            ("sqrt 25", 5.0),
            ("cbrt27", 3.0),
            ("cbrt(-8)", -2.0),
        ]);
    }

    #[test]
    fn trigonometric() {
        check(&[
            ("sin0", 0.0),
            ("cos0", 1.0),
            ("cos(3.141592653589793)", -1.0),
            ("tan0", 0.0),
            ("sec0", 1.0),
            ("csc(1.5707963267948966)", 1.0),
            ("cot(0.7853981633974483)", 1.0),
        ]);
    }

    #[test]
    fn inverse_trigonometric() {
        check(&[
            ("asin1", PI / 2.0),
            ("arcsin1", PI / 2.0),
            ("acos(-1)", PI),
            ("arccos1", 0.0),
            ("atan1", PI / 4.0),
            ("arctan0", 0.0),
        ]);
    }

    #[test]
    fn hyperbolic() {
        check(&[
            ("sinh0", 0.0),
            ("cosh0", 1.0),
            ("tanh0", 0.0),
            ("sinh1", (E - 1.0 / E) / 2.0),
            ("asinh0", 0.0),
            ("arsinh0", 0.0),
            ("acosh1", 0.0),
            ("arcosh1", 0.0),
            ("atanh0", 0.0),
            ("artanh0", 0.0),
        ]);
    }

    #[test]
    fn exponential_and_logarithmic() {
        check(&[
            ("exp0", 1.0),
            ("exp1", E),
            ("ln(exp(2))", 2.0),
            ("log1000", 3.0),
            ("lg100", 2.0),
            ("lb8", 3.0),
        ]);
    }

    #[test]
    fn rounding_and_sign() {
        check(&[
            ("abs(-3.5)", 3.5),
            ("floor2.7", 2.0),
            ("floor(-2.3)", -3.0),
            ("ceil2.1", 3.0),
            ("round2.5", 3.0),
            ("trunc(-2.7)", -2.0),
            ("sign(-5)", -1.0),
            ("sgn5", 1.0),
            ("sign0", 0.0),
        ]);
    }

    #[test]
    fn gamma() {
        check(&[("gamma5", 24.0), ("gamma(0.5)", PI.sqrt())]);
    }

    #[test]
    fn combined() {
        check(&[
            ("sin0+cos0", 1.0),
            ("lg(100)+lb(8)", 5.0),
            ("-abs3", -3.0),
            ("sqrt(sqrt(16))", 2.0),
            ("floor(2.7)*ceil(2.1)", 6.0),
            ("2sqrt(9)+1", 7.0),
        ]);
    }

    #[test]
    fn invalid_input() {
        for input in ["", "5.32.3", "(1+2", "1+2)", "2 $ 3", "foo(1)", "1+", "*2"] {
            assert_eq!(expr(input), None, "`{input}` should not parse");
        }
    }
}
