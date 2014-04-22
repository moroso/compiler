#![crate_id="lexer"]
#![crate_type="lib"]
extern crate regexp;
use regexp::Regexp;
use regexp::NFA;

/// A single regexp for a token.
struct LexerRule {
    matcher: NFA,
    token: Token,
}

pub enum Token {
    WhiteSpace,
    Ident,
    Number,
    HexNumber,
    String,
    Let,
    Character(char),
}

pub struct Lexer {
    rules: ~[LexerRule],
}

impl Lexer {
    fn add_rule(&mut self, regexp: &str, token: Token) {
        let nfa = Regexp::parse(regexp).compile();
        self.rules.push(LexerRule {
            matcher: nfa,
            token: token,
            }
        )
    }

    fn add_char_rule(&mut self, c: char) {
        if c == '@' || c == '#' || c == 'a' || c == ' ' {
            self.add_rule(format!("{:c}", c), Character(c));
        } else {
            self.add_rule(format!("\\\\{:c}", c), Character(c));
        }
    }

    pub fn new() -> Lexer {
        let mut l: Lexer = Lexer { rules: ~[] };
        // Note: rules are in decreasing order of priority if there's a
        // conflict. In particular, reserved words must go before Ident.

        l.add_rule("let", Let);
        l.add_rule("*(\\ )", WhiteSpace);
        l.add_rule("\\a*(\\@)", Ident);
        l.add_rule("\\#*(\\#)", Number);
        l.add_rule("0x|(\\#,a,b,c,d,e,f,A,B,C,D,E,F)"+
                   "*(|(\\#,a,b,c,d,e,f,A,B,C,D,E,F))", HexNumber);
        // TODO: this needs to be improved.
        l.add_rule("\"*(|(\\@,\\ ,\\\\\"))\"", String);
        l.add_char_rule('(');
        l.add_char_rule(')');
        l.add_char_rule('+');
        l.add_char_rule('-');
        l.add_char_rule(';');
        l.add_char_rule(':');
        l.add_char_rule('=');
        l
    }

    pub fn tokenize(&self, s: &str) -> (~[(Token, ~str)]) {
        let mut pos = 0u;
        let mut result = ~[];
        while pos < s.len() {
            let mut longest = 0u;
            let mut best_token = None;
            let mut best_str = ~"";
            for rule in self.rules.iter() {
                let m = rule.matcher.match_string(s.slice_from(pos));
                match m {
                    Some(ref s) if s.len() > longest => {
                        best_token = Some(rule.token);
                        best_str = s.clone();
                        longest = s.len();
                    },
                    _ => {},
                }
            }
            pos += longest;
            match best_token.unwrap() {
                WhiteSpace => {},
                x => result.push((x, best_str))
            }
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use Lexer;

    #[test]
    fn test() {
        let l = Lexer::new();

        print!("{:?}\n", l.tokenize("f(x - 0x3f5B)+1 \"Hello\\\" World\""));
        print!("{:?}\n", l.tokenize("let x: int = 5;"));
        assert!(false);
    }
}