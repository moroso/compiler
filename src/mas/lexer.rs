use std::fmt;
use std::fmt::{Formatter, Display};
use std::io;
//use std::str::StrExt;
use std::ascii::AsciiExt;
use util::lexer::*;
use super::ast;
use super::ast::*;

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Token {
    WS,

    Long,
    DotGlobl,

    Reg(ast::Reg),
    CoReg(ast::CoReg),
    PredReg(ast::Pred),
    NumLit(u32),
    IdentTok(String),
    Shift(ShiftType),
    LoadStore(LsuWidth),
    Flush(FlushType),

    Colon,
    LBrace,
    RBrace,
    Semi,
    Plus,
    Dash,
    DashColon,
    Lt,
    Lts,
    Le,
    Les,
    Bs,
    Bc,
    LParen,
    RParen,
    Eq,
    EqEq,
    Tilde,
    TildePipe,
    Amp,
    Caret,
    Nor,
    Pipe,
    Sxb,
    Sxh,
    Mov,
    Mvn,
    Sub,
    Rsb,
    Star,
    StarS,
    StarU,
    Slash,
    SlashW,
    SlashS,
    SlashSW,
    SlashU,
    SlashUW,
    Xor,
    Or,
    And,
    Nop,
    B,
    Bl,
    Break,
    Syscall,
    Eret,
    Ovf,
    Fence,

    Gets,
    Predicates,

    // Special
    Eof,

    // The following two serve as flags for the lexer (to indicate when we
    // enter and exit a multi-line comment). They should never be part of
    // the token stream it generates.
    BeginComment,
    EndComment,
}
allow_string!(Token);

// XXX: This is a workaround I don't totally understand and might not work.
fn mk_rule<A: 'static, T: RuleMatcher<A>+'static, U: TokenMaker<A, Token>+'static>(
    matcher: T, maker: U)
    -> Box<LexerRuleT<Token>> {
    box LexerRule::<A, _, _>::new(matcher, maker) as Box<LexerRuleT<Token>>
}

pub fn new_asm_lexer<'a, T: BufReader, S: ?Sized + ToString>(
    name: &S,
    buffer: T) -> Lexer<'a, T, Token> {

    macro_rules! lexer_rules {
        ( $( $c:expr => $m:expr ),*) => (
            vec!( $( mk_rule($m, $c) ),* )
                )
    }
    macro_rules! matcher { ( $e:expr ) => ( regex!(concat!("^(?:", $e, ")"))) }

    // Matcher for a register, such as "r8".
    struct RegisterRule;
    impl RuleMatcher<ast::Reg> for RegisterRule {
        fn find(&self, s: &str) -> Option<(usize, ast::Reg)> {
            let matcher = matcher!(r"r(\d+|l)");
            match matcher.captures(s) {
                Some(groups) => {
                    Some((groups.at(0).unwrap().len(),
                          if groups.at(1).unwrap() == "l" {
                              ast::Reg { index: 31 }
                          } else {
                              let n = u8::from_str_radix(groups.at(1).unwrap(), 10).unwrap();
                              ast::Reg { index: n }
                          }))
                },
                _ => {
                    let matcher = matcher!(r"lr");
                    match matcher.captures(s) {
                        Some(groups) => Some((groups.at(0).unwrap().len(),
                                              ast::Reg { index: 31 })),
                        _ => None
                    }
                }
            }
        }
    }

    struct CharLiteralRule;
    impl RuleMatcher<u32> for CharLiteralRule {
        fn find(&self, s: &str) -> Option<(usize, u32)> {
            let matcher = matcher!(r"'(.)'");

            match matcher.captures(s) {
                Some(groups) => {
                    let c = groups.at(1).unwrap().as_bytes()[0] as u32;

                    Some((groups.at(0).unwrap().len(), c))
                },
                _ => None
            }
        }
    }

    struct NumberLiteralRule;
    impl RuleMatcher<u32> for NumberLiteralRule {
        fn find(&self, s: &str) -> Option<(usize, u32)> {
            let matcher = matcher!(r"(-?)((?:0[xX]([:xdigit:]+))|(?:0[bB]([01]+))|(?:\d+))(?:([uUiI])(32|16|8)?)?");

            match matcher.captures(s) {
                Some(groups) => {
                    // TODO: match on (groups.at(3).unwrap(), groups.at(4).unwrap()) when rust
                    // fixes issue #14927.
                    let (num_str, radix) = match groups.at(3) {
                        None => match groups.at(4) {
                            None => (groups.at(2).unwrap(), 10),
                            Some(bin) => (bin, 2),
                        },
                        Some(hex) => (hex, 16),
                    };

                    let negated = groups.at(1).unwrap() == "-";

                    let mut num = u32::from_str_radix(num_str, radix).unwrap();

                    if negated { num = -(num as i32) as u32; }

                    Some((groups.at(0).unwrap().len(), num))
                },
                _ => None
            }
        }
    }

    struct PredicateRule;
    impl RuleMatcher<Pred> for PredicateRule {
        fn find(&self, s: &str) -> Option<(usize, Pred)> {
            let matcher = matcher!(r"(!?)p([0123])");
            match matcher.captures(s) {
                Some(groups) => {
                    Some((groups.at(0).unwrap().len(),
                          Pred {
                              inverted: groups.at(1).unwrap() == "!",
                              reg: groups.at(2).unwrap().as_bytes()[0] - '0' as u8
                          }))
                },
                _ => None
            }
        }
    }

    struct ShiftRule;
    impl RuleMatcher<ShiftType> for ShiftRule {
        fn find(&self, s: &str) -> Option<(usize, ShiftType)> {
            let matcher = matcher!(r"(<<|>>u|>>s|>>r)");
            match matcher.captures(s) {
                Some(groups) => {
                    let shift_type = match groups.at(1).unwrap() {
                        "<<" => SllShift,
                        ">>u" => SrlShift,
                        ">>s" => SraShift,
                        ">>r" => RorShift,
                        _ => panic!(),
                    };

                    Some((groups.at(0).unwrap().len(), shift_type))
                },
                _ => None
            }
        }
    }

    struct LoadStoreRule;
    impl RuleMatcher<LsuWidth> for LoadStoreRule {
        fn find(&self, s: &str) -> Option<(usize, LsuWidth)> {
            let matcher = matcher!(r"\*(sc|llsc|ll|w|l|h|b)");
            match matcher.captures(s) {
                Some(groups) =>
                    Some((groups.at(0).unwrap().len(),
                          match groups.at(1).unwrap() {
                              "w" |
                              "l" => LsuWidthL,
                              "h" => LsuWidthH,
                              "b" => LsuWidthB,
                              "ll" |
                              "sc" |
                              "llsc" => LsuLLSC,
                              _ => panic!(),
                          }
                          )),
                    _ => None,
            }
        }
    }

    struct CoRegRule;
    impl RuleMatcher<ast::CoReg> for CoRegRule {
        fn find(&self, s: &str) -> Option<(usize, ast::CoReg)> {
            let matcher = matcher!(r"(?i:(PFLAGS|PTB|EHA|EPC|EC0|EC1|EC2|EC3|EA0|EA1|SP0|SP1|SP2|SP3))");
            match matcher.captures(s) {
                Some(groups) =>
                    Some((groups.at(0).unwrap().len(),
                          match &groups.at(1).unwrap().to_ascii_uppercase()[..] {
                              "PFLAGS" => PFLAGS,
                              "PTB" => PTB,
                              "EHA" => EHA,
                              "EPC" => EPC,
                              "EC0" => EC0,
                              "EC1" => EC1,
                              "EC2" => EC2,
                              "EC3" => EC3,
                              "EA0" => EA0,
                              "EA1" => EA1,
                              "SP0" => SP0,
                              "SP1" => SP1,
                              "SP2" => SP2,
                              "SP3" => SP3,
                              _ => panic!(),
                          })),
                _ => None,
            }
        }
    }

    struct FlushRule;
    impl RuleMatcher<FlushType> for FlushRule {
        fn find(&self, s: &str) -> Option<(usize, FlushType)> {
            let matcher = matcher!(r"(?i:flush\.(data|inst|dtlb|itlb))");
            match matcher.captures(s) {
                Some(groups) =>
                    Some((groups.at(0).unwrap().len(),
                          match &groups.at(1).unwrap().to_ascii_uppercase()[..] {
                              "DATA" => DataFlush,
                              "INST" => InstFlush,
                              "DTLB" => DtlbFlush,
                              "ITLB" => ItlbFlush,
                              _ => panic!(),
                          })),
                _ => None,
            }
        }
    }

    let rules = lexer_rules! {
        // Whitespace, including C-style comments
        Token::WS         => matcher!(r"//.*|\s"),

        Token::Colon      => ":",
        Token::LBrace     => "{",
        Token::RBrace     => "}",
        Token::Semi       => ";",
        Token::Gets       => "<-",
        Token::Predicates => "?",
        Token::Plus       => "+",
        Token::Dash       => "-",
        Token::Sub        => matcher!(r"(?i:sub)"),
        Token::DashColon  => "-:",
        Token::Rsb        => matcher!(r"(?i:rsb)"),
        Token::Star       => "*",
        Token::StarU      => matcher!(r"(?i:\*u)"),
        Token::StarS      => matcher!(r"(?i:\*s)"),
        Token::Slash      => "/",
        Token::SlashW     => "/w",
        Token::SlashU     => matcher!(r"(?i:/u)"),
        Token::SlashS     => matcher!(r"(?i:/s)"),
        Token::SlashUW    => matcher!(r"(?i:/(uw|wu))"),
        Token::SlashSW    => matcher!(r"(?i:/(sw|ws))"),
        Token::Lt         => "<",
        Token::Lt         => "<u",
        Token::Lt         => matcher!(r"(?i:ltu)"),
        Token::Lts        => "<s",
        Token::Lts        => matcher!(r"(?i:lts)"),
        Token::Le         => "<=",
        Token::Le         => "<=u",
        Token::Le         => matcher!(r"(?i:leu)"),
        Token::Les        => "<=s",
        Token::Les        => matcher!(r"(?i:les)"),
        Token::Bs         => matcher!(r"(?i:bs)"),
        Token::Bc         => matcher!(r"(?i:bc)"),
        Token::LParen     => "(",
        Token::RParen     => ")",
        Token::Eq         => "=",
        Token::EqEq       => "==",
        Token::Tilde      => "~",
        Token::Amp        => "&",
        Token::And        => matcher!(r"(?i:and)"),
        Token::Caret      => "^",
        Token::Xor        => matcher!(r"(?i:xor)"),
        Token::TildePipe  => "~|",
        Token::Nor        => matcher!(r"(?i:nor)"),
        Token::Pipe       => "|",
        Token::Or         => matcher!(r"(?i:or)"),
        Token::Sxb        => matcher!(r"(?i:sxb)"),
        Token::Sxh        => matcher!(r"(?i:sxh)"),
        Token::Mov        => matcher!(r"(?i:mov)"),
        Token::Mvn        => matcher!(r"(?i:mvn)"),
        Token::Nop        => matcher!(r"(?i:nop)"),
        Token::B          => matcher!(r"(?i:b)"),
        Token::Bl         => matcher!(r"(?i:bl)"),
        Token::Break      => matcher!(r"(?i:break)"),
        Token::Syscall    => matcher!(r"(?i:syscall)"),
        Token::Eret       => matcher!(r"(?i:eret)"),
        Token::Fence      => matcher!(r"(?i:fence)"),
        Token::Ovf        => matcher!(r"(?i:ovf)"),

        // TODO: the other ops we need.

        // TODO: shifts. Currently, these are lexed incorrectly!

        Token::DotGlobl   => ".globl",
        Token::Long       => matcher!(r"(?i:long)"),

        Token::NumLit     => CharLiteralRule,
        Token::NumLit     => NumberLiteralRule,

        Token::Reg        => RegisterRule,
        Token::CoReg      => CoRegRule,
        Token::PredReg    => PredicateRule,
        Token::Shift      => ShiftRule,
        Token::LoadStore  => LoadStoreRule,
        Token::Flush      => FlushRule,
        Token::IdentTok   => matcher!(r"[a-zA-Z_]\w*"),
        // TODO: a specific matcher for this.
        Token::IdentTok   => matcher!(r"\.[0-9]+(a|b)?"),

        Token::BeginComment => matcher!(r"/\*")
    };

    // A special set of rules, just for when we're within a multi-line
    // comment.
    let comment_rules = lexer_rules! {
        // The start of a multi-line comment.
        // This is to properly handle nested multiline comments.
        // We increase the multiline-comment-nesting-counter with this.
        Token::BeginComment => matcher!(r"/\*"),

        // The end of a multi-line comment.
        // We decrease the multiline-comment-nesting-counter with this.
        Token::EndComment => matcher!(r"\*/"),

        // If we're within a comment, any string not
        // containing "/*" or "*/" is considered whitespace.
        Token::WS => matcher!(r"(?:.|\n)")
    };

    let morasm = Language {
        rules: rules,
        comment_rules: comment_rules,
        eof: Token::Eof,
        ws: Token::WS,
        begin_comment: Token::BeginComment,
        end_comment: Token::EndComment,
    };

    Lexer::new(morasm, name, buffer)
}

// TODO: genericize these, so they're not duplicated between here and
// compiler_lexer.
impl TokenMaker<(), Token> for Token {
    fn mk_tok(&self, _: ()) -> Token { self.clone() }
}

impl<T, F: Fn(T) -> Token> TokenMaker<T, Token> for F {
    fn mk_tok(&self, arg: T) -> Token { (*self)(arg) }
}

// Convenience for tests
pub fn asm_lexer_from_str(s: &str) -> Lexer<io::BufReader<&[u8]>, Token> {
    let bytes = s.as_bytes();
    let buffer = io::BufReader::new(bytes);
    new_asm_lexer("test", buffer)
}
