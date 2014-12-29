use std::io;
use std::str::StrExt;
use std::ascii::AsciiExt;
use util::lexer::*;
use super::ast;
use super::ast::*;

pub use self::Token::*;

#[deriving(Eq, PartialEq, Clone, Show)]
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
    SlashS,
    SlashU,
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

pub fn new_asm_lexer<'a, T: BufReader, Sized? S: StrExt>(
    name: &S,
    buffer: T) -> Lexer<'a, T, Token> {

    macro_rules! lexer_rules {
        ( $( $c:expr => $m:expr ),*) => (
            vec!( $( box LexerRule { matcher: $m, maker: $c } as Box<LexerRuleT<Token>> ),* )
                )
    }
    macro_rules! matcher { ( $e:expr ) => ( regex!(concat!("^(?:", $e, ")"))) }

    // Matcher for a register, such as "r8".
    struct RegisterRule;
    impl RuleMatcher<ast::Reg> for RegisterRule {
        fn find(&self, s: &str) -> Option<(uint, ast::Reg)> {
            use std::num::from_str_radix;

            let matcher = matcher!(r"r(\d+|l)");
            match matcher.captures(s) {
                Some(groups) => {
                    Some((groups.at(0).unwrap().len(),
                          if groups.at(1).unwrap() == "l" {
                              ast::Reg { index: 31 }
                          } else {
                              let n = from_str_radix(groups.at(1).unwrap(), 10).unwrap();
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
        fn find(&self, s: &str) -> Option<(uint, u32)> {
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
        fn find(&self, s: &str) -> Option<(uint, u32)> {
            use std::num::from_str_radix;

            let matcher = matcher!(r"(-?)((?:0[xX]([:xdigit:]+))|(?:0[bB]([01]+))|(?:\d+))(?:([uUiI])(32|16|8)?)?");

            match matcher.captures(s) {
                Some(groups) => {
                    // TODO: match on (groups.at(3).unwrap(), groups.at(4).unwrap()) when rust
                    // fixes issue #14927.
                    let (num_str, radix) = match groups.at(3).unwrap() {
                        "" => match groups.at(4).unwrap() {
                            "" => (groups.at(2).unwrap(), 10),
                            bin => (bin, 2),
                        },
                        hex => (hex, 16),
                    };

                    let negated = groups.at(1).unwrap() == "-";

                    let mut num: u32 = from_str_radix(num_str, radix).unwrap();

                    if negated { num = -(num as i32) as u32; }

                    Some((groups.at(0).unwrap().len(), num))
                },
                _ => None
            }
        }
    }

    struct PredicateRule;
    impl RuleMatcher<Pred> for PredicateRule {
        fn find(&self, s: &str) -> Option<(uint, Pred)> {
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
        fn find(&self, s: &str) -> Option<(uint, ShiftType)> {
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
        fn find(&self, s: &str) -> Option<(uint, LsuWidth)> {
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
        fn find(&self, s: &str) -> Option<(uint, ast::CoReg)> {
            let matcher = matcher!(r"(?i:(PFLAGS|PTB|EHA|EPC|EC0|EC1|EC2|EC3|EA0|EA1|SP0|SP1|SP2|SP3))");
            match matcher.captures(s) {
                Some(groups) =>
                    Some((groups.at(0).unwrap().len(),
                          match groups.at(1).unwrap().to_ascii_uppercase().as_slice() {
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
        fn find(&self, s: &str) -> Option<(uint, FlushType)> {
            let matcher = matcher!(r"(?i:flush\.(data|inst|dtlb|itlb))");
            match matcher.captures(s) {
                Some(groups) =>
                    Some((groups.at(0).unwrap().len(),
                          match groups.at(1).unwrap().to_ascii_uppercase().as_slice() {
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
        WS         => matcher!(r"//.*|\s"),

        Colon      => ":",
        LBrace     => "{",
        RBrace     => "}",
        Semi       => ";",
        Gets       => "<-",
        Predicates => "->",
        Plus       => "+",
        Dash       => "-",
        Sub        => matcher!(r"(?i:sub)"),
        DashColon  => "-:",
        Rsb        => matcher!(r"(?i:rsb)"),
        Star       => "*",
        StarU      => matcher!(r"(?i:\*u)"),
        StarS      => matcher!(r"(?i:\*s)"),
        Slash      => "/",
        SlashU     => matcher!(r"(?i:/u)"),
        SlashS     => matcher!(r"(?i:/s)"),
        Lt         => "<",
        Lt         => "<u",
        Lt         => matcher!(r"(?i:ltu)"),
        Lts        => "<s",
        Lts        => matcher!(r"(?i:lts)"),
        Le         => "<=",
        Le         => "<=u",
        Le         => matcher!(r"(?i:leu)"),
        Les        => "<=s",
        Les        => matcher!(r"(?i:les)"),
        Bs         => matcher!(r"(?i:bs)"),
        Bc         => matcher!(r"(?i:bc)"),
        LParen     => "(",
        RParen     => ")",
        Eq         => "=",
        EqEq       => "==",
        Tilde      => "~",
        Amp        => "&",
        And        => matcher!(r"(?i:and)"),
        Caret      => "^",
        Xor        => matcher!(r"(?i:xor)"),
        TildePipe  => "~|",
        Nor        => matcher!(r"(?i:nor)"),
        Pipe       => "|",
        Or         => matcher!(r"(?i:or)"),
        Sxb        => matcher!(r"(?i:sxb)"),
        Sxh        => matcher!(r"(?i:sxh)"),
        Mov        => matcher!(r"(?i:mov)"),
        Mvn        => matcher!(r"(?i:mvn)"),
        Nop        => matcher!(r"(?i:nop)"),
        B          => matcher!(r"(?i:b)"),
        Bl         => matcher!(r"(?i:bl)"),
        Break      => matcher!(r"(?i:break)"),
        Syscall    => matcher!(r"(?i:syscall)"),
        Eret       => matcher!(r"(?i:eret)"),
        Fence      => matcher!(r"(?i:fence)"),
        Ovf        => matcher!(r"(?i:ovf)"),

        // TODO: the other ops we need.

        // TODO: shifts. Currently, these are lexed incorrectly!

        DotGlobl   => ".globl",
        Long       => matcher!(r"(?i:long)"),

        NumLit     => CharLiteralRule,
        NumLit     => NumberLiteralRule,

        Reg        => RegisterRule,
        CoReg      => CoRegRule,
        PredReg    => PredicateRule,
        Shift      => ShiftRule,
        LoadStore  => LoadStoreRule,
        Flush      => FlushRule,
        IdentTok   => matcher!(r"[a-zA-Z_]\w*"),
        // TODO: a specific matcher for this.
        IdentTok   => matcher!(r"\.[0-9]+(a|b)?"),

        BeginComment => matcher!(r"/\*")
    };

    // A special set of rules, just for when we're within a multi-line
    // comment.
    let comment_rules = lexer_rules! {
        // The start of a multi-line comment.
        // This is to properly handle nested multiline comments.
        // We increase the multiline-comment-nesting-counter with this.
        BeginComment => matcher!(r"/\*"),

        // The end of a multi-line comment.
        // We decrease the multiline-comment-nesting-counter with this.
        EndComment => matcher!(r"\*/"),

        // If we're within a comment, any string not
        // containing "/*" or "*/" is considered whitespace.
        WS => matcher!(r"(?:.|\n)")
    };

    let morasm = Language {
        rules: rules,
        comment_rules: comment_rules,
        eof: Eof,
        ws: WS,
        begin_comment: BeginComment,
        end_comment: EndComment,
    };

    Lexer::new(morasm, name, buffer)
}

// TODO: genericize these, so they're not duplicated between here and
// compiler_lexer.
impl TokenMaker<(), Token> for Token {
    fn mk_tok(&self, _: ()) -> Token { self.clone() }
}

impl<T> TokenMaker<T, Token> for fn(T) -> Token {
    fn mk_tok(&self, arg: T) -> Token { (*self)(arg) }
}

// Convenience for tests
pub fn asm_lexer_from_str(s: &str) -> Lexer<io::BufferedReader<io::MemReader>, Token> {
    let bytes = s.as_bytes().to_vec();
    let buffer = io::BufferedReader::new(io::MemReader::new(bytes));
    new_asm_lexer("test", buffer)
}
