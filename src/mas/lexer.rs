use std::io;
use util::lexer::*;
use super::ast;
use super::ast::*;
use regex::Regex;

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
    LSquare,
    RSquare,
    Semi,
    Comma,
    DotDot,
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

    Define,
    Struct,

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
    -> Box<dyn LexerRuleT<Token>> {
    Box::new(LexerRule::<A, _, _>::new(matcher, maker)) as Box<dyn LexerRuleT<Token>>
}

pub fn new_asm_lexer<'a, T: BufReader, S: ?Sized + ToString>(
    name: &S,
    buffer: T) -> Lexer<'a, T, Token> {

    macro_rules! lexer_rules {
        ( $( $c:expr => $m:expr ),*) => (
            vec!( $( mk_rule($m, $c) ),* )
                )
    }
    macro_rules! matcher { ( $e:expr ) => ( Regex::new(concat!("^(?:", $e, ")")).unwrap()) }

    // Matcher for a register, such as "r8".
    struct RegisterRule { re: Regex }
    impl RegisterRule {
        pub fn new() -> RegisterRule {
            RegisterRule { re: matcher!(r"r(\d+|l)") }
        }
    }
    impl RuleMatcher<ast::Reg> for RegisterRule {
        fn find(&self, s: &str) -> Option<(usize, ast::Reg)> {
            match self.re.captures(s) {
                Some(groups) => {
                    Some((groups[0].len(),
                          if &groups[1] == "l" {
                              ast::Reg { index: 31 }
                          } else {
                              let n = u8::from_str_radix(&groups[1], 10).unwrap();
                              ast::Reg { index: n }
                          }))
                },
                _ => {
                    let matcher = matcher!(r"lr");
                    match matcher.captures(s) {
                        Some(groups) => Some((groups[0].len(),
                                              ast::Reg { index: 31 })),
                        _ => None
                    }
                }
            }
        }
    }

    struct CharLiteralRule { re: Regex }
    impl CharLiteralRule {
        pub fn new() -> CharLiteralRule {
            CharLiteralRule { re: matcher!(r"'(.)'") }
        }
    }
    impl RuleMatcher<u32> for CharLiteralRule {
        fn find(&self, s: &str) -> Option<(usize, u32)> {
            match self.re.captures(s) {
                Some(groups) => {
                    let c = groups[1].as_bytes()[0] as u32;

                    Some((groups[0].len(), c))
                },
                _ => None
            }
        }
    }

    struct NumberLiteralRule { re: Regex }
    impl NumberLiteralRule {
        pub fn new() -> NumberLiteralRule {
            NumberLiteralRule { re: matcher!(r"(-?)((?:0[xX]([[:xdigit:]]+))|(?:0[bB]([01]+))|(?:\d+))(?:([uUiI])(32|16|8)?)?") }
        }
    }
    impl RuleMatcher<u32> for NumberLiteralRule {
        fn find(&self, s: &str) -> Option<(usize, u32)> {
            match self.re.captures(s) {
                Some(groups) => {
                    let (num_str, radix) = match (groups.get(3), groups.get(4)) {
                        (None, None) => (&groups[2], 10),
                        (None, Some(bin)) => (bin.as_str(), 2),
                        (Some(hex), _) => (hex.as_str(), 16),
                    };

                    let negated = &groups[1] == "-";

                    let mut num = u32::from_str_radix(num_str, radix).unwrap();

                    if negated { num = -(num as i32) as u32; }

                    Some((groups[0].len(), num))
                },
                _ => None
            }
        }
    }

    struct PredicateRule { re: Regex }
    impl PredicateRule {
        pub fn new() -> PredicateRule {
            PredicateRule { re: matcher!(r"(!?)p([0123])") }
        }
    }
    impl RuleMatcher<Pred> for PredicateRule {
        fn find(&self, s: &str) -> Option<(usize, Pred)> {
            match self.re.captures(s) {
                Some(groups) => {
                    Some((groups[0].len(),
                          Pred {
                              inverted: &groups[1] == "!",
                              reg: PredReg::from_u8(groups[2].as_bytes()[0] - b'0')
                          }))
                },
                _ => None
            }
        }
    }

    struct ShiftRule { re: Regex }
    impl ShiftRule {
        pub fn new() -> ShiftRule {
            ShiftRule { re: matcher!(r"(<<|>>u|>>s|>>r)") }
        }
    }
    impl RuleMatcher<ShiftType> for ShiftRule {
        fn find(&self, s: &str) -> Option<(usize, ShiftType)> {
            match self.re.captures(s) {
                Some(groups) => {
                    let shift_type = match &groups[1] {
                        "<<" => SllShift,
                        ">>u" => SrlShift,
                        ">>s" => SraShift,
                        ">>r" => RorShift,
                        _ => panic!(),
                    };

                    Some((groups[0].len(), shift_type))
                },
                _ => None
            }
        }
    }

    struct LoadStoreRule { re: Regex }
    impl LoadStoreRule {
        pub fn new() -> LoadStoreRule {
            LoadStoreRule { re: matcher!(r"\*(sc|llsc|ll|w|l|h|b)") }
        }
    }
    impl RuleMatcher<LsuWidth> for LoadStoreRule {
        fn find(&self, s: &str) -> Option<(usize, LsuWidth)> {
            match self.re.captures(s) {
                Some(groups) =>
                    Some((groups[0].len(),
                          match &groups[1] {
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

    struct CoRegRule { re: Regex }
    impl CoRegRule {
        pub fn new() -> CoRegRule {
            CoRegRule { re: matcher!(r"(?i:(PFLAGS|PTB|EHA|EPC|EC0|EC1|EC2|EC3|EA0|EA1|SP0|SP1|SP2|SP3|RESV..))") }
        }
    }
    impl RuleMatcher<ast::CoReg> for CoRegRule {
        fn find(&self, s: &str) -> Option<(usize, ast::CoReg)> {
            match self.re.captures(s) {
                Some(groups) =>
                    Some((groups[0].len(),
                          match &groups[1].to_ascii_uppercase()[..] {
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
                              "RESV10" => RESV10,
                              "RESV11" => RESV11,
                              "RESV12" => RESV12,
                              "RESV13" => RESV13,
                              "RESV14" => RESV14,
                              "RESV15" => RESV15,
                              "RESV20" => RESV20,
                              "RESV21" => RESV21,
                              "RESV22" => RESV22,
                              "RESV23" => RESV23,
                              "RESV24" => RESV24,
                              "RESV25" => RESV25,
                              "RESV26" => RESV26,
                              "RESV27" => RESV27,
                              "RESV28" => RESV28,
                              "RESV29" => RESV29,
                              "RESV30" => RESV30,
                              "RESV31" => RESV31,
                              _ => panic!(),
                          })),
                _ => None,
            }
        }
    }

    struct FlushRule { re: Regex }
    impl FlushRule {
        pub fn new() -> FlushRule {
            FlushRule { re: matcher!(r"(?i:flush\.(data|inst|dtlb|itlb))") }
        }
    }
    impl RuleMatcher<FlushType> for FlushRule {
        fn find(&self, s: &str) -> Option<(usize, FlushType)> {
            match self.re.captures(s) {
                Some(groups) =>
                    Some((groups[0].len(),
                          match &groups[1].to_ascii_uppercase()[..] {
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
        Token::LSquare    => "[",
        Token::RSquare    => "]",
        Token::Semi       => ";",
        Token::Comma      => ",",
        Token::DotDot     => "..",
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

        Token::NumLit     => CharLiteralRule::new(),
        Token::NumLit     => NumberLiteralRule::new(),

        Token::Reg        => RegisterRule::new(),
        Token::CoReg      => CoRegRule::new(),
        Token::PredReg    => PredicateRule::new(),
        Token::Shift      => ShiftRule::new(),
        Token::LoadStore  => LoadStoreRule::new(),
        Token::Flush      => FlushRule::new(),
        Token::IdentTok   => matcher!(r"[a-zA-Z_]\w*"),
        Token::Define     => ".define",
        Token::Struct     => ".struct",

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
