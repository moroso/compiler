use util::lexer::*;
use assembler::*;

#[deriving(Eq, PartialEq, Clone, Show)]
pub enum AsmToken {
    AsmWS,

    AsmLong,
    AsmDotGlobl,

    AsmReg(Reg),
    AsmPredReg(Pred),
    AsmNumLit(u32),
    AsmIdentTok(String),
    AsmShift(ShiftType),

    AsmColon,
    AsmLBrace,
    AsmRBrace,
    AsmSemi,
    AsmPlus,
    AsmMinus,
    AsmLt,
    AsmLe,
    AsmGt,
    AsmGe,
    AsmStar,
    AsmLParen,
    AsmRParen,
    AsmEqEq,

    AsmGets,
    AsmPredicates,

    // Special
    AsmEof,
    // The following two serve as flags for the lexer (to indicate when we
    // enter and exit a multi-line comment). They should never be part of
    // the token stream it generates.
    AsmBeginComment,
    AsmEndComment,
}

pub fn new_asm_lexer<T: Buffer, S: StrAllocating>(
    name: S,
    buffer: T) -> Lexer<T, AsmToken> {

    macro_rules! lexer_rules {
        ( $( $c:expr => $m:expr ),*) => (
            vec!( $( box LexerRule { matcher: $m, maker: $c } as Box<LexerRuleT<AsmToken>> ),* )
                )
    }
    macro_rules! matcher { ( $e:expr ) => ( regex!(concat!("^(?:", $e, ")"))) }

    // Matcher for a register, such as "r8".
    struct RegisterRule;
    impl RuleMatcher<Reg> for RegisterRule {
        fn find(&self, s: &str) -> Option<(uint, Reg)> {
            use std::num::from_str_radix;

            let matcher = matcher!(r"r(\d+|l)");
            match matcher.captures(s) {
                Some(groups) => {
                    Some((groups.at(0).len(),
                          if groups.at(1) == "l" {
                              Reg { index: 31 }
                          } else {
                              let n = from_str_radix(groups.at(1), 10).unwrap();
                              Reg { index: n }
                          }))
                },
                _ => None
            }

        }
    }

    struct CharLiteralRule;
    impl RuleMatcher<u32> for CharLiteralRule {
        fn find(&self, s: &str) -> Option<(uint, u32)> {
            let matcher = matcher!(r"'(.)'");

            match matcher.captures(s) {
                Some(groups) => {
                    let c = groups.at(1)[0] as u32;

                    Some((groups.at(0).len(), c))
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
                    // TODO: match on (groups.at(3), groups.at(4)) when rust
                    // fixes issue #14927.
                    let (num_str, radix) = match groups.at(3) {
                        "" => match groups.at(4) {
                            "" => (groups.at(2), 10),
                            bin => (bin, 2),
                        },
                        hex => (hex, 16),
                    };

                    let negated = groups.at(1) == "-";

                    let mut num: u32 = from_str_radix(num_str, radix).unwrap();

                    if negated { num = -(num as i32) as u32; }

                    Some((groups.at(0).len(), num))
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
                    Some((groups.at(0).len(),
                          Pred {
                              inverted: groups.at(1) == "!",
                              reg: groups.at(2)[0] - '0' as u8
                          }))
                },
                _ => None
            }
        }
    }

    struct ShiftRule;
    impl RuleMatcher<ShiftType> for ShiftRule {
        fn find(&self, s: &str) -> Option<(uint, ShiftType)> {
            let matcher = matcher!(r"(<<|>>)(u|s|r)?");
            match matcher.captures(s) {
                Some(groups) => {
                    let shift_type = match groups.at(1) {
                        "<<" => {
                            if groups.at(2) != "r" { SllShift }
                            else { fail!("Can't rotate left.") }
                        },
                        ">>" => match groups.at(2) {
                            "u" => SrlShift,
                            "s" => SraShift,
                            "r" => RorShift,
                            _ => fail!(),
                        },
                        _ => fail!(),
                    };

                    Some((groups.at(0).len(), shift_type))
                },
                _ => None
            }
        }
    }

    let rules = lexer_rules! {
        // Whitespace, including C-style comments
        AsmWS         => matcher!(r"//.*|\s"),

        AsmColon      => ":",
        AsmLBrace     => "{",
        AsmRBrace     => "}",
        AsmSemi       => ";",
        AsmGets       => "<-",
        AsmPredicates => "->",
        AsmPlus       => "+",
        AsmMinus      => "-",
        AsmLt         => "<",
        AsmGt         => ">",
        AsmLe         => "<=",
        AsmGe         => ">=",
        AsmStar       => "*",
        AsmLParen     => "(",
        AsmRParen     => ")",
        AsmEqEq       => "==",
        // TODO: the other ops we need.

        // TODO: shifts. Currently, these are lexed incorrectly!

        AsmDotGlobl   => ".globl",
        AsmLong       => "long",

        AsmNumLit     => CharLiteralRule,
        AsmNumLit     => NumberLiteralRule,

        AsmReg        => RegisterRule,
        AsmPredReg    => PredicateRule,
        AsmIdentTok   => matcher!(r"[a-zA-Z_]\w*"),
        // TODO: a specific matcher for this.
        AsmIdentTok   => matcher!(r"\.[0-9]+(a|b)?")
    };

    // A special set of rules, just for when we're within a multi-line
    // comment.
    let comment_rules = lexer_rules! {
        // The start of a multi-line comment.
        // This is to properly handle nested multiline comments.
        // We increase the multiline-comment-nesting-counter with this.
        AsmBeginComment => matcher!(r"/\*"),

        // The end of a multi-line comment.
        // We decrease the multiline-comment-nesting-counter with this.
        AsmEndComment => matcher!(r"\*/"),

        // If we're within a comment, any string not
        // containing "/*" or "*/" is considered whitespace.
        AsmWS => matcher!(r"(?:[^*/]|/[^*]|\*[^/])*")
    };


    Lexer::new(
        Language {
            rules: rules,
            comment_rules: comment_rules,
            eof:AsmEof,
            ws: AsmWS,
            begin_comment: AsmBeginComment,
            end_comment: AsmEndComment
        }, name, buffer)

}

// TODO: genericize these, so they're not duplicated between here and
// compiler_lexer.
impl TokenMaker<(), AsmToken> for AsmToken {
    fn mk_tok(&self, _: ()) -> AsmToken { self.clone() }
}

impl<T> TokenMaker<T, AsmToken> for fn(T) -> AsmToken {
    fn mk_tok(&self, arg: T) -> AsmToken { (*self)(arg) }
}
