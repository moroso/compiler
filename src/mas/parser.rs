//! Parse a Moroso asm file.

use mas::lexer::*;
use mas::ast::*;
use util::lexer::{Lexer, SourceToken};
use std::iter::Peekable;
use std::num::from_int;
use span::{SourcePos, Span, mk_sp};

pub struct AsmParser<T> {
    tokens: Peekable<SourceToken<Token>, Lexer<T, Token>>,
    last_span: Span,
}

/// Several instructions are encoded with a value and an even shift amount.
/// Given a number `n` and amount `width` in which it must fit, this function
/// will either return the value (fitting into `width`) bits together with
/// the corresponding shift amount divided by 2, or None if it cannot be done.
pub fn pack_int(n: u32, width: u8) -> Option<(u32, u8)> {
    // We do this by trying all rotations and seeing if any of them works.
    // Yeah, we could be more clever about it, but there's really no need.
    fn rol(n: u32, amt: u8) -> u32 {
        (n << amt) | (n >> (32-amt))
    }

    for i in range(0, 16) {
        let i = i as u8;
        let new_n = rol(n, 2*i);
        let mut max_bit: u8 = 0;
        for j in range(0, 32) {
            let j = j as u8;
            if new_n & (1<<j) != 0 { max_bit = j }
        }
        if max_bit < width {
            return Some((new_n, i));
        }
    }

    None
}

fn tok_to_op(tok: &Token) -> Option<AluOp> {
    match *tok {
        Plus => Some(AddAluOp),
        Amp | And => Some(AndAluOp),
        TildePipe | Nor => Some(NorAluOp),
        Pipe | Or => Some(OrAluOp),
        Dash | Sub => Some(SubAluOp),
        DashColon | Rsb => Some(RsbAluOp),
        Caret | Xor => Some(XorAluOp),
        _ => None,
    }
}

impl<T: Buffer> AsmParser<T> {

    pub fn new(tokens: Peekable<SourceToken<Token>, Lexer<T, Token>>
           ) -> AsmParser<T> {
        AsmParser {
            tokens: tokens,
            last_span: mk_sp(SourcePos::new(), 0)
        }
    }

    // Note: the next four functions were taking from the Mb parser.
    // We may want to see if we can split them out into a parsing util
    // package or something.

    /// "Peek" at the next token, returning the token, without consuming
    /// it from the stream.
    fn peek<'a>(&'a mut self) -> &'a Token {
        match self.tokens.peek() {
            Some(st) => &st.tok,
            None => fail!("Tried to peek past EOF"),
        }
    }

    /// Consume the next token from the stream, returning it.
    fn eat(&mut self) -> Token {
        match self.tokens.next() {
            Some(st) => {
                self.last_span = st.sp;
                st.tok
            }
            None => fail!("Tried to read past EOF"),
        }
    }

    /// Consume one token from the stream, erroring if it's not
    /// the `expected`. Returns the string corresponding to that
    /// token.
    fn expect(&mut self, expected: Token) {
        let tok = self.eat();
        if tok != expected{
            self.error(format!("Expected {}, found {}",
                               expected, tok));
        }
    }

    fn error<'a, T: Str>(&self, message: T) -> ! {
        fail!("\n{}\nat {}", message.as_slice(), self.last_span.get_begin())
    }

    /// Assert that `num` fits into `size` bits.
    fn assert_num_size(&self, num: u32, size: u8) {
        let mask: u32 = (-1 as u32) << size;
        if (num & mask) != 0 {
            self.error(format!("Number {} (0b{:t}) has more than {} bits.",
                               num, num, size))
        }
    }

    // asm-specific parsing functions start here!

    /// Parse either a register by itself, or a register with a shift
    /// (e.g. (r7 << 3) ).
    fn parse_reg_maybe_shift(&mut self) -> (Reg, (ShiftType, u8)) {
        let tok = self.eat();
        match tok {
            // Bare register.
            Reg(reg) => (reg, (from_int(0).unwrap(), 0)),

            // Something parenthesized.
            LParen => {
                // No matter what, there should be a register.
                let reg = match *self.peek() {
                    Reg(reg) => { self.eat(); reg },
                    _ => self.error(format!("Expected register; got {}", tok)),
                };
                let inner_tok = self.eat();
                match inner_tok {
                    // There's just the one register, that for some reason
                    // is in parentheses.
                    RParen => (reg, (from_int(0).unwrap(), 0)),

                    // There's a shift.
                    Shift(shifttype) => {
                        match self.eat() {
                            NumLit(num) => {
                                self.assert_num_size(
                                    num, 5);
                                self.expect(RParen);
                                (reg, (shifttype, num as u8))
                            },
                            _ => self.error("Need a shift amount"),
                        }
                    },
                    _ => self.error(format!("Unexpected token {}", inner_tok)),
                }
            },
            _ => self.error(format!("Unexpected token {}", tok)),
        }
    }

    /// This function assumes we've parsed the destination register
    /// and the "gets" arrow, and possibly a (unary) op before it.
    /// All of these things are passed in as parameters; it does the
    /// parsing of the rest of the instruction.
    /// This function is not responsible for functions that have only
    /// one register operand and do shifts to it, or that have a
    /// literal as the first operand.
    fn parse_reg_and_maybe_binop(&mut self, pred: Pred, rd: Reg,
                                 op: Option<AluOp>) -> InstNode {
        let reg = match self.eat() {
            Reg(reg) => reg,
            _ => self.error("Expected reg."),
        };

        match op {
            // If an op had been given, there can't be a binop later.
            Some(o) => {
                ALU1RegInst(
                    pred,
                    o,
                    rd,
                    reg,
                    from_int(0).unwrap(),
                    0)
            },
            // No op was given before this, so the instruction so far looks
            // like "p1 -> r3 <- r6". Either we're done, or there's a binary
            // operator after this.
            None => {
                match tok_to_op(self.peek()) {
                    Some(op) => {
                        // It's a binary operator. It can be followed by
                        // a register (which may be shifted), or a literal,
                        // or '.long'.
                        self.eat();
                        match *self.peek() {
                            LParen | Reg(..) => {
                                let (rs,
                                     (shifttype, shiftamt)
                                     ) = self.parse_reg_maybe_shift();
                                ALU2RegInst(
                                    pred,
                                    op,
                                    rd,
                                    reg,
                                    rs,
                                    shifttype,
                                    shiftamt)
                            },
                            NumLit(num) => {
                                let (val, rot) = self.pack_int_unwrap(num, 10);
                                ALU2ShortInst(
                                    pred,
                                    op,
                                    rd,
                                    reg,
                                    val,
                                    rot)
                            },
                            _ => self.error("Unexpected token."),
                        }
                    },
                    None => {
                        // There was no binary operator. We're done.
                        ALU1RegInst(
                            pred,
                            MovAluOp,
                            rd,
                            reg,
                            from_int(0).unwrap(),
                            0)
                    }
                }
            }
        }
        
    }

    /// Convenience function. Will try to take `n` and pack it into
    /// `size` bits and an even-numbered shift, generating an error if
    /// that fails.
    fn pack_int_unwrap(&self, n: u32, size: u8) -> (u32, u8) {
        match pack_int(n, size) {
            Some(x) => x,
            None => self.error(
                format!("Cannot pack {} into {} bits+shift.",
                        n, size))
        }
    }

    /// Parse everything to the right of the '<- op' in an instruction.
    /// The predicate and destination registers were already parsed and
    /// are passed in as parameters. If there was a unary op, that's
    /// also passed in.
    fn parse_expr(&mut self, pred: Pred, rd: Reg,
                 op: Option<AluOp>) -> InstNode {
        match *self.peek() {
            NumLit(n) => {
                // We're just storing a number.
                self.eat();
                let (val, rot) = self.pack_int_unwrap(n, 15);
                ALU1ShortInst(
                    pred,
                    op.unwrap_or(MovAluOp),
                    rd,
                    val,
                    rot)
            },
            Reg(..) => {
                // There's a register. Either that's *all* there is
                // (we're applying a unary op to a register), or there's
                // a binary op. `parse_reg_and_maybe_binop` will take
                // care of figuring all that out.
                self.parse_reg_and_maybe_binop(pred, rd, op)
            },
            _ => {
                // The only option left is that we're applying a unary op
                // to a *shifted* register.
                let (reg, (shifttype, shiftamt)) = self.parse_reg_maybe_shift();
                ALU1RegInst(
                    pred,
                    op.unwrap_or(MovAluOp),
                    rd,
                    reg,
                    shifttype,
                    shiftamt
                    )
            }
        }
    }

    /// Parse the actual op/literal part of an instruction; that is,
    /// everything to the right of the '<-'.
    /// We're passed in the predicate and destination register, which
    /// have already been parsed by now.
    fn parse_op_or_expr(&mut self, pred: Pred, rd: Reg) -> InstNode {
        match *self.peek() {
            // TODO: helper function that takes unary op tokens to their ops.
            NumLit(..) |
            Reg(..) |
            LParen => self.parse_expr(pred, rd, None),
            Mov => { self.eat(); self.parse_expr(pred, rd, Some(MovAluOp)) },
            Tilde |
            Mvn => { self.eat(); self.parse_expr(pred, rd, Some(MvnAluOp)) },
            Sxb => { self.eat(); self.parse_expr(pred, rd, Some(SxbAluOp)) },
            Sxh => { self.eat(); self.parse_expr(pred, rd, Some(SxhAluOp)) },
            _ => unimplemented!(),
        }
    }

    /// Parse an entire instruction.
    pub fn parse_inst(&mut self) -> InstNode {
        // Begin by parsing the predicate register for this instruction.
        let pred = match *self.peek() {
            PredReg(pred) => {
                self.eat();
                self.expect(Predicates);

                pred.clone()
            },
             // If no predicate is given, it's always true:
            _ => true_pred,
        };

        match *self.peek() {
            Reg(reg) => {
                self.eat();
                self.expect(Gets);
                self.parse_op_or_expr(pred, reg.clone())
            },
            _ => unimplemented!()
        }
    }
}

// Convenience function for testing
pub fn inst_from_str(s: &str) -> InstNode {
    let mut parser = AsmParser::new(asm_lexer_from_str(s).peekable());
    parser.parse_inst()
}


#[cfg(test)]
mod tests {
    use super::*;
    use mas::ast::*;

    #[test]
    fn test_pack_int() {
        assert_eq!(pack_int(0b0, 8), Some((0, 0)));
        assert_eq!(pack_int(0b1, 8), Some((1, 0)));
        assert_eq!(pack_int(0b11, 1), None);
        assert_eq!(pack_int(0b11, 2), Some((0b11, 0)));
        assert_eq!(pack_int(0b1100, 2), Some((0b11, 15)));
        assert_eq!(pack_int(0b110, 2), None);
        assert_eq!(pack_int(0b11000000000000000000000000000000, 2),
                   Some((0b11, 1)));
        assert_eq!(pack_int(0b10000000000000000000000000000001, 2), None);
        assert_eq!(pack_int(0b10000000000000000000000000000001, 3),
                   Some((0b110, 1)));
    }

    #[test]
    fn test_parse_inst_alu1short() {
        assert_eq!(inst_from_str("p1 -> r4 <- 5"),
                   ALU1ShortInst(Pred { inverted: false,
                                        reg: 1 },
                                 MovAluOp,
                                 Reg { index: 4 },
                                 5,
                                 0));

        assert_eq!(inst_from_str("!p1 -> r4 <- 5"),
                   ALU1ShortInst(Pred { inverted: true,
                                        reg: 1 },
                                 MovAluOp,
                                 Reg { index: 4 },
                                 5,
                                 0));

        assert_eq!(inst_from_str("!p1 -> r4 <- ~5"),
                   ALU1ShortInst(Pred { inverted: true,
                                        reg: 1 },
                                 MvnAluOp,
                                 Reg { index: 4 },
                                 5,
                                 0));

        assert_eq!(inst_from_str("r4 <- ~5"),
                   ALU1ShortInst(Pred { inverted: false,
                                        reg: 3 },
                                 MvnAluOp,
                                 Reg { index: 4 },
                                 5,
                                 0));

        assert_eq!(inst_from_str("r4 <- SxB 5"),
                   ALU1ShortInst(Pred { inverted: false,
                                        reg: 3 },
                                 SxbAluOp,
                                 Reg { index: 4 },
                                 5,
                                 0));

        assert_eq!(inst_from_str(
            "r4 <- SxB 0b10000000000000000000000000000001"),
                   ALU1ShortInst(Pred { inverted: false,
                                        reg: 3 },
                                 SxbAluOp,
                                 Reg { index: 4 },
                                 0b110,
                                 1));

        // This is just long enough to be packed.
        assert_eq!(inst_from_str(
            "r4 <- SxB 0b111111111111111"),
                   ALU1ShortInst(Pred { inverted: false,
                                        reg: 3 },
                                 SxbAluOp,
                                 Reg { index: 4 },
                                 0b111111111111111,
                                 0));

        assert_eq!(inst_from_str(
            "r4 <- SxB -0xffffffff"),
                   ALU1ShortInst(Pred { inverted: false,
                                        reg: 3 },
                                 SxbAluOp,
                                 Reg { index: 4 },
                                 1,
                                 0));
    }

    #[test]
    #[should_fail]
    fn test_invalid_short_in_alu1inst()
    {
        // This value is too long to be packed into the instruction.
        inst_from_str("r4 <- 0b1111111111111111");
    }

    #[test]
    fn test_parse_inst_alu2short() {
        assert_eq!(inst_from_str("r6 <- r7 + 0"),
                   ALU2ShortInst(Pred { inverted: false,
                                        reg: 3 },
                                 AddAluOp,
                                 Reg { index: 6 },
                                 Reg { index: 7 },
                                 0,
                                 0));

        assert_eq!(inst_from_str("r6 <- r7 + 0b100"),
                   ALU2ShortInst(Pred { inverted: false,
                                        reg: 3 },
                                 AddAluOp,
                                 Reg { index: 6 },
                                 Reg { index: 7 },
                                 0b100,
                                 0));

        assert_eq!(inst_from_str("r6 <- r7 + 0b1111111111"),
                   ALU2ShortInst(Pred { inverted: false,
                                        reg: 3 },
                                 AddAluOp,
                                 Reg { index: 6 },
                                 Reg { index: 7 },
                                 0b1111111111,
                                 0));

        assert_eq!(inst_from_str("r6 <- r7 | 0b1111111111"),
                   ALU2ShortInst(Pred { inverted: false,
                                        reg: 3 },
                                 OrAluOp,
                                 Reg { index: 6 },
                                 Reg { index: 7 },
                                 0b1111111111,
                                 0));

        assert_eq!(inst_from_str("r6 <- r7 oR 0b1111111111"),
                   ALU2ShortInst(Pred { inverted: false,
                                        reg: 3 },
                                 OrAluOp,
                                 Reg { index: 6 },
                                 Reg { index: 7 },
                                 0b1111111111,
                                 0));

        assert_eq!(inst_from_str("!p2 -> r6 <- r7 oR 0b1111111111"),
                   ALU2ShortInst(Pred { inverted: true,
                                        reg: 2 },
                                 OrAluOp,
                                 Reg { index: 6 },
                                 Reg { index: 7 },
                                 0b1111111111,
                                 0));

        assert_eq!(inst_from_str(
            "!p2 -> r6 <- r7 oR 0b10000000000000000000000000000001"),
                   ALU2ShortInst(Pred { inverted: true,
                                        reg: 2 },
                                 OrAluOp,
                                 Reg { index: 6 },
                                 Reg { index: 7 },
                                 0b110,
                                 1));

        assert_eq!(inst_from_str(
            "r6 <- r7 + -0xffffffff"),
                   ALU2ShortInst(Pred { inverted: false,
                                        reg: 3 },
                                 AddAluOp,
                                 Reg { index: 6 },
                                 Reg { index: 7 },
                                 1,
                                 0));
    }

    #[test]
    #[should_fail]
    fn test_invalid_short_in_alu2short() {
        // Value is too long for the instruction.
        inst_from_str("r6 <- r7 + 0b11111111111");
    }

    #[test]
    fn test_parse_inst_alu1reg() {
        assert_eq!(inst_from_str("p1 -> r4 <- r5"),
                   ALU1RegInst(Pred { inverted: false,
                                      reg: 1 },
                               MovAluOp,
                               Reg { index: 4 },
                               Reg { index: 5 },
                               SllShift,
                               0));


        assert_eq!(inst_from_str("p1 -> r4 <- (r5)"),
                   ALU1RegInst(Pred { inverted: false,
                                      reg: 1 },
                               MovAluOp,
                               Reg { index: 4 },
                               Reg { index: 5 },
                               SllShift,
                               0));


        assert_eq!(inst_from_str("p1 -> r4 <- (r5 << 6)"),
                   ALU1RegInst(Pred { inverted: false,
                                      reg: 1 },
                               MovAluOp,
                               Reg { index: 4 },
                               Reg { index: 5 },
                               SllShift,
                               6));
    }

    #[test]
    #[should_fail]
    fn test_invalid_shift_in_alu1reg() {
        inst_from_str("r4 <- (r5 << 33)");
    }

    #[test]
    fn test_parse_inst_alu2reg() {
        assert_eq!(inst_from_str("r6 <- r7 + r8"),
                   ALU2RegInst(Pred { inverted: false,
                                      reg: 3 },
                               AddAluOp,
                               Reg { index: 6 },
                               Reg { index: 7 },
                               Reg { index: 8 },
                               SllShift,
                               0));

        assert_eq!(inst_from_str("r6 <- r7 + (r8)"),
                   ALU2RegInst(Pred { inverted: false,
                                      reg: 3 },
                               AddAluOp,
                               Reg { index: 6 },
                               Reg { index: 7 },
                               Reg { index: 8 },
                               SllShift,
                               0));

        assert_eq!(inst_from_str("r6 <- r7 + (r8 << 6)"),
                   ALU2RegInst(Pred { inverted: false,
                                      reg: 3 },
                               AddAluOp,
                               Reg { index: 6 },
                               Reg { index: 7 },
                               Reg { index: 8 },
                               SllShift,
                               6));

        assert_eq!(inst_from_str("r6 <- r7 -: r8"),
                   ALU2RegInst(Pred { inverted: false,
                                      reg: 3 },
                               RsbAluOp,
                               Reg { index: 6 },
                               Reg { index: 7 },
                               Reg { index: 8 },
                               SllShift,
                               0));

        assert_eq!(inst_from_str("r6 <- r7 rsb r8"),
                   ALU2RegInst(Pred { inverted: false,
                                      reg: 3 },
                               RsbAluOp,
                               Reg { index: 6 },
                               Reg { index: 7 },
                               Reg { index: 8 },
                               SllShift,
                               0));

        assert_eq!(inst_from_str("!p1 -> r6 <- r7 rsb r8"),
                   ALU2RegInst(Pred { inverted: true,
                                      reg: 1 },
                               RsbAluOp,
                               Reg { index: 6 },
                               Reg { index: 7 },
                               Reg { index: 8 },
                               SllShift,
                               0));
    }
}