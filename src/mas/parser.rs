//! Parse a Moroso asm file.

use mas::lexer::*;
use mas::ast::*;
use mas::util::{pack_int, fits_in_bits};
use util::lexer::{Lexer, SourceToken};
use std::iter::Peekable;
use std::num::from_int;
use span::{SourcePos, Span, mk_sp};

pub struct AsmParser<T> {
    tokens: Peekable<SourceToken<Token>, Lexer<T, Token>>,
    last_span: Span,
    error_on_misplaced_inst: bool,
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

fn tok_to_unop(tok: &Token) -> Option<AluOp> {
    match *tok {
        Mov => Some(MovAluOp),
        Tilde |
        Mvn => Some(MvnAluOp),
        Sxb => Some(SxbAluOp),
        Sxh => Some(SxhAluOp),
        _ => None,
    }
}

fn tok_to_cmp(tok: &Token) -> Option<CompareType> {
    match *tok {
        Lt => Some(CmpLTU),
        Le => Some(CmpLEU),
        Lts => Some(CmpLTS),
        Les => Some(CmpLES),
        EqEq => Some(CmpEQ),
        Eq => Some(CmpEQ),
        Bs => Some(CmpBS),
        Amp => Some(CmpBS),
        Bc => Some(CmpBC),
        _ => None,
    }
}

enum InstType {
    ALUInstType,
    ControlType,
    MemoryType,
    LongType,
}

fn classify_inst(inst: &InstNode) -> InstType {
    match *inst {
        ALU1ShortInst(..) |
        ALU2ShortInst(..) |
        ALU1RegInst(..) |
        ALU2RegInst(..) |
        ALU2LongInst(..) |
        ALU1LongInst(..) |
        ALU1RegShInst(..) |
        CompareShortInst(..) |
        CompareRegInst(..) |
        NopInst => ALUInstType,
        LongInst(..) => LongType,
        LoadInst(..) |
        StoreInst(..) => MemoryType,
    }
}

impl<T: Buffer> AsmParser<T> {

    pub fn new(tokens: Peekable<SourceToken<Token>, Lexer<T, Token>>
           ) -> AsmParser<T> {
        AsmParser {
            tokens: tokens,
            last_span: mk_sp(SourcePos::new(), 0),
            error_on_misplaced_inst: true,
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
        if !fits_in_bits(num, size) {
            self.error(format!("Number {} (0b{:t}) has more than {} bits.",
                               num, num, size))
        }
    }

    fn assert_signed_num_size(&self, num: i32, size: u8) {
        self.assert_num_size(if num < 0 { -num } else { num } as u32,
                             size);
        // But we also have to make sure the sign bit is okay.
        // This checks that the sign bit is cleared if n is
        // nonnegative, and set if it's negative.
        if (num >= 0) != (num & (1<<(size-1)) == 0) {
            self.error(format!(
                "Signed number {} (0b{:t}) needs more than {} bits.",
                num, num, size));
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
                InstNode::alu1reg(
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
                        // a register (which may be shifted), a literal,
                        // or 'long' keyword.
                        self.eat();
                        match *self.peek() {
                            LParen | Reg(..) => {
                                let (rs,
                                     (shifttype, shiftamt)
                                     ) = self.parse_reg_maybe_shift();
                                InstNode::alu2reg(
                                    pred,
                                    op,
                                    rd,
                                    reg,
                                    rs,
                                    shifttype,
                                    shiftamt)
                            },
                            NumLit(num) => {
                                self.eat();
                                let (val, rot) = self.pack_int_unwrap(num, 10);
                                InstNode::alu2short(
                                    pred,
                                    op,
                                    rd,
                                    reg,
                                    val,
                                    rot)
                            },
                            Long => {
                                self.eat();
                                InstNode::alu2long(
                                    pred,
                                    op,
                                    rd,
                                    reg)
                            },
                            _ => self.error("Unexpected token."),
                        }
                    },
                    None => {
                        // There was no binary ALU operator. Either there's a
                        // shift operator instead, or we're done.
                        match *self.peek() {
                            Shift(shift) => {
                                // It's a shift. There's should be a reg
                                // after it, and that's it.
                                self.eat();
                                match self.eat() {
                                    Reg(reg2) =>
                                        InstNode::alu1regsh(
                                            pred,
                                            rd,
                                            reg,
                                            shift,
                                            reg2),
                                    _ => self.error("Unexpected token."),
                                }
                            },
                            _ => {
                                // Just the register. So it's just a move.
                                InstNode::alu1reg(
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
                InstNode::alu1short(
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
            Long => {
                self.eat();
                InstNode::alu1long(
                    pred,
                    op.unwrap_or(MovAluOp),
                    rd)
            },
            _ => {
                // The only option left is that we're applying a unary op
                // to a *shifted* register.
                let (reg, (shifttype, shiftamt)) = self.parse_reg_maybe_shift();
                InstNode::alu1reg(
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

    /// Parses anything that comes after the width specifier in a load or
    /// store.
    /// So, in *l(r8 + 12), this parses the (r8 + 12) part.
    fn parse_deref_common(&mut self) -> (Reg, i32) {
        self.expect(LParen);
        let reg = match self.eat() {
            Reg(reg) => reg,
            _ => self.error("Expected register."),
        };

        let next_tok = self.eat();
        let offs: i32 = match next_tok {
            Plus |
            Dash => {
                match self.eat() {
                    NumLit(n) => {
                        let mut n = n as i32;
                        if next_tok == Dash { n = -n; }
                        // We must fit into 12 bits...
                        self.assert_signed_num_size(n, 12);
                        self.expect(RParen);

                        n
                    },
                    _ => self.error("Need a number."),
                }
            },
            RParen => 0,
            _ => self.error("Need +, -, or a closing paren."),
        };

        (reg, offs)
    }

    // Parse a load. We've already parsed the predicate and destination
    // register; this will take something like
    // *l(r8 + 6)
    // and return the appropriate instruction.
    fn parse_load(&mut self, pred: Pred, rd: Reg) -> InstNode {
        let width = match self.eat() {
            LoadStore(width) => width,
            _ => fail!("ICE"),
        };

        let (reg, offs) = self.parse_deref_common();

        InstNode::load(pred,
                       LsuOp {
                           store: false, // it's a load.
                           width: width,
                       },
                       rd,
                       reg,
                       offs)
    }

    /// Parse the actual op/literal part of an instruction; that is,
    /// everything to the right of the '<-'.
    /// We're passed in the predicate and destination register, which
    /// have already been parsed by now.
    fn parse_op_or_expr(&mut self, pred: Pred, rd: Reg) -> InstNode {
        match tok_to_unop(self.peek()) {
            Some(tok) => {
                // There's a unary operator!
                self.eat();
                self.parse_expr(pred, rd, Some(tok))
            },
            None => match *self.peek() {
                // No unary operator. That means there must be a literal,
                // a register, or an opening paren (for a shift).
                NumLit(..) |
                Reg(..) |
                Long |
                LParen => self.parse_expr(pred, rd, None),
                LoadStore(..) => self.parse_load(pred, rd),
                _ => self.error("Unexpected token."),
            }
        }
    }

    // Assumes the "long" keyword has already been consumed.
    pub fn parse_long(&mut self) -> InstNode {
        match self.eat() {
            NumLit(n) => InstNode::long(n),
            _ => self.error("Must have a numeric literal for long."),
        }
    }

    pub fn parse_store(&mut self, pred: Pred, width: LsuWidth) -> InstNode {
        let (reg, offs) = self.parse_deref_common();

        self.expect(Gets);

        let rhsreg = match self.eat() {
            Reg(rhsreg) => rhsreg,
            _ => self.error("Expected a register."),
        };

        InstNode::store(pred,
                        LsuOp {
                            store: false, // it's a load.
                            width: width,
                        },
                        reg,
                        offs,
                        rhsreg)
    }

    /// Parse a conditional. We've already parsed the predicate register,
    /// and the "<-".
    pub fn parse_conditional(&mut self, pred: Pred,
                             dest_pred: Pred) -> InstNode {
        if dest_pred.inverted {
            self.error("Cannot assign to an inverted predicate");
        }

        let reg = match self.eat() {
            Reg(reg) => reg,
            _ => self.error("Expected register."),
        };

        let op = match tok_to_cmp(&self.eat()) {
            Some(op) => op,
            None => self.error("Expected a comparison op."),
        };

        match *self.peek() {
            NumLit(n) => {
                self.eat();
                let (val, rot) = self.pack_int_unwrap(n, 10);
                InstNode::compareshort(
                    pred,
                    dest_pred,
                    reg,
                    op,
                    val,
                    rot)
            },
            _ => {
                let (reg_rt,
                     (shifttype, shiftamt)) = self.parse_reg_maybe_shift();
                InstNode::comparereg(
                    pred,
                    dest_pred,
                    reg,
                    op,
                    reg_rt,
                    shifttype,
                    shiftamt)
            }
        }
    }

    /// Parse an entire instruction.
    pub fn parse_inst(&mut self) -> InstNode {
        // Begin by parsing the predicate register for this instruction.
        let mut cur_tok = self.eat();
        let pred = match cur_tok {
            PredReg(pred) => {
                // There's a predicate register, but we don't know what
                // context it appears in: we may be assigning to it, or
                // predicating based on it.
                match *self.peek() {
                    Predicates => {
                        // It's actually predicating.
                        self.eat();
                        cur_tok = self.eat();

                        Some(pred)
                    },
                    Gets => // It's being assigned to. Punt on this.
                        None,
                    _ => self.error("Unexpected token."),
                }
            },
            _ => None,
        };

        match cur_tok {
            Reg(reg) => {
                self.expect(Gets);
                self.parse_op_or_expr(pred.unwrap_or(true_pred), reg.clone())
            },
            PredReg(destpred) => {
                self.expect(Gets);
                self.parse_conditional(pred.unwrap_or(true_pred), destpred)
            },
            LoadStore(width) =>
                self.parse_store(pred.unwrap_or(true_pred), width),
            Nop => {
                if pred != None {
                    self.error("Cannot have a predicate for a nop.");
                }

                NopInst
            },
            Long => {
                if pred != None {
                    self.error("Cannot have a predicate for a long");
                }

                self.parse_long()
            },
            _ => unimplemented!()
        }
    }

    pub fn parse_inst_packet(&mut self) -> InstPacket {
        let mut insts: InstPacket = [NopInst, NopInst, NopInst, NopInst];

        self.expect(LBrace);
        for i in range(0u, 4u) {
            match *self.peek() {
                RBrace => {
                    self.eat();
                    return insts;
                }
                _ => {
                    insts[i] = self.parse_inst();
                    if self.error_on_misplaced_inst {
                        match classify_inst(&insts[i]) {
                            ControlType => {
                                if i != 0 {
                                    self.error("Control instructions are only allowed in the first slot.");
                                }
                            },
                            MemoryType => {
                                if i > 1 {
                                    self.error("Memory instructions are only allowed in the first two slots.");
                                }
                            },
                            _ => {}
                        }
                    }
                    if *self.peek() == Semi { self.eat(); }
                }
            }
        }
        self.expect(RBrace);

        insts
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
    use mas::util::pack_int;

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

        assert_eq!(inst_from_str("r4 <- lr"),
                   ALU1RegInst(Pred { inverted: false,
                                      reg: 3 },
                               MovAluOp,
                               Reg { index: 4 },
                               Reg { index: 31 },
                               SllShift,
                               0));
    }

    #[test]
    #[should_fail]
    fn test_invalid_shift_in_alu1reg() {
        // We're shifting by too much.
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

    #[test]
    fn test_parse_inst_alu2long() {
        assert_eq!(inst_from_str("r6 <- r7 + long"),
                   ALU2LongInst(Pred { inverted: false,
                                      reg: 3 },
                                AddAluOp,
                                Reg { index: 6 },
                                Reg { index: 7 }));

        assert_eq!(inst_from_str("!p2 -> r6 <- r7 + long"),
                   ALU2LongInst(Pred { inverted: true,
                                       reg: 2 },
                                AddAluOp,
                                Reg { index: 6 },
                                Reg { index: 7 }));
    }

    #[test]
    fn test_parse_inst_alu1long() {
        assert_eq!(inst_from_str("r6 <- long"),
                   ALU1LongInst(Pred { inverted: false,
                                       reg: 3 },
                                MovAluOp,
                                Reg { index : 6 }));

        assert_eq!(inst_from_str("r6 <- ~long"),
                   ALU1LongInst(Pred { inverted: false,
                                       reg: 3 },
                                MvnAluOp,
                                Reg { index : 6 }));

        assert_eq!(inst_from_str("!p2 -> r6 <- long"),
                   ALU1LongInst(Pred { inverted: true,
                                       reg: 2 },
                                MovAluOp,
                                Reg { index : 6 }));
    }

    #[test]
    fn test_parse_inst_alu1regsh() {
        assert_eq!(inst_from_str("r6 <- r7 << r8"),
                   ALU1RegShInst(Pred { inverted: false,
                                        reg: 3 },
                                 Reg { index: 6 },
                                 Reg { index: 7 },
                                 SllShift,
                                 Reg { index: 8 }));

        assert_eq!(inst_from_str("r6 <- r7 >>u r8"),
                   ALU1RegShInst(Pred { inverted: false,
                                        reg: 3 },
                                 Reg { index: 6 },
                                 Reg { index: 7 },
                                 SrlShift,
                                 Reg { index: 8 }));

        assert_eq!(inst_from_str("!p2 -> r6 <- r7 >>s r8"),
                   ALU1RegShInst(Pred { inverted: true,
                                        reg: 2 },
                                 Reg { index: 6 },
                                 Reg { index: 7 },
                                 SraShift,
                                 Reg { index: 8 }));
    }

    #[test]
    fn test_parse_inst_nop() {
        assert_eq!(inst_from_str("nop"), NopInst);
    }

    #[test]
    #[should_fail]
    fn test_parse_nop_with_pred() {
        // Nop instructions can't have predicates.
        inst_from_str("p0 -> nop");
    }

    #[test]
    fn test_parse_inst_long() {
        assert_eq!(inst_from_str("long 0x56"), LongInst(0x56));
    }

    #[test]
    #[should_fail]
    fn test_parse_long_with_pred() {
        // Long directives can't have predicates.
        inst_from_str("p0 -> long 0x56");
    }

    #[test]
    fn test_parse_inst_load() {
        assert_eq!(inst_from_str("r9 <- *l(r8)"),
                   LoadInst(Pred { inverted: false,
                                   reg: 3 },
                            LsuOp { store: false,
                                    width: LsuWidthL },
                            Reg { index: 9 },
                            Reg { index: 8 },
                            0));

        assert_eq!(inst_from_str("r9 <- *l(r8 + 6)"),
                   LoadInst(Pred { inverted: false,
                                   reg: 3 },
                            LsuOp { store: false,
                                    width: LsuWidthL },
                            Reg { index: 9 },
                            Reg { index: 8 },
                            6));

        assert_eq!(inst_from_str("r9 <- *l(r8 - 6)"),
                   LoadInst(Pred { inverted: false,
                                   reg: 3 },
                            LsuOp { store: false,
                                    width: LsuWidthL },
                            Reg { index: 9 },
                            Reg { index: 8 },
                            -6));

        assert_eq!(inst_from_str("r9 <- *l(r8 + -0x6)"),
                   LoadInst(Pred { inverted: false,
                                   reg: 3 },
                            LsuOp { store: false,
                                    width: LsuWidthL },
                            Reg { index: 9 },
                            Reg { index: 8 },
                            -6));

        assert_eq!(inst_from_str("r9 <- *llsc(r8 + -0x6)"),
                   LoadInst(Pred { inverted: false,
                                   reg: 3 },
                            LsuOp { store: false,
                                    width: LsuLLSC },
                            Reg { index: 9 },
                            Reg { index: 8 },
                            -6));

        // The largest positive value that can be used as an offset.
        assert_eq!(inst_from_str("r9 <- *l(r8 + 0b11111111111)"),
                   LoadInst(Pred { inverted: false,
                                   reg: 3 },
                            LsuOp { store: false,
                                    width: LsuWidthL },
                            Reg { index: 9 },
                            Reg { index: 8 },
                            0b11111111111));

        // The most negative value that can be used as an offset.
        assert_eq!(inst_from_str("r9 <- *l(r8 - 0b100000000000)"),
                   LoadInst(Pred { inverted: false,
                                   reg: 3 },
                            LsuOp { store: false,
                                    width: LsuWidthL },
                            Reg { index: 9 },
                            Reg { index: 8 },
                            -0b100000000000));
    }

    #[test]
    #[should_fail]
    fn test_load_with_bad_offset() {
        // This offset is too long.
        inst_from_str("r9 <- *l(r8 + 0b100000000000)");
    }

    #[test]
    #[should_fail]
    fn test_load_with_bad_negative_offset() {
        // This offset is too long.
        inst_from_str("r9 <- *l(r8 - 0b100000000001)");
    }

    #[test]
    fn test_parse_inst_store() {
        assert_eq!(inst_from_str("*l(r8) <- r9"),
                   StoreInst(Pred { inverted: false,
                                    reg: 3 },
                             LsuOp { store: false,
                                     width: LsuWidthL },
                             Reg { index: 8 },
                             0,
                             Reg { index: 9 }
                             ));

        assert_eq!(inst_from_str("*l(r8 + 6) <- r9"),
                   StoreInst(Pred { inverted: false,
                                    reg: 3 },
                             LsuOp { store: false,
                                     width: LsuWidthL },
                             Reg { index: 8 },
                             6,
                             Reg { index: 9 }
                             ));

        assert_eq!(inst_from_str("*l(r8 - 6) <- r9"),
                   StoreInst(Pred { inverted: false,
                                    reg: 3 },
                             LsuOp { store: false,
                                     width: LsuWidthL },
                             Reg { index: 8 },
                             -6,
                             Reg { index: 9 }
                             ));

        assert_eq!(inst_from_str("*l(r8 + -0x6) <- r9"),
                   StoreInst(Pred { inverted: false,
                                    reg: 3 },
                             LsuOp { store: false,
                                     width: LsuWidthL },
                             Reg { index: 8 },
                             -6,
                             Reg { index: 9 }
                             ));

        assert_eq!(inst_from_str("*llsc(r8 + -0x6) <- r9"),
                   StoreInst(Pred { inverted: false,
                                    reg: 3 },
                             LsuOp { store: false,
                                     width: LsuLLSC },
                             Reg { index: 8 },
                             -6,
                             Reg { index: 9 }
                             ));

        // The largest positive value that can be used as an offset.
        assert_eq!(inst_from_str("*l(r8 + 0b11111111111) <- r9"),
                   StoreInst(Pred { inverted: false,
                                    reg: 3 },
                             LsuOp { store: false,
                                     width: LsuWidthL },
                             Reg { index: 8 },
                             0b11111111111,
                             Reg { index: 9 }
                             ));

        // The most negative value that can be used as an offset.
        assert_eq!(inst_from_str("*l(r8 - 0b100000000000) <- r9"),
                   StoreInst(Pred { inverted: false,
                                    reg: 3 },
                             LsuOp { store: false,
                                     width: LsuWidthL },
                             Reg { index: 8 },
                             -0b100000000000,
                             Reg { index: 9 }
                             ));
    }

    #[test]
    fn test_parse_inst_compareshort() {
        assert_eq!(inst_from_str("p1 <- r3 == 0"),
                   CompareShortInst(Pred { inverted: false,
                                           reg: 3 },
                                    Pred { inverted: false,
                                           reg: 1 },
                                    Reg { index: 3 },
                                    CmpEQ,
                                    0,
                                    0));

        assert_eq!(inst_from_str("p1 <- r3 <=s 0"),
                   CompareShortInst(Pred { inverted: false,
                                           reg: 3 },
                                    Pred { inverted: false,
                                           reg: 1 },
                                    Reg { index: 3 },
                                    CmpLES,
                                    0,
                                    0));

        assert_eq!(inst_from_str("!p0 -> p1 <- r3 == 0"),
                   CompareShortInst(Pred { inverted: true,
                                           reg: 0 },
                                    Pred { inverted: false,
                                           reg: 1 },
                                    Reg { index: 3 },
                                    CmpEQ,
                                    0,
                                    0));

        assert_eq!(inst_from_str("p1 <- r3 == 0b1111111111"),
                   CompareShortInst(Pred { inverted: false,
                                           reg: 3 },
                                    Pred { inverted: false,
                                           reg: 1 },
                                    Reg { index: 3 },
                                    CmpEQ,
                                    0b1111111111,
                                    0));

        assert_eq!(inst_from_str(
            "p1 <- r3 == 0b10000000000000000000000000000001"),
                   CompareShortInst(Pred { inverted: false,
                                           reg: 3 },
                                    Pred { inverted: false,
                                           reg: 1 },
                                    Reg { index: 3 },
                                    CmpEQ,
                                    0b110,
                                    1));
    }

    #[test]
    #[should_fail]
    fn test_compareshort_bad_const() {
        inst_from_str("p1 <- r3 == 0b11111111111");
    }

    #[test]
    #[should_fail]
    fn test_compareshort_negated_dest() {
        inst_from_str("!p1 <- r3 == 0");
    }

    #[test]
    fn test_parse_inst_comparereg() {
        assert_eq!(inst_from_str("p1 <- r3 == r4"),
                   CompareRegInst(Pred { inverted: false,
                                         reg: 3 },
                                  Pred { inverted: false,
                                         reg: 1 },
                                  Reg { index: 3 },
                                  CmpEQ,
                                  Reg { index: 4 },
                                  SllShift,
                                  0));

        assert_eq!(inst_from_str("!p0 -> p1 <- r3 == r4"),
                   CompareRegInst(Pred { inverted: true,
                                         reg: 0 },
                                  Pred { inverted: false,
                                         reg: 1 },
                                  Reg { index: 3 },
                                  CmpEQ,
                                  Reg { index: 4 },
                                  SllShift,
                                  0));

        assert_eq!(inst_from_str("!p0 -> p1 <- r3 == (r4 << 5)"),
                   CompareRegInst(Pred { inverted: true,
                                         reg: 0 },
                                  Pred { inverted: false,
                                         reg: 1 },
                                  Reg { index: 3 },
                                  CmpEQ,
                                  Reg { index: 4 },
                                  SllShift,
                                  5));
    }
}