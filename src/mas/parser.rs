//! Parse a Moroso asm file.

use mas::lexer::*;
use mas::ast::*;
use mas::util::{pack_int, fits_in_bits};
use util::lexer::{Lexer, SourceToken, BufReader};
use std::iter::Peekable;
use std::num::FromPrimitive;
use span::{SourcePos, Span, mk_sp};
use std::collections::BTreeMap;
use std::io::{BufRead, Read, Write};

pub use self::InstType::*;

pub struct AsmParser<'a, T: BufRead> {
    tokens: Peekable<Lexer<'a, T, Token>>,
    last_span: Span,
    error_on_misplaced_inst: bool,
}

fn tok_to_op(tok: &Token) -> Option<AluOp> {
    match *tok {
        Token::Plus => Some(AddAluOp),
        Token::Amp | Token::And => Some(AndAluOp),
        Token::TildePipe | Token::Nor => Some(NorAluOp),
        Token::Pipe | Token::Or => Some(OrAluOp),
        Token::Dash | Token::Sub => Some(SubAluOp),
        Token::DashColon | Token::Rsb => Some(RsbAluOp),
        Token::Caret | Token::Xor => Some(XorAluOp),
        _ => None,
    }
}

fn tok_to_unop(tok: &Token) -> Option<AluOp> {
    match *tok {
        Token::Mov => Some(MovAluOp),
        Token::Tilde |
        Token::Mvn => Some(MvnAluOp),
        Token::Sxb => Some(SxbAluOp),
        Token::Sxh => Some(SxhAluOp),
        _ => None,
    }
}

fn tok_to_cmp(tok: &Token) -> Option<CompareType> {
    match *tok {
        Token::Lt => Some(CmpLTU),
        Token::Le => Some(CmpLEU),
        Token::Lts => Some(CmpLTS),
        Token::Les => Some(CmpLES),
        Token::EqEq => Some(CmpEQ),
        Token::Eq => Some(CmpEQ),
        Token::Bs => Some(CmpBS),
        Token::Amp => Some(CmpBS),
        Token::Bc => Some(CmpBC),
        _ => None,
    }
}

pub enum InstType {
    ALUInstType,
    ControlType,
    MemoryType,
    LongType,
}

pub fn classify_inst(inst: &InstNode) -> InstType {
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
        CompareLongInst(..) |
        NopInst => ALUInstType,
        LongInst(..) => LongType,
        LoadInst(..) |
        StoreInst(..) => MemoryType,
        BranchImmInst(..) |
        BranchRegInst(..) |
        BreakInst(..) |
        SyscallInst(..) |
        MtcInst(..) |
        MfcInst(..) |
        EretInst(..) |
        FenceInst(..) |
        MthiInst(..) |
        MfhiInst(..) |
        DivInst(..) |
        MultInst(..) |
        FlushInst(..) => ControlType,
    }
}

impl<'a, T: BufRead> AsmParser<'a, T> {

    pub fn new(tokens: Peekable<Lexer<'a, T, Token>>
           ) -> AsmParser<'a, T> {
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
    fn peek(&mut self) -> &Token {
        match self.tokens.peek() {
            Some(st) => &st.tok,
            None => panic!("Tried to peek past EOF"),
        }
    }

    /// Consume the next token from the stream, returning it.
    fn eat(&mut self) -> Token {
        match self.tokens.next() {
            Some(st) => {
                self.last_span = st.sp;
                st.tok
            }
            None => panic!("Tried to read past EOF"),
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

    fn error<U: AsRef<str>>(&self, message: U) -> ! {
        panic!("\n{}\nat {}", message.as_slice(), self.last_span.get_begin())
    }

    /// Assert that `num` fits into `size` bits.
    fn assert_num_size(&self, num: u32, size: u8) {
        if !fits_in_bits(num, size) {
            self.error(format!("Number {} (0b{:b}) has more than {} bits.",
                               num, num, size))
        }
    }

    fn assert_signed_num_size(&self, num: i32, size: u8) {
        self.assert_num_size(if num < 0 { -num } else { num } as u32,
                             size);
        // But we also have to make sure the sign bit is okay.
        // This checks that the sign bit is cleared if n is
        // nonnegative, and set if it's negative.
        if (num >= 0) != (num & (1<<((size-1) as uint)) == 0) {
            self.error(format!(
                "Signed number {} (0b{:b}) needs more than {} bits.",
                num, num, size));
        }
    }


    // asm-specific parsing functions start here!

    /// Parse either a register by itself, or a register with a shift
    /// (e.g. (r7 << 3) ).
    fn parse_reg_maybe_shift(&mut self) -> (Reg, ShiftType,
                                            Result<u8, Reg>) {
        let tok = self.eat();
        match tok {
            // Bare register.
            Token::Reg(reg) => (reg, FromPrimitive::from_isize(0).unwrap(), Ok(0)),

            // Something parenthesized.
            Token::LParen => {
                // No matter what, there should be a register.
                let reg = match *self.peek() {
                    Token::Reg(reg) => { self.eat(); reg },
                    _ => self.error(format!("Expected register; got {}", tok)),
                };
                let inner_tok = self.eat();
                match inner_tok {
                    // There's just the one register, that for some reason
                    // is in parentheses.
                    Token::RParen => (reg, FromPrimitive::from_isize(0).unwrap(), Ok(0)),

                    // There's a shift.
                    Token::Shift(shifttype) => {
                        match self.eat() {
                            Token::NumLit(num) => {
                                self.assert_num_size(
                                    num, 5);
                                self.expect(Token::RParen);

                                (reg, shifttype, Ok(num as u8))
                            },
                            Token::Reg(reg2) => {
                                self.expect(Token::RParen);
                                (reg, shifttype, Err(reg2))}
                            ,
                            _ => self.error("Need a shift amount"),
                        }
                    },
                    _ => self.error(format!("Unexpected token {}", inner_tok)),
                }
            },
            _ => self.error(format!("Unexpected token {}", tok)),
        }
    }

    fn parse_mul_or_div(&mut self, pred: Pred, rd: Reg, rs: Reg) -> InstNode {
        let cons: fn(Pred, bool, Reg, Reg, Reg) -> InstNode;
        let is_signed;

        match self.eat() {
            Token::Star |
            Token::StarU => { cons = InstNode::mult; is_signed = false; }
            Token::StarS => { cons = InstNode::mult; is_signed = true; }
            Token::Slash |
            Token::SlashU => { cons = InstNode::div; is_signed = false; }
            Token::SlashS => { cons = InstNode::div; is_signed = true; }
            _ => self.error("ICE: should have a mult/div here.")
        }

        match self.eat() {
            Token::Reg(rt) => cons(pred, is_signed, rd, rs, rt),
            _ => self.error("Expected register.")
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
            Token::Reg(reg) => reg,
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
                    FromPrimitive::from_isize(0).unwrap(),
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
                            Token::LParen | Token::Reg(..) => {
                                let (rs,
                                     shifttype,
                                     shiftamt) = self.parse_reg_maybe_shift();
                                let shiftamt = shiftamt.ok()
                                    .expect("Shifts by registers not allowed");
                                InstNode::alu2reg(
                                    pred,
                                    op,
                                    rd,
                                    reg,
                                    rs,
                                    shifttype,
                                    shiftamt)
                            },
                            Token::NumLit(num) => {
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
                            Token::Long => {
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
                        // shift operator instead, or a mult/div, or we're done.
                        match *self.peek() {
                            Token::Shift(shift) => {
                                // It's a shift. There's should be a reg
                                // after it, and that's it.
                                self.eat();
                                match self.eat() {
                                    Token::Reg(reg2) =>
                                        InstNode::alu1regsh(
                                            pred,
                                            rd,
                                            MovAluOp,
                                            reg,
                                            shift,
                                            reg2),
                                    _ => self.error("Unexpected token."),
                                }
                            },
                            Token::Star |
                            Token::StarS |
                            Token::StarU |
                            Token::Slash |
                            Token::SlashS |
                            Token::SlashU => {
                                self.parse_mul_or_div(pred, rd, reg)
                            }
                            _ => {
                                // Just the register. So it's just a move.
                                InstNode::alu1reg(
                                    pred,
                                    MovAluOp,
                                    rd,
                                    reg,
                                    FromPrimitive::from_isize(0).unwrap(),
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
            Token::NumLit(n) => {
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
            Token::Reg(..) => {
                // There's a register. Either that's *all* there is
                // (we're applying a unary op to a register), or there's
                // a binary op. `parse_reg_and_maybe_binop` will take
                // care of figuring all that out.
                self.parse_reg_and_maybe_binop(pred, rd, op)
            },
            Token::Long => {
                self.eat();
                InstNode::alu1long(
                    pred,
                    op.unwrap_or(MovAluOp),
                    rd)
            },
            _ => {
                // The only option left is that we're applying a unary op
                // to a *shifted* register.
                let (reg, shifttype, shiftamt) = self.parse_reg_maybe_shift();
                match shiftamt {
                    Ok(amt) =>
                        InstNode::alu1reg(
                            pred,
                            op.unwrap_or(MovAluOp),
                            rd,
                            reg,
                            shifttype,
                            amt
                                ),
                    Err(reg2) =>
                        InstNode::alu1regsh(
                            pred,
                            rd,
                            op.unwrap_or(MovAluOp),
                            reg,
                            shifttype,
                            reg2
                            )
                }
            }
        }
    }

    /// Parses anything that comes after the width specifier in a load or
    /// store.
    /// So, in *l(r8 + 12), this parses the (r8 + 12) part.
    fn parse_deref_common(&mut self) -> (Reg, i32) {
        self.expect(Token::LParen);
        let reg = match self.eat() {
            Token::Reg(reg) => reg,
            _ => self.error("Expected register."),
        };

        let next_tok = self.eat();
        let offs: i32 = match next_tok {
            Token::Plus |
            Token::Dash => {
                match self.eat() {
                    Token::NumLit(n) => {
                        let mut n = n as i32;
                        if next_tok == Token::Dash { n = -n; }
                        // We must fit into 12 bits...
                        self.assert_signed_num_size(n, 12);
                        self.expect(Token::RParen);

                        n
                    },
                    _ => self.error("Need a number."),
                }
            },
            Token::RParen => 0,
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
            Token::LoadStore(width) => width,
            _ => panic!("ICE"),
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
                Token::NumLit(..) |
                Token::Reg(..) |
                Token::Long |
                Token::LParen => self.parse_expr(pred, rd, None),
                Token::LoadStore(..) => self.parse_load(pred, rd),
                Token::CoReg(..) => self.parse_mfc(pred, rd),
                Token::Ovf => self.parse_mfhi(pred, rd),
                _ => self.error("Unexpected token."),
            }
        }
    }

    // Assumes the "long" keyword has already been consumed.
    pub fn parse_long(&mut self) -> InstNode {
        match self.eat() {
            Token::NumLit(n) => InstNode::long(n),
            _ => self.error("Must have a numeric literal for long."),
        }
    }

    pub fn parse_store(&mut self, pred: Pred, width: LsuWidth) -> InstNode {
        let (reg, offs) = self.parse_deref_common();

        self.expect(Token::Gets);

        let rhsreg = match self.eat() {
            Token::Reg(rhsreg) => rhsreg,
            _ => self.error("Expected a register."),
        };

        InstNode::store(pred,
                        LsuOp {
                            store: true,
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
            Token::Reg(reg) => reg,
            _ => self.error("Expected register."),
        };

        let op = match tok_to_cmp(&self.eat()) {
            Some(op) => op,
            None => self.error("Expected a comparison op."),
        };

        match *self.peek() {
            Token::NumLit(n) => {
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
            Token::Long => {
                self.eat();
                InstNode::comparelong(
                    pred,
                    dest_pred,
                    reg,
                    op)
            },
            _ => {
                let (reg_rt, shifttype, shiftamt) =
                    self.parse_reg_maybe_shift();
                let shiftamt = shiftamt.ok().expect(
                    "Shifts by regisers are not allowed here");
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

    pub fn parse_branch(&mut self, pred: Pred, linked: bool) -> InstNode {
        let cur_tok = self.eat();
        match cur_tok {
            Token::IdentTok(name) =>
                InstNode::branchimm(
                    pred,
                    linked,
                    JumpLabel(name)),
            Token::NumLit(n) => {
                let n = n as i32;
                self.assert_signed_num_size(n, 25);
                InstNode::branchimm(
                    pred,
                    linked,
                    JumpOffs(n as i32))
            },
            Token::Reg(reg) => {
                match *self.peek() {
                    Token::Plus |
                    Token::Dash => {
                        let subtracted = self.eat() == Token::Dash;
                        match self.eat() {
                            Token::NumLit(n) => {
                                let mut n = n as i32;
                                if subtracted {
                                    n = -n;
                                }
                                self.assert_signed_num_size(n, 25);
                                InstNode::branchreg(
                                    pred,
                                    linked,
                                    reg,
                                    n)
                            },
                            _ => self.error("Expected a number."),
                        }
                    },
                    _ => InstNode::branchreg(
                        pred,
                        linked,
                        reg,
                        0)
                }
            },
            _ => self.error("Expected a jump target.")
        }
    }

    fn parse_break_or_syscall<F>(&mut self, pred: Pred,
                                 cons: F) -> InstNode
        where F: Fn(Pred, u32) -> InstNode {
        match *self.peek() {
            Token::NumLit(num) => {
                self.eat();
                self.assert_num_size(num, 5);
                cons(pred, num)
            },
            _ => cons(pred, 0),
        }
    }

    fn parse_mtc(&mut self, pred: Pred, coreg: CoReg) -> InstNode {
        self.expect(Token::Gets);
        match self.eat() {
            Token::Reg(reg) => InstNode::mtc(pred, coreg, reg),
            _ => self.error("Expected register.")
        }
    }

    fn parse_mfc(&mut self, pred: Pred, rd: Reg) -> InstNode {
        match self.eat() {
            Token::CoReg(coreg) => InstNode::mfc(pred, rd, coreg),
            _ => self.error("Expected coprocessor register.")
        }
    }

    fn parse_mthi(&mut self, pred: Pred) -> InstNode {
        self.expect(Token::Gets);
        match self.eat() {
            Token::Reg(reg) => InstNode::mthi(pred, reg),
            _ => self.error("Expected register.")
        }
    }

    fn parse_mfhi(&mut self, pred: Pred, rd: Reg) -> InstNode {
        self.expect(Token::Ovf);
        InstNode::mfhi(pred, rd)
    }

    fn parse_flush(&mut self, pred: Pred, flushtype: FlushType) -> InstNode {
        match self.eat() {
            Token::Reg(reg) => InstNode::flush(pred, flushtype, reg),
            _ => self.error("Expected register.")
        }
    }

    /// Parse an entire instruction.
    pub fn parse_inst(&mut self) -> InstNode {
        // Begin by parsing the predicate register for this instruction.
        let mut cur_tok = self.eat();
        let pred = match cur_tok {
            Token::PredReg(pred) => {
                // There's a predicate register, but we don't know what
                // context it appears in: we may be assigning to it, or
                // predicating based on it.
                match *self.peek() {
                    Token::Predicates => {
                        // It's actually predicating.
                        self.eat();
                        cur_tok = self.eat();

                        Some(pred)
                    },
                    Token::Gets => // It's being assigned to. Punt on this.
                        None,
                    _ => self.error("Unexpected token."),
                }
            },
            _ => None,
        };

        match cur_tok {
            Token::Reg(reg) => {
                self.expect(Token::Gets);
                self.parse_op_or_expr(pred.unwrap_or(TRUE_PRED), reg.clone())
            },
            Token::PredReg(destpred) => {
                self.expect(Token::Gets);
                self.parse_conditional(pred.unwrap_or(TRUE_PRED), destpred)
            },
            Token::LoadStore(width) =>
                self.parse_store(pred.unwrap_or(TRUE_PRED), width),
            Token::B => self.parse_branch(pred.unwrap_or(TRUE_PRED), false),
            Token::Bl => self.parse_branch(pred.unwrap_or(TRUE_PRED), true),
            Token::Break => self.parse_break_or_syscall(
                pred.unwrap_or(TRUE_PRED),
                InstNode::breaknum),
            Token::Syscall => self.parse_break_or_syscall(
                pred.unwrap_or(TRUE_PRED),
                InstNode::syscall),
            Token::CoReg(cr) => self.parse_mtc(pred.unwrap_or(TRUE_PRED), cr),
            Token::Eret => InstNode::eret(pred.unwrap_or(TRUE_PRED)),
            Token::Fence => InstNode::fence(pred.unwrap_or(TRUE_PRED)),
            Token::Flush(flushtype) => self.parse_flush(
                pred.unwrap_or(TRUE_PRED),
                flushtype),
            Token::Ovf => self.parse_mthi(pred.unwrap_or(TRUE_PRED)),
            Token::Nop => {
                if pred != None {
                    self.error("Cannot have a predicate for a nop.");
                }

                InstNode::nop()
            },
            Token::Long => {
                if pred != None {
                    self.error("Cannot have a predicate for a long");
                }

                self.parse_long()
            },
            _ => self.error(format!("Unknown token {}\n", cur_tok)),
        }
    }

    pub fn parse_inst_packet(&mut self) -> InstPacket {
        let mut insts: InstPacket = [NopInst, NopInst, NopInst, NopInst];

        self.expect(Token::LBrace);
        for i in 0 .. 4 {
            match *self.peek() {
                Token::RBrace => {
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
                    if *self.peek() == Token::Semi { self.eat(); }
                }
            }
        }
        self.expect(Token::RBrace);

        insts
    }

    pub fn parse_toplevel(&mut self) -> (Vec<InstPacket>,
                                         BTreeMap<String, uint>) {
        let mut labels = BTreeMap::new();
        let mut packets = vec!();
        let mut instnum = 0;

        loop {
            match *self.peek() {
                Token::IdentTok(..) => {
                    let name = match self.eat() {
                        Token::IdentTok(name) => name,
                        _ => panic!()
                    };
                    self.expect(Token::Colon);
                    if labels.insert(name.clone(), instnum).is_some() {
                        self.error(format!("Label '{}' redefined.", name));
                    }
                },
                Token::Eof => {
                    return (packets, labels)
                },
                _ => {
                    packets.push(self.parse_inst_packet());
                    instnum += 1;
                }
            }
        }
    }
}

// Convenience function for testing
pub fn inst_from_str(s: &str) -> InstNode {
    let mut parser = AsmParser::new(asm_lexer_from_str(s).peekable());
    let result = parser.parse_inst();
    assert_eq!(parser.tokens.peek().unwrap().tok, Token::Eof);
    result
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
    #[should_panic]
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
    #[should_panic]
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
    #[should_panic]
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
                                 MovAluOp,
                                 Reg { index: 7 },
                                 SllShift,
                                 Reg { index: 8 }));

        assert_eq!(inst_from_str("r6 <- r7 >>u r8"),
                   ALU1RegShInst(Pred { inverted: false,
                                        reg: 3 },
                                 Reg { index: 6 },
                                 MovAluOp,
                                 Reg { index: 7 },
                                 SrlShift,
                                 Reg { index: 8 }));

        assert_eq!(inst_from_str("!p2 -> r6 <- r7 >>s r8"),
                   ALU1RegShInst(Pred { inverted: true,
                                        reg: 2 },
                                 Reg { index: 6 },
                                 MovAluOp,
                                 Reg { index: 7 },
                                 SraShift,
                                 Reg { index: 8 }));

        assert_eq!(inst_from_str("r6 <- ~(r7 << r8)"),
                   ALU1RegShInst(Pred { inverted: false,
                                        reg: 3 },
                                 Reg { index: 6 },
                                 MvnAluOp,
                                 Reg { index: 7 },
                                 SllShift,
                                 Reg { index: 8 }));

        assert_eq!(inst_from_str("r6 <- ~(r7 >>u r8)"),
                   ALU1RegShInst(Pred { inverted: false,
                                        reg: 3 },
                                 Reg { index: 6 },
                                 MvnAluOp,
                                 Reg { index: 7 },
                                 SrlShift,
                                 Reg { index: 8 }));

        assert_eq!(inst_from_str("!p2 -> r6 <- ~(r7 >>s r8)"),
                   ALU1RegShInst(Pred { inverted: true,
                                        reg: 2 },
                                 Reg { index: 6 },
                                 MvnAluOp,
                                 Reg { index: 7 },
                                 SraShift,
                                 Reg { index: 8 }));
    }

    #[test]
    fn test_parse_inst_nop() {
        assert_eq!(inst_from_str("nop"), NopInst);
    }

    #[test]
    #[should_panic]
    fn test_parse_nop_with_pred() {
        // Nop instructions can't have predicates.
        inst_from_str("p0 -> nop");
    }

    #[test]
    fn test_parse_inst_long() {
        assert_eq!(inst_from_str("long 0x56"), LongInst(Immediate(0x56)));
    }

    #[test]
    #[should_panic]
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
    #[should_panic]
    fn test_load_with_bad_offset() {
        // This offset is too long.
        inst_from_str("r9 <- *l(r8 + 0b100000000000)");
    }

    #[test]
    #[should_panic]
    fn test_load_with_bad_negative_offset() {
        // This offset is too long.
        inst_from_str("r9 <- *l(r8 - 0b100000000001)");
    }

    #[test]
    fn test_parse_inst_store() {
        assert_eq!(inst_from_str("*l(r8) <- r9"),
                   StoreInst(Pred { inverted: false,
                                    reg: 3 },
                             LsuOp { store: true,
                                     width: LsuWidthL },
                             Reg { index: 8 },
                             0,
                             Reg { index: 9 }
                             ));

        assert_eq!(inst_from_str("*l(r8 + 6) <- r9"),
                   StoreInst(Pred { inverted: false,
                                    reg: 3 },
                             LsuOp { store: true,
                                     width: LsuWidthL },
                             Reg { index: 8 },
                             6,
                             Reg { index: 9 }
                             ));

        assert_eq!(inst_from_str("*l(r8 - 6) <- r9"),
                   StoreInst(Pred { inverted: false,
                                    reg: 3 },
                             LsuOp { store: true,
                                     width: LsuWidthL },
                             Reg { index: 8 },
                             -6,
                             Reg { index: 9 }
                             ));

        assert_eq!(inst_from_str("*l(r8 + -0x6) <- r9"),
                   StoreInst(Pred { inverted: false,
                                    reg: 3 },
                             LsuOp { store: true,
                                     width: LsuWidthL },
                             Reg { index: 8 },
                             -6,
                             Reg { index: 9 }
                             ));

        assert_eq!(inst_from_str("*llsc(r8 + -0x6) <- r9"),
                   StoreInst(Pred { inverted: false,
                                    reg: 3 },
                             LsuOp { store: true,
                                     width: LsuLLSC },
                             Reg { index: 8 },
                             -6,
                             Reg { index: 9 }
                             ));

        // The largest positive value that can be used as an offset.
        assert_eq!(inst_from_str("*l(r8 + 0b11111111111) <- r9"),
                   StoreInst(Pred { inverted: false,
                                    reg: 3 },
                             LsuOp { store: true,
                                     width: LsuWidthL },
                             Reg { index: 8 },
                             0b11111111111,
                             Reg { index: 9 }
                             ));

        // The most negative value that can be used as an offset.
        assert_eq!(inst_from_str("*l(r8 - 0b100000000000) <- r9"),
                   StoreInst(Pred { inverted: false,
                                    reg: 3 },
                             LsuOp { store: true,
                                     width: LsuWidthL },
                             Reg { index: 8 },
                             -0b100000000000,
                             Reg { index: 9 }
                             ));
    }

    #[test]
    fn test_parse_inst_comparelong() {
        assert_eq!(inst_from_str("p1 <- r3 == long"),
                   CompareLongInst(Pred { inverted: false,
                                          reg: 3 },
                                   Pred { inverted: false,
                                          reg: 1 },
                                   Reg { index: 3 },
                                   CmpEQ));


        assert_eq!(inst_from_str("!p1 -> p1 <- r3 == long"),
                   CompareLongInst(Pred { inverted: true,
                                          reg: 1 },
                                   Pred { inverted: false,
                                          reg: 1 },
                                   Reg { index: 3 },
                                   CmpEQ));
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
    #[should_panic]
    fn test_compareshort_bad_const() {
        inst_from_str("p1 <- r3 == 0b11111111111");
    }

    #[test]
    #[should_panic]
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

    #[test]
    fn test_parse_inst_branchimm() {
        assert_eq!(inst_from_str("b 0"),
                   BranchImmInst(Pred { inverted: false,
                                        reg: 3 },
                                 false,
                                 JumpOffs(0),
                                 ));

        assert_eq!(inst_from_str("!p0 -> b 1"),
                   BranchImmInst(Pred { inverted: true,
                                        reg: 0 },
                                 false,
                                 JumpOffs(1),
                                 ));

        assert_eq!(inst_from_str("bl -1"),
                   BranchImmInst(Pred { inverted: false,
                                        reg: 3 },
                                 true,
                                 JumpOffs(-1),
                                 ));

        assert_eq!(inst_from_str("bl 0xffffff"),
                   BranchImmInst(Pred { inverted: false,
                                        reg: 3 },
                                 true,
                                 JumpOffs(0xffffff),
                                 ));

        assert_eq!(inst_from_str("bl -0x1000000"),
                   BranchImmInst(Pred { inverted: false,
                                        reg: 3 },
                                 true,
                                 JumpOffs(-0x1000000),
                                 ));
    }

    #[test]
    #[should_panic]
    fn test_branchimm_bad_offs() {
        inst_from_str("bl 0x1000000");
    }

    #[test]
    #[should_panic]
    fn test_branchimm_bad_negative_offs() {
        inst_from_str("bl -0x1000001");
    }

    #[test]
    fn test_parse_inst_branchreg() {
        assert_eq!(inst_from_str("b r6"),
                   BranchRegInst(Pred { inverted: false,
                                        reg: 3 },
                                 false,
                                 Reg { index: 6 },
                                 0,
                                 ));

        assert_eq!(inst_from_str("!p0 -> b r6"),
                   BranchRegInst(Pred { inverted: true,
                                        reg: 0 },
                                 false,
                                 Reg { index: 6 },
                                 0,
                                 ));

        assert_eq!(inst_from_str("b r6 + 1"),
                   BranchRegInst(Pred { inverted: false,
                                        reg: 3 },
                                 false,
                                 Reg { index: 6 },
                                 1,
                                 ));

        assert_eq!(inst_from_str("b r6 - 1"),
                   BranchRegInst(Pred { inverted: false,
                                        reg: 3 },
                                 false,
                                 Reg { index: 6 },
                                 -1,
                                 ));

        assert_eq!(inst_from_str("b r6 + 0xffffff"),
                   BranchRegInst(Pred { inverted: false,
                                        reg: 3 },
                                 false,
                                 Reg { index: 6 },
                                 0xffffff,
                                 ));

        assert_eq!(inst_from_str("b r6 + -0x1000000"),
                   BranchRegInst(Pred { inverted: false,
                                        reg: 3 },
                                 false,
                                 Reg { index: 6 },
                                 -0x1000000,
                                 ));
    }

    #[test]
    #[should_panic]
    fn test_branchreg_bad_offs() {
        inst_from_str("bl r6 + 0x1000000");
    }

    #[test]
    #[should_panic]
    fn test_branchreg_bad_negative_offs() {
        inst_from_str("bl r6 - 0x1000001");
    }

    #[test]
    fn test_parse_inst_break() {
        assert_eq!(inst_from_str("break"),
                   BreakInst(Pred { inverted: false,
                                    reg: 3 },
                             0));
        assert_eq!(inst_from_str("break 3"),
                   BreakInst(Pred { inverted: false,
                                    reg: 3 },
                             3));
    }

    #[test]
    fn test_parse_inst_syscall() {
        assert_eq!(inst_from_str("syscall"),
                   SyscallInst(Pred { inverted: false,
                                      reg: 3 },
                               0));
        assert_eq!(inst_from_str("syscall 3"),
                   SyscallInst(Pred { inverted: false,
                                      reg: 3 },
                               3));
    }

    #[test]
    fn test_parse_inst_mtc() {
        assert_eq!(inst_from_str("EA0 <- r6"),
                   MtcInst(Pred { inverted: false,
                                  reg: 3 },
                           EA0,
                           Reg { index: 6 }));

        assert_eq!(inst_from_str("!p0 -> eA0 <- r6"),
                   MtcInst(Pred { inverted: true,
                                  reg: 0 },
                           EA0,
                           Reg { index: 6 }));
    }

    #[test]
    fn test_parse_inst_mfc() {
        assert_eq!(inst_from_str("r6 <- EA0"),
                   MfcInst(Pred { inverted: false,
                                  reg: 3 },
                           Reg { index: 6 },
                           EA0));

        assert_eq!(inst_from_str("!p0 -> r6 <- eA0"),
                   MfcInst(Pred { inverted: true,
                                  reg: 0 },
                           Reg { index: 6 },
                           EA0));
    }

    #[test]
    fn test_parse_inst_eret() {
        assert_eq!(inst_from_str("eRet"),
                   EretInst(Pred { inverted: false,
                                   reg: 3 }));

        assert_eq!(inst_from_str("!p0 -> eret"),
                   EretInst(Pred { inverted: true,
                                   reg: 0 }));
    }

    #[test]
    fn test_parse_inst_fence() {
        assert_eq!(inst_from_str("fenCe"),
                   FenceInst(Pred { inverted: false,
                                    reg: 3 }));

        assert_eq!(inst_from_str("!p0 -> fence"),
                   FenceInst(Pred { inverted: true,
                                    reg: 0 }));
    }

    #[test]
    fn test_parse_inst_mthi() {
        assert_eq!(inst_from_str("oVf <- r6"),
                   MthiInst(Pred { inverted: false,
                                   reg: 3 },
                            Reg { index: 6 }));

        assert_eq!(inst_from_str("!p0 -> OVF <- r6"),
                   MthiInst(Pred { inverted: true,
                                   reg: 0 },
                            Reg { index: 6 }));
    }

    #[test]
    fn test_parse_inst_mfhi() {
        assert_eq!(inst_from_str("r6 <- OVF"),
                   MfhiInst(Pred { inverted: false,
                                   reg: 3 },
                            Reg { index: 6 }));

        assert_eq!(inst_from_str("!p0 -> r6 <- oVf"),
                   MfhiInst(Pred { inverted: true,
                                   reg: 0 },
                            Reg { index: 6 }));
    }

    #[test]
    fn test_parse_inst_mult() {
        assert_eq!(inst_from_str("r6 <- r1 * r2"),
                   MultInst(Pred { inverted: false,
                                   reg: 3 },
                            false,
                            Reg { index: 6 },
                            Reg { index: 1 },
                            Reg { index: 2 }));

        assert_eq!(inst_from_str("r6 <- r1 *s r2"),
                   MultInst(Pred { inverted: false,
                                   reg: 3 },
                            true,
                            Reg { index: 6 },
                            Reg { index: 1 },
                            Reg { index: 2 }));

        assert_eq!(inst_from_str("r6 <- r1 *u r2"),
                   MultInst(Pred { inverted: false,
                                   reg: 3 },
                            false,
                            Reg { index: 6 },
                            Reg { index: 1 },
                            Reg { index: 2 }));

        assert_eq!(inst_from_str("!p0 -> r6 <- r1 * r2"),
                   MultInst(Pred { inverted: true,
                                   reg: 0 },
                            false,
                            Reg { index: 6 },
                            Reg { index: 1 },
                            Reg { index: 2 }));
    }

    #[test]
    fn test_parse_inst_div() {
        assert_eq!(inst_from_str("r6 <- r1 / r2"),
                   DivInst(Pred { inverted: false,
                                  reg: 3 },
                           false,
                           Reg { index: 6 },
                           Reg { index: 1 },
                           Reg { index: 2 }));

        assert_eq!(inst_from_str("r6 <- r1 /s r2"),
                   DivInst(Pred { inverted: false,
                                  reg: 3 },
                           true,
                           Reg { index: 6 },
                           Reg { index: 1 },
                           Reg { index: 2 }));

        assert_eq!(inst_from_str("r6 <- r1 /u r2"),
                   DivInst(Pred { inverted: false,
                                  reg: 3 },
                           false,
                           Reg { index: 6 },
                           Reg { index: 1 },
                           Reg { index: 2 }));

        assert_eq!(inst_from_str("!p0 -> r6 <- r1 / r2"),
                   DivInst(Pred { inverted: true,
                                  reg: 0 },
                           false,
                           Reg { index: 6 },
                           Reg { index: 1 },
                           Reg { index: 2 }));
    }

    #[test]
    fn test_parse_inst_flush() {
        assert_eq!(inst_from_str("flush.Data r6"),
                   FlushInst(Pred { inverted: false,
                                    reg: 3 },
                             DataFlush,
                             Reg { index: 6 }));


        assert_eq!(inst_from_str("!p0 -> fluSh.ItLb r6"),
                   FlushInst(Pred { inverted: true,
                                    reg: 0 },
                             ItlbFlush,
                             Reg { index: 6 }));
    }
}
