/// TEMPORARY FILE.
/// This is for testing out/playing with the intermediate representation
/// stuff. It should be deleted when the IR code gets to the point where
/// this is no longer needed.
///
use std::io;
use lexer::Lexer;
use parser::Parser;
use session::Interner;
use ir::liveness::LivenessAnalyzer;
use ir::ast_to_intermediate::ASTToIntermediate;
use ir::constant_fold::ConstantFolder;
use ir::ssa::ToSSA;

pub fn main() {
    let buffer = io::BufferedReader::new(io::MemReader::new(
        //Vec::from_slice("{*(a*5+6*7)=4u16*(5u16+1u16)*foo; b=a+1; c=6+7<2 || a}".as_bytes())
        Vec::from_slice("{ while(1<2) { x = x + 1; x = 5; z = x + x; x = z; } }".as_bytes())
        ));
    let mut parser = Parser::new();
    let mut interner = Interner::new();
    let lexer = Lexer::new("<stdin>", buffer);

    let ast = parser.parse_with(lexer, &mut interner, |p| p.parse_expr());
    let mut conv = ASTToIntermediate::new(&mut interner);
    let (ops, var) = conv.convert_expr(&ast);

    print!("{}\n", ast);
    print!("{}\n", (&ops, var));

    let mut ssa = ToSSA::new(box ops);
    ssa.to_ssa();
    print!("{}\n", ssa.ops);

    let mut folder = ConstantFolder::new(ssa.ops);
    folder.constant_fold();
    print!("{}\n", folder.ops);

    let mut liveness = LivenessAnalyzer::new(folder.ops);
    liveness.seed();
    print!("{}\n", liveness.ops);
    print!("{}\n", liveness.opinfo);
    liveness.propagate();
    for z in liveness.opinfo.iter().zip(liveness.ops.iter()) {
        print!("{}\n", z);
    }
}
