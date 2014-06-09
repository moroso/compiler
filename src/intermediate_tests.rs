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
use package::Package;
use ir::intermediate_to_c::IRToC;

use std::io::stdio;

fn package_from_stdin() -> Package {
    Package::new("<stdin>", stdio::stdin())
}

pub fn package_to_ir(p: Package) {
    let Package {
        module:  module,
        session: mut session,
        typemap: typemap,
    } = p;

    let mut result =
    {
        let mut converter = ASTToIntermediate::new(&mut session.interner);

        let mut result = vec!();

        for item in module.val.items.iter() {
            print!("{}\n", item);
            let (insts, var) = converter.convert_item(item);
            print!("{}\n\n", insts);
            result.push(insts);
        }
        result
    };

    for insts in result.mut_iter() {
        ToSSA::to_ssa(insts);
        ConstantFolder::fold(insts);
        print!("{}\n", IRToC::convert_function(&session.interner,
                                               insts));
    }
}

pub fn main() {
    let package = package_from_stdin();

    package_to_ir(package);

    return;

    let buffer = io::BufferedReader::new(io::MemReader::new(
        //Vec::from_slice("{*(a*5+6*7)=4u16*(5u16+1u16)*foo; b=a+1; c=6+7<2 || a}".as_bytes())
        //Vec::from_slice("{ while(1<2) { x = x + 1; x = 5; z = x + x; x = z; } }".as_bytes())
        //Vec::from_slice("{ a += 1; *(b+3) += 1; }".as_bytes())
        Vec::from_slice("{ r = 1; while (e>0) { if e%2 != 0 { r = r * b; } b = b * b; e = e / 2; }; return r; }".as_bytes())
        ));
    let mut parser = Parser::new();
    let mut interner = Interner::new();
    let lexer = Lexer::new("<stdin>", buffer);

    let ast = parser.parse_with(lexer, &mut interner, |p| p.parse_expr());
    let mut conv = ASTToIntermediate::new(&mut interner);
    let (mut ops, var) = conv.convert_expr(&ast);

    print!("{}\n", ast);
    print!("{}\n", (&ops, var));

    ToSSA::to_ssa(&mut ops);
    print!("{}\n", ops);

    ConstantFolder::fold(&mut ops);
    print!("{}\n", ops);

    let opinfo = LivenessAnalyzer::analyze(&ops);
    for z in opinfo.iter().zip(ops.iter()) {
        print!("{}", z);
    }
}
