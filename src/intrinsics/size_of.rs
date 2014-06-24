use mc::ast::{Module, NodeId};
use mc::ast::Type;

use std::iter::AdditiveIterator;
use std::vec::unzip;

use mc::ast::defmap::*;
use mc::session::*;
use typechecker::*;
use util::*;

/// The size of the tag for enums.
static enum_tag_size: u64 = 4;

/// Return the alignment for an object of a certain size.
pub fn alignment(size: u64) -> u64 {
    if size > 2 { 4 }
    else { size }
}

fn struct_field_sizes(session: &Session,
                      typemap: &Typemap,
                      fields: &Vec<(Name, Type)>) -> Vec<(Name, u64)> {
    fields.iter()
        .map(|&(n, ref t)| (n, size_of_ty(session, typemap.types.get(&t.id.to_uint()))))
        .collect()
}

// TODO: a function that returns the vector of sizes of the components
// of a struct/tuple/enumitem.
pub fn offset_of_struct_field(session: &Session,
                              typemap: &Typemap,
                              node: &NodeId,
                              field: &Name) -> u64 {
    let def = session.defmap.find(node).unwrap();
    match *def {
        StructDef(_, ref fields, _) => {
            let names_and_sizes = struct_field_sizes(session, typemap, fields);
            let (names, sizes) = unzip(names_and_sizes.move_iter());

            for i in range(0, sizes.len())
            {
                if names.get(i) == field {
                    return offset_of(&sizes, i);
                }
            }
            fail!("Cannot find field {}\n", field);
        }
        _ => fail!("Looking up struct field offset in a non-struct.")
    }
}

pub fn size_of_def(session: &Session, typemap: &Typemap, node: &NodeId) -> u64 {
    let def = session.defmap.find(node).unwrap();

    match *def {
        StructDef(_, ref fields, _) => {
            let sizes = struct_field_sizes(session, typemap, fields).iter()
                .map(|&(_, y)| y).collect();
            packed_size(&sizes)
        },
        EnumDef(_, ref variants, _) => {
            let max_variant_size = variants.iter()
                .map(|v| size_of_def(session, typemap, v)).max().unwrap();
            if max_variant_size == 0 {
                enum_tag_size
            } else {
                packed_size(&vec!(enum_tag_size, max_variant_size))
            }
        },
        VariantDef(_, _, ref types) => {
            let sizes = types.iter()
                .map(|t| size_of_ty(session, typemap.types.get(&t.id.to_uint())))
                .collect();
            packed_size(&sizes)
        }
        _ => fail!("Size of {} not supported.\n", def),
    }
}

pub fn size_of_ty(session: &Session, ty: &Ty) -> u64 {
    match *ty {
        BoolTy => 1,
        IntTy(ref w) |
        UintTy(ref w) => match *w {
            AnyWidth |
            Width32 => 4,
            Width16 => 2,
            Width8 => 1,
        },
        GenericIntTy |
        PtrTy(..) |
        FuncTy(..) => 4,
        UnitTy => 0,
        ArrayTy(ref t, ref l) => size_of_ty(session, &t.val) * l.unwrap(),
        TupleTy(ref tys) =>
            packed_size(
                &tys.iter().map(|t| size_of_ty(session, &t.val)).collect()),
        BoundTy(..) |
        BottomTy => fail!("Type {} should not be appearing here.", ty),
        _ => fail!("Unimplemented: {}", ty),
    }
}

/// Return how much padding will be needed before an item of size `size`
/// in a structure in which `size_so_far` bytes of items are already present.
fn padding_of(size_so_far: u64, size: u64) -> u64 {
    let this_alignment = alignment(size);
    let offset = size_so_far % this_alignment;
    (this_alignment - offset) % this_alignment
}

/// Return how much extra space will be needed for an object of size `size`,
/// assuming that `size_so_far` of items are already present before it.
/// This includes padding necessary to align the new item.
fn increment_of(size_so_far: u64, size: u64) -> u64 {
    padding_of(size_so_far, size) + size
}

/// Return the total size, including padding for alignment, of a structure
/// whose members have the sizes given by the `sizes` vector.
pub fn packed_size(sizes: &Vec<u64>) -> u64 {
    let mut size_so_far = 0;
    for size in sizes.iter() {
        size_so_far += increment_of(size_so_far, *size);
    }

    size_so_far
}

pub fn offset_of(sizes: &Vec<u64>, item: uint) -> u64 {
    let mut size_so_far = 0;
    for size in sizes.iter().take(item) {
        size_so_far += increment_of(size_so_far, *size);
    }

    size_so_far + padding_of(size_so_far, *sizes.get(item))
}

#[cfg(test)]
mod tests {
    use mc::ast::visitor::Visitor;
    use mc::ast::NodeId;
    use mc::lexer::{Lexer, new_mb_lexer};
    use mc::parser::Parser;
    use typechecker::Typechecker;

    use std::io;
    use std::io::stdio;

    use mc::session::*;

    use super::*;

    // Helper function: we pass it a string describing a type and an expected
    // size, and it asserts that the size is what we expect.
    fn test_ty_size(t: &str, expected_size: u64) {
        let buffer = io::BufferedReader::new(io::MemReader::new(
            Vec::from_slice(t.as_bytes())
                ));
        let lexer = new_mb_lexer("<stdin>", buffer);

        let mut session = Session::new();
        let ast = Parser::parse_with(&mut session, lexer, |p| p.parse_type());

        let mut typeck = Typechecker::new(&session);
        typeck.visit_type(&ast);

        let typemap = typeck.get_typemap();

        let ty = typemap.types.get(&ast.id.to_uint());
        assert_eq!(size_of_ty(&session, ty), expected_size);
    }

    #[test]
    fn test_sizeof_basic() {
        test_ty_size("u32", 4);
        test_ty_size("u32[2]", 8);
        test_ty_size("bool", 1);
        test_ty_size("bool[4]", 4);
        test_ty_size("fn(u32, u32) -> u32", 4);
        test_ty_size("*(u32[5])", 4);
        test_ty_size("(u32, bool, u32)", 12);
        test_ty_size("(bool, bool, u32)", 8);
        test_ty_size("(u32, bool)", 5);
    }

    #[test]
    fn test_packed_size() {
        assert_eq!(packed_size(&vec!(1)), 1);
        assert_eq!(packed_size(&vec!(2)), 2);
        assert_eq!(packed_size(&vec!(3)), 3);
        assert_eq!(packed_size(&vec!(4)), 4);
        assert_eq!(packed_size(&vec!(4, 1)), 5);
        assert_eq!(packed_size(&vec!(1, 4)), 8);
        assert_eq!(packed_size(&vec!(4, 1, 4)), 12);
        assert_eq!(packed_size(&vec!(1, 4, 4)), 12);
        assert_eq!(packed_size(&vec!(4, 4, 1)), 9);
        assert_eq!(packed_size(&vec!(4, 1, 1)), 6);
        assert_eq!(packed_size(&vec!(1, 1, 4)), 8);
        assert_eq!(packed_size(&vec!(3, 1)), 4);
        assert_eq!(packed_size(&vec!(1, 3)), 7);
    }

    // Asserts that the `expected` vector describes the offsets for the items
    // whose sizes are in `sizes`.
    fn test_offset_helper(sizes: &Vec<u64>, expected: &Vec<u64>) {
        for i in range(0, sizes.len()) {
            assert_eq!(offset_of(sizes, i), *expected.get(i));
        }
    }

    #[test]
    fn test_offset_of() {
        test_offset_helper(&vec!(1, 2, 4, 1,  2),
                           &vec!(0, 2, 4, 8, 10));
        test_offset_helper(&vec!(2, 1, 4, 2,  1),
                           &vec!(0, 2, 4, 8, 10));
        test_offset_helper(&vec!(4, 4, 1,  4,  2,  1),
                           &vec!(0, 4, 8, 12, 16, 18));
        test_offset_helper(&vec!(1, 1, 1, 1, 1),
                           &vec!(0, 1, 2, 3, 4));
    }

    // Asserts that the structure described by `t` has size `expected_size`.
    fn test_sizeof_structure_helper(t: &str, expected_size: u64) {
        let mut session = Session::new();
        let module = session.parse_str(t);

        let mut typeck = Typechecker::new(&session);
        typeck.visit_module(&module);

        assert_eq!(size_of_def(&session, &typeck.get_typemap(), &NodeId(0)), expected_size);
    }

    #[test]
    fn test_sizeof_enum() {
        test_sizeof_structure_helper("enum a { X, Y }", 4);
        test_sizeof_structure_helper("enum a { X(u32), Y }", 8);
        test_sizeof_structure_helper("enum a { X(u32), Y(u32) }", 8);
        test_sizeof_structure_helper("enum a { X(u32), Y(u8) }", 8);
        test_sizeof_structure_helper("enum a { X(u32), Y(u8, u8) }", 8);
        test_sizeof_structure_helper("enum a { X(u32), Y(u8, u32) }", 12);
    }

    #[test]
    fn test_sizeof_struct() {
        test_sizeof_structure_helper("struct a { }", 0);
        test_sizeof_structure_helper("struct a { x: u32 }", 4);
        test_sizeof_structure_helper("struct a { x: u32, y: u32 }", 8);
        test_sizeof_structure_helper("struct a { x: (u32, u32), y: u32 }", 12);
        test_sizeof_structure_helper("struct a { x: u8 }", 1);
        test_sizeof_structure_helper("struct a { x: u8, y: u32 }", 8);
    }
}
