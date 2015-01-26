use std::borrow::BorrowFrom;
use std::fmt::{self, Formatter};
use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Name(u32);

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub struct Interner {
    strings: RefCell<HashMap<String, Name>>,
}

impl BorrowFrom<Name> for u32 {
    fn borrow_from(name: &Name) -> &u32 {
        let Name(ref id) = *name;
        id
    }
}

impl Interner {
    pub fn new() -> Interner {
        Interner { strings: RefCell::new(HashMap::new()) }
    }

    pub fn name_to_str<'a>(&'a self, name: &Name) -> &'a str {
        // A BiMap would be nice here
        let strings = self.strings.borrow();
        for x in strings.iter() {
            if x.1 == name {
                unsafe {
                    use std::mem::copy_lifetime;
                    return copy_lifetime(self, x.0).as_slice();
                }
            }
        }

        panic!()
    }

    pub fn intern(&self, s: String) -> Name {
        //match self.strings.find_equiv(&s) {
        //    Some(name) => *name,
        //    None => {
        //        let name = Name(self.strings.len());
        //        self.strings.insert(s, name);
        //        name
        //    }
        //}
        let mut strings = self.strings.borrow_mut();
        let name = Name(strings.len() as u32);
        *strings.entry(s).get().unwrap_or_else(|&:vacant| vacant.insert(name))
    }
}

