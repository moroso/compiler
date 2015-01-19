use session::Session;
use syntax::expand::Expansion;

pub use self::defmap::DefMap;
pub use self::pathmap::PathMap;
pub use self::resolver::Resolver;
//pub use self::typechecker::Typechecker;

mod defmap;
mod pathmap;
mod resolver;
//mod typechecker;

pub struct Analysis {
    pub defmap: DefMap,
    pub pathmap: PathMap,
    pub resolver: Resolver,
    expn: Expansion,
}

impl Analysis {
    pub fn run(session: &mut Session, expn: Expansion) -> Analysis {
        Analysis {
            defmap: DefMap::new(&expn),
            pathmap: PathMap::new(&expn),
            resolver: Resolver::new(session, &expn),
            expn: expn,
        }
    }
}
