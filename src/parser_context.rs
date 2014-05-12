/* Holds extra information associated with each node of the AST.
 * This include spans (storing where in the source file the node came
 * from), resolver maps, definition maps, type maps, and string interning
 * maps.
 */

use collections::TreeMap;
use span::Span;
use ast::NodeId;
use ast::defmap::DefMap;
use resolve::Resolver;

pub struct ParserContext {
    pub spanmap: TreeMap<NodeId, Span>,
    pub defmap: DefMap,
    pub resolver: Resolver,
}

impl ParserContext {
    pub fn new() -> ParserContext {
        ParserContext {
            spanmap: TreeMap::new(),
            defmap: DefMap::new(),
            resolver: Resolver::new(),
        }
    }
}