/* Holds extra information associated with each node of the AST.
 * This include spans (storing where in the source file the node came
 * from), resolver maps, definition maps, type maps, and string interning
 * maps.
 */

use collections::hashmap::HashMap;
use span::Span;
use ast::NodeId;

pub struct ParserContext {
    pub spanmap: HashMap<NodeId, Span>,
}

impl ParserContext {
    pub fn new() -> ParserContext {
        ParserContext {
            spanmap: HashMap::new()
        }
    }
}