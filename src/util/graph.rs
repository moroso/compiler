pub use rustc::middle::graph::{Graph, Edge, EdgeIndex};
pub use Vertex      = rustc::middle::graph::Node;
pub use VertexIndex = rustc::middle::graph::NodeIndex;

use collections::TreeSet;

pub trait GraphExt<V, E> {
    fn toposort(&self) -> Result<Vec<V>, V>;
}

impl<V: Copy+Eq+Ord, E> GraphExt<V, E> for Graph<V, E> {
    fn toposort(&self) -> Result<Vec<V>, V> {
        let mut list = vec!();
        let mut visiting = TreeSet::new();
        let mut visited = TreeSet::new();

        // there's gotta be a better way to do this :(
        fn visit<V: Copy+Eq+Ord, E>(graph: &Graph<V, E>,
                               vid: VertexIndex,
                               visiting: &mut TreeSet<V>,
                               visited: &mut TreeSet<V>,
                               list: &mut Vec<V>)
                               -> Option<V> {
            let vertex = graph.node_data(vid);

            if visiting.contains(vertex) {
                return Some(*vertex);
            }

            if !visited.contains(vertex) {
                visiting.insert(*vertex);
                graph.each_outgoing_edge(vid, |_, e| {
                    visit(graph, e.target(), visiting, visited, list);
                    true
                });
                visited.insert(*vertex);
                visiting.remove(vertex);
                list.push(*vertex);
            }

            None
        }

        let mut err = None;
        self.each_node(|vid, _| {
            let vertex = self.node_data(vid);
            if !visited.contains(vertex) {
                err = visit(self, vid, &mut visiting, &mut visited, &mut list);
                if err.is_some() {
                    return false
                }
            }
            true
        });

        assert!(list.len() == visited.len());
        assert!(list.len() == self.all_nodes().len());

        Ok(list)
   }
}
