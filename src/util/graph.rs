pub use rustc::middle::graph::{Graph, Edge, EdgeIndex};
pub use rustc::middle::graph::Node      as Vertex;
pub use rustc::middle::graph::NodeIndex as VertexIndex;

use collections::BTreeSet;

pub trait GraphExt<V, E> {
    fn toposort(&self) -> Result<Vec<V>, V>;
}

impl<V: Copy+Eq+Ord, E> GraphExt<V, E> for Graph<V, E> {
    fn toposort(&self) -> Result<Vec<V>, V> {
        let mut list = vec!();
        let mut visiting = BTreeSet::new();
        let mut visited = BTreeSet::new();

        // there's gotta be a better way to do this :(
        fn visit<V: Copy+Eq+Ord, E>(graph: &Graph<V, E>,
                               vid: VertexIndex,
                               visiting: &mut BTreeSet<V>,
                               visited: &mut BTreeSet<V>,
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
