use std::fmt::Debug;
use std::collections::BTreeSet;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct VertexIndex(pub usize);
impl VertexIndex {
    pub fn node_id(&self) -> usize {
        let VertexIndex(x) = *self;
        x
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct EdgeIndex(pub usize);
impl EdgeIndex {
    pub fn edge_id(&self) -> usize {
        let EdgeIndex(x) = *self;
        x
    }
}

pub struct Vertex<V> {
    pub data: V,
}

pub struct Edge<E> {
    pub data: E,
    src: VertexIndex,
    dest: VertexIndex,
}

impl<E> Edge<E> {
    pub fn target(&self) -> VertexIndex {
        self.dest
    }
}

pub struct Graph<V, E> {
    vertices: Vec<Vertex<V>>,
    edges_by_vertex: Vec<Vec<Edge<E>>>,
    next_edge_index: usize,
}

impl<V, E> Graph<V, E> {
    // Roughly follows a subset of the graph implementation
    // in rustc_data_structures.
    pub fn new() -> Graph<V, E> {
        Graph {
            vertices: vec!(),
            edges_by_vertex: vec!(),
            next_edge_index: 0,
        }
    }

    pub fn add_node(&mut self, node: V) -> VertexIndex {
        let next_index = self.vertices.len();
        self.vertices.push(Vertex { data: node });
        self.edges_by_vertex.push(vec!());

        VertexIndex(next_index)
    }

    pub fn each_node<'a, F>(&'a self, mut f: F)
        where F: FnMut(VertexIndex, &'a Vertex<V>) -> bool {
        for (i, v) in self.vertices.iter().enumerate() {
            if !f(VertexIndex(i), v) { return; }
        }
    }

    pub fn node_data(&self, id: VertexIndex) -> &V {
        &self.vertices[id.node_id()].data
    }

    pub fn all_nodes(&self) -> &[Vertex<V>] {
        &self.vertices
    }

    pub fn outgoing_edges(&self, vid: VertexIndex) -> ::std::slice::Iter<Edge<E>> {
        self.edges_by_vertex[vid.node_id()].iter()
    }

    pub fn add_edge(&mut self, source: VertexIndex, target: VertexIndex, data: E) -> EdgeIndex {
        assert!(source.node_id() < self.vertices.len());
        assert!(target.node_id() < self.vertices.len());

        let new_edge = Edge {
            data: data,
            src: source,
            dest: target,
        };

        self.edges_by_vertex[source.node_id()].push(new_edge);

        let ret = EdgeIndex(self.next_edge_index);
        self.next_edge_index += 1;

        ret
    }
}

pub trait GraphExt<V> {
    fn toposort(&self) -> Result<Vec<V>, V>;
}

impl<V: Copy+Eq+Ord+Debug, E: Debug> GraphExt<V> for Graph<V, E> {
    // Returns a list in which a node appears *after* anything it had
    // an edge to.
    fn toposort(&self) -> Result<Vec<V>, V> {
        let mut list = vec!();
        let mut visiting = BTreeSet::new();
        let mut visited = BTreeSet::new();

        // there's gotta be a better way to do this :(
        fn visit<V: Copy+Eq+Ord+Debug, E: Debug>(graph: &Graph<V, E>,
                                                 vid: VertexIndex,
                                                 visiting: &mut BTreeSet<V>,
                                                 visited: &mut BTreeSet<V>,
                                                 list: &mut Vec<V>)
                                                 -> Option<V> {
            let vertex = graph.node_data(vid);

            if visiting.contains(vertex) {
                return Some(*vertex);
            }

            let mut err = None;
            if !visited.contains(vertex) {
                visiting.insert(*vertex);
                for e in graph.outgoing_edges(vid) {
                    err = visit(graph, e.target(), visiting, visited, list);
                    if err.is_some() {
                        break;
                    }
                }
                visited.insert(*vertex);
                visiting.remove(vertex);
                list.push(*vertex);
            }

            err
        }

        let mut err = None;
        self.each_node(|vid, _| {
            let vertex = self.node_data(vid);
            if !visited.contains(vertex) {
                err = visit(self, vid, &mut visiting, &mut visited, &mut list);
            }
            err.is_none()
        });

        match err {
            None => {
                assert!(list.len() == visited.len());
                assert!(list.len() == self.all_nodes().len());
                Ok(list)
            }
            Some(x) => Err(x)
        }
   }
}
