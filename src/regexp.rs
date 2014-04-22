//! A basic regexp class.

extern crate collections;

use std::fmt;
use std::slice;
use std::iter::{FromIterator, range_inclusive};
use collections::hashmap::HashSet;

/// A character class
pub enum CharClass {
    Alpha,
    AlphaNum,
    Num,
    WhiteSpace,
}

/// Stores a parsed regexp.
pub enum Regexp {
    /// A regexp that matches just the empty string
    Empty,
    /// The Kleene Star of a regexp
    Star(~Regexp),
    /// The concatenation of two regexps
    Concat(~Regexp, ~Regexp),
    /// Alternation ("or") of two regexps
    Alternate(~Regexp, ~Regexp),
    /// Regexp matching a single-character string
    SingleCharacter(char),
    /// Regexp matching a certain character class
    CharacterClass(CharClass),
}

// A type for a single "matcher" for a single transition. A matcher either
// matches a single character, or corresponds to an epsilon transition
// (which does not consume any characters).
enum TransitionMatcher {
    Epsilon,
    TransChar(u8),
}

// A type for a single transition. Includes a matcher and a target state
// to transition to when the matcher matches.
struct Transition {
    matcher: TransitionMatcher,
    target_state: uint,
}

// A single state in an NFA, which contains a set of transitions to
// other states.
struct NFAState {
    transitions: ~[Transition],
}

impl fmt::Show for CharClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Alpha => write!(f.buf, "{}", "\\a"),
            AlphaNum => write!(f.buf, "{}", "\\@"),
            Num => write!(f.buf, "{}", "\\#"),
            WhiteSpace => write!(f.buf, "{}", "\\ "),
        }
    }
}


impl Clone for Transition {
    fn clone(&self) -> Transition {
        Transition {
            matcher: self.matcher,
            target_state: self.target_state,
        }
    }
}

impl Transition {
    /// Returns which state this transition takes us to on input c, or
    /// None if this transition doesn't apply to c (including if it's
    /// an epsilon transition).
    fn state_with(&self, c: u8) -> Option<uint> {
        match self.matcher {
            TransChar(x) if x==c => Some(self.target_state),
            _ => None,
        }
    }
}

impl NFAState {
    /// Return all states that can be reached from this one when the character
    /// `c` is input, without following epsilon transitions.
    fn transition(&self, c: u8) -> HashSet<uint> {
        let mut result = HashSet::<uint>::new();
        for trans in self.transitions.iter() {
            match trans.state_with(c) {
                Some(s) => {result.insert(s);},
                _ => {},
            }
        }

        result
    }

    /// Returns all states reachable from this one with epsilon transitions.
    fn epsilons(&self) -> HashSet<uint> {
        let mut result = HashSet::<uint>::new();
        for trans in self.transitions.iter() {
            match trans.matcher {
                Epsilon => {result.insert(trans.target_state);},
                _ => {},
            }
        }

        result
    }

    /// Add a new transition from this state to `target`, with the given
    /// matcher.
    fn add_transition(&mut self, matcher: TransitionMatcher, target: uint) {
        self.transitions.push(
            Transition {
                matcher: matcher,
                target_state: target,
            }
        )
    }
}

impl Clone for NFAState {
    fn clone(&self) -> NFAState {
        NFAState {
            transitions: self.transitions.clone()
        }
    }
}

/// An NFA, compiled from a regexp. These have a single start state and
/// a single accept state; multiple accept states can be simulated with
/// epsilon transitions.
pub struct NFA {
    states: ~[NFAState],
    start_state: uint,
    accept_state: uint,
}

impl NFA {
    /// An NFA that matches only the empty string.
    fn empty_str() -> NFA {
        NFA {
            states: ~[
                NFAState {
                    transitions: ~[],
                }
                ],
            start_state: 0u,
            accept_state: 0u,
        }
    }

    /// An NFA matching only the single-character string whose one character
    /// is `c`.
    fn single_char(c: u8) -> NFA {
        NFA {
            states: ~[
                NFAState {
                    transitions: ~[
                        Transition{
                            matcher: TransChar(c),
                            target_state: 1u,
                        },
                        ],
                },
                NFAState {
                    transitions: ~[],
                }
                ],
            start_state: 0u,
            accept_state: 1u,
        }
    }

    /// An NFA matching one character out of a certain class.
    fn char_class(class: CharClass) -> NFA {
        let mut result = NFA {
            states: ~[
                NFAState {
                    transitions: ~[],
                },
                NFAState {
                    transitions: ~[],
                }
                ],
            start_state: 0u,
            accept_state: 1u,
        };

        match class {
            Alpha => {
                for c in
                    range_inclusive('a' as u8, 'z' as u8).chain(
                        range_inclusive('A' as u8, 'Z' as u8)) {
                    result.states[result.start_state].add_transition(
                        TransChar(c),
                        result.accept_state
                    );
                }
            },
            AlphaNum => {
                for c in 
                    range_inclusive('a' as u8, 'z' as u8).chain(
                        range_inclusive('A' as u8, 'Z' as u8).chain(
                            range_inclusive('0' as u8, '9' as u8))) {
                        result.states[result.start_state].add_transition(
                            TransChar(c),
                            result.accept_state
                    );
                }
            },
            Num => {
                for c in 
                    range_inclusive('0' as u8, '9' as u8) {
                        result.states[result.start_state].add_transition(
                            TransChar(c),
                            result.accept_state
                    );
                }
            },
            WhiteSpace => {
                for c in [' ' as u8, '\t' as u8, '\n' as u8].iter() {
                    result.states[result.start_state].add_transition(
                        TransChar(*c),
                        result.accept_state
                    );
                }
            }
        }

        result
    }

    /// Merge two NFAs into one. This operation preserves the state numbers
    /// of the first NFA, but modifies the state numbers of `n2` (renumbering
    /// them so that they start after all the states of `self`).
    /// The second NFA will initially be "dead" in the merged NFA: none
    /// of the states will be reachable. But we can later modify the states
    /// of the merged NFA to use the states from `n2`.
    fn merge(&mut self, mut n2: ~NFA) {
        let n1_states = self.states.len();

        // Iterate over all transitions in the second NFA, adding
        // n1_states to each one.
        n2.states = FromIterator::from_iter(
            n2.states.iter()
                .map(|state| NFAState {
                    transitions: FromIterator::from_iter(
                        state.transitions.iter()
                            .map(|transition| match *transition {
                                Transition {
                                    matcher: x,
                                    target_state: t
                                } => Transition { matcher: x,
                                                  target_state: t+n1_states }
                            }
                        )
                    )
                }
            )
        );
        //TODO(mrwright): there *has* to be some way to do this without
        // cloning...
        self.states = slice::append(self.states.clone(), n2.states);
    }

    /// Mutate `self` so that it becomes an NFA accepting strings that
    /// can be broken into two substrings `s=s1s2`, so that `self` matches
    /// `s1` and `n2` matches `s2`. (That is to say: this new NFA matches
    /// all strings formed by concatenating a string matched by `self`
    /// and a string matched by `n2`.) Also returns the new NFA.
    fn concatenate(mut self, n2: ~NFA) -> NFA {
        let offset = self.states.len();
        let n2_start = n2.start_state + offset;
        let n2_accept = n2.accept_state + offset;
        self.merge(n2);
        self.states[self.accept_state].add_transition(Epsilon,
                                                      n2_start);
        self.accept_state = n2_accept;

        self
    }

    /// Mutate `self` so that it matches any concatenation of any number
    /// of strings it would have accepted before the operation.
    /// This corresponds to the Kleene star operator.
    /// Returns the new NFA.
    fn star(mut self) -> NFA {
        self.states[self.accept_state].add_transition(Epsilon,
                                                      self.start_state);
        self.states[self.start_state].add_transition(Epsilon,
                                                     self.accept_state);
        self
    }

    /// Mutate `self` so that it matches all strings that it would previously
    /// have matched, as well as all strings matched by `n2`.
    /// Returns the new NFA.
    fn alternate(mut self, n2: ~NFA) -> NFA {
        let offset = self.states.len();
        let n2_start = n2.start_state + offset;
        let n2_accept = n2.accept_state + offset;
        self.merge(n2);
        let new_start = self.states.len();
        let new_accept = new_start + 1;
        self.states.push(NFAState {
            transitions: ~[
                Transition {
                    matcher: Epsilon,
                    target_state: self.start_state,
                },
                Transition {
                    matcher: Epsilon,
                    target_state: n2_start,
                }
                ]
        });
        self.states.push(NFAState {
            transitions: ~[],
        });
        self.states[self.accept_state].add_transition(Epsilon,
                                                      new_accept);
        self.states[n2_accept].add_transition(Epsilon,
                                              new_accept);
        self.start_state = new_start;
        self.accept_state = new_accept;

        self
    }

    /// Given a set of states, returns all states (including the original
    /// ones) that can be reached from them by epsilon transitions in this
    /// NFA.
    fn apply_epsilons(&self, states: HashSet<uint>) -> HashSet<uint> {
        let mut result = states.clone();
        // All states we're given must be added to the queue of states
        // to look at.
        let mut queue: ~[uint] = FromIterator::from_iter(
            states.iter().map(|&x|x));

        while queue.len() > 0 {
            let state = queue.pop().unwrap();
            // Fetch all states that can be reached in one step from this one.
            let new_states = self.states[state].epsilons();

            for &state in new_states.iter() {
                // Insert returns true if the value wasn't already there.
                // If that's the case, we add this new state to our queue
                // so that we'll follow all transitions out of it as well.
                if result.insert(state) {
                    queue.push(state);
                }
            }
        }

        result
    }

    /// Match the NFA against a string, returning the longest prefix
    /// of the string that matches, or None if there is no match.
    pub fn match_string(&self, s: &str) -> Option<~str> {
        let mut states = HashSet::<uint>::new();
        states.insert(self.start_state);
        states = self.apply_epsilons(states);
        let mut pos = 0;
        let mut result = None;

        // Needed to handle the empty string.
        if states.contains(&self.accept_state) {
            result = Some(~"");
        }

        while states.len() > 0 && pos < s.len() {
            let mut new_states = HashSet::<uint>::new();
            for &state in states.iter() {
                for &other_state in self.states[state]
                                        .transition(s[pos]).iter() {
                    new_states.insert(other_state);
                }
            }

            states = self.apply_epsilons(new_states);
            pos+=1;

            if states.contains(&self.accept_state) {
                result = Some(s.slice_to(pos).to_owned());
            }
        }

        result
    }
}

impl Regexp {
    /// Helper function: given a string of the form `(X)Y`, return
    /// the substrings X and Y, if the parentheses in X are balanced.
    fn match_parens(exp: &str) -> (~str, ~str) {
        assert!(exp[0] as char == '(');
        let mut depth = 0;
        let mut j = -1;
        for i in range(0, exp.len()) {
            if exp[i] as char == '(' { depth+=1; };
            if exp[i] as char == ')' { depth-=1; };
            if depth == 0 { j=i; break; }
        }
        if j == -1 { fail!("Unbalanced parens."); }
        (exp.slice(1, j).to_owned(), exp.slice_from(j+1).to_owned())
    }

    /// Helper function: given a string of the form `(X,Y,Z,...)`,
    /// return the vector `~[~"X",~"Y",...]`. `X`, `Y`, and so forth
    /// may contain commas as long as they are at least one level
    /// deep in parentheses.
    fn match_params(exp: &str) -> (~[~str]) {
        let mut depth = 0;
        let mut last = 0;
        let mut res: ~[~str] = ~[];
        for i in range(0, exp.len()) {
            if exp[i] as char == '(' { depth+=1; };
            if exp[i] as char == ')' { depth-=1; };
            if depth == 0 && exp[i] as char == ',' {
                res.push(exp.slice(last, i).to_owned());
                last = i+1
            }
        }
        res.push(exp.slice(last, exp.len()).to_owned());
        res
    }

    /// Helper function: given a regular expression and remaining string,
    /// construct the regexp that matches this regexp followed by the one
    /// given in the string.
    fn handle_rest(~self, rest: &str) -> ~Regexp {
        if rest.len() == 0 {
            self
        } else {
            ~Concat(self, Regexp::parse(rest))
        }
    }

    /// Parse a string into a regexp. The syntax is a little odd:
    /// `*(...)` to do a Kleene star;
    /// `|(a,b,c)` to OR the regexps `a`, `b`, and `c` together.
    pub fn parse(exp: &str) -> ~Regexp {
        if exp.len() == 0 { return ~Empty; }
        if exp.len() == 1 { return ~SingleCharacter(exp[0] as char); }
        
        if exp[0] as char == '(' {
            let (inner, rest) = Regexp::match_parens(exp);
            let inner_r = Regexp::parse(inner);
            inner_r.handle_rest(rest)
        } else if exp[0] as char == '\\' {
            let inner_r =
                ~match exp[1] as char {
                    'a' => CharacterClass(Alpha),
                    '@' => CharacterClass(AlphaNum),
                    '#' => CharacterClass(Num),
                    ' ' => CharacterClass(WhiteSpace),
                    x   => SingleCharacter(x),
                };
            inner_r.handle_rest(exp.slice_from(2))
        } else if exp[0] as char == '|' {
            let (inner, rest) = Regexp::match_parens(exp.slice_from(1));
            let strs = Regexp::match_params(inner);
            let inner_r = strs.iter().skip(1)
                .map(|x| Regexp::parse(*x))
                .fold(Regexp::parse(strs[0]), |x, y| ~Alternate(x, y));
            inner_r.handle_rest(rest)
        } else if exp[0] as char == '*' {
            let (inner, rest) = Regexp::match_parens(exp.slice_from(1));
            let r = ~Star(Regexp::parse(inner));
            r.handle_rest(rest)
        } else {
            let rest1 = exp.slice_from(1);
            let r2 = Regexp::parse(rest1);
            ~Concat(~SingleCharacter(exp[0] as char), r2)
        }
    }

    /// Compile this regular expression into an NFA, which can then be
    /// used for matching on strings.
    pub fn compile(&self) -> NFA {
        match *self {
            Empty => NFA::empty_str(),
            SingleCharacter(c) => NFA::single_char(c as u8),
            Star(ref r) => r.compile().star(),
            Concat(ref r1, ref r2) => r1.compile().concatenate(~r2.compile()),
            Alternate(ref r1, ref r2) => r1.compile().alternate(~r2.compile()),
            CharacterClass(class) => NFA::char_class(class),
        }
    }
}

// Pretty-printing: converts a regexp object back into a regexp.
// The string we get should always parse back to the same regexp
// object that we started with.
impl fmt::Show for Regexp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Empty => write!(f.buf, ""),
            SingleCharacter(x) => write!(f.buf, "{}", x),
            Star(ref x) => write!(f.buf, "*({})", x),
            Alternate(ref x, ref y) => write!(f.buf, "|({},{})", x, y),
            Concat(ref x, ref y) => write!(f.buf, "{}{}", x, y),
            CharacterClass(class) => write!(f.buf, "{}", class),
        }
    }
}

impl Eq for Regexp {
    //TODO: there has to be a better way to do this...

    /// Tests for structural equality, not semantic equality.
    /// That is, two regexps may match exactly the same set of strings,
    /// but still not be considered equal.
    fn eq(&self, other: &Regexp) -> bool {
        match *self {
            Empty => match *other { Empty => true, _ => false },
            SingleCharacter(x) => match *other {
                SingleCharacter(y) => x==y, _ => false
            },
            Star(ref x) => match *other { Star(ref y) => x==y, _ => false },
            Alternate(ref x1, ref x2) => match *other {
                Alternate(ref y1, ref y2) => x1==y1 && x2==y2, _ => false },
            Concat(ref x1, ref x2) => match *other {
                Concat(ref y1, ref y2) => x1==y1 && x2==y2, _ => false
            },
            CharacterClass(class1) => match *other {
                CharacterClass(class2) => class1 as uint ==class2 as uint,
                _ => false
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parsing() {
        let re = Regexp::parse("*(ab)|(c,de,e)");
        assert_eq!(re, ~Concat(~Star(~Concat(~SingleCharacter('a'),
                                             ~SingleCharacter('b'))),
                               ~Alternate(~Alternate(~SingleCharacter('c'),
                                                     ~Concat(
                                                         ~SingleCharacter('d'),
                                                         ~SingleCharacter('e')
                                                         )
                                                     ),
                                          ~SingleCharacter('e'))));
        assert_eq!(re.to_str(), ~"*(ab)|(|(c,de),e)");

        let re2 = Regexp::parse("a*(\\#)");
        assert_eq!(re2, ~Concat(~SingleCharacter('a'),
                                ~Star(~CharacterClass(Num))));
    }

    #[test]
    fn test_nfas() {
        let r1 = Regexp::parse("a").compile();
        assert_eq!(r1.match_string(""), None);
        assert_eq!(r1.match_string("a"), Some(~"a"));
        assert_eq!(r1.match_string("aa"), Some(~"a"));
        assert_eq!(r1.match_string("b"), None);
        assert_eq!(r1.match_string("ab"), Some(~"a"));

        let r2 = Regexp::parse("*(a)").compile();
        assert_eq!(r2.match_string(""), Some(~""));
        assert_eq!(r2.match_string("a"), Some(~"a"));
        assert_eq!(r2.match_string("aa"), Some(~"aa"));
        assert_eq!(r2.match_string("b"), Some(~""));
        assert_eq!(r2.match_string("ab"), Some(~"a"));

        let r3 = Regexp::parse("ab").compile();
        assert_eq!(r3.match_string(""), None);
        assert_eq!(r3.match_string("a"), None);
        assert_eq!(r3.match_string("aa"), None);
        assert_eq!(r3.match_string("b"), None);
        assert_eq!(r3.match_string("ab"), Some(~"ab"));
        assert_eq!(r3.match_string("abc"), Some(~"ab"));
        assert_eq!(r3.match_string("abb"), Some(~"ab"));

        let r4 = Regexp::parse("|(a,b)c|(d,e)").compile();
        assert_eq!(r4.match_string(""), None);
        assert_eq!(r4.match_string("a"), None);
        assert_eq!(r4.match_string("ac"), None);
        assert_eq!(r4.match_string("bc"), None);
        assert_eq!(r4.match_string("acd"), Some(~"acd"));
        assert_eq!(r4.match_string("bcd"), Some(~"bcd"));
        assert_eq!(r4.match_string("ace"), Some(~"ace"));
        assert_eq!(r4.match_string("bce"), Some(~"bce"));
        assert_eq!(r4.match_string("add"), None);

        let r5 = Regexp::parse("*(|(ab,cd))").compile();
        assert_eq!(r5.match_string(""), Some(~""));
        assert_eq!(r5.match_string("a"), Some(~""));
        assert_eq!(r5.match_string("ab"), Some(~"ab"));
        assert_eq!(r5.match_string("aba"), Some(~"ab"));
        assert_eq!(r5.match_string("abab"), Some(~"abab"));
        assert_eq!(r5.match_string("abac"), Some(~"ab"));
        assert_eq!(r5.match_string("abc"), Some(~"ab"));
        assert_eq!(r5.match_string("abcd"), Some(~"abcd"));
        assert_eq!(r5.match_string("abab"), Some(~"abab"));
        assert_eq!(r5.match_string("ababcdabcd"), Some(~"ababcdabcd"));
        assert_eq!(r5.match_string("ababcdabcda"), Some(~"ababcdabcd"));
        assert_eq!(r5.match_string("ababcdabcdeeab"), Some(~"ababcdabcd"));

        let r6 = Regexp::parse("|(*(ab),*(cd))").compile();
        assert_eq!(r6.match_string(""), Some(~""));
        assert_eq!(r6.match_string("a"), Some(~""));
        assert_eq!(r6.match_string("ab"), Some(~"ab"));
        assert_eq!(r6.match_string("aba"), Some(~"ab"));
        assert_eq!(r6.match_string("abab"), Some(~"abab"));
        assert_eq!(r6.match_string("abc"), Some(~"ab"));
        assert_eq!(r6.match_string("abcd"), Some(~"ab"));

        let r7 = Regexp::parse("\\(*(\\#)\\)").compile();
        assert_eq!(r7.match_string(""), None);
        assert_eq!(r7.match_string("("), None);
        assert_eq!(r7.match_string("()"), Some(~"()"));
        assert_eq!(r7.match_string("(a)"), None);
        assert_eq!(r7.match_string("(0)"), Some(~"(0)"));
        assert_eq!(r7.match_string("(0123)"), Some(~"(0123)"));
    }
}
