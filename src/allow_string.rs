/* This is a hack to deal with the fact that Rust separated the "Show"
 * and "String" traits. We should work on removing it eventually,
 * though now that I've said that I've certainly condemned this
 * comment (and hack) to lasting forever.
 */

macro_rules! allow_string {
    ($st:ty) => {
        impl fmt::Display for $st {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                (self as &Debug).fmt(f)
            }
        }
    }
}
