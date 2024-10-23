/// Indicates that the calling scope is unlikely to be executed.
#[cold]
#[inline]
fn cold() {}

/// Indicates that the condition is likely `true`.
#[inline]
pub fn likely(condition: bool) -> bool {
    if !condition {
        cold()
    }
    condition
}
