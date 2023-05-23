#[test]
fn no_stack_overflow() {
    let mut s = "(module (func \n".to_string();
    for _ in 0..10_000 {
        s.push_str("(i32.add\n");
    }
    assert!(wat::parse_str(&s).is_err());
}
