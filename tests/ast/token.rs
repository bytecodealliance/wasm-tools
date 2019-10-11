use wart::ast::*;

#[test]
fn id() {
    assert_parses!("$x", Id::new("x"));
    assert_parses!("$y", Id::new("y"));
    assert_parses!("$_abc", Id::new("_abc"));
}

#[test]
fn index() {
    assert_parses!("$x", Index::Id(Id::new("x")));
    assert_parses!("0", Index::Num(0));
}
