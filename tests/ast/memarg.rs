
use wart::ast::*;

#[test]
fn memargs() {
    assert_parses!("", MemArg { align: None, offset: 0 });
    assert_parses!("offset=1", MemArg { align: None, offset: 1 });
    assert_parses!("offset=0x1", MemArg { align: None, offset: 1 });
    assert_parses!("align=1", MemArg { align: Some(1), offset: 0 });
    assert_parses!("align=0x1", MemArg { align: Some(1), offset: 0 });
    assert_parses!("offset=1 align=0x1", MemArg { align: Some(1), offset: 1 });
}
