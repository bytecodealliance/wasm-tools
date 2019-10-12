use wart::ast::Instruction::*;
use wart::ast::*;

macro_rules! expr {
    ($($t:tt)*) => (Expression { instrs: vec![$($t)*] })
}

fn bt() -> BlockType<'static> {
    BlockType {
        label: None,
        result: None,
    }
}

#[test]
fn exprs() {
    assert_parses!("i32.add", expr![I32Add]);
    assert_parses!("i32.add i32.sub", expr![I32Add, I32Sub]);
    assert_parses!("i32.add (i32.sub)", expr![I32Add, I32Sub]);
    assert_parses!("(i32.add (i32.sub))", expr![I32Sub, I32Add]);
    assert_parses!("block end", expr![Block(bt()), End(None)]);
    assert_parses!("(block)", expr![Block(bt()), End(None)]);
    assert_parses!("(loop)", expr![Loop(bt()), End(None)]);
    assert_parses!(
        "(block (loop))",
        expr![Block(bt()), Loop(bt()), End(None), End(None)]
    );
    assert_parses!(
        "(if i32.sub (then i32.add))",
        expr![I32Sub, If(bt()), I32Add, End(None)]
    );
    assert_parses!(
        "(if (i32.sub) (then i32.add))",
        expr![I32Sub, If(bt()), I32Add, End(None)]
    );
    assert_parses!(
        "(if (i32.sub) (then i32.add) (else i32.mul))",
        expr![I32Sub, If(bt()), I32Add, Else(None), I32Mul, End(None)]
    );
}
