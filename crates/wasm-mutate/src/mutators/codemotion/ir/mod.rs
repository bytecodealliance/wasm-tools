use std::{borrow::Borrow, cell::RefCell, collections::HashMap, io::Write, process::Command, slice::{ChunksExactMut, Iter}, time::{SystemTime, UNIX_EPOCH}};

use wasm_encoder::{BlockType, Function, Instruction};
use wasmparser::{Operator, Range};
use crate::{error::EitherType, mutators::OperatorAndByteOffset};
pub struct ASTBuilder;

#[derive(Debug)]
struct AST {
    // Index of the root node
    root: usize,
    // Nodes of the tree
    nodes: Vec<Node>
}

impl AST {

    /// Build a new empty AST
    pub fn new(root: usize, nodes: Vec<Node>) -> Self {
        AST{
            root,
            nodes
        }
    }


    /// Encodes the AST
    /// It traverses the AST starting from the last root
    pub fn write_ast<'a>(&self, newfunc: &mut Function, operators: &Vec<OperatorAndByteOffset>, input_wasm: &'a [u8]) -> crate::Result<()> {
        
        println!("AST {:?}", self);
        
        fn write_ast_rec<'a>(ast: &AST, current_node: &Node, newfunc: &mut Function, operators: &Vec<OperatorAndByteOffset>, input_wasm: &'a [u8]) {
            match current_node {
                Node::IfElse(condition, then, alternative) => {
                    // write condition first
                    write_ast_rec(ast, &ast.nodes[*condition], newfunc,operators, input_wasm);
                    newfunc.instruction(&Instruction::If(BlockType::Empty));
                    for ch in then {
                        write_ast_rec(ast, &ast.nodes[*ch], newfunc,operators, input_wasm);
                    }

                    if let Some(alternative) = alternative {
                        newfunc.instruction(&Instruction::Else);

                        for ch in alternative {
                            write_ast_rec(ast, &ast.nodes[*ch], newfunc,operators, input_wasm);
                        }
                    }
                    newfunc.instruction(&Instruction::End);

                },
                Node::Code { range } => {
                    let operator_range = (range.start, range.end);


                    //print operator
                    for operator in &operators[range.start..range.end]{
                        println!("{:?}", operator.0)
                    }

                    let bytes_range = (&operators[operator_range.0].1, &operators[operator_range.1].1);
                    let piece_of_code = &input_wasm[*bytes_range.0..*bytes_range.1];
                    
                    newfunc.raw(piece_of_code.to_vec());
                },
                Node::Loop(body) => {
                    newfunc.instruction(&Instruction::Loop(BlockType::Empty));
                    for ch in body {
                        write_ast_rec(ast, &ast.nodes[*ch], newfunc,operators, input_wasm);
                    }
                    newfunc.instruction(&Instruction::End);

                },
                Node::Block(body) => {
                    newfunc.instruction(&Instruction::Block(BlockType::Empty));
                    for ch in body {
                        write_ast_rec(ast, &ast.nodes[*ch], newfunc,operators, input_wasm);
                    }
                    newfunc.instruction(&Instruction::End);
                },
                Node::Root(body) => {

                    for ch in body {
                        write_ast_rec(ast, &ast.nodes[*ch], newfunc,operators, input_wasm);
                    }
                    // Closing end
                    newfunc.instruction(&Instruction::End);
                },
            }
        }

        write_ast_rec(self, &self.nodes[self.root], newfunc, operators, input_wasm);

        Ok(())
    }
}

#[derive(Debug, Clone)]
enum Node {
    IfElse(usize, Vec<usize>, Option<Vec<usize>>),
    Code{
        range: Range
    },
    Loop(Vec<usize>),
    Block(Vec<usize>),
    Root(Vec<usize>)
}


impl ASTBuilder {

    fn dotify_ast<'a>(&self, root: usize, nodes: &[Node], operators: &'a [OperatorAndByteOffset]) -> String {
        let mut r = String::new();
        r.push_str("digraph AST {\n   node [shape=box]; \n");

        let mut worklist = Vec::new();
        worklist.push(root);

        let bbcontent = |r: Range| {
            operators[r.start..r.end].iter().map(|(o, _)|format!("{:?}", o)).collect::<Vec<String>>().join("\n")
        };
        let mut uniqueid = nodes.len();
        while let Some(id) = worklist.pop(){
            let node = &nodes[id];
            match node {
                Node::IfElse(condition, then, alternative) => {
                    // Add edges to children then and push into worklist
                    r.push_str(&format!(
                        "{} -> {}[label=\"condition\", style=dashed, color=red];\n",
                    id, condition),);
                    worklist.push(*condition);
                    r.push_str(&format!("\t{} [label=\"if\"]\n", id));
                    // Create then branch
                    uniqueid += 1;
                    r.push_str(&format!(
                        "{} -> {}[label=\"then\"];\n",
                        id, uniqueid),);
                    r.push_str(&format!("\t{} [label=\"\"]\n", uniqueid));
                    for ch in then {
                        r.push_str(&format!(
                        "{} -> {};\n",
                            uniqueid, ch),);
                        worklist.push(*ch)
                    }

                    if let Some(alternative) = alternative {
                        uniqueid += 1;
                        r.push_str(&format!(
                            "{} -> {}[label=\"else\"];\n",
                            id, uniqueid),);
                        r.push_str(&format!("\t{} [label=\"\"]\n", uniqueid));
                        for ch in alternative {
                            r.push_str(&format!(
                            "{} -> {};\n",
                                uniqueid, ch),);
                            worklist.push(*ch)
                        }
                    }
                },
                Node::Code{range} => {
                    r.push_str(&format!("\t{} [label=\"{}\"]\n", id, bbcontent(*range)));
                },
                Node::Loop(children) => {
                    r.push_str(&format!("\t{} [label=\"loop\"]\n", id));

                    for ch in children {
                        r.push_str(&format!(
                        "{} -> {};\n",
                            id, ch),);
                        worklist.push(*ch)
                    }
                },
                Node::Block(body) | Node::Root(body) => {
                    r.push_str(&format!("\t{} [label=\"block\"]\n", id));

                    for ch in body {
                        r.push_str(&format!(
                        "{} -> {};\n",
                            id, ch),);
                        worklist.push(*ch)
                    }
                },
            }
        }

        r.push_str("\n}");
        println!("root {}", r);
        r
    }

    fn build_ast<'a>(&mut self, position: usize, operators: &'a [OperatorAndByteOffset]) -> crate::Result<AST>{
        
        let (root, nodes) = self.parse(position, operators)?;
        println!("root {:?}", root);

        let dot = self.dotify_ast(root, &nodes, operators);
        // debug purposes, remove
        let name = format!("{}.dot", std::thread::current().name().unwrap());
        let bin =  std::env::var("DOT").unwrap();
        let mut f = std::fs::File::create(name.clone()).unwrap();
        f.write(dot.as_bytes()).unwrap();

        Command::new(&bin)
            .arg("-Tpng")
            .arg(name.clone())
            .arg("-o")
            .arg(format!("{}.png", name))
            .spawn()
            .expect("Process cannot be launched");
            
        Ok(AST::new(root, nodes))
    }


    fn parse<'a>(&mut self, position: usize, operators: &'a [OperatorAndByteOffset]) -> crate::Result<(usize, Vec<Node>)>{

        #[derive(Debug)]
        enum State {
            If,
            Else,
            Loop,
            Block,
            Root
        }

        let mut nodes = Vec::new();
        let mut nodes = RefCell::new(nodes);
       
        let mut frame_stack = vec![
            State::Root
        ];
        let mut state_stack = RefCell::new(Vec::new());
        let mut current_parsing = RefCell::new(Vec::new());
        
        let push_state = || {
            state_stack.borrow_mut().push(current_parsing.clone());
            *current_parsing.borrow_mut() = vec![]
        };
        let pop_state = ||  {
            state_stack.borrow_mut().pop().expect("Missing state").borrow().clone()
        };

        let mut current_code = Range::new(0,0);

        for (idx, (operator, _)) in operators.iter().enumerate() {

            println!("{} operator {:?}",idx,  operator);
            match operator {
                Operator::If { .. } => {
                    // push current code 
                    if current_code.end != current_code.start {
                        let id= nodes.borrow().len();
                        nodes.borrow_mut().push(Node::Code{
                            range: current_code.clone()
                        });
                        current_parsing.borrow_mut().push(id);
                    }
                    current_code.start = idx + 1;
                    current_code.end = idx + 1;
                    push_state();
                    frame_stack.push(State::If);
                }
                Operator::Else { .. } => {
                    println!("range {:?}", current_code);
                    if current_code.end != current_code.start {
                        let id= nodes.borrow().len();
                        nodes.borrow_mut().push(Node::Code{
                            range: current_code.clone()
                        });
                        current_parsing.borrow_mut().push(id);
                    }
                    current_code.start = idx + 1;
                    current_code.end = idx + 1;
                    push_state();
                    frame_stack.push(State::Else);
                }
                Operator::Block {..} => {
                    if current_code.end != current_code.start {
                        let id= nodes.borrow().len();
                        nodes.borrow_mut().push(Node::Code{
                            range: current_code.clone()
                        });
                        current_parsing.borrow_mut().push(id);
                    }
                    current_code.start = idx + 1;
                    current_code.end = idx + 1;
                    push_state();
                    frame_stack.push(State::Block);
                }
                Operator::Loop {.. } => {
                    if current_code.end != current_code.start {
                        let id= nodes.borrow().len();
                        nodes.borrow_mut().push(Node::Code{
                            range: current_code.clone()
                        });
                        current_parsing.borrow_mut().push(id);
                    }
                    current_code.start = idx + 1;
                    current_code.end = idx + 1;
                    frame_stack.push(State::Loop);
                    push_state();
                }
                Operator::End => {
                    if current_code.end != current_code.start {
                        let id= nodes.borrow().len();
                        nodes.borrow_mut().push(Node::Code{
                            range: current_code.clone()
                        });
                        current_parsing.borrow_mut().push(id);
                    }
                    current_code.start = idx + 1;
                    current_code.end = idx + 1;
                    println!("Pop state {:?} {:?}", frame_stack, state_stack.borrow());
                    let last_frame = frame_stack.pop().expect("Missing frame");
                    match last_frame {
                        State::If => {                            
                            let then_branch = current_parsing.borrow().clone();
                            *current_parsing.borrow_mut() = pop_state();

                            println!("{:?}", current_parsing.borrow());
                            let condition = current_parsing.borrow_mut().pop().expect("Missing condition");
                            let id = nodes.borrow().len();
                            nodes.borrow_mut().push(Node::IfElse(
                                condition, then_branch, None
                            ));

                            current_parsing.borrow_mut().push(id);
                        },
                        State::Else => {
                            let last_frame = frame_stack.pop().expect("Missing frame");
                            // Validate parent
                            match last_frame {
                                State::If => {  },
                                _ => unreachable!("Invalid parent frame")
                            }
                            let else_branch = current_parsing.borrow().clone();
                            let then_branch = pop_state();
                            *current_parsing.borrow_mut() = pop_state();

                            let condition = current_parsing.borrow_mut().pop().expect("Missing condition");
                            let id = nodes.borrow().len();
                            nodes.borrow_mut().push(Node::IfElse(
                                condition, then_branch, Some(else_branch)
                            ));

                            current_parsing.borrow_mut().push(id);
                        },
                        State::Loop => {
                            
                            let children = current_parsing.borrow().clone();
                            *current_parsing.borrow_mut() = pop_state();
                            let id = nodes.borrow().len();
                            nodes.borrow_mut().push(Node::Loop(children));
                            current_parsing.borrow_mut().push(id);
                        },
                        State::Block => {
                            let children = current_parsing.borrow().clone();
                            *current_parsing.borrow_mut() = pop_state();
                            let id = nodes.borrow().len();
                            nodes.borrow_mut().push(Node::Block(children));
                            current_parsing.borrow_mut().push(id);
                        },
                        State::Root => {
                            // break
                            break
                        },
                    }
                }
                _ => {
                    current_code.end += 1;
                }
            }
        }
        
        let roots = current_parsing.borrow().clone();
        // Create a block with the current parsing state
        let root = Node::Root(roots.clone());
        let root_id = nodes.borrow().len();
        nodes.borrow_mut().push(root);
        let nodes = nodes.borrow().clone();
        Ok((root_id, nodes))
    }
}


#[cfg(test)]
mod tests {
    use crate::{WasmMutate, mutators::{Mutator, OperatorAndByteOffset, codemotion::ir::ASTBuilder, peephole::PeepholeMutator}};
    use rand::{rngs::SmallRng, SeedableRng};
    use wasm_encoder::{CodeSection, Function, FunctionSection, Instruction, Module, TypeSection, ValType};
    use wasmparser::Parser;

    use super::AST;

    fn assert_correct_ast(wasm: &str, expected_ast: &AST) -> bool {
        
        let original = &wat::parse_str(wasm,
        )
        .unwrap();
        let mut parser = Parser::new(0);
        let mut consumed = 0;
        loop {
            let (payload, size) = match parser.parse(&original[consumed..], true).unwrap() {
                wasmparser::Chunk::NeedMoreData(_) => {
                    panic!("This should not happen")
                }
                wasmparser::Chunk::Parsed { consumed, payload } => (payload, consumed),
            };

            consumed += size;

            match payload {
                wasmparser::Payload::CodeSectionEntry(reader) => {
                    let operators = reader
                        .get_operators_reader()
                        .unwrap()
                        .into_iter_with_offsets()
                        .collect::<wasmparser::Result<Vec<OperatorAndByteOffset>>>()
                        .unwrap();

                    let ast = ASTBuilder.build_ast(0, &operators).unwrap();

                    todo!();
                    //assert!(ast.is_ok());
                    //let ast = ast.unwrap();

                    // If the check fails the dotify string will be printed in console
                    //println!("AST\n{}", ast.dotify(&operators, true));
                    //println!("successors\n{:?}", ast.successors);
                    //println!("pred\n{:?}", ast.parents);

                    // assert_eq!(ast.successors, expected_ast.successors);

                    return true
                }
                wasmparser::Payload::End => {
                    break;
                }
                _ => {
                    // Do nothing
                }
            }
        }
        false
    }

    #[test]
    fn test_ast_builder_block1() {
        assert_correct_ast(r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                block
                    i32.const 100
                    drop
                end
                i32.const 101
            )
        )
        "#, &AST{
            root: 0,
            nodes: vec![]
        });
    }

    #[test]
    fn test_ast_builder_block2() {
        assert_correct_ast(r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                block
                    i32.const 100
                    drop
                    block
                        i32.const 100
                        drop
                    end
                end
            )
        )
        "#, &AST{
            root: 0,
            nodes: vec![]
        });
    }

    #[test]
    fn test_ast_builder_block3() {
        assert_correct_ast(r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                block
                    i32.const 100
                    drop
                    block
                        i32.const 101
                        br_if 2
                    end
                end
            )
        )
        "#, &AST{
            root: 0,
            nodes: vec![]
        });
    }

    #[test]
    fn test_ast_builder_block_and_loop() {
        assert_correct_ast(r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                block
                    i32.const 100
                    drop
                    loop
                        i32.const 100
                        br 1
                    end
                end
            )
        )
        "#, &AST{
            root: 0,
            nodes: vec![]
        });
    }

    #[test]
    fn test_ast_builder_if() {
        assert_correct_ast(r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                local.get 0
                local.get 0
                i32.add
                i32.load
                if
                    i32.const 100
                    i32.const 87
                end
                i32.const -14
            )
        )
        "#, &AST{
            root: 0,
            nodes: vec![]
        });
    }
    #[test]
    fn test_ast_builder_if_else_nested() {
        assert_correct_ast(r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)

                local.get 0
                local.get 0
                i32.add
                i32.load
                if
                    i32.const 100
                else
                    i32.const 99
                    if
                        i32.const 98
                    else
                        i32.const 97
                    end
                    i32.const -96
                end
                i32.const -95
            )
        )
        "#, &AST{
            root: 0,
            nodes: vec![]
        });
    }


    #[test]
    fn test_ast_builder_if_else_nested2() {
        assert_correct_ast(r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)

                local.get 0
                local.get 0
                i32.add
                i32.load
                if
                    i32.const 100
                else
                    i32.const 87
                    if 
                        i32.const 88
                    else
                        i32.const 89
                    end
                    if
                        i32.const 88
                    else
                        i32.const 89
                    end
                end
                
                i32.const -14
            )
        )
        "#, &AST{
            root: 0,
            nodes: vec![]
        });
    }

    #[test]
    fn test_ast_builder_if_else_appended() {
        assert_correct_ast(r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)

                local.get 0
                local.get 0
                i32.add
                i32.load
                if
                    i32.const 100
                else
                    i32.const 87
                    i32.const -10
                end
                i32.const 100
                i32.add
                if
                    i32.const 88
                else
                    i32.const 89
                end
                i32.const -14
            )
        )
        "#, &AST{
            root: 0,
            nodes: vec![]
        });
    }
    #[test]
    fn test_ast_builder_break2() {

        assert_correct_ast(r#"
        (module
                (memory 1)
                (func (export "exported_func") (param i32) (result i32)
                    loop
                        i32.const 10
                        local.get 0
                        i32.add
                        local.tee 0
                        br_if 1
                        i32.const 100
                        loop
                            i32.const 10
                            local.get 0
                            i32.add
                            local.tee 0
                            br 2
                        end
                    end
                    local.get 0
                )
            )
        "#, &AST{
            root: 0,
            nodes: vec![]
        });
    }

    #[test]
    fn test_ast_builder_break3() {

        assert_correct_ast(r#"
        (module
                (memory 1)
                (func (export "exported_func") (param i32) (result i32)
                    loop
                        i32.const 10
                        local.get 0
                        i32.add
                        local.tee 0
                        br_if 1
                        i32.const 100
                        loop
                            i32.const 10
                            local.get 0
                            i32.add
                            local.tee 0
                            br_if 1
                        end
                    end
                    local.get 0
                )
            )
        "#, &AST{
            root: 0,
            nodes: vec![]
        });
    }

    #[test]
    fn test_ast_builder_break4() {

        assert_correct_ast(r#"
        (module
                (memory 1)
                (func (export "exported_func") (param i32) (result i32)
                    loop
                        i32.const 10
                        local.get 0
                        i32.add
                        local.tee 0
                        br_if 1
                        i32.const 100
                        loop
                            i32.const 10
                            local.get 0
                            i32.add
                            local.tee 0
                            br_table 1 2
                        end
                    end
                    local.get 0
                )
            )
        "#, &AST{
            root: 0,
            nodes: vec![]
        });
    }
    #[test]
    fn test_ast_builder_break1() {

        assert_correct_ast(r#"
        (module
                (memory 1)
                (func (export "exported_func") (param i32) (result i32)
                    loop
                        i32.const 10
                        local.get 0
                        i32.add
                        local.tee 0
                        br_if 1
                        i32.const 100
                        br_if 1
                    end
                    local.get 0
                )
            )
        "#, &AST{
            root: 0,
            nodes: vec![]
        });
    }
    #[test]
    fn test_ast_builder_if_else1() {

        assert_correct_ast(r#"
        (module
                (memory 1)
                (func (export "exported_func") (param i32) (result i32)

                    local.get 0
                    local.get 0
                    i32.add
                    i32.load
                    if
                        i32.const 100
                    else
                        i32.const 87
                    end
                    i32.const 100
                    end
                )
            )
        "#, &AST{
            root: 0,
            nodes: vec![]
        });
    }
    #[test]
    fn test_ast_builder_loop() {
        assert_correct_ast(r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)

                loop
                i32.const 1
                local.get 0
                i32.add
                local.set 0
            end
            )
        )
        "#, &AST{
            root: 0,
            nodes: vec![]
        });
    }

    #[test]
    fn test_ast_builder_if_else_with_return() {
        
        assert_correct_ast(r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)

                local.get 0
                local.get 0
                i32.add
                i32.load
                if
                    i32.const 100
                else
                    return
                    i32.const 130
                end
            )
        )
        "#, &AST{
            root: 0,
            nodes: vec![]
        });
    }
    #[test]
    fn test_ast_builder_and_encode() {
        let original = &wat::parse_str(
            r#"
        (module
            (func (param i32) (result i32)

                local.get 0
                local.get 0
                i32.add
                i32.load
                if
                    i32.const 54
                    if
                        i32.const 55
                        i32.const -55
                    end
                    if
                        i32.const -10
                        call 0
                    else
                        return
                        i32.const 120
                    end
                    i32.const -10
                else
                    i32.const 87
                end
                i32.const 56
                i32.add
                loop
                    i32.const 1
                    local.get 0
                    i32.add
                    local.set 0
                    loop
                        local.get 0
                        local.set 0
                        br 1
                        
                    end
                    loop
                        i32.const 1
                        local.get 0
                        i32.add
                        local.set 0
                        loop
                            local.get 0
                            local.set 0
                            br 1
                        end
                        br 1
                    end

                    br 1
                end
            
            )
        )
        "#,
        )
        .unwrap();

        let mut parser = Parser::new(0);
        let mut consumed = 0;
        loop {
            let (payload, size) = match parser.parse(&original[consumed..], true).unwrap() {
                wasmparser::Chunk::NeedMoreData(_) => {
                    panic!("This should not happen")
                }
                wasmparser::Chunk::Parsed { consumed, payload } => (payload, consumed),
            };

            consumed += size;

            match payload {
                wasmparser::Payload::CodeSectionEntry(reader) => {
                    let operators = reader
                        .get_operators_reader()
                        .unwrap()
                        .into_iter_with_offsets()
                        .collect::<wasmparser::Result<Vec<OperatorAndByteOffset>>>()
                        .unwrap();

                        
                    let ast = ASTBuilder.build_ast(0, &operators);

                    assert!(ast.is_ok());

                    let mut module = Module::new();
                    let mut code = CodeSection::new();
                    let mut newfunc = Function::new(vec![]);

                    let ast = ast.unwrap();
                    ast.write_ast(&mut newfunc, &operators, &original);                    
                    code.function(&newfunc);

                    let mut types = TypeSection::new();
                    let params = vec![ValType::I32];
                    let results = vec![ValType::I32];
                    types.function(params, results);
                    module.section(&types);
                    
                    // Encode the function section.
                    let mut functions = FunctionSection::new();
                    let type_index = 0;
                    functions.function(type_index);
                    module.section(&functions);
                    
                    module.section(&code);

                    let encoded = module.finish();
                    let text = wasmprinter::print_bytes(encoded.clone()).unwrap();
                    println!("{}", text);

                    assert_eq!(&encoded, original); 

                }
                wasmparser::Payload::End => {
                    break;
                }
                _ => {
                    // Do nothing
                }
            }
        }
    }
}
