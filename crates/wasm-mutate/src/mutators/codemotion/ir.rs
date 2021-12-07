//! Parsing and econding for code motion mutators.
use crate::{
    module::map_block_type,
    mutators::{codemotion::ir::parse_context::ParseContext, OperatorAndByteOffset},
};
use wasm_encoder::{Function, Instruction};
use wasmparser::{Operator, Range, TypeOrFuncType};

use self::parse_context::{Ast, Node, State};

/// Encodes an AST back to a Wasm function
pub struct AstBuilder;
pub(crate) mod parse_context;

/// Encodes the Wasm Ast
pub trait AstWriter {
    /// Encodes a loop node.
    ///
    /// Redefine this if your implementation mutates loops. For example, if your
    /// implementation unrolls the first iteration of a particular loop, override
    /// this method. For the other loops in the Wasm that your implementation is not
    /// mutating, you can call `write_loop_default`.
    fn write_loop<'a>(
        &self,
        ast: &Ast,
        nodeidx: usize,
        body: &[usize],
        newfunc: &mut Function,
        operators: &Vec<OperatorAndByteOffset>,
        input_wasm: &'a [u8],
        ty: &TypeOrFuncType,
    ) -> crate::Result<()> {
        self.write_loop_default(ast, nodeidx, body, newfunc, operators, input_wasm, ty)
    }

    /// Default encoding for a loop node
    ///
    /// This function is called by the defaut implementation
    /// of the `write_loop` method
    fn write_loop_default<'a>(
        &self,
        ast: &Ast,
        _nodeidx: usize,
        body: &[usize],
        newfunc: &mut Function,
        operators: &Vec<OperatorAndByteOffset>,
        input_wasm: &'a [u8],
        ty: &TypeOrFuncType,
    ) -> crate::Result<()> {
        newfunc.instruction(&Instruction::Loop(map_block_type(*ty)?));
        for ch in body {
            self.write(ast, *ch, newfunc, operators, input_wasm)?;
        }
        newfunc.instruction(&Instruction::End);
        Ok(())
    }

    /// Default encoding for a block node
    ///
    /// This function is called by the defaut implementation
    /// of the `write_block` method
    fn write_block_default<'a>(
        &self,
        ast: &Ast,
        _nodeidx: usize,
        body: &[usize],
        newfunc: &mut Function,
        operators: &Vec<OperatorAndByteOffset>,
        input_wasm: &'a [u8],
        ty: &TypeOrFuncType,
    ) -> crate::Result<()> {
        newfunc.instruction(&Instruction::Block(map_block_type(*ty)?));
        for ch in body {
            self.write(ast, *ch, newfunc, operators, input_wasm)?;
        }
        newfunc.instruction(&Instruction::End);
        Ok(())
    }

    /// Encodes a block node.
    ///
    /// Redefine this if your implementation mutates blocks. For example, if your
    /// implementation modifies the internal structure of a particular block, by
    /// for example, inserting some nop operations between its children, override
    /// this method. For the other blocks in the Wasm that your implementation is not
    /// mutating, you can call `write_block_default`.
    ///
    fn write_block<'a>(
        &self,
        ast: &Ast,
        nodeidx: usize,
        body: &[usize],
        newfunc: &mut Function,
        operators: &Vec<OperatorAndByteOffset>,
        input_wasm: &'a [u8],
        ty: &TypeOrFuncType,
    ) -> crate::Result<()> {
        self.write_block_default(ast, nodeidx, body, newfunc, operators, input_wasm, ty)
    }

    /// Encodes a if/else node.
    ///
    /// Redefine this if your implementation mutates if/else constructions. For example, if your
    /// implementation modifies a particular if/else, override
    /// this method. For the other if/else constructions in the Wasm that your implementation is not
    /// mutating, you can call `write_if_else_default`.
    fn write_if_else<'a>(
        &self,
        ast: &Ast,
        nodeidx: usize,
        then: &[usize],
        alternative: &Option<Vec<usize>>,
        newfunc: &mut Function,
        operators: &Vec<OperatorAndByteOffset>,
        input_wasm: &'a [u8],
        ty: &TypeOrFuncType,
    ) -> crate::Result<()> {
        self.write_if_else_default(
            ast,
            nodeidx,
            then,
            alternative,
            newfunc,
            operators,
            input_wasm,
            ty,
        )
    }

    /// Default encoding for an if-else node
    ///
    /// This function is called by the defaut implementation
    /// of the `write_if_else` method
    fn write_if_else_default<'a>(
        &self,
        ast: &Ast,
        _nodeidx: usize,
        then: &[usize],
        alternative: &Option<Vec<usize>>,
        newfunc: &mut Function,
        operators: &Vec<OperatorAndByteOffset>,
        input_wasm: &'a [u8],
        ty: &TypeOrFuncType,
    ) -> crate::Result<()> {
        newfunc.instruction(&Instruction::If(map_block_type(*ty)?));

        for ch in then {
            self.write(ast, *ch, newfunc, operators, input_wasm)?;
        }

        if let Some(alternative) = alternative {
            newfunc.instruction(&Instruction::Else);

            for ch in alternative {
                self.write(ast, *ch, newfunc, operators, input_wasm)?;
            }
        }
        newfunc.instruction(&Instruction::End);
        Ok(())
    }

    /// Encodes a code node.
    ///
    /// Redefine this if your implementation mutates basic block constructions. For example, if your
    /// implementation replaces the basic block with an unreachable instruction, override
    /// this method. For the other nodes in the Wasm that your implementation is not
    /// mutating, you can call `write_if_else_default`.
    fn write_code<'a>(
        &self,
        _ast: &Ast,
        _nodeidx: usize,
        range: Range,
        newfunc: &mut Function,
        operators: &Vec<OperatorAndByteOffset>,
        input_wasm: &'a [u8],
    ) -> crate::Result<()> {
        let operator_range = (range.start, range.end);
        let bytes_range = (
            &operators[operator_range.0].1,
            &operators[operator_range.1].1,
        );
        let piece_of_code = &input_wasm[*bytes_range.0..*bytes_range.1];
        newfunc.raw(piece_of_code.to_vec());
        Ok(())
    }

    /// Default encoding for code node
    ///
    /// This function is called by the defaut implementation
    /// of the `write_code` method
    fn write_code_default<'a>(
        &self,
        ast: &Ast,
        nodeidx: usize,
        range: Range,
        newfunc: &mut Function,
        operators: &Vec<OperatorAndByteOffset>,
        input_wasm: &'a [u8],
    ) -> crate::Result<()> {
        self.write_code(ast, nodeidx, range, newfunc, operators, input_wasm)
    }

    /// Encoding discriminator for the Ast nodes
    ///
    /// It calls the corresponding methods depending on the node type
    fn write<'a>(
        &self,
        ast: &Ast,
        nodeidx: usize,
        newfunc: &mut Function,
        operators: &Vec<OperatorAndByteOffset>,
        input_wasm: &'a [u8],
    ) -> crate::Result<()> {
        let node = &ast.get_nodes()[nodeidx];

        match node {
            Node::IfElse {
                consequent,
                alternative,
                ty,
                range: _,
            } => {
                self.write_if_else(
                    ast,
                    nodeidx,
                    consequent,
                    alternative,
                    newfunc,
                    operators,
                    input_wasm,
                    ty,
                )?;
            }
            Node::Code { range } => {
                self.write_code(ast, nodeidx, *range, newfunc, operators, input_wasm)?;
            }
            Node::Loop { body, ty, range: _ } => {
                self.write_loop(ast, nodeidx, body, newfunc, operators, input_wasm, ty)?
            }
            Node::Block { body, ty, range: _ } => {
                self.write_block(ast, nodeidx, body, newfunc, operators, input_wasm, ty)?
            }
            Node::Root(body) => {
                for ch in body {
                    self.write(ast, *ch, newfunc, operators, input_wasm)?;
                }
                // Closing end
                newfunc.instruction(&Instruction::End);
            }
        }
        Ok(())
    }
}

impl AstWriter for Ast {
    /* It has the default implementation */
}

impl AstBuilder {
    /// Returns an Ast from the operators collected from the Wasm function
    pub fn build_ast<'a>(&self, operators: &'a [OperatorAndByteOffset]) -> crate::Result<Ast> {
        self.parse(operators)
    }

    /// Parsing algorith to construct the Ast
    fn parse<'a>(&self, operators: &'a [OperatorAndByteOffset]) -> crate::Result<Ast> {
        let mut parse_context = ParseContext::default();
        // Push the first frame, the root
        parse_context.push_frame(State::Root, None, 0);

        for (idx, (operator, _)) in operators.iter().enumerate() {
            match operator {
                Operator::If { ty } => {
                    // push current code first
                    if !parse_context.current_code_is_empty() {
                        parse_context.push_current_code_as_node();
                    }
                    parse_context.reset_code_range_at(idx + 1);
                    parse_context.push_state();
                    parse_context.push_frame(State::If, Some(*ty), idx);
                }
                Operator::Else => {
                    if !parse_context.current_code_is_empty() {
                        parse_context.push_current_code_as_node();
                    }
                    parse_context.reset_code_range_at(idx + 1);
                    parse_context.push_state();
                    parse_context.push_frame(State::Else, None, idx);
                }
                Operator::Block { ty } => {
                    if !parse_context.current_code_is_empty() {
                        parse_context.push_current_code_as_node();
                    }
                    parse_context.reset_code_range_at(idx + 1);
                    parse_context.push_state();
                    parse_context.push_frame(State::Block, Some(*ty), idx);
                }
                Operator::Loop { ty } => {
                    if !parse_context.current_code_is_empty() {
                        parse_context.push_current_code_as_node();
                    }
                    parse_context.reset_code_range_at(idx + 1);
                    parse_context.push_state();
                    parse_context.push_frame(State::Loop, Some(*ty), idx);
                }
                Operator::End => {
                    if !parse_context.current_code_is_empty() {
                        parse_context.push_current_code_as_node();
                    }
                    parse_context.reset_code_range_at(idx + 1);

                    let (last_frame, ty, frame_start) = parse_context.pop_frame()?;
                    match last_frame {
                        State::If => {
                            let then_branch = parse_context.get_current_parsing();
                            parse_context.pop_state()?;
                            // if return type of condition is empty, then the condition is the previous
                            parse_context.push_node_to_current_parsing(Node::IfElse {
                                consequent: then_branch,
                                alternative: None,
                                ty: ty.expect("Missing if type"),
                                range: Range::new(frame_start, idx),
                            });
                        }
                        State::Else => {
                            let (last_frame, ty, if_start) = parse_context.pop_frame()?;
                            // Validate parent
                            match last_frame {
                                State::If => {}
                                _ => unreachable!("Invalid parent frame"),
                            }
                            let else_branch = parse_context.get_current_parsing();
                            let then_branch = parse_context.pop_state()?;
                            parse_context.pop_state()?;
                            // if return type of condition is empty, then the condition is the previous
                            parse_context.push_node_to_current_parsing(Node::IfElse {
                                consequent: then_branch,
                                alternative: Some(else_branch),
                                ty: ty.expect("Missing if type"),
                                range: Range::new(if_start, idx),
                            });
                        }
                        State::Loop => {
                            let children = parse_context.get_current_parsing();
                            parse_context.pop_state()?;

                            parse_context.push_node_to_current_parsing(Node::Loop {
                                body: children,
                                ty: ty.expect("Missing block type for loop"),
                                range: Range::new(frame_start, idx),
                            });
                        }
                        State::Block => {
                            let children = parse_context.get_current_parsing();
                            parse_context.pop_state()?;

                            parse_context.push_node_to_current_parsing(Node::Block {
                                body: children,
                                ty: ty.expect("Missing block type for loop"),
                                range: Range::new(frame_start, idx),
                            });
                        }
                        State::Root => {
                            // break
                            break;
                        }
                    }
                }
                _ => parse_context.append_instruction_to_current_code(),
            }
        }
        Ok(parse_context.finish())
    }
}
