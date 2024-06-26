/* Copyright 2024 Mozilla Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use crate::{BlockType, FrameKind};

/// To compute the arity of "variable-arity" operators, the operator_arity macro needs
/// some information about the module's types and the current control stack. The
/// ModuleArity trait exposes this information.
pub trait ModuleArity {
    /// Length of tag type with given idex
    fn tag_len(&self, at: u32) -> Option<u32>;

    /// Arity (params and results) of function with given index
    fn func_type_arity(&self, function_idx: u32) -> Option<(u32, u32)>;

    /// Arity (params and results) of type with given index
    fn sub_type_arity(&self, type_idx: u32) -> Option<(u32, u32)>;

    /// Current height of control stack
    fn control_stack_height(&self) -> u32;

    /// BlockType and FrameKind of label with given index
    fn label_block(&self, depth: u32) -> Option<(BlockType, FrameKind)>;

    /// Computes arity (params and results) of given BlockType
    fn block_type_arity(&self, ty: BlockType) -> Option<(u32, u32)> {
        match ty {
            BlockType::Empty => Some((0, 0)),
            BlockType::Type(_) => Some((0, 1)),
            BlockType::FuncType(t) => self.sub_type_arity(t),
        }
    }
}

/// The operator_arity macro interprets the annotations in the for_each_operator macro
/// to compute the arity of each operator. It needs access to a ModuleArity implementation.
#[macro_export]
macro_rules! operator_arity {
    (arities $self:ident $({ $($arg:ident: $argty:ty),* })? arity $($ann:tt)*) => {
	{
	    let params  = (|| -> Option<(i32, i32)> { operator_arity!(params  $self { $($($arg: $argty),*)? } $($ann)*) })();
	    let results = (|| -> Option<(i32, i32)> { operator_arity!(results $self { $($($arg: $argty),*)? } $($ann)*) })();
	    match (params, results) {
		(Some((a,_)), Some((_,d))) if a >= 0 && d >= 0  => (Some((a as u32, d as u32))),
		_ => None,
	    }
	}
    };

    (arities $self:ident $({ $($arg:ident: $argty:ty),* })? $cat:ident $($ann:tt)*) => {
	    Some(operator_arity!(fixed $cat $($ann)*))
    };

    (params  $self:ident { $($arg:ident: $argty:ty),* } $val:literal $($tokens:tt)*) => { { let rest = operator_arity!(params $self { $($arg: $argty),* } $($tokens)*)?;
											    Some(($val + rest.0, $val + rest.1)) } };
    (params  $self:ident { $($arg:ident: $argty:ty),* } $cat:ident   $($tokens:tt)*) => { { let (a, b) = operator_arity!(count $self { $($arg: $argty),* } $cat)?;
											    let (c, d) = operator_arity!(params $self { $($arg: $argty),* } $($tokens)*)?;
											    Some((a as i32 + c as i32, b as i32 + d as i32)) } };
    (params  $self:ident { $($arg:ident: $argty:ty),* } ->           $($tokens:tt)*) => { Some((0, 0)) };
    (params  $self:ident { $($arg:ident: $argty:ty),* })                             => { Some((0, 0)) };

    (results $self:ident { $($arg:ident: $argty:ty),* } $val:literal $($tokens:tt)*) => { operator_arity!(results $self { $($arg: $argty),* } $($tokens)*) };
    (results $self:ident { $($arg:ident: $argty:ty),* } $cat:ident   $($tokens:tt)*) => { operator_arity!(results $self { $($arg: $argty),* } $($tokens)*) };
    (results $self:ident { $($arg:ident: $argty:ty),* } ->           $($tokens:tt)*) => { operator_arity!(params  $self { $($arg: $argty),* } $($tokens)*) };

    (count $self:ident { $tag_index:ident: $_:ty } tag) => {{
	operator_arity!(tag_index $tag_index);
	let n = $self.tag_len($tag_index)?;
	Some((n, n))
    }};

    (count $self:ident { $func_index:ident: $_:ty } func) => {{
	operator_arity!(func_index $func_index);
	$self.func_type_arity($func_index)
    }};

    (count $self:ident { $type_index:ident: $($_:tt)* } type) => {{
	operator_arity!(type_index $type_index);
	$self.sub_type_arity($type_index)
    }};

    (count $self:ident { $arg1:ident: $argty:ty, $size:ident: $sizety:ty } size) => {{
        operator_arity!(size_value $size);
        Some(($size, $size))
    }};

    (count $self:ident { $depth:ident: $($_:tt)* } br) => {{
        operator_arity!(depth $depth);
	let (ty, kind) = $self.label_block($depth)?;
	let (params, results) = $self.block_type_arity(ty)?;
	let n = match kind {
	    FrameKind::Loop => params,
	    _ => results
	};
	Some((n, n))
    }};

    (count $self:ident { $($_:ident: $__:ty),* } ret) => {{
	let (ty, _) = $self.control_stack_height().checked_sub(1)
	    .and_then(|x| $self.label_block(x))?;
	let n = $self.block_type_arity(ty)?.1;
        Some((n, n))
    }};

    (count $self:ident { $blockty:ident: $($_:tt)* } enter) => {{
        operator_arity!(blockty $blockty);
        let (params, _results) = $self.block_type_arity($blockty)?;
        Some((params, params))
    }};

    (count $self:ident {} else) => {{
	let (ty, kind) = $self.label_block(0)?;
	let (params, results) = $self.block_type_arity(ty)?;
	Some(match kind {
	    FrameKind::If => (results, params),
	    _ => (0, 0)
	})
    }};

    (count $self:ident { $($_: ident: $__:ty),* } leave) => {{
	let (ty, _) = $self.label_block(0)?;
	let (_params, results) = $self.block_type_arity(ty)?;
	Some((results, results))
    }};

    (count $self:ident { $try_table:ident: $($_:tt)* } try_table) => {{
        operator_arity!(try_table $try_table);
        let (params, _results) = $self.block_type_arity($try_table.ty)?;
        Some((params, params))
    }};

    (count $self:ident { $br_table:ident: $($_:tt)* } br_table) => {{
        operator_arity!(br_table $br_table);
	let relative_depth: u32 = $br_table.default();
	operator_arity!(count $self { relative_depth: u32 } br)
    }};

    (tag_index tag_index $($_:tt)*) => {};
    (func_index function_index $($_:tt)*) => {};
    (type_index type_index $($_:tt)*) => {};
    (type_index struct_type_index $($_:tt)*) => {};
    (size_value array_size $($_:tt)*) => {};
    (depth relative_depth $($_:tt)*) => {};
    (blockty blockty $($_:tt)*) => {};
    (try_table try_table $($_:tt)*) => {};
    (br_table targets $($_:tt)*) => {};

    (fixed load lane $($_:tt)*)        => {(2, 1)};
    (fixed load $($_:tt)*)             => {(1, 1)};
    (fixed store $($_:tt)*)            => {(2, 0)};
    (fixed test $($_:tt)*)             => {(1, 1)};
    (fixed unary $($_:tt)*)            => {(1, 1)};
    (fixed binary $($_:tt)*)           => {(2, 1)};
    (fixed cmp $($_:tt)*)              => {(2, 1)};
    (fixed shift $($_:tt)*)            => {(2, 1)};
    (fixed splat $($_:tt)*)            => {(1, 1)};
    (fixed ternary $($_:tt)*)          => {(3, 1)};
    (fixed conversion $($_:tt)*)       => {(1, 1)};
    (fixed push $($_:tt)*)             => {(0, 1)};
    (fixed extract $($_:tt)*)          => {(1, 1)};
    (fixed replace $($_:tt)*)          => {(2, 1)};
    (fixed atomic rmw array $($_:tt)*) => {(3, 1)};
    (fixed atomic rmw $($_:tt)*)       => {(2, 1)};
    (fixed atomic cmpxchg $($_:tt)*)   => {(3, 1)};
}
