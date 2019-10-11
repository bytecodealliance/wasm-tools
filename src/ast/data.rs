// use crate::ast::{Index, Expression};
// use crate::parser::{Parse, Parser, Result};
// use std::borrow::Cow;
//
// pub struct Data<'a> {
//     memory: Index<'a>,
//     offset: Expression,
//     data: Vec<Cow<'a, [u8]>>,
// }
//
// impl<'a> Parse<'a> for Data<'a> {
//     fn parse(parser: &mut Parser<'a>) -> Result<Self> {
//         let memory = parser.parse()?;
//         let offset = parser.parens(|parser| {
//         })?;
//         let mut data = Vec::new();
//         while !parser.is_empty()? {
//             data.push(parser.string()?);
//         }
//
//         Ok(Data { memory, offset, data })
//     }
// }
