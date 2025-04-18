/* Copyright 2018 Mozilla Foundation
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

use crate::{BinaryReader, FromReader, MemoryType, Result, SectionLimited};

/// A reader for the memory section of a WebAssembly module.
pub type MemorySectionReader<'a> = SectionLimited<'a, MemoryType>;

impl<'a> FromReader<'a> for MemoryType {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        let pos = reader.original_position();
        let flags = reader.read_u8()?;

        if (flags & !0b1111) != 0 {
            bail!(pos, "invalid memory limits flags");
        }

        let memory64 = flags & 0b0100 != 0;
        let shared = flags & 0b0010 != 0;
        let has_max = flags & 0b0001 != 0;
        let has_page_size = flags & 0b1000 != 0;

        Ok(MemoryType {
            memory64,
            shared,
            initial: if reader.memory64() {
                reader.read_var_u64()?
            } else {
                reader.read_var_u32()?.into()
            },
            maximum: if !has_max {
                None
            } else if reader.memory64() {
                Some(reader.read_var_u64()?)
            } else {
                Some(reader.read_var_u32()?.into())
            },
            page_size_log2: if has_page_size {
                let val = reader.read_var_u32()?;
                if val >= 64 {
                    bail!(pos, "invalid custom page size");
                }
                Some(val)
            } else {
                None
            },
        })
    }
}
