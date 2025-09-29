use crate::{
    Enum, Flags, Function, List, Own, Resource, Type, Val, Variant, Wit, WitOption, WitResult,
};
use rand::distr::{SampleString, StandardUniform};
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use std::collections::HashMap;
use std::mem;

const MAX_SIZE: usize = 1 << 20;
const MAX_LEN: usize = 100;

pub struct Generator {
    rng: SmallRng,
    remaining: usize,
    resource_ctors: HashMap<Resource, Function>,
}

impl Generator {
    pub fn new(wit: Wit, seed: u64) -> Generator {
        let mut resource_ctors = HashMap::new();
        for f in wit
            .iter_funcs()
            .filter(|f| f.is_import() && f.name().starts_with("[constructor]"))
        {
            let Some(Type::Own(resource)) = f.result() else {
                panic!()
            };
            let prev = resource_ctors.insert(resource, f);
            assert!(prev.is_none());
        }
        Generator {
            rng: SmallRng::seed_from_u64(seed),
            remaining: MAX_SIZE,
            resource_ctors,
        }
    }

    pub fn reset_remaining(&mut self) {
        self.remaining = MAX_SIZE;
    }

    pub fn generate(&mut self, ty: Type) -> Val {
        match ty {
            Type::U8 => Val::U8(self.primitive()),
            Type::U16 => Val::U16(self.primitive()),
            Type::U32 => Val::U32(self.primitive()),
            Type::U64 => Val::U64(self.primitive()),
            Type::S8 => Val::S8(self.primitive()),
            Type::S16 => Val::S16(self.primitive()),
            Type::S32 => Val::S32(self.primitive()),
            Type::S64 => Val::S64(self.primitive()),
            Type::F32 => Val::F32(self.primitive()),
            Type::F64 => Val::F64(self.primitive()),
            Type::Bool => Val::Bool(self.primitive()),
            Type::Char => Val::Char(self.primitive()),
            Type::String => Val::String(self.string()),
            Type::Record(r) => Val::Record(self.record(r.fields().map(|f| f.1))),
            Type::Tuple(t) => Val::Tuple(self.record(t.types())),
            Type::Flags(f) => Val::Flags(self.flags(f)),
            Type::Enum(t) => Val::Enum(self.enum_(t)),
            Type::Variant(t) => self.variant(t),
            Type::Option(t) => Val::Option(self.option(t)),
            Type::Result(t) => Val::Result(self.result(t)),
            Type::Alias(t) => self.generate(t.ty()),
            Type::List(t) => {
                if t.ty() == Type::U8 {
                    Val::ByteList(self.byte_list())
                } else {
                    Val::GenericList(self.generic_list(t))
                }
            }

            Type::Own(ty) => Val::Own(self.own(ty)),
            Type::Borrow(_) => unimplemented!("should be handled at caller"),

            Type::ErrorContext => todo!(),
            Type::Stream(_) => todo!(),
            Type::Future(_) => todo!(),
            Type::FixedSizeList(_) => todo!(),
        }
    }

    pub fn rng(&mut self) -> &mut SmallRng {
        &mut self.rng
    }

    fn discount(&mut self, size: usize) {
        self.remaining = self.remaining.saturating_sub(size);
    }

    fn primitive<T>(&mut self) -> T
    where
        StandardUniform: rand::distr::Distribution<T>,
    {
        self.discount(mem::size_of::<T>());
        self.rng.random()
    }

    fn string(&mut self) -> String {
        let len = self.rng.random_range(0..=self.remaining.min(MAX_LEN));
        let ret = StandardUniform.sample_string(&mut self.rng, len);
        self.discount(ret.len());
        ret
    }

    fn record(&mut self, fields: impl Iterator<Item = Type>) -> Vec<Val> {
        fields.map(|t| self.generate(t)).collect()
    }

    fn flags(&mut self, ty: Flags) -> u32 {
        self.discount(4);
        let flags = self.rng.random::<u32>();
        match ty.names().len() {
            // 32 => flags,
            n => flags & ((1 << n) - 1),
        }
    }

    fn enum_(&mut self, ty: Enum) -> u32 {
        self.discount(4);
        self.rng.random_range(0..ty.names().len() as u32)
    }

    fn variant(&mut self, ty: Variant) -> Val {
        // count the size for the discriminant
        self.discount(4);
        let case = self.rng.random_range(0..ty.cases().len() as u32);
        let payload_ty = ty.cases().nth(case as usize).unwrap().1;
        let payload = payload_ty.map(|t| Box::new(self.generate(t)));
        Val::Variant(case, payload)
    }

    fn option(&mut self, ty: WitOption) -> Option<Box<Val>> {
        self.discount(1);
        if self.rng.random() {
            Some(Box::new(self.generate(ty.ty())))
        } else {
            None
        }
    }

    fn result(&mut self, ty: WitResult) -> Result<Option<Box<Val>>, Option<Box<Val>>> {
        self.discount(1);
        if self.rng.random() {
            Ok(ty.ok().map(|t| Box::new(self.generate(t))))
        } else {
            Err(ty.err().map(|t| Box::new(self.generate(t))))
        }
    }

    fn byte_list(&mut self) -> Vec<u8> {
        let len = self.rng.random_range(0..=self.remaining.min(MAX_LEN));
        self.discount(len);
        let mut result = vec![0; len];
        self.rng.fill(&mut result[..]);
        result
    }

    fn generic_list(&mut self, ty: List) -> Vec<Val> {
        let mut result = Vec::new();
        while self.remaining > 0 && self.rng.random_ratio(9, 10) {
            result.push(self.generate(ty.ty()));
        }
        result
    }

    pub fn own(&mut self, ty: Resource) -> Own {
        let rep = self.primitive::<u32>();
        // If `ty` is exported by this component, meaning that we have a
        // constructor, then use that to make an owned resource. Otherwise
        // it must be registered in `resource_ctors` previously.
        //
        // This means that the caller component in roundtrip testing will use
        // `resource_ctors`, or call into the callee, to make a resource. The
        // callee component will use `Own::new`, and both should create
        // distinct owned handles with the same rep internally which is what's
        // used for equality in the callee.
        if ty.new().is_some() {
            Own::new(ty, rep as usize)
        } else {
            let Val::Own(ret) =
                super::call_import_func(self.resource_ctors[&ty], &[Val::U32(rep)]).unwrap()
            else {
                panic!()
            };
            ret
        }
    }
}
