use pretty_assertions::assert_eq;
use wit_encoder::{
    Field, Flag, Params, ResourceFunc, Result_, Results, Type, TypeDef, TypeDefKind, VariantCase,
};

const PACKAGE: &str = indoc::indoc! {"
    package wit-encoder:tests;

    /// interface documentation
    interface type-defs {
      type t1 = u8;
      type t2 = u16;
      type t3 = u32;
      type t4 = u64;
      type t5 = s8;
      type t6 = s16;
      type t7 = s32;
      type t8 = s64;
      type t9a = f32;
      type t9b = f32;
      type t10a = f64;
      type t10b = f64;
      type t11 = char;
      type t12 = list<char>;
      type t13 = string;
      type t14 = option<u32>;
      type t15 = result<u32, u32>;
      type t16 = result<_, u32>;
      type t17 = result<u32>;
      /// this is a documentation comment
      type t18 = result;
      record t20 {  }
      record t21 {
        a: u32,
      }
      record t22 {
        a: u32,
      }
      record t23 {
        a: u32,
        b: u64,
      }
      record t24 {
        a: u32,
        b: u64,
      }
      record t25 {
        x: u32,
      }
      record %record {
        a: u32,
      }
      type t26 = tuple<>;
      type t27 = tuple<u32>;
      type t29 = tuple<u32, u64>;
      flags t30 {
      }
      flags t31 {
        /// option a
        a,
        /// option b
        b,
        /// option c
        c,
      }
      flags t32 {
        a,
        b,
        c,
      }
      variant t33 {
        a,
      }
      variant t34 {
        a,
        b,
      }
      variant t35 {
        a,
        b,
      }
      variant t36 {
        a,
        b(u32),
      }
      variant t37 {
        a,
        b(option<u32>),
      }
      enum t41 {
        a,
        b,
        c,
      }
      enum t42 {
        a,
        b,
        c,
      }
      type t43 = bool;
      type t44 = string;
      type t45 = list<list<list<t32>>>;
      type t46 = t44;
      type foo = bar;
      type bar = u32;
      resource t50 {
      }
      resource t51 {
        /// create a new t51
        constructor(a: u32);
        /// set a
        set-a: func(a: u32);
        /// get a
        get-a: func() -> u32;
        /// do b
        b: static func();
      }
    }
"};

#[test]
fn types() {
    let name = wit_encoder::PackageName::new("wit-encoder", "tests", None);
    let mut package = wit_encoder::Package::new(name);
    package.interface({
        let mut interface = wit_encoder::Interface::new("type-defs");
        interface.docs(Some("interface documentation"));
        interface.type_def(TypeDef::type_("t1", Type::U8));
        interface.type_def(TypeDef::type_("t2", Type::U16));
        interface.type_def(TypeDef::type_("t3", Type::U32));
        interface.type_def(TypeDef::type_("t4", Type::U64));
        interface.type_def(TypeDef::type_("t5", Type::S8));
        interface.type_def(TypeDef::type_("t6", Type::S16));
        interface.type_def(TypeDef::type_("t7", Type::S32));
        interface.type_def(TypeDef::type_("t8", Type::S64));
        interface.type_def(TypeDef::type_("t9a", Type::F32));
        interface.type_def(TypeDef::type_("t9b", Type::F32));
        interface.type_def(TypeDef::type_("t10a", Type::F64));
        interface.type_def(TypeDef::type_("t10b", Type::F64));
        interface.type_def(TypeDef::type_("t11", Type::Char));
        interface.type_def(TypeDef::type_("t12", Type::list(Type::Char)));
        interface.type_def(TypeDef::type_("t13", Type::String));
        interface.type_def(TypeDef::type_("t14", Type::option(Type::U32)));
        interface.type_def(TypeDef::type_(
            "t15",
            Type::result_both(Type::U32, Type::U32),
        ));
        interface.type_def(TypeDef::type_("t16", Type::result(Result_::err(Type::U32))));
        interface.type_def(TypeDef::type_("t17", Type::result(Result_::ok(Type::U32))));
        interface.type_def({
            let mut type_ = TypeDef::type_("t18", Type::result(Result_::empty()));
            type_.docs(Some("this is a documentation comment"));
            type_
        });

        interface.type_def(TypeDef::record("t20", Vec::<Field>::new()));
        interface.type_def(TypeDef::record("t21", [Field::new("a", Type::U32)]));
        interface.type_def(TypeDef::record("t22", [Field::new("a", Type::U32)]));
        interface.type_def(TypeDef::record(
            "t23",
            [Field::new("a", Type::U32), Field::new("b", Type::U64)],
        ));
        interface.type_def(TypeDef::record(
            "t24",
            [Field::new("a", Type::U32), Field::new("b", Type::U64)],
        ));
        interface.type_def(TypeDef::record("t25", [Field::new("x", Type::U32)]));
        interface.type_def(TypeDef::record("record", [Field::new("a", Type::U32)]));

        interface.type_def(TypeDef::type_("t26", Type::tuple(Vec::<Type>::new())));
        interface.type_def(TypeDef::type_("t27", Type::tuple([Type::U32])));
        interface.type_def(TypeDef::type_("t29", Type::tuple([Type::U32, Type::U64])));

        interface.type_def(TypeDef::flags("t30", Vec::<Flag>::new()));
        interface.type_def(TypeDef::flags(
            "t31",
            [("a", "option a"), ("b", "option b"), ("c", "option c")],
        ));
        interface.type_def(TypeDef::flags("t32", [("a",), ("b",), ("c",)]));

        interface.type_def(TypeDef::variant("t33", [("a",)]));
        interface.type_def(TypeDef::variant("t34", [("a",), ("b",)]));
        interface.type_def(TypeDef::variant("t35", [("a",), ("b",)]));
        interface.type_def(TypeDef::new(
            "t36",
            TypeDefKind::Variant(
                [VariantCase::empty("a"), VariantCase::value("b", Type::U32)].into(),
            ),
        ));
        interface.type_def(TypeDef::new(
            "t37",
            TypeDefKind::Variant(
                [
                    VariantCase::empty("a"),
                    VariantCase::value("b", Type::option(Type::U32)),
                ]
                .into(),
            ),
        ));

        interface.type_def(TypeDef::enum_("t41", ["a", "b", "c"]));
        interface.type_def(TypeDef::enum_("t42", ["a", "b", "c"]));

        interface.type_def(TypeDef::type_("t43", Type::Bool));
        interface.type_def(TypeDef::type_("t44", Type::String));
        interface.type_def(TypeDef::type_(
            "t45",
            Type::list(Type::list(Type::list(Type::named("t32")))),
        ));
        interface.type_def(TypeDef::type_("t46", Type::named("t44")));
        interface.type_def(TypeDef::type_("foo", Type::named("bar")));
        interface.type_def(TypeDef::type_("bar", Type::U32));

        interface.type_def(TypeDef::resource("t50", Vec::<ResourceFunc>::new()));
        interface.type_def(TypeDef::resource(
            "t51",
            [
                {
                    let mut func = ResourceFunc::constructor();
                    func.params(Params::from_iter([("a", Type::U32)]));
                    func.docs(Some("create a new t51"));
                    func
                },
                {
                    let mut func = ResourceFunc::method("set-a");
                    func.params(Params::from_iter([("a", Type::U32)]));
                    func.docs(Some("set a"));
                    func
                },
                {
                    let mut func = ResourceFunc::method("get-a");
                    func.results(Results::anon(Type::U32));
                    func.docs(Some("get a"));
                    func
                },
                {
                    let mut func = ResourceFunc::static_("b");
                    func.docs(Some("do b"));
                    func
                },
            ],
        ));
        interface
    });
    assert_eq!(PACKAGE, package.to_string());
}
