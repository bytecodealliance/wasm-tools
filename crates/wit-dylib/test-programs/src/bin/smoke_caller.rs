use test_programs::*;

export_test!(struct MyInterpreter);

impl TestCase for MyInterpreter {
    fn call_export(
        wit: Wit,
        func: ExportFunction,
        args: impl ExactSizeIterator<Item = Val>,
    ) -> Option<Val> {
        assert_eq!(func.interface(), None);
        assert_eq!(func.name(), "run");
        assert_eq!(func.params().len(), 0);
        assert!(func.result().is_none());
        assert_eq!(args.len(), 0);

        let ret = Self::call_import(wit, Some("a:b/x"), "x", &[]);
        assert!(ret.is_none());

        None
    }

    fn initialize(wit: Wit) {
        assert_eq!(wit.iter_records().len(), 0);
        assert_eq!(wit.iter_resources().len(), 0);
        assert_eq!(wit.iter_tuples().len(), 0);
        assert_eq!(wit.iter_aliases().len(), 0);
        assert_eq!(wit.iter_futures().len(), 0);
        assert_eq!(wit.iter_streams().len(), 0);
        assert_eq!(wit.iter_lists().len(), 0);
        assert_eq!(wit.iter_options().len(), 0);
        assert_eq!(wit.iter_results().len(), 0);
        assert_eq!(wit.iter_fixed_size_lists().len(), 0);
        assert_eq!(wit.iter_variants().len(), 0);
        assert_eq!(wit.iter_flags().len(), 0);
        assert_eq!(wit.iter_enums().len(), 0);
        assert_eq!(wit.iter_import_funcs().len(), 1);
        assert_eq!(wit.iter_export_funcs().len(), 1);
    }
}
