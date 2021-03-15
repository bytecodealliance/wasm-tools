//! Configuring the shape of generated Wasm modules.

use arbitrary::{Arbitrary, Result, Unstructured};

/// Configuration for a generated module.
///
/// Don't care to configure your generated modules? Just use
/// [`Module`][crate::Module], which internally uses
/// [`DefaultConfig`][crate::DefaultConfig].
///
/// If you want to configure generated modules, then define a `MyConfig` type,
/// implement this trait for it, and use
/// [`ConfiguredModule<MyConfig>`][crate::ConfiguredModule] instead of `Module`.
///
/// Every trait method has a provided default implementation, so that you only
/// need to override the methods for things you want to change away from the
/// default.
pub trait Config: for<'a> Arbitrary<'a> + Default + Clone {
    /// The minimum number of types to generate. Defaults to 0.
    fn min_types(&self) -> usize {
        0
    }

    /// The maximum number of types to generate. Defaults to 100.
    fn max_types(&self) -> usize {
        100
    }

    /// The minimum number of imports to generate. Defaults to 0.
    ///
    /// Note that if the sum of the maximum function[^1], table, global and
    /// memory counts is less than the minimum number of imports, then it will
    /// not be possible to satisfy all constraints (because imports count
    /// against the limits for those element kinds). In that case, we strictly
    /// follow the max-constraints, and can fail to satisfy this minimum number.
    ///
    /// [^1]: the maximum number of functions is also limited by the number of
    ///       function types arbitrarily chosen; strictly speaking, then, the
    ///       maximum number of imports that can be created due to
    ///       max-constraints is `sum(min(num_func_types, max_funcs), max_tables,
    ///       max_globals, max_memories)`.
    fn min_imports(&self) -> usize {
        0
    }

    /// The maximum number of imports to generate. Defaults to 100.
    fn max_imports(&self) -> usize {
        100
    }

    /// The minimum number of functions to generate. Defaults to 0.  This
    /// includes imported functions.
    fn min_funcs(&self) -> usize {
        0
    }

    /// The maximum number of functions to generate. Defaults to 100.  This
    /// includes imported functions.
    fn max_funcs(&self) -> usize {
        100
    }

    /// The minimum number of globals to generate. Defaults to 0.  This includes
    /// imported globals.
    fn min_globals(&self) -> usize {
        0
    }

    /// The maximum number of globals to generate. Defaults to 100.  This
    /// includes imported globals.
    fn max_globals(&self) -> usize {
        100
    }

    /// The minimum number of exports to generate. Defaults to 0.
    fn min_exports(&self) -> usize {
        0
    }

    /// The maximum number of exports to generate. Defaults to 100.
    fn max_exports(&self) -> usize {
        100
    }

    /// The minimum number of element segments to generate. Defaults to 0.
    fn min_element_segments(&self) -> usize {
        0
    }

    /// The maximum number of element segments to generate. Defaults to 100.
    fn max_element_segments(&self) -> usize {
        100
    }

    /// The minimum number of elements within a segment to generate. Defaults to
    /// 0.
    fn min_elements(&self) -> usize {
        0
    }

    /// The maximum number of elements within a segment to generate. Defaults to
    /// 100.
    fn max_elements(&self) -> usize {
        100
    }

    /// The minimum number of data segments to generate. Defaults to 0.
    fn min_data_segments(&self) -> usize {
        0
    }

    /// The maximum number of data segments to generate. Defaults to 100.
    fn max_data_segments(&self) -> usize {
        100
    }

    /// The maximum number of instructions to generate in a function
    /// body. Defaults to 100.
    ///
    /// Note that some additional `end`s, `else`s, and `unreachable`s may be
    /// appended to the function body to finish block scopes.
    fn max_instructions(&self) -> usize {
        100
    }

    /// The minimum number of memories to use. Defaults to 0. This includes
    /// imported memories.
    fn min_memories(&self) -> u32 {
        0
    }

    /// The maximum number of memories to use. Defaults to 1. This includes
    /// imported memories.
    ///
    /// Note that more than one memory is in the realm of the multi-memory wasm
    /// proposal.
    fn max_memories(&self) -> usize {
        1
    }

    /// The minimum number of tables to use. Defaults to 0. This includes
    /// imported tables.
    fn min_tables(&self) -> u32 {
        0
    }

    /// The maximum number of tables to use. Defaults to 1. This includes
    /// imported tables.
    ///
    /// Note that more than one table is in the realm of the reference types
    /// proposal.
    fn max_tables(&self) -> usize {
        1
    }

    /// The maximum, in 64k Wasm pages, of any memory's initial or maximum size.
    /// Defaults to 2^16 = 65536 (the maximum possible for 32-bit Wasm).
    fn max_memory_pages(&self) -> u32 {
        65536
    }

    /// Whether every Wasm memory must have a maximum size specified. Defaults
    /// to `false`.
    fn memory_max_size_required(&self) -> bool {
        false
    }

    /// The maximum number of instances to use. Defaults to 10. This includes
    /// imported instances.
    ///
    /// Note that this is irrelevant unless module linking is enabled.
    fn max_instances(&self) -> usize {
        10
    }

    /// The maximum number of modules to use. Defaults to 10. This includes
    /// imported modules.
    ///
    /// Note that this is irrelevant unless module linking is enabled.
    fn max_modules(&self) -> usize {
        10
    }

    /// Control the probability of generating memory offsets that are in bounds
    /// vs. potentially out of bounds.
    ///
    /// Return a tuple `(a, b, c)` where
    ///
    /// * `a / (a+b+c)` is the probability of generating a memory offset within
    ///   `0..memory.min_size`, i.e. an offset that is definitely in bounds of a
    ///   non-empty memory. (Note that if a memory is zero-sized, however, no
    ///   offset will ever be in bounds.)
    ///
    /// * `b / (a+b+c)` is the probability of generating a memory offset within
    ///   `memory.min_size..memory.max_size`, i.e. an offset that is possibly in
    ///   bounds if the memory has been grown.
    ///
    /// * `c / (a+b+c)` is the probability of generating a memory offset within
    ///   the range `memory.max_size..`, i.e. an offset that is definitely out
    ///   of bounds.
    ///
    /// At least one of `a`, `b`, and `c` must be non-zero.
    ///
    /// If you want to always generate memory offsets that are definitely in
    /// bounds of a non-zero-sized memory, for example, you could return `(1, 0,
    /// 0)`.
    ///
    /// By default, returns `(75, 24, 1)`.
    fn memory_offset_choices(&self) -> (u32, u32, u32) {
        (75, 24, 1)
    }

    /// The minimum size, in bytes, of all leb-encoded integers. Defaults to 1.
    ///
    /// This is useful for ensuring that all leb-encoded integers are decoded as
    /// such rather than as simply one byte. This will forcibly extend leb
    /// integers with an over-long encoding in some locations if the size would
    /// otherwise be smaller than number returned here.
    fn min_uleb_size(&self) -> u8 {
        1
    }

    /// Determines whether the bulk memory proposal is enabled for generating
    /// insructions. Defaults to `false`.
    fn bulk_memory_enabled(&self) -> bool {
        false
    }

    /// Determines whether the reference types proposal is enabled for
    /// generating insructions. Defaults to `false`.
    fn reference_types_enabled(&self) -> bool {
        false
    }

    /// Determines whether the module linking proposal is enabled.
    ///
    /// Defaults to `false`.
    fn module_linking_enabled(&self) -> bool {
        false
    }

    /// Determines whether a `start` export may be included. Defaults to `true`.
    fn allow_start_export(&self) -> bool {
        true
    }

    /// Returns the maximal size of the `alias` section.
    fn max_aliases(&self) -> usize {
        1_000
    }

    /// Returns the maximal nesting depth of modules with the module linking
    /// proposal.
    fn max_nesting_depth(&self) -> usize {
        10
    }

    /// Returns the maximal effective size of any type generated by wasm-smith.
    ///
    /// Note that this number is roughly in units of "how many types would be
    /// needed to represent the recursive type". A function with 8 parameters
    /// and 2 results would take 11 types (one for the type, 10 for
    /// params/results). A module type with 2 imports and 3 exports would
    /// take 6 (module + imports + exports) plus the size of each import/export
    /// type. This is a somewhat rough measurement that is not intended to be
    /// very precise.
    fn max_type_size(&self) -> u32 {
        1_000
    }
}

/// The default configuration.
#[derive(Arbitrary, Debug, Default, Copy, Clone)]
pub struct DefaultConfig;

impl Config for DefaultConfig {}

/// A module configuration that uses [swarm testing].
///
/// Dynamically -- but still deterministically, via its `Arbitrary`
/// implementation -- chooses configuration options.
///
/// [swarm testing]: https://www.cs.utah.edu/~regehr/papers/swarm12.pdf
///
/// Note that we pick only *maximums*, not minimums, here because it is more
/// complex to describe the domain of valid configs when minima are involved
/// (`min <= max` for each variable) and minima are mostly used to ensure
/// certain elements are present, but do not widen the range of generated Wasm
/// modules.
#[derive(Clone, Debug, Default)]
pub struct SwarmConfig {
    max_types: usize,
    max_imports: usize,
    max_funcs: usize,
    max_globals: usize,
    max_exports: usize,
    max_element_segments: usize,
    max_elements: usize,
    max_data_segments: usize,
    max_instructions: usize,
    max_memories: usize,
    min_uleb_size: u8,
    max_tables: usize,
    max_memory_pages: u32,
    bulk_memory_enabled: bool,
    reference_types_enabled: bool,
    module_linking_enabled: bool,
    max_aliases: usize,
    max_nesting_depth: usize,
}

impl<'a> Arbitrary<'a> for SwarmConfig {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        const MAX_MAXIMUM: usize = 1000;

        let reference_types_enabled: bool = u.arbitrary()?;
        let max_tables = if reference_types_enabled { 100 } else { 1 };

        Ok(SwarmConfig {
            max_types: u.int_in_range(0..=MAX_MAXIMUM)?,
            max_imports: u.int_in_range(0..=MAX_MAXIMUM)?,
            max_funcs: u.int_in_range(0..=MAX_MAXIMUM)?,
            max_globals: u.int_in_range(0..=MAX_MAXIMUM)?,
            max_exports: u.int_in_range(0..=MAX_MAXIMUM)?,
            max_element_segments: u.int_in_range(0..=MAX_MAXIMUM)?,
            max_elements: u.int_in_range(0..=MAX_MAXIMUM)?,
            max_data_segments: u.int_in_range(0..=MAX_MAXIMUM)?,
            max_instructions: u.int_in_range(0..=MAX_MAXIMUM)?,
            max_memories: u.int_in_range(0..=100)?,
            max_tables,
            max_memory_pages: u.int_in_range(0..=65536)?,
            min_uleb_size: u.int_in_range(0..=5)?,
            bulk_memory_enabled: u.arbitrary()?,
            reference_types_enabled,
            module_linking_enabled: u.arbitrary()?,
            max_aliases: u.int_in_range(0..=MAX_MAXIMUM)?,
            max_nesting_depth: u.int_in_range(0..=10)?,
        })
    }
}

impl Config for SwarmConfig {
    fn max_types(&self) -> usize {
        self.max_types
    }

    fn max_imports(&self) -> usize {
        self.max_imports
    }

    fn max_funcs(&self) -> usize {
        self.max_funcs
    }

    fn max_globals(&self) -> usize {
        self.max_globals
    }

    fn max_exports(&self) -> usize {
        self.max_exports
    }

    fn max_element_segments(&self) -> usize {
        self.max_element_segments
    }

    fn max_elements(&self) -> usize {
        self.max_elements
    }

    fn max_data_segments(&self) -> usize {
        self.max_data_segments
    }

    fn max_instructions(&self) -> usize {
        self.max_instructions
    }

    fn max_memories(&self) -> usize {
        self.max_memories
    }

    fn max_tables(&self) -> usize {
        self.max_tables
    }

    fn max_memory_pages(&self) -> u32 {
        self.max_memory_pages
    }

    fn min_uleb_size(&self) -> u8 {
        self.min_uleb_size
    }

    fn bulk_memory_enabled(&self) -> bool {
        self.bulk_memory_enabled
    }

    fn reference_types_enabled(&self) -> bool {
        self.reference_types_enabled
    }

    fn module_linking_enabled(&self) -> bool {
        self.module_linking_enabled
    }

    fn max_aliases(&self) -> usize {
        self.max_aliases
    }

    fn max_nesting_depth(&self) -> usize {
        self.max_nesting_depth
    }
}
