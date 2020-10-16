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
pub trait Config: Arbitrary + Default {
    /// The maximum number of imports to generate. Defaults to 100.
    fn max_imports(&self) -> usize {
        100
    }

    /// The maximum number of functions to generate. Defaults to 100.
    fn max_funcs(&self) -> usize {
        100
    }

    /// The maximum number of globals to generate. Defaults to 100.
    fn max_globals(&self) -> usize {
        100
    }

    /// The maximum number of exports to generate. Defaults to 100.
    fn max_exports(&self) -> usize {
        100
    }

    /// The maximum number of element segments to generate. Defaults to 100.
    fn max_element_segments(&self) -> usize {
        100
    }

    /// The maximum number of elements within a segment to generate. Defaults to
    /// 100.
    fn max_elements(&self) -> usize {
        100
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

    /// The maximum number of memories to use. Defaults to 1.
    ///
    /// Note that more than one memory is in the realm of the multi-memory wasm
    /// proposal.
    fn max_memories(&self) -> u32 {
        1
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
#[derive(Clone, Debug, Default)]
pub struct SwarmConfig {
    max_imports: usize,
    max_funcs: usize,
    max_globals: usize,
    max_exports: usize,
    max_element_segments: usize,
    max_elements: usize,
    max_data_segments: usize,
    max_instructions: usize,
    max_memories: u32,
    min_uleb_size: u8,
}

impl Arbitrary for SwarmConfig {
    fn arbitrary(u: &mut Unstructured<'_>) -> Result<Self> {
        const MAX_MAXIMUM: usize = 1000;
        Ok(SwarmConfig {
            max_imports: u.int_in_range(0..=MAX_MAXIMUM)?,
            max_funcs: u.int_in_range(0..=MAX_MAXIMUM)?,
            max_globals: u.int_in_range(0..=MAX_MAXIMUM)?,
            max_exports: u.int_in_range(0..=MAX_MAXIMUM)?,
            max_element_segments: u.int_in_range(0..=MAX_MAXIMUM)?,
            max_elements: u.int_in_range(0..=MAX_MAXIMUM)?,
            max_data_segments: u.int_in_range(0..=MAX_MAXIMUM)?,
            max_instructions: u.int_in_range(0..=MAX_MAXIMUM)?,
            max_memories: u.int_in_range(0..=(MAX_MAXIMUM as u32))?,
            min_uleb_size: u.int_in_range(0..=5)?,
        })
    }
}

impl Config for SwarmConfig {
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

    fn max_memories(&self) -> u32 {
        self.max_memories
    }

    fn min_uleb_size(&self) -> u8 {
        self.min_uleb_size
    }
}
