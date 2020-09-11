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
}
