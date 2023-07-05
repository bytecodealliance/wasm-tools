use arbitrary::{Arbitrary, Result, Unstructured};

#[derive(Debug, Clone)]
#[cfg_attr(feature = "clap", derive(clap::Parser))]
pub struct Config {
    #[cfg_attr(feature = "clap", clap(long, default_value_t = Config::default().max_packages))]
    pub max_packages: usize,
    #[cfg_attr(feature = "clap", clap(long, default_value_t = Config::default().max_type_size))]
    pub max_type_size: usize,
    #[cfg_attr(feature = "clap", clap(long, default_value_t = Config::default().max_interface_items))]
    pub max_interface_items: usize,
    #[cfg_attr(feature = "clap", clap(long, default_value_t = Config::default().max_world_items))]
    pub max_world_items: usize,
    #[cfg_attr(feature = "clap", clap(long, default_value_t = Config::default().max_pkg_items))]
    pub max_pkg_items: usize,
    #[cfg_attr(feature = "clap", clap(long, default_value_t = Config::default().max_type_parts))]
    pub max_type_parts: usize,
    #[cfg_attr(feature = "clap", clap(long, default_value_t = Config::default().max_files_per_package))]
    pub max_files_per_package: usize,
    #[cfg_attr(feature = "clap", clap(long, default_value_t = Config::default().max_resource_items))]
    pub max_resource_items: usize,
}

impl Default for Config {
    fn default() -> Config {
        Config {
            max_packages: 10,
            max_type_size: 100,
            max_interface_items: 10,
            max_world_items: 10,
            max_pkg_items: 10,
            max_type_parts: 10,
            max_files_per_package: 10,
            max_resource_items: 10,
        }
    }
}

impl Arbitrary<'_> for Config {
    fn arbitrary(u: &mut Unstructured<'_>) -> Result<Config> {
        Ok(Config {
            max_packages: u.int_in_range(1..=20)?,
            max_files_per_package: u.int_in_range(1..=10)?,
            max_type_size: u.int_in_range(0..=1000)?,
            max_interface_items: u.int_in_range(0..=20)?,
            max_world_items: u.int_in_range(0..=10)?,
            max_pkg_items: u.int_in_range(0..=10)?,
            max_type_parts: u.int_in_range(1..=10)?,
            max_resource_items: u.int_in_range(0..=10)?,
        })
    }
}
