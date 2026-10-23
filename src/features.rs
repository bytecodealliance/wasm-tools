//! Shared parsing of the `--features` flag for subcommands.

use anyhow::{Result, bail};
use bitflags::Flags;
use wasmparser::WasmFeatures;

/// Helper structure used to parse the `--features` flag of subcommands.
#[derive(clap::Parser)]
pub struct CliFeatures {
    /// Comma-separated list of WebAssembly features to enable during
    /// parsing and validation.
    ///
    /// If a "-" character is present in front of a feature it will disable that
    /// feature. For example "-simd" will disable the simd proposal.
    ///
    /// The placeholder "all" can be used to enable all wasm features and the
    /// term "-all" can be used to disable all features.
    ///
    /// The default set of features enabled are all WebAssembly proposals that
    /// are at phase 4 or after. This means that the default set of features
    /// accepted are relatively bleeding edge. Versions of the WebAssembly
    /// specification can also be selected. The "wasm1" or "mvp" feature can
    /// select the original WebAssembly specification and "wasm2" can be used to
    /// select the 2.0 version.
    ///
    /// Available feature options can be found in the wasmparser crate:
    /// <https://github.com/bytecodealliance/wasm-tools/blob/main/crates/wasmparser/src/features.rs>
    #[clap(long, short = 'f', value_parser = parse_features)]
    features: Vec<Vec<FeatureAction>>,
}

#[derive(Clone)]
enum FeatureAction {
    Reset(WasmFeatures),
    Enable(WasmFeatures),
    Disable(WasmFeatures),
}

impl CliFeatures {
    /// Returns the set of features configured, starting from
    /// [`WasmFeatures::default`].
    pub fn features(&self) -> WasmFeatures {
        self.features_from(WasmFeatures::default())
    }

    /// Returns the set of features configured, starting from `base`.
    pub fn features_from(&self, base: WasmFeatures) -> WasmFeatures {
        let mut ret = base;

        for action in self.features.iter().flat_map(|v| v) {
            match action {
                FeatureAction::Enable(features) => {
                    ret |= *features;
                }
                FeatureAction::Disable(features) => {
                    ret &= !*features;
                }
                FeatureAction::Reset(features) => {
                    ret = *features;
                }
            }
        }

        ret
    }
}

fn parse_features(arg: &str) -> Result<Vec<FeatureAction>> {
    let mut ret = Vec::new();

    const GROUPS: &[(&str, WasmFeatures)] = &[
        ("mvp", WasmFeatures::MVP),
        ("wasm1", WasmFeatures::WASM1),
        ("wasm2", WasmFeatures::WASM2),
        ("wasm3", WasmFeatures::WASM3),
        ("lime1", WasmFeatures::LIME1),
    ];

    enum Action {
        ChangeAll,
        Group(WasmFeatures),
        Modify(WasmFeatures),
    }

    fn actions() -> impl Iterator<Item = (&'static str, Action)> {
        WasmFeatures::FLAGS
            .iter()
            .map(|f| (f.name(), Action::Modify(*f.value())))
            .chain(
                GROUPS
                    .iter()
                    .map(|(name, features)| (*name, Action::Group(*features))),
            )
            .chain([("all", Action::ChangeAll)])
    }

    fn flag_name(name: &str) -> String {
        name.to_lowercase().replace('_', "-")
    }

    'outer: for part in arg.split(',').map(|s| s.trim()).filter(|s| !s.is_empty()) {
        let (enable, part) = if let Some(part) = part.strip_prefix("-") {
            (false, part)
        } else {
            (true, part)
        };
        for (name, action) in actions() {
            if part != flag_name(name) {
                continue;
            }
            match action {
                Action::ChangeAll => {
                    ret.push(if enable {
                        FeatureAction::Enable(WasmFeatures::all())
                    } else {
                        FeatureAction::Disable(WasmFeatures::all())
                    });
                }
                Action::Modify(feature) => {
                    ret.push(if enable {
                        FeatureAction::Enable(feature)
                    } else {
                        FeatureAction::Disable(feature)
                    });
                }
                Action::Group(features) => {
                    if !enable {
                        bail!("cannot disable `{part}`, it can only be enabled");
                    }
                    ret.push(FeatureAction::Reset(features));
                }
            }
            continue 'outer;
        }

        let mut error = format!("unknown feature `{part}`\n");
        error.push_str("Valid features: ");
        let mut first = true;
        for (name, _) in actions() {
            if first {
                first = false;
            } else {
                error.push_str(", ");
            }
            error.push_str(&flag_name(name));
        }
        bail!("{error}")
    }

    Ok(ret)
}
