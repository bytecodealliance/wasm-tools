//! Helper script to manage versions in this repository for various crates.
//!
//! Three subcommands:
//!
//! * `./publish diff wasmparser` - shows a git diff for the wasmparser
//!   crate from the last tagged version to now. Useful for figuring out if a
//!   major version bump is needed or not.
//!
//! * `./publish bump crate1:major crate2:minor ...` - performs a major or minor
//!   version bump of the crates specified. All crates not mentioned here
//!   which transitively depend on these crates are minor-bumped.
//!
//! * `./publish publish` - attempts to publish all crates. Only publishes if
//!   their current version isn't already published. Will add wasmtime
//!   publication group automatically. A git tag is created for all published
//!   crates.

use std::collections::HashMap;
use std::env;
use std::fs;
use std::os::unix::prelude::*;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::thread;
use std::time::Duration;

// Crates we care about publishing sorted topologically.
const CRATES_TO_PUBLISH: &[&str] = &[
    "wasmparser",
    "wasm-encoder",
    "wasmprinter",
    "wast",
    "wat",
    "wasmparser-dump",
    "wasm-smith",
    "wasm-mutate",
    "wasm-shrink",
    "wasm-tools",
];

#[derive(Clone)]
struct Crate {
    manifest: PathBuf,
    name: String,
    version: String,
    // Only set by `bump_version` if the crate was actually updated to get a new
    // version.
    new_version: Option<String>,
    publish: bool,
}

fn main() {
    let mut crates = Vec::new();
    crates.push(read_crate("./Cargo.toml".as_ref()));
    find_crates("crates".as_ref(), &mut crates);

    let pos = CRATES_TO_PUBLISH
        .iter()
        .enumerate()
        .map(|(i, c)| (*c, i))
        .collect::<HashMap<_, _>>();
    crates.sort_by_key(|krate| pos.get(&krate.name[..]));

    match &env::args().nth(1).expect("must have one argument")[..] {
        "bump" => {
            let bumps = env::args()
                .skip(2)
                .map(|s| {
                    if let Some(s) = s.strip_suffix(":major") {
                        (s.to_string(), true)
                    } else if let Some(s) = s.strip_suffix(":minor") {
                        (s.to_string(), false)
                    } else {
                        panic!("unknown: {}", s);
                    }
                })
                .collect::<Vec<_>>();
            for (i, mut krate) in crates.clone().into_iter().enumerate() {
                bump_version(&mut krate, &mut crates, &bumps);
                crates[i] = krate;
            }
            // update the lock file
            assert!(Command::new("cargo")
                .arg("fetch")
                .status()
                .unwrap()
                .success());
        }

        "publish" => {
            // We have so many crates to publish we're frequently either
            // rate-limited or we run into issues where crates can't publish
            // successfully because they're waiting on the index entries of
            // previously-published crates to propagate. This means we try to
            // publish in a loop and we remove crates once they're successfully
            // published. Failed-to-publish crates get enqueued for another try
            // later on.
            for _ in 0..5 {
                crates.retain(|krate| !publish(krate));

                if crates.is_empty() {
                    break;
                }

                println!(
                    "{} crates failed to publish, waiting for a bit to retry",
                    crates.len(),
                );
                thread::sleep(Duration::from_secs(20));
            }

            assert!(crates.is_empty(), "failed to publish all crates");

            println!("");
            println!("===================================================================");
            println!("");
            println!("Don't forget to push tags for this release!");
            println!("");
            println!("    $ git push --tags");
        }

        "diff" => {
            let krate = env::args().nth(2).unwrap();
            let krate = crates.iter().find(|c| c.name == krate).unwrap();
            Command::new("git")
                .arg("diff")
                .arg(format!("{}-{}..HEAD", krate.name, krate.version))
                .arg("--")
                .arg(krate.manifest.parent().unwrap())
                .exec();
        }

        s => panic!("unknown command: {}", s),
    }
}

// Recursively looks for `Cargo.toml` in `dir`
fn find_crates(dir: &Path, dst: &mut Vec<Crate>) {
    if dir.join("Cargo.toml").exists() {
        let krate = read_crate(&dir.join("Cargo.toml"));
        if !krate.publish || CRATES_TO_PUBLISH.iter().any(|c| krate.name == *c) {
            dst.push(krate);
        } else {
            panic!("failed to find {:?} in whitelist or blacklist", krate.name);
        }
    }

    for entry in dir.read_dir().unwrap() {
        let entry = entry.unwrap();
        if entry.file_type().unwrap().is_dir() {
            find_crates(&entry.path(), dst);
        }
    }
}

fn read_crate(manifest: &Path) -> Crate {
    let mut name = None;
    let mut version = None;
    let mut publish = true;
    for line in fs::read_to_string(manifest).unwrap().lines() {
        if name.is_none() && line.starts_with("name = \"") {
            name = Some(
                line.replace("name = \"", "")
                    .replace("\"", "")
                    .trim()
                    .to_string(),
            );
        }
        if version.is_none() && line.starts_with("version = \"") {
            version = Some(
                line.replace("version = \"", "")
                    .replace("\"", "")
                    .trim()
                    .to_string(),
            );
        }
        if line.starts_with("publish = false") {
            publish = false;
        }
    }
    let name = name.unwrap();
    let version = version.unwrap();
    Crate {
        manifest: manifest.to_path_buf(),
        name,
        version,
        publish,
        new_version: None,
    }
}

fn bump_version(krate: &mut Crate, crates: &mut [Crate], bumps: &[(String, bool)]) {
    let contents = fs::read_to_string(&krate.manifest).unwrap();

    let next_version = |target: &Crate| -> String {
        // If this crate is publishable then if it's explicitly requested on the
        // command line we bump the version. Otherwise we also force a version
        // bump if it's the same as this `krate` requested originally for this
        // function. This forced bump will be thrown away if no other
        // dependencies get updated.
        if CRATES_TO_PUBLISH.contains(&&target.name[..]) {
            if let Some((_, major)) = bumps.iter().find(|(s, _)| *s == target.name) {
                return bump(&target.version, !*major);
            }
            if target.name == krate.name {
                return bump(&target.version, true);
            }
        }

        // Prefer the `new_version`, if set, over the old version. The new
        // version will be updated by this point since crates are sorted
        // topologically.
        target.new_version.clone().unwrap_or(target.version.clone())
    };

    let mut new_manifest = String::new();
    let mut is_deps = false;
    let mut updated_deps = false;
    for line in contents.lines() {
        let mut rewritten = false;
        if !is_deps && line.starts_with("version =") {
            if CRATES_TO_PUBLISH.contains(&&krate.name[..]) {
                new_manifest.push_str(&line.replace(&krate.version, &next_version(krate)));
                rewritten = true;
            }
        }

        is_deps = if line.starts_with("[") {
            line.contains("dependencies")
        } else {
            is_deps
        };

        for other in crates.iter() {
            // If `other` isn't a published crate then it's not going to get a
            // bumped version so we don't need to update anything in the
            // manifest.
            if !other.publish {
                continue;
            }
            if !is_deps || !line.starts_with(&format!("{} ", other.name)) {
                continue;
            }
            if !line.contains(&other.version) {
                if !line.contains("version =") || !krate.publish {
                    continue;
                }
                panic!(
                    "{:?} has a dep on {} but doesn't list version {}",
                    krate.manifest, other.name, other.version
                );
            }
            let next = next_version(other);
            if next != other.version {
                rewritten = true;
                updated_deps = true;
                new_manifest.push_str(&line.replace(&other.version, &next));
            }
            break;
        }
        if !rewritten {
            new_manifest.push_str(line);
        }
        new_manifest.push_str("\n");
    }

    // Only actually rewrite the manifest if this crate was explicitly requested
    // to get bumped or one of its dependencies changed, otherwise nothing
    // changed about it.
    if updated_deps || bumps.iter().any(|(s, _)| *s == krate.name) {
        let new = next_version(krate);
        println!("bump `{}` {} => {}", krate.name, krate.version, new,);
        fs::write(&krate.manifest, new_manifest).unwrap();
        krate.new_version = Some(new);
    }
}

/// Performs a major version bump increment on the semver version `version`.
///
/// This function will perform a semver-major-version bump on the `version`
/// specified. This is used to calculate the next version of a crate in this
/// repository since we're currently making major version bumps for all our
/// releases. This may end up getting tweaked as we stabilize crates and start
/// doing more minor/patch releases, but for now this should do the trick.
fn bump(version: &str, patch_bump: bool) -> String {
    let mut iter = version.split('.').map(|s| s.parse::<u32>().unwrap());
    let major = iter.next().expect("major version");
    let minor = iter.next().expect("minor version");
    let patch = iter.next().expect("patch version");

    if patch_bump {
        return format!("{}.{}.{}", major, minor, patch + 1);
    }
    if major != 0 {
        format!("{}.0.0", major + 1)
    } else if minor != 0 {
        format!("0.{}.0", minor + 1)
    } else {
        format!("0.0.{}", patch + 1)
    }
}

fn publish(krate: &Crate) -> bool {
    if !CRATES_TO_PUBLISH.iter().any(|s| *s == krate.name) {
        return true;
    }

    // First make sure the crate isn't already published at this version. This
    // script may be re-run and there's no need to re-attempt previous work.
    let output = Command::new("curl")
        .arg(&format!("https://crates.io/api/v1/crates/{}", krate.name))
        .output()
        .expect("failed to invoke `curl`");
    if output.status.success()
        && String::from_utf8_lossy(&output.stdout)
            .contains(&format!("\"newest_version\":\"{}\"", krate.version))
    {
        println!(
            "skip publish {} because {} is latest version",
            krate.name, krate.version,
        );
        return true;
    }

    let status = Command::new("cargo")
        .arg("publish")
        .current_dir(krate.manifest.parent().unwrap())
        .status()
        .expect("failed to run cargo");
    if !status.success() {
        println!("FAIL: failed to publish `{}`: {}", krate.name, status);
        return false;
    }

    let status = Command::new("git")
        .arg("tag")
        .arg(format!("{}-{}", krate.name, krate.version))
        .status()
        .expect("failed to run git");
    if !status.success() {
        panic!("FAIL: failed to tag: {}", status);
    }

    // After we've published then make sure that the `wasmtime-publish` group is
    // added to this crate for future publications. If it's already present
    // though we can skip the `cargo owner` modification.
    let output = Command::new("curl")
        .arg(&format!(
            "https://crates.io/api/v1/crates/{}/owners",
            krate.name
        ))
        .output()
        .expect("failed to invoke `curl`");
    if output.status.success()
        && String::from_utf8_lossy(&output.stdout).contains("wasmtime-publish")
    {
        println!(
            "wasmtime-publish already listed as an owner of {}",
            krate.name
        );
        return true;
    }

    // Note that the status is ignored here. This fails most of the time because
    // the owner is already set and present, so we only want to add this to
    // crates which haven't previously been published.
    let status = Command::new("cargo")
        .arg("owner")
        .arg("-a")
        .arg("github:bytecodealliance:wasmtime-publish")
        .arg(&krate.name)
        .status()
        .expect("failed to run cargo");
    if !status.success() {
        panic!(
            "FAIL: failed to add wasmtime-publish as owner `{}`: {}",
            krate.name, status
        );
    }

    true
}
