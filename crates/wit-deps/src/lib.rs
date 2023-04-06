//! `wit-deps` core

#![forbid(clippy::unwrap_used)]
#![warn(clippy::pedantic)]
#![warn(missing_docs)]

mod cache;
mod digest;
mod lock;
mod manifest;

pub use cache::{Cache, Local as LocalCache};
pub use digest::{Digest, Reader as DigestReader, Writer as DigestWriter};
pub use lock::{Entry as LockEntry, Lock};
pub use manifest::{Entry as ManifestEntry, Manifest};

use std::collections::BTreeSet;
use std::ffi::OsStr;
use std::path::Path;

use anyhow::{bail, Context};
use directories::ProjectDirs;
use futures::{AsyncRead, AsyncWrite, TryStreamExt};
use tokio::fs;
use tokio_stream::wrappers::ReadDirStream;
use tracing::debug;

/// WIT dependency identifier
pub type Identifier = String;
// TODO: Introduce a rich type with name validation
//#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq)]
//pub struct Identifier(String);

fn is_wit(path: impl AsRef<Path>) -> bool {
    path.as_ref()
        .extension()
        .map(|ext| ext.eq_ignore_ascii_case("wit"))
        .unwrap_or_default()
}

/// Unpacks all WIT interfaces found within `wit` subtree of a tar archive read from `tar` to `dst`
///
/// # Errors
///
/// Returns and error if the operation fails
pub async fn untar(tar: impl AsyncRead + Unpin, dst: impl AsRef<Path>) -> anyhow::Result<()> {
    let dst = dst.as_ref();

    match fs::remove_dir_all(dst).await {
        Ok(()) => {}
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
        Err(e) => bail!("failed to remove `{}`: {e}", dst.display()),
    };
    fs::create_dir_all(dst)
        .await
        .with_context(|| format!("failed to create `{}`", dst.display()))?;

    async_tar::Archive::new(tar)
        .entries()
        .context("failed to unpack archive metadata")?
        .try_for_each(|mut e| async move {
            let path = e.path()?;
            let mut path = path.into_iter().map(OsStr::to_str);
            match (path.next(), path.next(), path.next(), path.next()) {
                (Some(Some("wit")), Some(Some(name)), None, None)
                | (Some(_), Some(Some("wit")), Some(Some(name)), None)
                    if is_wit(name) =>
                {
                    e.unpack(dst.join(name)).await?;
                    Ok(())
                }
                _ => Ok(()),
            }
        })
        .await
        .context("failed to unpack archive")?;
    Ok(())
}

/// Packages path into a `wit` subtree in deterministic `tar` archive and writes it to `dst`.
///
/// # Errors
///
/// Returns and error if the operation fails
pub async fn tar<T>(path: impl AsRef<Path>, dst: T) -> std::io::Result<T>
where
    T: AsyncWrite + Sync + Send + Unpin,
{
    let path = path.as_ref();
    let mut tar = async_tar::Builder::new(dst);
    tar.mode(async_tar::HeaderMode::Deterministic);
    let names = fs::read_dir(path)
        .await
        .map(ReadDirStream::new)?
        .try_filter_map(|e| async move {
            let name = e.file_name();
            if !is_wit(&name) {
                return Ok(None);
            }
            if e.file_type().await?.is_dir() {
                return Ok(None);
            }
            Ok(Some(name))
        })
        .try_collect::<BTreeSet<_>>()
        .await?;
    for name in names {
        tar.append_path_with_name(path.join(&name), Path::new("wit").join(name))
            .await?;
    }
    tar.into_inner().await
}

/// Given a TOML-encoded manifest and optional TOML-encoded lock, ensures that the path pointed to by
/// `deps` is in sync with the manifest and lock. This is a potentially destructive operation!
/// Returns a lock if the lock passed to this function was either `None` or out-of-sync.
///
/// # Errors
///
/// Returns an error if anything in the pipeline fails
pub async fn lock(
    manifest: impl AsRef<str>,
    lock: Option<impl AsRef<str>>,
    deps: impl AsRef<Path>,
    packages: impl IntoIterator<Item = &Identifier>,
) -> anyhow::Result<Option<String>> {
    let manifest: Manifest =
        toml::from_str(manifest.as_ref()).context("failed to decode manifest")?;

    let old_lock = lock
        .as_ref()
        .map(AsRef::as_ref)
        .map(toml::from_str)
        .transpose()
        .context("failed to decode lock")?;

    let dirs = ProjectDirs::from("", "", env!("CARGO_PKG_NAME"));
    let cache = dirs.as_ref().map(ProjectDirs::cache_dir).map(|cache| {
        debug!("using cache at `{}`", cache.display());
        LocalCache::from(cache)
    });

    let deps = deps.as_ref();
    let lock = manifest
        .lock(deps, old_lock.as_ref(), cache.as_ref(), packages)
        .await
        .with_context(|| format!("failed to lock deps to `{}`", deps.display()))?;

    match old_lock {
        Some(old_lock) if lock == old_lock => Ok(None),
        _ => {
            let lock = toml::to_string(&lock).context("failed to encode lock")?;
            Ok(Some(lock))
        }
    }
}

/// Like [lock](self::lock()), but reads and writes the lock under path specified in `lock`.
///
/// Returns `true` if the lock was updated and `false` otherwise.
///
/// # Errors
///
/// Returns an error if anything in the pipeline fails
pub async fn lock_path(
    manifest: impl AsRef<str>,
    lock_path: impl AsRef<Path>,
    deps: impl AsRef<Path>,
    packages: impl IntoIterator<Item = &Identifier>,
) -> anyhow::Result<bool> {
    let lock_path = lock_path.as_ref();
    let lock = match fs::read_to_string(&lock_path).await {
        Ok(lock) => Some(lock),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => None,
        Err(e) => bail!("failed to read lock at `{}`: {e}", lock_path.display()),
    };
    if let Some(lock) = self::lock(manifest, lock, deps, packages)
        .await
        .context("failed to lock dependencies")?
    {
        if let Some(parent) = lock_path.parent() {
            fs::create_dir_all(parent).await.with_context(|| {
                format!(
                    "failed to create lock parent directory `{}`",
                    parent.display()
                )
            })?;
        }
        fs::write(&lock_path, &lock)
            .await
            .with_context(|| format!("failed to write lock to `{}`", lock_path.display()))?;
        Ok(true)
    } else {
        Ok(false)
    }
}

/// Ensure dependency manifest, lock and dependencies are in sync
#[macro_export]
macro_rules! lock {
    () => {
        $crate::lock_path(
            include_str!("wit/deps.toml"),
            "wit/deps.lock",
            "wit/deps",
            None,
        )
    };
}
