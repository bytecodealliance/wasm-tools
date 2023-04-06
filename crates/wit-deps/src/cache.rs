use std::path::{Path, PathBuf};

use anyhow::{bail, Context as _};
use async_trait::async_trait;
use futures::io::BufReader;
use futures::{AsyncBufRead, AsyncWrite};
use tokio::fs::{self, File, OpenOptions};
use tokio_util::compat::{Compat, TokioAsyncReadCompatExt};
use url::{Host, Url};

/// Resource caching layer
#[async_trait]
pub trait Cache {
    /// Type returned by the [Self::get] method
    type Read: AsyncBufRead + Unpin;
    /// Type returned by the [Self::insert] method
    type Write: AsyncWrite + Unpin;

    /// Returns an read handle for the entry from the cache associated with a given url
    async fn get(&self, url: &Url) -> anyhow::Result<Option<Self::Read>>;

    /// Returns a write handle for the entry associated with a given url
    async fn insert(&self, url: &Url) -> anyhow::Result<Self::Write>;
}

/// Local caching layer
#[derive(Clone, Debug)]
pub struct Local<'a>(&'a Path);

impl Local<'_> {
    fn path(&self, url: &Url) -> impl AsRef<Path> {
        let mut path = PathBuf::from(self.0);
        match url.host() {
            Some(Host::Ipv4(ip)) => {
                path.push(ip.to_string());
            }
            Some(Host::Ipv6(ip)) => {
                path.push(ip.to_string());
            }
            Some(Host::Domain(domain)) => {
                path.push(domain);
            }
            None => {}
        }
        if let Some(segments) = url.path_segments() {
            for seg in segments {
                path.push(seg);
            }
        }
        path
    }
}

#[async_trait]
impl Cache for Local<'_> {
    type Read = BufReader<Compat<File>>;
    type Write = Compat<File>;

    async fn get(&self, url: &Url) -> anyhow::Result<Option<Self::Read>> {
        match File::open(self.path(url)).await {
            Ok(file) => Ok(Some(BufReader::new(file.compat()))),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None),
            Err(e) => bail!("failed to lookup `{url}` in cache: {e}"),
        }
    }

    async fn insert(&self, url: &Url) -> anyhow::Result<Self::Write> {
        let path = self.path(url);
        if let Some(parent) = path.as_ref().parent() {
            fs::create_dir_all(parent)
                .await
                .context("failed to create directory")?;
        }
        OpenOptions::new()
            .create_new(true)
            .write(true)
            .open(path)
            .await
            .map(TokioAsyncReadCompatExt::compat)
            .context("failed to open file for writing")
    }
}

impl<'a> From<&'a Path> for Local<'a> {
    fn from(path: &'a Path) -> Self {
        Self(path)
    }
}

impl<'a> From<&'a str> for Local<'a> {
    fn from(path: &'a str) -> Self {
        Self::from(Path::new(path))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn local_path() {
        assert_eq!(
            Local::from("test")
                .path(
                    &"https://example.com/foo/bar.tar.gz"
                        .parse()
                        .expect("failed to parse URL")
                )
                .as_ref(),
            Path::new("test")
                .join("example.com")
                .join("foo")
                .join("bar.tar.gz")
        );
    }
}
