use core::fmt;
use core::pin::Pin;
use core::task::{Context, Poll};

use futures::{AsyncRead, AsyncWrite};
use hex::FromHex;
use serde::ser::SerializeStruct;
use serde::{de, Deserialize, Serialize};
use sha2::{Digest as _, Sha256, Sha512};

/// A resource digest
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Digest {
    /// Sha256 digest of a resource
    pub sha256: [u8; 32],
    /// Sha512 digest of a resource
    pub sha512: [u8; 64],
}

impl<'de> Deserialize<'de> for Digest {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        const FIELDS: [&str; 2] = ["sha256", "sha512"];

        struct Visitor;
        impl<'de> de::Visitor<'de> for Visitor {
            type Value = Digest;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("a resource digest")
            }

            fn visit_map<V>(self, mut map: V) -> Result<Self::Value, V::Error>
            where
                V: de::MapAccess<'de>,
            {
                let mut sha256 = None;
                let mut sha512 = None;
                while let Some((k, v)) = map.next_entry::<String, String>()? {
                    match k.as_ref() {
                        "sha256" => {
                            if sha256.is_some() {
                                return Err(de::Error::duplicate_field("sha256"));
                            }
                            sha256 = FromHex::from_hex(v).map(Some).map_err(|e| {
                                de::Error::custom(format!("invalid `sha256` field value: {e}"))
                            })?;
                        }
                        "sha512" => {
                            if sha512.is_some() {
                                return Err(de::Error::duplicate_field("sha512"));
                            }
                            sha512 = FromHex::from_hex(v).map(Some).map_err(|e| {
                                de::Error::custom(format!("invalid `sha512` field value: {e}"))
                            })?;
                        }
                        k => return Err(de::Error::unknown_field(k, &FIELDS)),
                    }
                }
                let sha256 = sha256.ok_or_else(|| de::Error::missing_field("sha256"))?;
                let sha512 = sha512.ok_or_else(|| de::Error::missing_field("sha512"))?;
                Ok(Digest { sha256, sha512 })
            }
        }
        deserializer.deserialize_struct("Entry", &FIELDS, Visitor)
    }
}

impl Serialize for Digest {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("Digest", 2)?;
        state.serialize_field("sha256", &hex::encode(self.sha256))?;
        state.serialize_field("sha512", &hex::encode(self.sha512))?;
        state.end()
    }
}

/// A reader wrapper, which hashes the bytes read
pub struct Reader<T> {
    reader: T,
    sha256: Sha256,
    sha512: Sha512,
}

impl<T: AsyncRead + Unpin> AsyncRead for Reader<T> {
    fn poll_read(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut [u8],
    ) -> Poll<std::io::Result<usize>> {
        Pin::new(&mut self.reader).poll_read(cx, buf).map_ok(|n| {
            self.sha256.update(&buf[..n]);
            self.sha512.update(&buf[..n]);
            n
        })
    }
}

impl<T> From<T> for Reader<T> {
    fn from(reader: T) -> Self {
        Self {
            reader,
            sha256: Sha256::new(),
            sha512: Sha512::new(),
        }
    }
}

impl<T> From<Reader<T>> for Digest {
    fn from(hashed: Reader<T>) -> Self {
        let sha256 = hashed.sha256.finalize().into();
        let sha512 = hashed.sha512.finalize().into();
        Self { sha256, sha512 }
    }
}

/// A writer wrapper, which hashes the bytes written
pub struct Writer<T> {
    writer: T,
    sha256: Sha256,
    sha512: Sha512,
}

impl<T: AsyncWrite + Unpin> AsyncWrite for Writer<T> {
    fn poll_write(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<std::io::Result<usize>> {
        Pin::new(&mut self.writer).poll_write(cx, buf).map_ok(|n| {
            self.sha256.update(&buf[..n]);
            self.sha512.update(&buf[..n]);
            n
        })
    }

    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::io::Result<()>> {
        Pin::new(&mut self.writer).poll_flush(cx)
    }

    fn poll_close(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::io::Result<()>> {
        Pin::new(&mut self.writer).poll_close(cx)
    }
}

impl<T> From<T> for Writer<T> {
    fn from(writer: T) -> Self {
        Self {
            writer,
            sha256: Sha256::new(),
            sha512: Sha512::new(),
        }
    }
}

impl<T> From<Writer<T>> for Digest {
    fn from(hashed: Writer<T>) -> Self {
        let sha256 = hashed.sha256.finalize().into();
        let sha512 = hashed.sha512.finalize().into();
        Self { sha256, sha512 }
    }
}
