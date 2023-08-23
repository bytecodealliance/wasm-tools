use semver::Version;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct SerializableVersion(Version);

impl<'de> Deserialize<'de> for SerializableVersion {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Version::parse(&s)
            .map(SerializableVersion)
            .map_err(serde::de::Error::custom)
    }
}

impl Serialize for SerializableVersion {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // Assuming there's a method to convert Version to a string representation
        let s = self.0.to_string();
        serializer.serialize_str(&s)
    }
}

impl From<SerializableVersion> for Version {
    fn from(v: SerializableVersion) -> Self {
        v.0
    }
}

impl From<Version> for SerializableVersion {
    fn from(v: Version) -> Self {
        SerializableVersion(v)
    }
}

impl AsRef<Version> for SerializableVersion {
    fn as_ref(&self) -> &Version {
        &self.0
    }
}

impl AsMut<Version> for SerializableVersion {
    fn as_mut(&mut self) -> &mut Version {
        &mut self.0
    }
}

impl std::fmt::Display for SerializableVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.to_string().fmt(f)
    }
}
