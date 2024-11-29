use anyhow::Result;
use serde_derive::{Deserialize, Serialize};
use spdx::Expression;
use std::fmt;
use std::fmt::Display;
use wasmparser::Parser;

use crate::{rewrite_wasm, Producers};

/// Metadata used by a Warg registry
#[derive(Debug, Deserialize, Serialize, Clone, Default, PartialEq)]
pub struct RegistryMetadata {
    /// List of authors who has created this package.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub authors: Option<Vec<String>>,

    /// Package description in markdown format.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,

    /// SPDX License Expression
    /// <https://spdx.github.io/spdx-spec/v2.3/SPDX-license-expressions/>
    /// SPDX License List: <https://spdx.org/licenses/>
    #[serde(skip_serializing_if = "Option::is_none")]
    pub license: Option<String>,

    /// A list of custom licenses that should be referenced to from the license expression.
    /// <https://spdx.github.io/spdx-spec/v2.3/other-licensing-information-detected/>
    #[serde(skip_serializing_if = "Option::is_none")]
    pub custom_licenses: Option<Vec<CustomLicense>>,

    /// A list of links that can contain predefined link types or custom links for use with tooling or registries.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub links: Option<Vec<Link>>,

    /// A list of categories that a package should be listed under when uploaded to a registry.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub categories: Option<Vec<String>>,
}

const LICENSE_REF: &str = "LicenseRef-";

impl RegistryMetadata {
    /// Merge into an existing wasm module. Rewrites the module with this registry-metadata section
    /// overwriting its existing one, or adds this registry-metadata section if none is present.
    pub fn add_to_wasm(&self, input: &[u8]) -> Result<Vec<u8>> {
        rewrite_wasm(&None, &Producers::empty(), Some(&self), input)
    }

    /// Parse a Wasm binary and extract the `Registry` section, if there is any.
    pub fn from_wasm(bytes: &[u8]) -> Result<Option<Self>> {
        let mut depth = 0;
        for payload in Parser::new(0).parse_all(bytes) {
            let payload = payload?;
            use wasmparser::Payload::*;
            match payload {
                ModuleSection { .. } | ComponentSection { .. } => depth += 1,
                End { .. } => depth -= 1,
                CustomSection(c) if c.name() == "registry-metadata" && depth == 0 => {
                    let registry = RegistryMetadata::from_bytes(&c.data(), 0)?;
                    return Ok(Some(registry));
                }
                _ => {}
            }
        }
        Ok(None)
    }

    /// Gets the registry-matadata from a slice of bytes
    pub fn from_bytes(bytes: &[u8], offset: usize) -> Result<Self> {
        let registry: RegistryMetadata = serde_json::from_slice(&bytes[offset..])?;
        return Ok(registry);
    }

    /// Validate the `RegistryMetadata::license` an
    /// `RegistryMetadata::CustomLicense` are valid SPDX expressions.
    pub fn validate(&self) -> Result<()> {
        fn validate_expression(expression: &str) -> Result<Vec<String>> {
            let expression = Expression::parse(expression)?;

            let mut licenses = Vec::new();

            for license in expression.iter() {
                match license {
                    spdx::expression::ExprNode::Op(_) => continue,
                    spdx::expression::ExprNode::Req(req) => {
                        if let spdx::LicenseItem::Spdx { .. } = req.req.license {
                            // Continue if it's a license that exists on the Spdx license list
                            continue;
                        }

                        let license_id = req.req.to_string();

                        // Strip "LicenseRef-", convert to lowercase and then append
                        if let Some(id) = license_id.strip_prefix(LICENSE_REF) {
                            licenses.push(id.to_lowercase());
                        }
                    }
                }
            }

            Ok(licenses)
        }

        match (&self.license, &self.custom_licenses) {
            (None, Some(custom_licenses)) => {
                let ids = custom_licenses
                    .iter()
                    .map(|license| license.id.clone())
                    .collect::<Vec<String>>()
                    .join(", ");

                return Err(anyhow::anyhow!(
                    "{ids} are defined but nevered referenced in license expression"
                ));
            }
            (Some(license), Some(custom_licenses)) => {
                let licenses = validate_expression(license.as_str())?;

                if !licenses.is_empty() {
                    for license in &licenses {
                        let mut match_found = false;
                        for custom_license in custom_licenses {
                            // Ignore license id casing
                            if custom_license.id.to_lowercase() == *license {
                                match_found = true;
                            }
                        }

                        if !match_found {
                            return Err(anyhow::anyhow!(
                                "No matching reference for license '{license}' was defined"
                            ));
                        }
                    }
                }
            }
            (Some(license), None) => {
                let licenses = validate_expression(license.as_str())?;

                if !licenses.is_empty() {
                    return Err(anyhow::anyhow!(
                        "Reference to custom license exists but no custom license was given"
                    ));
                }
            }
            (None, None) => {}
        }

        Ok(())
    }

    /// Get authors
    pub fn get_authors(&self) -> Option<&Vec<String>> {
        self.authors.as_ref()
    }

    /// Set authors
    pub fn set_authors(&mut self, authors: Option<Vec<String>>) {
        self.authors = authors;
    }

    /// Get description
    pub fn get_description(&self) -> Option<&String> {
        self.description.as_ref()
    }

    /// Set description
    pub fn set_description(&mut self, description: Option<String>) {
        self.description = description;
    }

    /// Get license
    pub fn get_license(&self) -> Option<&String> {
        self.license.as_ref()
    }

    /// Set license
    pub fn set_license(&mut self, license: Option<String>) {
        self.license = license;
    }

    /// Get custom_licenses
    pub fn get_custom_licenses(&self) -> Option<&Vec<CustomLicense>> {
        self.custom_licenses.as_ref()
    }

    /// Set custom_licenses
    pub fn set_custom_licenses(&mut self, custom_licenses: Option<Vec<CustomLicense>>) {
        self.custom_licenses = custom_licenses;
    }

    /// Get links
    pub fn get_links(&self) -> Option<&Vec<Link>> {
        self.links.as_ref()
    }

    /// Set links
    pub fn set_links(&mut self, links: Option<Vec<Link>>) {
        self.links = links;
    }

    /// Get categories
    pub fn get_categories(&self) -> Option<&Vec<String>> {
        self.categories.as_ref()
    }

    /// Set categories
    pub fn set_categories(&mut self, categories: Option<Vec<String>>) {
        self.categories = categories;
    }

    pub(crate) fn display(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let spaces = std::iter::repeat(" ").take(indent).collect::<String>();

        if let Some(authors) = &self.authors {
            writeln!(f, "{spaces}authors:")?;
            for author in authors {
                writeln!(f, "{spaces}    {author}")?;
            }
        }

        if let Some(license) = &self.license {
            writeln!(f, "{spaces}license:")?;
            writeln!(f, "{spaces}    {license}")?;
        }

        if let Some(links) = &self.links {
            writeln!(f, "{spaces}links:")?;
            for link in links {
                writeln!(f, "{spaces}    {link}")?;
            }
        }

        if let Some(categories) = &self.categories {
            writeln!(f, "{spaces}categories:")?;
            for category in categories {
                writeln!(f, "{spaces}    {category}")?;
            }
        }

        if let Some(description) = &self.description {
            writeln!(f, "{spaces}description:")?;
            writeln!(f, "{spaces}    {description}")?;
        }

        if let Some(custom_licenses) = &self.custom_licenses {
            writeln!(f, "{spaces}custom_licenses:")?;
            for license in custom_licenses {
                license.display(f, indent + 4)?;
            }
        }

        Ok(())
    }
}

impl Display for RegistryMetadata {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display(f, 0)
    }
}

/// A link from the registry to an external website
#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct Link {
    /// The type of link
    pub ty: LinkType,
    /// The address of the link
    pub value: String,
}

impl Display for Link {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.ty, self.value)
    }
}

/// What kind of link is this?
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub enum LinkType {
    /// Documentation
    Documentation,
    /// Homepage
    Homepage,
    /// Code repository
    Repository,
    /// Funding and donations
    Funding,
    /// Some other link type
    #[serde(untagged)]
    Custom(String),
}

impl Display for LinkType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            LinkType::Documentation => "Documentation",
            LinkType::Homepage => "Homepage",
            LinkType::Repository => "Repository",
            LinkType::Funding => "Funding",
            LinkType::Custom(s) => s.as_str(),
        };

        write!(f, "{s}")
    }
}

/// A human readable short form license identifier for a license not on the SPDX
/// License List.
#[derive(Debug, Deserialize, Serialize, Default, Clone, PartialEq)]
pub struct CustomLicense {
    /// License Identifier
    /// Provides a locally unique identifier to refer to licenses that are not found on the SPDX License List.
    /// <https://spdx.github.io/spdx-spec/v2.3/other-licensing-information-detected/#101-license-identifier-field>
    pub id: String,

    /// License Name
    /// Provide a common name of the license that is not on the SPDX list.
    /// <https://spdx.github.io/spdx-spec/v2.3/other-licensing-information-detected/#103-license-name-field>
    pub name: String,

    /// Extracted Text
    /// Provides a copy of the actual text of the license reference extracted from the package or file that is associated with the License Identifier to aid in future analysis.
    /// <https://spdx.github.io/spdx-spec/v2.3/other-licensing-information-detected/#102-extracted-text-field>
    pub text: String,

    /// License Cross Reference
    /// Provides a pointer to the official source of a license that is not included in the SPDX License List, that is referenced by the License Identifier.
    /// <https://spdx.github.io/spdx-spec/v2.3/other-licensing-information-detected/#104-license-cross-reference-field>
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reference: Option<String>,
}

impl CustomLicense {
    fn display(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let spaces = std::iter::repeat(" ").take(indent).collect::<String>();

        writeln!(f, "{spaces}{}:", self.id)?;
        writeln!(f, "{spaces}    name: {}", self.name)?;

        if let Some(reference) = &self.reference {
            writeln!(f, "{spaces}    reference: {reference}")?;
        }

        writeln!(f, "{spaces}    text:")?;
        writeln!(f, "{spaces}        {}", self.text)?;

        Ok(())
    }
}

impl Display for CustomLicense {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display(f, 0)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::Metadata;
    use wasm_encoder::Module;

    #[test]
    fn overwrite_registry_metadata() {
        let module = Module::new().finish();
        let registry_metadata = RegistryMetadata {
            authors: Some(vec!["Foo".to_owned()]),
            ..Default::default()
        };
        let module = registry_metadata.add_to_wasm(&module).unwrap();

        let registry_metadata = RegistryMetadata {
            authors: Some(vec!["Bar".to_owned()]),
            ..Default::default()
        };
        let module = registry_metadata.add_to_wasm(&module).unwrap();

        let metadata = Metadata::from_binary(&module).unwrap();
        match metadata {
            Metadata::Module {
                registry_metadata, ..
            } => {
                let registry_metadata = registry_metadata.expect("some registry_metadata");
                assert_eq!(registry_metadata.authors.unwrap(), vec!["Bar".to_owned()]);
            }
            _ => panic!("metadata should be module"),
        }
    }
}
