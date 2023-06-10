use anyhow::Result;
use serde::{Deserialize, Serialize};
use spdx::Expression;
use std::borrow::Cow;
use std::mem;
use wasm_encoder::{ComponentSectionId, Encode, Section};
use wasmparser::{Parser, Payload::*};

const LICENSE_REF: &str = "LicenseRef-";

#[cfg_attr(feature = "clap", derive(clap::Parser))]
#[derive(Debug, Deserialize, Serialize, Default, PartialEq)]
pub struct RegistryMetadata {
    /// List of authors who has created this package.
    #[cfg_attr(feature = "clap", clap(short, long, value_name = "AUTHOR"))]
    #[serde(skip_serializing_if = "Option::is_none")]
    authors: Option<Vec<String>>,

    /// Package description in markdown format.
    #[cfg_attr(feature = "clap", clap(short, long, value_name = "DESCRIPTION"))]
    #[serde(skip_serializing_if = "Option::is_none")]
    description: Option<String>,

    /// SPDX License Expression
    /// https://spdx.github.io/spdx-spec/v2.3/SPDX-license-expressions/
    /// SPDX License List: https://spdx.org/licenses/
    #[cfg_attr(feature = "clap", clap(long, value_name = "LICENSE EXPRESSION"))]
    #[serde(skip_serializing_if = "Option::is_none")]
    license: Option<String>,

    /// A list of custom licenses that should be referenced to from the license expression.
    /// https://spdx.github.io/spdx-spec/v2.3/other-licensing-information-detected/
    #[cfg_attr(feature = "clap", clap(skip))]
    #[serde(skip_serializing_if = "Option::is_none")]
    custom_licenses: Option<Vec<CustomLicense>>,

    /// A list of links that can contain predefined link types or custom links for use with tooling or registries.
    #[cfg_attr(feature = "clap", clap(short, long, value_parser = parse_link, value_name="LINK=VALUE"))]
    #[serde(skip_serializing_if = "Option::is_none")]
    links: Option<Vec<Link>>,

    /// A list of categories that a package should be listed under when uploaded to a registry.
    #[cfg_attr(feature = "clap", clap(short, long, value_name = "CATEGORY"))]
    #[serde(skip_serializing_if = "Option::is_none")]
    categories: Option<Vec<String>>,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct Link {
    ty: LinkType,
    value: String,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub enum LinkType {
    Documentation,
    Homepage,
    Repository,
    Funding,
    Custom(String),
}

#[derive(Debug, Deserialize, Serialize, Default, Clone, PartialEq)]
pub struct CustomLicense {
    /// License Identifier
    /// Provides a locally unique identifier to refer to licenses that are not found on the SPDX License List.
    /// https://spdx.github.io/spdx-spec/v2.3/other-licensing-information-detected/#101-license-identifier-field
    id: String,

    /// License Name
    /// Provide a common name of the license that is not on the SPDX list.
    /// https://spdx.github.io/spdx-spec/v2.3/other-licensing-information-detected/#103-license-name-field
    name: String,

    /// Extracted Text
    /// Provides a copy of the actual text of the license reference extracted from the package or file that is associated with the License Identifier to aid in future analysis.
    /// https://spdx.github.io/spdx-spec/v2.3/other-licensing-information-detected/#102-extracted-text-field
    text: String,

    /// License Cross Reference
    /// Provides a pointer to the official source of a license that is not included in the SPDX License List, that is referenced by the License Identifier.
    /// https://spdx.github.io/spdx-spec/v2.3/other-licensing-information-detected/#104-license-cross-reference-field
    #[serde(skip_serializing_if = "Option::is_none")]
    reference: Option<String>,
}

#[cfg(feature = "clap")]
fn parse_link(s: &str) -> Result<Link> {
    s.split_once('=')
        .map(|(k, v)| {
            let ty = match k {
                "Documentation" => LinkType::Funding,
                "Homepage" => LinkType::Homepage,
                "Repository" => LinkType::Repository,
                "Funding" => LinkType::Funding,
                custom => LinkType::Custom(custom.to_owned()),
            };
            Link {
                ty,
                value: v.to_string(),
            }
        })
        .ok_or_else(|| anyhow::anyhow!("expected LINK=VALUE"))
}

impl RegistryMetadata {
    pub fn from_wasm(bytes: &[u8]) -> Result<Option<Self>> {
        let mut depth = 0;
        for payload in Parser::new(0).parse_all(bytes) {
            let payload = payload?;
            use wasmparser::Payload::*;
            match payload {
                ModuleSection { .. } | ComponentSection { .. } => depth += 1,
                End { .. } => depth -= 1,
                CustomSection(c) if c.name() == "registry-metadata" && depth == 0 => {
                    let registry: RegistryMetadata = serde_json::from_slice(&c.data())?;
                    return Ok(Some(registry));
                }
                _ => {}
            }
        }
        Ok(None)
    }

    pub fn validate(&self) -> Result<()> {
        let Some(license) = &self.license else {
            return Ok(())
        };

        let expression = Expression::parse(license)?;

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

                    if license_id.starts_with(LICENSE_REF) {
                        // Strip "LicenseRef-", convert to lowercase and then append
                        licenses.push(license_id[LICENSE_REF.len()..].to_lowercase());
                    }
                }
            }
        }

        match &self.custom_licenses {
            Some(custom_licenses) => {
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
                            "No matching reference for licence '{license}' was defined"
                        ));
                    }
                }

                Ok(())
            }
            None => Err(anyhow::anyhow!(
                "Reference to custom section exists but no custom sections was given"
            )),
        }
    }

    pub fn add_to_wasm(&self, input: &[u8]) -> Result<Vec<u8>> {
        rewrite_wasm(&self, input)
    }
}

fn rewrite_wasm(input_metadata: &RegistryMetadata, input: &[u8]) -> Result<Vec<u8>> {
    let mut stack = Vec::new();
    let mut output = Vec::new();

    for payload in Parser::new(0).parse_all(&input) {
        let payload = payload?;

        // Track nesting depth, so that we don't mess with inner registry-metadata sections:
        match payload {
            Version { encoding, .. } => {
                output.extend_from_slice(match encoding {
                    wasmparser::Encoding::Component => &wasm_encoder::Component::HEADER,
                    wasmparser::Encoding::Module => &wasm_encoder::Module::HEADER,
                });
            }
            ModuleSection { .. } | ComponentSection { .. } => {
                stack.push(mem::take(&mut output));
                continue;
            }
            End { .. } => {
                let mut parent = match stack.pop() {
                    Some(c) => c,
                    None => break,
                };
                if output.starts_with(&wasm_encoder::Component::HEADER) {
                    parent.push(ComponentSectionId::Component as u8);
                    output.encode(&mut parent);
                } else {
                    parent.push(ComponentSectionId::CoreModule as u8);
                    output.encode(&mut parent);
                }
                output = parent;
            }
            _ => {}
        }

        // Process the wasm sections:
        match payload {
            // Only rewrite the outermost registry-metadata section:
            CustomSection(c) if c.name() == "registry-metadata" && stack.len() == 0 => {
                // We do nothing here as we will overwrite it with the new registry metadata
            }

            // All other sections get passed through unmodified:
            _ => {
                if let Some((id, range)) = payload.as_section() {
                    wasm_encoder::RawSection {
                        id,
                        data: &input[range],
                    }
                    .append_to(&mut output);
                }
            }
        }
    }

    let registry_metadata = wasm_encoder::CustomSection {
        name: Cow::Borrowed("registry-metadata"),
        data: Cow::Owned(serde_json::to_vec(&input_metadata)?),
    };
    registry_metadata.append_to(&mut output);

    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn metadata_validation() {
        let metadata = RegistryMetadata {
            authors: Some(vec!["Foo Bar".to_owned()]),
            description: Some("description".to_owned()),
            license: Some("MIT OR LicenseRef-TESTLICENSE".to_owned()),
            custom_licenses: Some(vec![CustomLicense {
                id: "TESTLICENSE".to_owned(),
                name: "Test License".to_owned(),
                text: "This is a test license".to_owned(),
                reference: Some("http://test.com/license".to_owned()),
            }]),
            links: None,
            categories: None,
        };

        metadata.validate().unwrap();
    }

    #[test]
    fn read_write_registry_metadata() {
        let wat = "(module)";
        let bytes = wat::parse_str(wat).unwrap();

        let original_metadata = RegistryMetadata {
            authors: Some(vec!["Foo Bar".to_owned()]),
            description: Some("description".to_owned()),
            license: Some("MIT OR LicenseRef-TESTLICENSE".to_owned()),
            custom_licenses: Some(vec![CustomLicense {
                id: "TESTLICENSE".to_owned(),
                name: "Test License".to_owned(),
                text: "This is a test license".to_owned(),
                reference: Some("http://test.com/license".to_owned()),
            }]),
            links: None,
            categories: None,
        };

        let result = original_metadata.add_to_wasm(&bytes).unwrap();
        let read_metadata = RegistryMetadata::from_wasm(&result).unwrap().unwrap();

        assert_eq!(read_metadata, original_metadata);
    }

    #[test]
    fn overwrite_registry_metadata() {
        let wat = "(module)";
        let bytes = wat::parse_str(wat).unwrap();

        let original_metadata = RegistryMetadata {
            authors: Some(vec!["Foo Bar".to_owned()]),
            description: Some("description".to_owned()),
            license: Some("MIT OR LicenseRef-TESTLICENSE".to_owned()),
            custom_licenses: Some(vec![CustomLicense {
                id: "TESTLICENSE".to_owned(),
                name: "Test License".to_owned(),
                text: "This is a test license".to_owned(),
                reference: Some("http://test.com/license".to_owned()),
            }]),
            links: None,
            categories: None,
        };

        let bytes = original_metadata.add_to_wasm(&bytes).unwrap();

        let new_metadata = RegistryMetadata {
            authors: Some(vec!["Foo Bar Baz".to_owned()]),
            description: Some("description2".to_owned()),
            license: Some("MIT OR LicenseRef-TESTLICENSE2".to_owned()),
            custom_licenses: Some(vec![CustomLicense {
                id: "TESTLICENSE2".to_owned(),
                name: "Test License2".to_owned(),
                text: "This is a test license2".to_owned(),
                reference: Some("http://test.com/license2".to_owned()),
            }]),
            links: None,
            categories: None,
        };

        let result = new_metadata.add_to_wasm(&bytes).unwrap();
        let read_metadata = RegistryMetadata::from_wasm(&result).unwrap().unwrap();

        assert_eq!(read_metadata, new_metadata);
    }
}
