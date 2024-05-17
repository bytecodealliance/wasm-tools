use crate::VariantCase;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Variant {
    pub cases: Vec<VariantCase>,
}

impl<I, C> From<I> for Variant
where
    I: IntoIterator<Item = C>,
    C: Into<VariantCase>,
{
    fn from(value: I) -> Self {
        Self {
            cases: value.into_iter().map(|c| c.into()).collect(),
        }
    }
}
