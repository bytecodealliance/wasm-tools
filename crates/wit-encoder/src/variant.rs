use crate::VariantCase;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant {
    pub(crate) cases: Vec<VariantCase>,
}

impl Variant {
    pub fn cases(&self) -> &[VariantCase] {
        &self.cases
    }

    pub fn cases_mut(&mut self) -> &mut Vec<VariantCase> {
        &mut self.cases
    }
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
