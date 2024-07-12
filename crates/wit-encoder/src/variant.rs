use crate::VariantCase;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Variant {
    pub(crate) cases: Vec<VariantCase>,
}

impl Variant {
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn case(&mut self, case: impl Into<VariantCase>) {
        self.cases.push(case.into());
    }

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
