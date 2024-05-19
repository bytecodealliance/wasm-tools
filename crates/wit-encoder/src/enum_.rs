use crate::EnumCase;

/// A variant without a payload
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Enum {
    pub(crate) cases: Vec<EnumCase>,
}

impl Enum {
    pub fn cases(&self) -> &[EnumCase] {
        &self.cases
    }

    pub fn cases_mut(&mut self) -> &mut Vec<EnumCase> {
        &mut self.cases
    }
}
