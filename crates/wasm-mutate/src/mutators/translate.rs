#[derive(Debug, Hash, Eq, PartialEq, Copy, Clone)]
pub enum Item {
    Function,
    Table,
    Memory,
    Tag,
    Global,
    Type,
    Data,
    Element,
}
