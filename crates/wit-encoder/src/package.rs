#[derive(Clone)]
pub struct Package {
    pub name: PackageName,
    pub sources: SourceMap,
}

#[derive(Clone)]
pub struct PackageName {
    pub namespace: String,
    pub name: String,
    pub version: Option<Version>,
}
