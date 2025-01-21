use wasmtime::*;

#[derive(Clone)]
pub struct StoreLimits {
    pub remaining_memory: usize,
    pub oom: bool,
}

impl StoreLimits {
    fn alloc(&mut self, amt: usize) -> bool {
        match self.remaining_memory.checked_sub(amt) {
            Some(mem) => {
                self.remaining_memory = mem;
                true
            }
            None => {
                self.oom = true;
                false
            }
        }
    }
}

impl ResourceLimiter for StoreLimits {
    fn memory_growing(
        &mut self,
        current: usize,
        desired: usize,
        _maximum: Option<usize>,
    ) -> Result<bool> {
        Ok(self.alloc(desired - current))
    }

    fn table_growing(
        &mut self,
        current: usize,
        desired: usize,
        _maximum: Option<usize>,
    ) -> Result<bool> {
        let delta = (desired - current) * std::mem::size_of::<usize>();
        Ok(self.alloc(delta))
    }
}
