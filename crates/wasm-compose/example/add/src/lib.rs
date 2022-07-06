use interface::{Interface, Numbers};

struct Component;

impl Interface for Component {
    fn op(nums: Numbers) -> u32 {
        nums.x + nums.y
    }
}

interface::export!(Component);
