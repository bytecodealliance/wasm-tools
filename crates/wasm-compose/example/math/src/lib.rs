use import::Numbers;
use interface::Interface;

struct Component;

impl Interface for Component {
    fn execute() -> u32 {
        import::op(Numbers { x: 21649, y: 21661})
    }
}

interface::export!(Component);
