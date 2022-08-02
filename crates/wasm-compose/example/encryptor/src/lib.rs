use interface::Interface;

const IV: &[u8] = &[1, 2, 3, 4, 5];
const KEY: &[u8] = &[
    0x73, 0x68, 0x68, 0x21, 0x20, 0x69, 0x74, 0x27, 0x73, 0x20, 0x61, 0x20, 0x73, 0x65, 0x63, 0x72,
    0x65, 0x74, 0x21,
];

struct Component;

impl Interface for Component {
    fn encrypt(data: Vec<u8>) -> Vec<u8> {
        encryption::encrypt(IV, KEY, &data)
    }
}

interface::export!(Component);
