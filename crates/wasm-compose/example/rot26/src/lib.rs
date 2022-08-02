use encryption::Encryption;
use std::str;

struct Component;

impl Encryption for Component {
    fn encrypt(iv: Vec<u8>, key: Vec<u8>, plain_text: Vec<u8>) -> Vec<u8> {
        // Where we're going, we don't need initialization vectors or keys...
        let _ = (iv, key);
        rot26::encrypt(str::from_utf8(&plain_text).unwrap()).into_bytes()
    }
}

encryption::export!(Component);
