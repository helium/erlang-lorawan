use aes::Aes128;
use cmac::{Cmac, Mac, NewMac};
//use generic_array::{ArrayLength, GenericArray};

pub fn create_mac() -> () {
	// Create `Mac` trait implementation, namely CMAC-AES128
	let mut mac = Cmac::<Aes128>::new_from_slice(b"very secret key.").unwrap();
	mac.update(b"input message");

	// `result` has type `Output` which is a thin wrapper around array of
	// bytes for providing constant time equality check
	let result = mac.finalize();
	// To get underlying array use the `into_bytes` method, but be careful,
	// since incorrect use of the tag value may permit timing attacks which
	// defeat the security provided by the `Output` wrapper
	let tag_bytes = result.into_bytes();
	
	let mut mac = Cmac::<Aes128>::new_from_slice(b"very secret key.").unwrap();

	mac.update(b"input message");

	// `verify` will return `Ok(())` if tag is correct, `Err(MacError)` otherwise
	mac.verify(&tag_bytes).unwrap()
}