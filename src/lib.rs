use aes::Aes128;
use array2d::Array2D;
use cmac::{Cmac, Mac, NewMac};

use std::collections::HashMap;

//use generic_array::{ArrayLength, GenericArray};

pub fn test_mac() -> () {
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

    let mut mac2 = Cmac::<Aes128>::new_from_slice(b"very secret key.").unwrap();

    mac2.update(b"input message");

    // `verify` will return `Ok(())` if tag is correct, `Err(MacError)` otherwise
    mac2.verify(&tag_bytes).unwrap()
}

pub fn create_test_mac(fcnt: u16) -> &'static [u16] {
    let mut mac = Cmac::<Aes128>::new_from_slice(b"very secret key.").unwrap();
    let bytes = fcnt.to_be_bytes();
    mac.update(&bytes);
    let result = mac.finalize();
    let tag_bytes = result.into_bytes();
    println!("create_test_mac: tag_bytes.len()={}", tag_bytes.len());
    let array_u8: [u8; 16] = tag_bytes.as_slice().try_into().expect("Wrong length");
    unsafe {
        let (_prefix, shorts, _suffix) = array_u8.align_to::<u16>();
        let array_u16 = &shorts[0..8];
        let boxed_data: Box<[u16]> = array_u16.iter().cloned().collect();
        println!("create_test_mac: fcnt={} mac={:?}", fcnt, boxed_data);
        Box::leak(boxed_data)
    }
}

type CuMICState = array2d::Array2D<u16>;
type CuMIC = u16;

pub fn zero_state() -> CuMICState {
    let array = Array2D::fill_with(0u16, 8, 8);
    assert_eq!(array.get(0, 0), Some(&0));
    assert_eq!(array.get(7, 7), Some(&0));
    array
}

pub fn mutate_state(fcnt: u16, mic: &[u16], state: CuMICState) -> (CuMICState, u16) {
    let row: usize = (fcnt % 8) as usize;
    let mut array = state;
    for col in 0..8 {
        array[(row, col)] = mic[col];
    }
    println!(
        "mutate_state: fcnt={} mic={:?} array={:?}",
        fcnt, mic, array
    );
    let array_column_major = array.as_column_major();
    let array2 = Array2D::from_column_major(&array_column_major, 8, 8);
    let tag = generate_tag(fcnt, array2);
    (array, tag)
}

pub fn generate_tag(fcnt: u16, state: CuMICState) -> CuMIC {
    let mut tag: u16 = 0;
    let col: usize = (fcnt % 8) as usize;
    for element in state.column_iter(col) {
        tag = tag ^ element;
    }
    println!("generate_tag: fcnt={} tag={:?}", fcnt, tag);
    tag
}

fn mic_loop() {
    let mut tag_set = HashMap::new();
    let mut state: CuMICState = zero_state();
    for i in 0..100 {
        let mac0 = create_test_mac(i);
        let (state1, mic) = mutate_state(i, mac0, state);
        let result = tag_set.insert(mic, mic);
        // We expect (with high probability) the mic to never repeat
        assert_eq!(result, None);
        state = state1;
    }
}

pub fn test_mic() {
    println!("test_mic");
    let state0 = zero_state();
    let mut fcnt = 0;
    let mac0 = create_test_mac(fcnt);
    let (state1, _mic0) = mutate_state(fcnt, mac0, state0);
    //let _mic0 = generate_tag(fcnt, state1);
    // end-device puts mic0 into 16-bit mic
    // LNS mirrors the process (with the same key) and gets the same mic0
    fcnt = fcnt + 1;
    let mac1 = create_test_mac(fcnt);
    let (state2, _mic1) = mutate_state(fcnt, mac1, state1);
    //let _mic1 = generate_tag(fcnt, state2);
    fcnt = fcnt + 1;
    let mac2 = create_test_mac(fcnt);
    let (state3, _mic2) = mutate_state(fcnt, mac2, state2);
    //let _mic2 = generate_tag(fcnt, state3);
    fcnt = fcnt + 1;
    let mac3 = create_test_mac(fcnt);
    let (_state4, _mic3) = mutate_state(fcnt, mac3, state3);

    mic_loop();
}
