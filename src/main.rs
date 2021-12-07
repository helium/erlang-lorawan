#![allow(dead_code, unused_imports)]

extern crate ndarray;

use bytes::Bytes;
use heapless;
use lorawan_encoding::{creator, keys, maccommands};
use ndarray::{arr2, Array, ArrayBase, Ix2};
//use heapless::Vec; // fixed capacity `std::Vec`
//use heapless::consts::U32; // type level integer used to specify capacity
use loramic::*;
use lorawan_encoding::creator::*;
use lorawan_encoding::default_crypto::DefaultFactory;
use lorawan_encoding::keys::*;
use lorawan_encoding::maccommandcreator::*;
use lorawan_encoding::maccommands::*;
use lorawan_encoding::parser::*;

fn long_data_payload() -> String {
    // some text from loremipsum.de with a typo at the end
    String::from(
        "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor \
            invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et \
            accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, not",
    )
}

fn phy_long_dataup_payload() -> Vec<u8> {
    let mut res = Vec::new();
    res.extend_from_slice(&[
        0x40, 0x04, 0x03, 0x02, 0x01, 0x00, 0x00, 0x00, 0x01, 0x27, 0x5a, 0xe9, 0x94, 0x2a, 0x58,
        0x32, 0x21, 0x48, 0xba, 0xd6, 0xca, 0x7d, 0x74, 0x6e, 0x77, 0x4a, 0xf8, 0x66, 0x7a, 0x7b,
        0x72, 0x36, 0x4b, 0xe4, 0xe1, 0x9d, 0x2f, 0x5c, 0x23, 0x98, 0x4f, 0xe2, 0x5e, 0x8e, 0x2d,
        0xdb, 0xd5, 0x15, 0xb5, 0x4e, 0xbe, 0x80, 0xce, 0xc2, 0x1c, 0xd6, 0x5a, 0x88, 0x13, 0x0f,
        0xbe, 0x6d, 0x04, 0xaa, 0xb2, 0xbc, 0x39, 0xab, 0xbe, 0xd9, 0xe8, 0x73, 0xef, 0xc7, 0x85,
        0xe5, 0x65, 0x5d, 0x62, 0x72, 0xf8, 0x79, 0x6b, 0x1e, 0x83, 0x9f, 0x2b, 0x1b, 0xde, 0xab,
        0xa2, 0x01, 0x6c, 0x7e, 0xf9, 0x16, 0x9d, 0x51, 0xf4, 0xea, 0x26, 0x1b, 0xc6, 0x08, 0x9c,
        0x83, 0xb3, 0x3c, 0x6f, 0x30, 0xa7, 0x3c, 0xe1, 0x3c, 0x52, 0x55, 0x7c, 0x46, 0xd7, 0x91,
        0xe7, 0xe0, 0x1b, 0x39, 0xe0, 0xb8, 0x9c, 0x1d, 0x2e, 0x35, 0x08, 0x84, 0x1b, 0x67, 0xe3,
        0xec, 0x88, 0x6f, 0x96, 0xeb, 0x0e, 0x11, 0x16, 0x40, 0xd3, 0xc1, 0x94, 0xf1, 0x21, 0x49,
        0xab, 0x58, 0x4b, 0xd9, 0x31, 0xdc, 0x15, 0xfc, 0x11, 0x94, 0x97, 0xdc, 0xcb, 0xf2, 0xb5,
        0xb9, 0x16, 0xb8, 0x52, 0x42, 0x96, 0x33, 0x41, 0xa5, 0x8b, 0xb5, 0x87, 0x7b, 0xd5, 0xaf,
        0x9e, 0xe4, 0x2d, 0x8b, 0x6f, 0x48, 0x45, 0x85, 0xa6, 0xf9, 0xcb, 0xaf, 0xf7, 0x2e, 0xe1,
        0x09, 0x42, 0xe1, 0x23, 0x8c, 0x98, 0xd7, 0xbf, 0xe7, 0xca, 0x0b, 0x2d, 0xb2, 0x24, 0x8d,
        0xb9, 0x1c, 0xd2, 0x3a, 0x71, 0xc6, 0xdb, 0x9b, 0x76, 0x8c, 0xf7, 0xef, 0x17, 0xf0, 0x51,
        0xcf, 0x42, 0x3e, 0x73, 0x47, 0x7a, 0xbc, 0x9b, 0x0f, 0xf0, 0x62, 0xde, 0x1e, 0x85, 0x20,
        0x29, 0x92, 0xdd, 0xca, 0x58, 0x37, 0x44, 0x19, 0x0c, 0x4f, 0xf7, 0xe1, 0xb4, 0x2e, 0xa3,
        0xcc,
    ]);
    res
}

fn main() {
    let mut phy = DataPayloadCreator::new();
    let nwk_skey = AES128([2; 16]);
    let app_skey = AES128([1; 16]);
    let fctrl = FCtrl::new(0x00, true);
    phy.set_confirmed(false)
        .set_uplink(true)
        .set_f_port(1)
        .set_dev_addr(&[4, 3, 2, 1])
        .set_fctrl(&fctrl) // all flags set to false
        .set_fcnt(0);

    assert_eq!(
        phy.build(
            &long_data_payload().into_bytes()[..],
            &[],
            &nwk_skey,
            &app_skey
        )
        .unwrap(),
        &phy_long_dataup_payload()[..]
    );

    // let mut phy = JoinRequestCreator::new();
    let _key = keys::AES128([1; 16]);
    let _fcnt = 1;

    // let app_nonce_bytes = [1; 3];
    // phy.set_app_nonce(&app_nonce_bytes);
    // phy.set_net_id(&[1; 3]);
    // phy.set_dev_addr(&[1; 4]);
    // phy.set_dl_settings(2);
    // phy.set_rx_delay(1);
    // let mut freqs: Vec<lorawan_encoding::maccommands::Frequency, 1024> = Vec::new();
    // freqs.push(maccommands::Frequency::new(&[0x58, 0x6e, 0x84,]).unwrap()).unwrap();
    // freqs.push(maccommands::Frequency::new(&[0x88, 0x66, 0x84,]).unwrap()).unwrap();
    // phy.set_c_f_list(freqs).unwrap();
    // let payload = phy.build(&key).unwrap();
    // println!("Payload: {:x?}", payload);

    println!("The value of devaddr001 is: {:#04X?}", 0);
    assert_eq!(0, 0x0);

    test_mic();
}

// type SegArray<D> = Array<u8, D>;
type SegArray = Array<u8, Ix2>;
type MsgArray = Array<u32, Ix2>;

#[allow(unused_imports)]
#[allow(unused_mut)]
#[allow(dead_code)]
fn mac_gen(key: u32, counter: u32, _message: Bytes) -> Bytes {
    println!("mac_gen key={:#04X?} counter={}", key, counter);
    Bytes::new()
}

#[allow(unused_imports)]
#[allow(unused_mut)]
#[allow(dead_code)]
// τi ← SegAgg(segArray)
fn seg_agg(segarray: SegArray) -> u8 {
    println!("segarray: len={}", segarray.len());
    let mut tag: u8 = 0;
    for item in &segarray {
        tag = tag ^ item;
    }
    assert_eq!(segarray.len(), 6);
    tag
}

#[allow(unused_imports)]
#[allow(unused_mut)]
#[allow(dead_code)]
// τi ← TagGen(k, i,mi)
fn tag_gen(key: u32, counter: u32, _message: Bytes) -> Bytes {
    println!("mac_gen key={:#04X?} counter={}", key, counter);
    Bytes::new()
}

#[allow(unused_imports)]
#[allow(unused_mut)]
#[allow(dead_code)]
// valid/invalid ← TagVerify(k, i,mi, τi)
fn tag_verify(key: u32, counter: u32, _message: Bytes, _tag: Bytes) -> bool {
    println!("mac_gen key={:#04X?} counter={}", key, counter);
    true
}

#[allow(unused_imports)]
#[allow(unused_mut)]
#[allow(dead_code)]
// msgArray′ ← MsgSpec(msgArray)
fn msg_spec(_msgarray: MsgArray) -> MsgArray {
    println!("The value of devaddr001 is: {:#04X?}", 0);
    let mut a2 = arr2(&[[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
    a2
}