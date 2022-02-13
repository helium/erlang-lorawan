%% lorawan message types
-define(JOIN_REQUEST, 2#000).
-define(JOIN_REQ, 2#000).
-define(JOIN_ACCEPT, 2#001).
-define(CONFIRMED_UP, 2#100).
-define(UNCONFIRMED_UP, 2#010).
-define(CONFIRMED_DOWN, 2#101).
-define(UNCONFIRMED_DOWN, 2#011).
-define(RFU, 2#110).
-define(PRIORITY, 2#111).

-define(RX_DELAY, 0).
-define(FRAME_TIMEOUT, 200).
-define(JOIN_TIMEOUT, 2000).

-record(frame, {
    mtype :: 0..7,
    rfu :: 0..7,
    major :: 0..3,
    devaddr :: binary(),
    fctrlbits :: 0..256,
    fcnt :: integer(),
    fopts = [],
    fport :: 0..256,
    data :: binary()
}).

-record(stat, {
    time,
    lati,
    long,
    alti,
    rxnb,
    rxok,
    rxfw,
    ackr,
    dwnb,
    txnb,
    % TTN extensions
    mail,
    desc
}).

% end of file
