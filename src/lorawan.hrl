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
    mtype,
    devaddr,
    fctrlbits,
    fcnt,
    fopts = [],
    fport,
    data
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

-define(to_record(Record, Object, Default),
    list_to_tuple([Record | [maps:get(X, Object, Default) || X <- record_info(fields, Record)]])
).

-define(to_record(Record, Object), ?to_record(Record, Object, undefined)).

-define(to_map(Record, RecData),
    maps:from_list(
        lists:filtermap(
            fun
                ({_K, D, D}) -> false;
                ({K, V, _D}) -> {true, {K, V}}
            end,
            lists:zip3(
                record_info(fields, Record),
                lorawan_db:record_fields(RecData),
                tl(tuple_to_list(#Record{}))
            )
        )
    )
).

% end of file
