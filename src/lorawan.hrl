%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%

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

-record(frame, {conf, devaddr, adr, adr_ack_req, ack, fcnt, fopts, port, data}).

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

-define(REALM, <<"lorawan-server">>).

-record(config, {
    name :: nonempty_string(),
    admin_url :: string(),
    items_per_page :: integer(),
    slack_token :: 'undefined' | string(),
    email_from :: 'undefined' | string(),
    email_server :: 'undefined' | string(),
    email_user :: 'undefined' | string(),
    email_password :: 'undefined' | string()
}).

-record(user, {
    name :: nonempty_string(),
    pass_ha1 :: string(),
    scopes :: [string()],
    email :: string(),
    send_alerts :: boolean()
}).

-record(server, {
    sname :: atom(),
    router_perf :: [{calendar:datetime(), {integer(), integer()}}]
}).

-record(event, {
    evid :: binary(),
    severity :: atom(),
    first_rx :: calendar:datetime(),
    last_rx :: calendar:datetime(),
    count :: integer(),
    entity :: atom(),
    eid :: binary(),
    text :: binary(),
    args :: 'undefined' | binary()
}).

% end of file
