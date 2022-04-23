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

%% -type data_rate() :: {0..15, atom()}.
-type dr_range() :: {integer(), integer()}.
-type dr_id() :: integer().
-type dr_atom() :: atom().
-type power_offset() :: integer().

-record(datarate_plan, {
    drlist :: [{number(), atom(), atom()}]
}).

-record(data_rate, {
    id :: 1..8,
    name :: atom(),
    max_size :: integer(),
    no_repeater_size :: integer(),
    bit_rate :: integer()
}).

-record(tx_power, {
    id :: 1..8,
    eirp :: integer()
}).

-record(channel_plan, {
    id :: 1..13,
    name :: atom(),
    region :: atom(),
    dynamic_plan :: boolean(),
    min_freq :: number(),
    max_freq :: number(),
    u_channels :: [float()],
    d_channels :: [float()],
    channel_count :: integer(),
    join_channels :: dr_range(),
    data_rates :: [dr_atom()],
    tx_power :: [power_offset()],
    join_dr :: dr_range(),
    mandatory_dr :: dr_range(),
    optional_dr :: dr_range(),
    max_duty_cycle :: integer(),
    dwell_time_limit :: integer(),
    tx_param_setup_allowed :: boolean(),
    max_eirp_db :: integer(),
    default_rx1_offset :: integer(),
    rx1_offset :: integer(),
    rx2_datarate :: integer(),
    rx2_freq :: float(),
    beacon_freq :: number(),
    pingslot_freq :: number()
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
