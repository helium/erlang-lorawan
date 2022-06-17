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
-type data_rate() :: atom() | binary() | integer().

-record(channel_plan, {
    %% ID from the Region spec
    channel_plan_id :: 1..13,
    %% base_region: EU868, US915, AS923, etc.
    base_region :: atom(),
    %% plan_name: EU868_A, US915_SB2, AS923_1A, AS923_1B, etc.
    plan_name :: atom(),
    dynamic_plan :: boolean(),
    float_precision :: integer(),
    min_freq :: number(),
    max_freq :: number(),
    u_channels :: [float()],
    d_channels :: [float()],
    channel_count :: integer(),
    bank_offset :: integer(),
    join_channels :: dr_range(),
    data_rates :: [dr_atom()],
    tx_power :: [power_offset()],
    join_dr :: dr_range(),
    mask_dr :: dr_range(),
    mandatory_dr :: dr_range(),
    optional_dr :: dr_range(),
    max_duty_cycle :: integer(),
    uplink_dwell_time :: integer(),
    downlink_dwell_time :: integer(),
    tx_param_setup_allowed :: boolean(),
    max_eirp_db :: integer(),
    rx1_offset :: dr_range(),
    rx2_datarate :: integer(),
    rx2_freq :: float(),
    beacon_freq :: number(),
    pingslot_freq :: number()
}).

% end of file
