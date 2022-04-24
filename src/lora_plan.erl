-module(lora_plan).

-export([
    freq_to_channel/2,
    channel_to_freq/2,
    tx_power/2,
    tx_power_list/1,
    tx_power_table/1,
    region_to_plan/1,
    rx2_datarate/1,
    rx2_tuple/1,
    max_payload_size/1,
    datarate_to_index/2,
    index_to_datarate/2,
    datarate_to_atom/1,
    atom_to_datarate/1,
    downlink_eirp/2,
    max_uplink_snr/1,
    rx_window/5,
    join1_window/3,
    join2_window/2,
    rx1_window/4,
    rx2_window/3,
    rx1_or_rx2_window/4
]).

-include("lora.hrl").

-spec region_to_plan(atom()) -> #channel_plan{}.
region_to_plan(Region) ->
    case Region of
        'EU868' -> plan_eu868();
        'US915' -> plan_us915();
        'AU915' -> plan_au915();
        'CN470' -> plan_cn470();
        'KR920' -> plan_kr920();
        'IN865' -> plan_in865();
        'AS923' -> plan_as923_1();
        'AS923_1' -> plan_as923_1();
        'AS923_2' -> plan_as923_1();
        'AS923_3' -> plan_as923_1();
        'AS923_4' -> plan_as923_1()
    end.

-spec datarate_to_atom(binary()) -> atom().
datarate_to_atom(Binary) ->
    case Binary of
        <<"SF12BW125">> -> 'SF12BW125';
        <<"SF11BW125">> -> 'SF11BW125';
        <<"SF10BW125">> -> 'SF10BW125';
        <<"SF9BW125">> -> 'SF9BW125';
        <<"SF8BW125">> -> 'SF8BW125';
        <<"SF7BW125">> -> 'SF7BW125';
        <<"SF12BW250">> -> 'SF12BW250';
        <<"SF11BW250">> -> 'SF11BW250';
        <<"SF10BW250">> -> 'SF10BW250';
        <<"SF9BW250">> -> 'SF9BW250';
        <<"SF8BW250">> -> 'SF8BW250';
        <<"SF7BW250">> -> 'SF7BW250';
        <<"SF12BW500">> -> 'SF12BW500';
        <<"SF11BW500">> -> 'SF11BW500';
        <<"SF10BW500">> -> 'SF10BW500';
        <<"SF9BW500">> -> 'SF9BW500';
        <<"SF8BW500">> -> 'SF8BW500';
        <<"SF7BW500">> -> 'SF7BW500';
        <<"LRFHSS1BW137">> -> 'LRFHSS1BW137';
        <<"LRFHSS2BW137">> -> 'LRFHSS2BW137';
        <<"LRFHSS1BW336">> -> 'LRFHSS1BW336';
        <<"LRFHSS2BW336">> -> 'LRFHSS2BW336';
        <<"LRFHSS1BW1523">> -> 'LRFHSS1BW1523';
        <<"LRFHSS2BW1523">> -> 'LRFHSS2BW1523';
        <<"FSK50">> -> 'FSK50';
        <<"RFU">> -> 'RFU';
        _ -> 'RFU'
    end.

-spec datarate_to_tuple(atom()) -> {integer(), integer()}.
datarate_to_tuple(DataRate) ->
    case DataRate of
        'SF12BW125' -> {12, 125};
        'SF11BW125' -> {11, 125};
        'SF10BW125' -> {10, 125};
        'SF9BW125' -> {9, 125};
        'SF8BW125' -> {8, 125};
        'SF7BW125' -> {7, 125};
        'SF12BW250' -> {12, 250};
        'SF11BW250' -> {11, 250};
        'SF10BW250' -> {10, 250};
        'SF9BW250' -> {9, 250};
        'SF8BW250' -> {8, 250};
        'SF7BW250' -> {7, 250};
        'SF12BW500' -> {12, 500};
        'SF11BW500' -> {11, 500};
        'SF10BW500' -> {10, 500};
        'SF9BW500' -> {9, 500};
        'SF8BW500' -> {8, 500};
        'SF7BW500' -> {7, 500};
        'LRFHSS1BW137' -> {7, 125};
        'LRFHSS2BW137' -> {7, 125};
        'LRFHSS1BW336' -> {7, 125};
        'LRFHSS2BW336' -> {7, 125};
        'LRFHSS1BW1523' -> {7, 125};
        'LRFHSS2BW1523' -> {7, 125};
        'FSK50' -> {7, 125};
        'RFU' -> {7, 125};
        _ -> {7, 125}
    end.

-spec atom_to_datarate(atom()) -> binary().
atom_to_datarate(Atom) ->
    atom_to_binary(Atom).

%% ------------------------------------------------------------------
%% Receive Window Functions
%% ------------------------------------------------------------------

-record(rxq, {
    freq :: number(),
    datr :: binary() | integer(),
    codr :: binary(),
    time :: calendar:datetime(),
    tmms :: integer(),
    %% for future use
    reserved :: any(),
    rssi :: number(),
    lsnr :: number()
}).

-record(txq, {
    freq :: number(),
    datr :: binary() | integer(),
    codr :: binary(),
    time :: integer() | 'immediately' | calendar:datetime(),
    powe :: 'undefined' | integer()
}).

-define(JOIN1_WINDOW, join1_window).
-define(JOIN2_WINDOW, join2_window).
-define(RX1_WINDOW, rx1_window).
-define(RX2_WINDOW, rx2_window).

-type window() :: ?JOIN1_WINDOW | ?JOIN2_WINDOW | ?RX1_WINDOW | ?RX2_WINDOW.

new_txq() ->
    #txq{
        freq = 923.3,
        datr = <<"SF12BW125">>,
        codr = <<"hello">>,
        time = 0
    }.

-spec join1_window(#channel_plan{}, integer(), #rxq{}) -> #txq{}.
join1_window(Plan, DelaySeconds, RxQ) ->
    _Region = Plan#channel_plan.region,
    TxQ = new_txq(),
    tx_window(?JOIN1_WINDOW, RxQ, TxQ, DelaySeconds).

-spec join2_window(#channel_plan{}, #rxq{}) -> #txq{}.
join2_window(Plan, RxQ) ->
    _Region = Plan#channel_plan.region,
    TxQ = new_txq(),
    tx_window(?JOIN2_WINDOW, RxQ, TxQ).

-spec rx1_window(#channel_plan{}, number(), number(), #rxq{}) -> #txq{}.
rx1_window(Plan, DelaySeconds, _Offset, RxQ) ->
    _Region = Plan#channel_plan.region,
    TxQ = new_txq(),
    tx_window(?RX1_WINDOW, RxQ, TxQ, DelaySeconds).

-spec rx2_window(#channel_plan{}, number(), #rxq{}) -> #txq{}.
rx2_window(Plan, DelaySeconds, RxQ) ->
    _Region = Plan#channel_plan.region,
    TxQ = new_txq(),
    tx_window(?RX2_WINDOW, RxQ, TxQ, DelaySeconds).

-spec rx1_or_rx2_window(#channel_plan{}, number(), number(), #rxq{}) -> #txq{}.
rx1_or_rx2_window(Plan, Delay, Offset, RxQ) ->
    Region = Plan#channel_plan.region,
    case Region of
        'EU868' ->
            if
                % In Europe the RX Windows uses different frequencies,
                % TX power rules and Duty cycle rules.  If the signal is
                % poor then prefer window 2 where TX power is higher.
                % See - https://github.com/helium/router/issues/423
                RxQ#rxq.rssi < -80 -> rx2_window(Plan, Delay, RxQ);
                true -> rx1_window(Plan, Delay, Offset, RxQ)
            end;
        _ ->
            rx1_window(Plan, Delay, Offset, RxQ)
    end.

-spec tx_window(window(), #rxq{}, #txq{}) -> #txq{}.
tx_window(Window, #rxq{tmms = Stamp} = Rxq, TxQ) when is_integer(Stamp) ->
    tx_window(Window, Rxq, TxQ, 0).

%% LoRaWAN Link Layer v1.0.4 spec, Section 5.7 Setting Delay between TX and RX,
%% Table 45 and "RX2 always opens 1s after RX1."
-spec tx_window(atom(), #rxq{}, #txq{}, number()) -> #txq{}.
tx_window(?JOIN1_WINDOW, #rxq{tmms = Stamp}, TxQ, _RxDelaySeconds) when is_integer(Stamp) ->
    Delay = get_window(?JOIN1_WINDOW),
    TxQ#txq{time = Stamp + Delay};
tx_window(?JOIN2_WINDOW, #rxq{tmms = Stamp}, TxQ, _RxDelaySeconds) when is_integer(Stamp) ->
    Delay = get_window(?JOIN2_WINDOW),
    TxQ#txq{time = Stamp + Delay};
tx_window(Window, #rxq{tmms = Stamp}, TxQ, RxDelaySeconds) when is_integer(Stamp) ->
    %% TODO check if the time is a datetime, which would imply gps timebase
    Delay =
        case RxDelaySeconds of
            N when N < 2 ->
                get_window(Window);
            N ->
                case Window of
                    ?RX2_WINDOW ->
                        N * 1000000 + 1000000;
                    _ ->
                        N * 1000000
                end
        end,
    TxQ#txq{time = Stamp + Delay}.

%% These only specify LoRaWAN default values; see also tx_window()
-spec get_window(window()) -> number().
get_window(?JOIN1_WINDOW) -> 5000000;
get_window(?JOIN2_WINDOW) -> 6000000;
get_window(?RX1_WINDOW) -> 1000000;
get_window(?RX2_WINDOW) -> 2000000.

-spec rx_window(atom(), #channel_plan{}, non_neg_integer(), number(), #rxq{}) ->
    {number() | float(), atom(), integer() | 'immediately' | calendar:datetime()}.
rx_window(_Type, Plan, _RxDelay, _Offset, _RxQ) ->
    _Region = Plan#channel_plan.region,
    TxTime = calendar:now_to_datetime(os:timestamp()),
    {0, 'SF8BW125', TxTime}.

%% ------------------------------------------------------------------
%% DataRate Functions
%% ------------------------------------------------------------------

-spec datarate_to_index(#channel_plan{}, atom()) -> integer().
datarate_to_index(Plan, Atom) ->
    List = (Plan#channel_plan.data_rates),
    Index = index_of(Atom, List),
    Index.

-spec index_to_datarate(#channel_plan{}, integer()) -> atom().
index_to_datarate(Plan, Index) ->
    List = (Plan#channel_plan.data_rates),
    DR = lists:nth(Index, List),
    DR.

-spec max_uplink_snr(atom()) -> float().
max_uplink_snr(DataRate) ->
    {SF, _} = datarate_to_tuple(DataRate),
    max_snr(SF).

%% from SX1272 DataSheet, Table 13
max_snr(SF) ->
    %% dB
    -5 - 2.5 * (SF - 6).

-spec max_payload_size(integer()) -> integer().
max_payload_size(_DataRateID) ->
    250.

-spec rx2_datarate(#channel_plan{}) -> integer().
rx2_datarate(Plan) ->
    Plan#channel_plan.rx2_datarate.

-spec rx2_tuple(#channel_plan{}) -> {float(), atom()}.
rx2_tuple(Plan) ->
    RX2_Freq = Plan#channel_plan.rx2_freq,
    DRIndex = Plan#channel_plan.rx2_datarate,
    List = (Plan#channel_plan.data_rates),
    DRAtom = index_of(DRIndex, List),
    {RX2_Freq, DRAtom}.

-spec freq_to_channel(#channel_plan{}, number()) -> integer().
freq_to_channel(Plan, Freq) ->
    List = (Plan#channel_plan.u_channels),
    Channel = index_of(Freq, List),
    Channel.

-spec channel_to_freq(#channel_plan{}, integer()) -> number().
channel_to_freq(Plan, Ch) ->
    List = (Plan#channel_plan.u_channels),
    Freq = lists:nth(Ch, List),
    Freq.

-spec downlink_eirp(#channel_plan{}, float()) -> integer().
downlink_eirp(Plan, Freq) ->
    Region = (Plan#channel_plan.region),
    case Region of
        'EU868' ->
            BeaconRange = 869.4 =< Freq andalso Freq < 869.65,
            case BeaconRange of
                true -> 27;
                false -> (Plan#channel_plan.max_eirp_db)
            end;
        _ ->
            (Plan#channel_plan.max_eirp_db)
    end.

-spec tx_power(#channel_plan{}, integer()) -> pos_integer().
tx_power(Plan, Index) when Index < 16 ->
    List = (Plan#channel_plan.tx_power),
    Offset = lists:nth(Index, List),
    ComputedPower = Plan#channel_plan.max_eirp_db + Offset,
    ComputedPower.

-spec tx_power_list(#channel_plan{}) -> [pos_integer()].
tx_power_list(Plan) ->
    List = (Plan#channel_plan.tx_power),
    [Plan#channel_plan.max_eirp_db + Offset || Offset <- List].

-type tx_power_table_entry() :: {Index :: pos_integer(), DBm :: number()}.
%% A tuple of `{TableIndex, dBm}'.
-type tx_power_table() :: list(tx_power_table_entry()).
%% A table of available transmit powers, specific to a region.

-spec tx_power_table(#channel_plan{}) -> tx_power_table().
tx_power_table(Plan) ->
    TxPowers = tx_power_list(Plan),
    Len = length(TxPowers),
    IList = lists:seq(0, Len - 1),
    TList = lists:zip(IList, TxPowers),
    TList.

index_of(Value, List) ->
    Map = lists:zip(List, lists:seq(1, length(List))),
    case lists:keyfind(Value, 1, Map) of
        {Value, Index} -> Index;
        false -> notfound
    end.

plan_eu868() ->
    EU868 = #channel_plan{
        id = 1,
        name = 'EU868',
        region = 'EU868',
        dynamic_plan = true,
        min_freq = 863.0,
        max_freq = 870.0,
        %% channels = [867.1, 867.3, 867.5, 867.7, 867.9, 868.1, 868.3, 868.5],
        u_channels = [868.1, 868.3, 868.5, 864.3, 864.5, 864.7, 864.9, 865.1],
        d_channels = [868.1, 868.3, 868.5, 868.7, 868.9, 869.1, 869.3, 869.5],
        channel_count = 8,
        join_channels = {0, 2},
        data_rates = [
            'SF12BW125',
            'SF11BW125',
            'SF10BW125',
            'SF9BW125',
            'SF8BW125',
            'SF7BW125',
            'SF7BW250',
            'FSK50',
            'LRFHSS1BW137',
            'LRFHSS2BW137',
            'LRFHSS1BW336',
            'LRFHSS2BW336'
        ],
        % tx_power = [0,-2,-4,-6,-8,-10,-12,-14],
        % 16,14,12,10,8,6,4,2
        % ToDo: Current setting are wrong...
        tx_power = [0, -6, -9, -12, -15, -18],
        join_dr = {0, 5},
        mandatory_dr = {0, 5},
        optional_dr = {6, 7},
        max_duty_cycle = 1,
        dwell_time_limit = 0,
        tx_param_setup_allowed = false,
        % max_eirp_db = 16,
        max_eirp_db = 20,
        default_rx1_offset = 0,
        rx1_offset = 5,
        rx2_datarate = 0,
        rx2_freq = 869.525,
        beacon_freq = 869.525,
        pingslot_freq = 869.525
    },
    EU868.

plan_kr920() ->
    Plan = #channel_plan{
        id = 1,
        name = 'KR920',
        region = 'KR920',
        dynamic_plan = true,
        min_freq = 920.9,
        max_freq = 923.3,
        u_channels = [922.1, 922.3, 922.5, 921.5, 921.7, 921.9, 922.1, 922.3],
        d_channels = [920.9, 921.1, 921.3, 921.5, 921.7, 921.9, 922.1, 922.3],
        channel_count = 8,
        join_channels = {0, 2},
        data_rates = [
            'SF12BW125',
            'SF11BW125',
            'SF10BW125',
            'SF9BW125',
            'SF8BW125',
            'SF7BW125'
        ],
        tx_power = [0, -2, -4, -6, -8, -10, -12],
        join_dr = {0, 5},
        mandatory_dr = {0, 5},
        optional_dr = {0, 0},
        max_duty_cycle = 1,
        dwell_time_limit = 0,
        tx_param_setup_allowed = false,
        max_eirp_db = 14,
        default_rx1_offset = 0,
        rx1_offset = 5,
        rx2_datarate = 0,
        rx2_freq = 921.9,
        beacon_freq = 923.1,
        pingslot_freq = 923.1
    },
    Plan.

plan_as923_1() ->
    Plan = #channel_plan{
        id = 1,
        name = 'AS923_1',
        region = 'AS923_1',
        dynamic_plan = true,
        min_freq = 915.0,
        max_freq = 928.0,
        %% channels = [867.1, 867.3, 867.5, 867.7, 867.9, 868.1, 868.3, 868.5],
        u_channels = [923.2, 923.4, 923.6, 923.8, 924.0, 924.2, 924.4, 924.6],
        d_channels = [923.2, 923.4, 923.6, 923.8, 924.0, 924.2, 924.4, 924.6],
        channel_count = 8,
        join_channels = {0, 1},
        data_rates = [
            'SF12BW125',
            'SF11BW125',
            'SF10BW125',
            'SF9BW125',
            'SF8BW125',
            'SF7BW125',
            'SF7BW250',
            'FSK50',
            'LRFHSS1BW137',
            'LRFHSS2BW137',
            'LRFHSS1BW336',
            'LRFHSS2BW336'
        ],
        tx_power = [0, -2, -4, -6, -8, -10, -12, -14],
        join_dr = {2, 5},
        mandatory_dr = {0, 5},
        optional_dr = {6, 7},
        max_duty_cycle = 1,
        dwell_time_limit = 400,
        tx_param_setup_allowed = true,
        max_eirp_db = 16,
        default_rx1_offset = 0,
        rx1_offset = 7,
        rx2_datarate = 2,
        rx2_freq = 923.2,
        beacon_freq = 923.4,
        pingslot_freq = 923.4
    },
    Plan.

plan_au915() ->
    Plan = #channel_plan{
        id = 1,
        name = 'AU915',
        region = 'AU915',
        dynamic_plan = false,
        min_freq = 915.0,
        max_freq = 928.0,
        %% channels = [867.1, 867.3, 867.5, 867.7, 867.9, 868.1, 868.3, 868.5],
        u_channels = [915.2, 915.4, 915.6, 915.8, 916.0, 916.2, 916.4, 916.6],
        d_channels = [923.3, 923.9, 924.5, 925.1, 925.7, 926.3, 926.9, 927.5],
        channel_count = 8,
        join_channels = {0, 7},
        data_rates = [
            'SF12BW125',
            'SF11BW125',
            'SF10BW125',
            'SF9BW125',
            'SF8BW125',
            'SF7BW125',
            'SF8BW500',
            'LRFHSS1BW1523',
            'SF12BW500',
            'SF11BW500',
            'SF10BW500',
            'SF9BW500',
            'SF8BW500',
            'SF8BW500',
            'SF7BW500'
        ],
        %% tx_power = [0,-2,-4,-6,-8,-10,-12,-14,-16,-18,-20,-22,-24,-26,-28,0],
        tx_power = [0, -2, -4, -6, -8, -10, -12, -14, -16, -18, -20],
        join_dr = {2, 5},
        mandatory_dr = {0, 6},
        optional_dr = {7, 7},
        max_duty_cycle = 1,
        dwell_time_limit = 400,
        tx_param_setup_allowed = true,
        max_eirp_db = 30,
        default_rx1_offset = 0,
        rx1_offset = 5,
        rx2_datarate = 8,
        rx2_freq = 923.2,
        beacon_freq = 923.4,
        pingslot_freq = 923.4
    },
    Plan.

plan_us915() ->
    Plan = #channel_plan{
        id = 2,
        name = 'US915',
        region = 'US915',
        dynamic_plan = false,
        min_freq = 902.0,
        max_freq = 928.0,
        %% channels = [867.1, 867.3, 867.5, 867.7, 867.9, 868.1, 868.3, 868.5],
        %% u_channels = [903.9, 904.1, 904.3, 904.5, 904.7, 904.9, 905.1, 905.3],
        u_channels = [902.3, 902.5, 902.7, 902.9, 903.1, 903.3, 903.5, 903.7],
        d_channels = [923.3, 923.9, 924.5, 925.1, 925.7, 926.3, 926.9, 927.5],
        channel_count = 8,
        join_channels = {0, 7},
        data_rates = [
            'SF10BW125',
            'SF9BW125',
            'SF8BW125',
            'SF7BW125',
            'SF8BW500',
            'LRFHSS1BW1523',
            'LRFHSS2BW1523',
            'RFU',
            'SF12BW500',
            'SF11BW500',
            'SF10BW500',
            'SF9BW500',
            'SF8BW500',
            'SF8BW500'
        ],
        %% tx_power = [0,-2,-4,-6,-8,-10,-12,-14,-16,-18,-20,-22,-24,-26,-28,0],
        tx_power = [0, -2, -4, -6, -8, -10, -12, -14, -16, -18, -20],
        join_dr = {2, 5},
        mandatory_dr = {0, 4},
        optional_dr = {5, 6},
        max_duty_cycle = 10000,
        dwell_time_limit = 400,
        tx_param_setup_allowed = false,
        max_eirp_db = 30,
        default_rx1_offset = 0,
        rx1_offset = 3,
        rx2_datarate = 8,
        rx2_freq = 923.3,
        beacon_freq = 923.3,
        pingslot_freq = 923.3
    },
    Plan.

plan_in865() ->
    Plan = #channel_plan{
        id = 1,
        name = 'IN865',
        region = 'IN865',
        dynamic_plan = true,
        min_freq = 865.0,
        max_freq = 867.0,
        u_channels = [865.0625, 865.4025, 865.985, 866.3, 866.4, 866.5, 866.6, 866.7],
        d_channels = [866.0, 866.1, 866.2, 866.3, 866.4, 866.5, 866.6, 866.7],
        channel_count = 8,
        join_channels = {0, 2},
        data_rates = [
            'SF12BW125',
            'SF11BW125',
            'SF10BW125',
            'SF9BW125',
            'SF8BW125',
            'SF7BW125',
            'RFU',
            'FSK50'
        ],
        tx_power = [0, -2, -4, -6, -8, -10, -12, -14, -16, -18, -20],
        join_dr = {0, 5},
        mandatory_dr = {0, 5},
        optional_dr = {7, 7},
        max_duty_cycle = 1,
        dwell_time_limit = 0,
        tx_param_setup_allowed = false,
        max_eirp_db = 30,
        default_rx1_offset = 0,
        rx1_offset = 7,
        rx2_datarate = 2,
        rx2_freq = 866.550,
        beacon_freq = 866.550,
        pingslot_freq = 866.550
    },
    Plan.

plan_cn470() ->
    Plan = #channel_plan{
        id = 1,
        name = 'CN470',
        region = 'CN470',
        dynamic_plan = true,
        min_freq = 470.0,
        max_freq = 510.0,
        u_channels = [470.3, 470.5, 470.7, 470.9, 471.1, 471.3, 471.5, 471.7],
        d_channels = [500.3, 500.5, 500.7, 500.9, 501.1, 501.3, 501.5, 501.7],
        channel_count = 8,
        join_channels = {0, 2},
        data_rates = [
            'SF12BW125',
            'SF11BW125',
            'SF10BW125',
            'SF9BW125',
            'SF8BW125',
            'SF7BW125'
        ],
        tx_power = [0, -2, -4, -6, -8, -10, -12, -14],
        join_dr = {0, 5},
        mandatory_dr = {0, 5},
        optional_dr = {7, 7},
        max_duty_cycle = 1,
        dwell_time_limit = 0,
        tx_param_setup_allowed = false,
        max_eirp_db = 19,
        default_rx1_offset = 0,
        rx1_offset = 7,
        rx2_datarate = 0,
        rx2_freq = 505.3,
        beacon_freq = 508.3,
        pingslot_freq = 508.3
    },
    Plan.

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

validate_u_channels(Region, List) ->
    TList = [
        lora_region:uch2f(Region, F)
     || F <- [0, 1, 2, 3, 4, 5, 6, 7]
    ],
    ?assertEqual(List, TList).

validate_d_channels(Region, List) ->
    TList = [
        lora_region:dch2f(Region, F)
     || F <- [0, 1, 2, 3, 4, 5, 6, 7]
    ],
    ?assertEqual(List, TList).

%%-spec validate_u_frequences(atom(), [float()]) -> any().
validate_u_frequences('EU868', List) ->
    TList = [
        lora_region:f2uch('EU868', F)
     || F <- List
    ],
    ?assertEqual([0, 1, 2, -11, -10, -9, -8, -7], TList);
validate_u_frequences('CN470', List) ->
    TList = [
        lora_region:f2uch('CN470', F)
     || F <- List
    ],
    ?assertEqual([315, 316, 317, 318, 319, 320, 321, 322], TList);
validate_u_frequences('KR920', List) ->
    TList = [
        lora_region:f2uch('KR920', F)
     || F <- List
    ],
    ?assertEqual([0, 1, 2, -3, -2, -1, 0, 1], TList);
validate_u_frequences(Region, List) ->
    TList = [
        lora_region:f2uch(Region, F)
     || F <- List
    ],
    ?assertEqual([0, 1, 2, 3, 4, 5, 6, 7], TList).

%%-spec validate_d_frequences(atom(), [float()]) -> any().
validate_d_frequences('CN470', List) ->
    TList = [
        lora_region:f2dch('CN470', F)
     || F <- List
    ],
    ?assertEqual([465, 466, 467, 468, 469, 470, 471, 472], TList);
validate_d_frequences('KR920', List) ->
    TList = [
        lora_region:f2dch('KR920', F)
     || F <- List
    ],
    ?assertEqual([-6, -5, -4, -3, -2, -1, 0, 1], TList);
validate_d_frequences(Region, List) ->
    TList = [
        lora_region:f2dch(Region, F)
     || F <- List
    ],
    ?assertEqual([0, 1, 2, 3, 4, 5, 6, 7], TList).

validate_tx_power(Plan) ->
    Region = Plan#channel_plan.region,
    PowerTable = lora_region:uplink_power_table(Region),
    PT0 = [X || {_I, X} <- PowerTable],
    PT1 = tx_power_list(Plan),
    % io:format("Region=~w~n", [Region]),
    % io:format("PowerTable=~w~n", [PowerTable]),
    % io:format("PT0=~w~n", [PT0]),
    % io:format("PT1=~w~n", [PT1]),
    % io:format("Plan#channel_plan.tx_powers=~w~n", [Plan#channel_plan.tx_power]),
    ?assertEqual(PT0, PT1).

exercise_plan(Plan) ->
    Region = Plan#channel_plan.region,
    validate_tx_power(Plan),
    validate_u_channels(Region, Plan#channel_plan.u_channels),
    validate_d_channels(Region, Plan#channel_plan.d_channels),
    validate_u_frequences(Region, Plan#channel_plan.u_channels),
    validate_d_frequences(Region, Plan#channel_plan.d_channels).

payload_util_test() ->
    % EU868_Plan = plan_eu868(),
    % AS923_1_Plan = plan_as923_1(),

    Freq = lora_region:uch2f('EU868', 0),
    io:format("Freq=~w~n", [Freq]),

    exercise_plan(plan_eu868()),
    exercise_plan(plan_as923_1()),
    exercise_plan(plan_us915()),
    exercise_plan(plan_au915()),
    exercise_plan(plan_in865()),
    exercise_plan(plan_cn470()),
    exercise_plan(plan_kr920()),
    fin.

-endif.
%% end of file
