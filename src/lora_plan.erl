-module(lora_plan).

-export([
    datarate_to_index/2,
    datarate_to_binary/2,
    datarate_to_atom/2,
    up_to_down_datarate/3,
    max_uplink_payload_size/2,
    max_downlink_payload_size/2,
    max_payload_size/2,
    max_downlink_snr/3,
    max_uplink_snr/1,
    max_uplink_snr/2,
    freq_to_channel/2,
    channel_to_freq/2,
    tx_power/2,
    tx_power_list/1,
    tx_power_table/1,
    max_tx_power/1,
    region_to_plan/1,
    rx2_datarate/1,
    rx2_tuple/1,
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
        'EU868' -> plan_eu868_A();
        'EU433' -> plan_eu433_A();
        'US915' -> plan_us915_SB2();
        'AU915' -> plan_au915_SB2();
        'CN470' -> plan_cn470_A();
        'KR920' -> plan_kr920_A();
        'IN865' -> plan_in865_A();
        'AS923' -> plan_as923_A();
        'AS923_1' -> plan_as923_1A();
        'AS923_2' -> plan_as923_2A();
        'AS923_3' -> plan_as923_3A();
        'AS923_4' -> plan_as923_4A();
        'AS923_1B' -> plan_as923_1B()
    end.

%% ------------------------------------------------------------------
%% DataRate Functions
%% ------------------------------------------------------------------

-spec datarate_to_atom(#channel_plan{}, data_rate()) -> atom().
datarate_to_atom(Plan, Index) when is_integer(Index) ->
    List = (Plan#channel_plan.data_rates),
    Len = length(List),
    case Index < Len of
        true ->
            lists:nth(Index + 1, List);
        false ->
            'RFU'
    end;
datarate_to_atom(_Plan, Atom) when is_atom(Atom) ->
    Atom;
datarate_to_atom(_Plan, Binary) when is_binary(Binary) ->
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

-spec datarate_to_index(#channel_plan{}, data_rate()) -> integer().
datarate_to_index(_Plan, Index) when is_integer(Index) ->
    Index;
datarate_to_index(Plan, Atom) when is_atom(Atom) ->
    List = (Plan#channel_plan.data_rates),
    Index = index_of(Atom, List, 15),
    Index;
datarate_to_index(Plan, Binary) when is_binary(Binary) ->
    Atom = binary_to_atom(Binary),
    datarate_to_index(Plan, Atom).

-spec datarate_to_binary(#channel_plan{}, data_rate()) -> binary().
datarate_to_binary(Plan, Index) when is_integer(Index) ->
    Atom = datarate_to_atom(Plan, Index),
    datarate_to_binary(Plan, Atom);
datarate_to_binary(_Plan, Atom) when is_atom(Atom) ->
    atom_to_binary(Atom);
datarate_to_binary(_Plan, Binary) when is_binary(Binary) ->
    Binary.

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

-spec up_to_down_datarate(#channel_plan{}, integer(), integer()) -> integer().
up_to_down_datarate(Plan, Index, Offset) ->
    {MinOffset, MaxOffset} = Plan#channel_plan.rx1_offset,
    % io:format("up_to_down_datarate - Index=~w Offset=~w MinOffset=~w MaxOffset=~w~n",
    %    [Index, Offset, MinOffset, MaxOffset]),
    TheOffset =
        case Offset < MinOffset of
            true ->
                MinOffset;
            false ->
                case Offset > MaxOffset of
                    true ->
                        MaxOffset;
                    false ->
                        Offset
                end
        end,
    Region = Plan#channel_plan.base_region,
    OffsetList = dr_offset_list(Region, Index),
    DownIndex = lists:nth(TheOffset + 1, OffsetList),
    % io:format("up_to_down_datarate - DownIndex=~w~n", [DownIndex]),
    DownIndex.

dr_offset_list(Region, Index) when Region == 'US915' ->
    % io:format("dr_offset_list - Region=~w Index=~w~n", [Region, Index]),
    case Index of
        0 -> [10, 9, 8, 8];
        1 -> [11, 10, 9, 8];
        2 -> [12, 11, 10, 9];
        3 -> [13, 12, 11, 10];
        4 -> [13, 13, 12, 11]
    end;
dr_offset_list(Region, Index) when Region == 'AU915' ->
    case Index of
        0 -> [8, 8, 8, 8, 8, 8];
        1 -> [9, 8, 8, 8, 8, 8];
        2 -> [10, 9, 8, 8, 8, 8];
        3 -> [11, 10, 9, 8, 8, 8];
        4 -> [12, 11, 10, 9, 8, 8];
        5 -> [13, 12, 11, 10, 9, 8];
        6 -> [13, 13, 12, 11, 10, 9]
    end;
dr_offset_list(Region, Index) when Region == 'CN470' ->
    case Index of
        0 -> [0, 0, 0, 0, 0, 0];
        1 -> [1, 0, 0, 0, 0, 0];
        2 -> [2, 1, 0, 0, 0, 0];
        3 -> [3, 2, 1, 0, 0, 0];
        4 -> [4, 3, 2, 1, 0, 0];
        5 -> [5, 4, 3, 2, 1, 0]
    end;
dr_offset_list(_Region, Index) ->
    case Index of
        0 -> [0, 0, 0, 0, 0, 0];
        1 -> [1, 0, 0, 0, 0, 0];
        2 -> [2, 1, 0, 0, 0, 0];
        3 -> [3, 2, 1, 0, 0, 0];
        4 -> [4, 3, 2, 1, 0, 0];
        5 -> [5, 4, 3, 2, 1, 0];
        6 -> [6, 5, 4, 3, 2, 1];
        7 -> [7, 6, 5, 4, 3, 2]
    end.

-spec max_uplink_payload_size(#channel_plan{}, data_rate()) -> integer().
max_uplink_payload_size(Plan, DataRate) ->
    Atom = datarate_to_atom(Plan, DataRate),
    DwellTime = Plan#channel_plan.uplink_dwell_time,
    max_payload_size(Atom, DwellTime).

-spec max_downlink_payload_size(#channel_plan{}, data_rate()) -> integer().
max_downlink_payload_size(Plan, DataRate) ->
    Atom = datarate_to_atom(Plan, DataRate),
    DwellTime = Plan#channel_plan.downlink_dwell_time,
    max_payload_size(Atom, DwellTime).

-spec max_payload_size(atom(), integer()) -> integer().
max_payload_size(DataRate, DwellTime) ->
    case DwellTime of
        1000 ->
            case DataRate of
                'SF12BW125' -> 51;
                'SF11BW125' -> 51;
                'SF10BW125' -> 51;
                'SF9BW125' -> 115;
                'SF8BW125' -> 242;
                'SF7BW125' -> 242;
                _ -> 250
            end;
        400 ->
            case DataRate of
                'SF12BW125' -> 59;
                'SF11BW125' -> 59;
                'SF10BW125' -> 59;
                'SF9BW125' -> 123;
                'SF8BW125' -> 250;
                'SF7BW125' -> 250;
                'SF12BW500' -> 61;
                'SF11BW500' -> 137;
                'SF10BW500' -> 250;
                'SF9BW500' -> 250;
                'SF8BW500' -> 250;
                'SF7BW500' -> 250;
                'LRFHSS1BW137' -> 50;
                'LRFHSS2BW137' -> 115;
                'LRFHSS1BW336' -> 50;
                'LRFHSS2BW336' -> 115;
                'LRFHSS1BW1523' -> 50;
                'LRFHSS2BW1523' -> 115;
                _ -> 250
            end;
        401 ->
            case DataRate of
                'SF12BW125' -> 11;
                'SF11BW125' -> 11;
                'SF10BW125' -> 11;
                'SF9BW125' -> 53;
                'SF8BW125' -> 125;
                'SF7BW125' -> 242;
                'SF12BW500' -> 53;
                'SF11BW500' -> 129;
                'SF10BW500' -> 242;
                'SF9BW500' -> 242;
                'SF8BW500' -> 242;
                'SF7BW500' -> 242;
                'LRFHSS1BW137' -> 50;
                'LRFHSS2BW137' -> 115;
                'LRFHSS1BW336' -> 50;
                'LRFHSS2BW336' -> 115;
                'LRFHSS1BW1523' -> 50;
                'LRFHSS2BW1523' -> 115;
                _ -> 250
            end;
        _ ->
            case DataRate of
                'SF12BW125' -> 51;
                'SF11BW125' -> 51;
                'SF10BW125' -> 51;
                'SF9BW125' -> 115;
                'SF8BW125' -> 242;
                'SF7BW125' -> 242;
                'SF7BW250' -> 242;
                'SF12BW500' -> 53;
                'SF11BW500' -> 129;
                'SF10BW500' -> 242;
                'SF9BW500' -> 242;
                'SF8BW500' -> 242;
                'SF7BW500' -> 242;
                'LRFHSS1BW137' -> 51;
                'LRFHSS2BW137' -> 115;
                'LRFHSS1BW336' -> 50;
                'LRFHSS2BW336' -> 115;
                'LRFHSS1BW1523' -> 50;
                'LRFHSS2BW1523' -> 115;
                _ -> 250
            end
    end.

-spec max_uplink_snr(#channel_plan{}, data_rate()) -> number().
max_uplink_snr(Plan, DataRate) ->
    DataRateAtom = datarate_to_atom(Plan, DataRate),
    {SF, _} = datarate_to_tuple(DataRateAtom),
    max_snr(SF).

-spec max_uplink_snr(atom()) -> number().
max_uplink_snr(DataRateAtom) ->
    {SF, _} = datarate_to_tuple(DataRateAtom),
    max_snr(SF).

-spec max_downlink_snr(#channel_plan{}, data_rate(), number()) -> number().
max_downlink_snr(Plan, DataRate, Offset) ->
    Index = datarate_to_index(Plan, DataRate),
    DownDR = up_to_down_datarate(Plan, Index, Offset),
    DRAtom = datarate_to_atom(Plan, DownDR),
    {SF, _} = datarate_to_tuple(DRAtom),
    max_snr(SF).

%% sf to dB from SX1272 DataSheet, Table 13
max_snr(SF) ->
    -5 - 2.5 * (SF - 6).

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

new_txq(Freq, DataRate, Codr, Time) ->
    #txq{
        freq = Freq,
        datr = DataRate,
        codr = Codr,
        time = Time
    }.

-spec join1_window(#channel_plan{}, integer(), #rxq{}) -> #txq{}.
join1_window(Plan, DelaySeconds, RxQ) ->
    _Region = Plan#channel_plan.base_region,
    DownFreq = up_to_down_freq(Plan, RxQ#rxq.freq),
    DataRateIdx = datarate_to_index(Plan, RxQ#rxq.datr),
    DownDRIdx = up_to_down_datarate(Plan, DataRateIdx, 0),
    DownDRStr = datarate_to_binary(Plan, DownDRIdx),
    TxQ = new_txq(DownFreq, DownDRStr, RxQ#rxq.codr, RxQ#rxq.time),
    tx_window(?JOIN1_WINDOW, RxQ, TxQ, DelaySeconds).

-spec join2_window(#channel_plan{}, #rxq{}) -> #txq{}.
join2_window(Plan, RxQ) ->
    DownFreq = Plan#channel_plan.rx2_freq,
    DataRateStr = datarate_to_binary(Plan, Plan#channel_plan.rx2_datarate),
    TxQ = new_txq(DownFreq, DataRateStr, RxQ#rxq.codr, RxQ#rxq.time),
    tx_window(?JOIN2_WINDOW, RxQ, TxQ).

-spec rx1_window(#channel_plan{}, number(), number(), #rxq{}) -> #txq{}.
rx1_window(Plan, DelaySeconds, Offset, RxQ) ->
    _Region = Plan#channel_plan.base_region,
    DownFreq = up_to_down_freq(Plan, RxQ#rxq.freq),
    DataRateIdx = datarate_to_index(Plan, RxQ#rxq.datr),
    DownDRIdx = up_to_down_datarate(Plan, DataRateIdx, Offset),
    DownDRStr = datarate_to_binary(Plan, DownDRIdx),
    % io:format("rx1_window - DownDRStr=~w~n", [DownDRStr]),
    % io:format("rx1_window - DownDR=~w~n", [datarate_to_atom(Plan, DownDRStr)]),
    TxQ = new_txq(DownFreq, DownDRStr, RxQ#rxq.codr, RxQ#rxq.time),
    tx_window(?RX1_WINDOW, RxQ, TxQ, DelaySeconds).

-spec rx2_window(#channel_plan{}, number(), #rxq{}) -> #txq{}.
rx2_window(Plan, DelaySeconds, RxQ) ->
    DownFreq = Plan#channel_plan.rx2_freq,
    DataRateStr = datarate_to_binary(Plan, Plan#channel_plan.rx2_datarate),
    TxQ = new_txq(DownFreq, DataRateStr, RxQ#rxq.codr, RxQ#rxq.time),
    tx_window(?RX2_WINDOW, RxQ, TxQ, DelaySeconds).

-spec rx1_or_rx2_window(#channel_plan{}, number(), number(), #rxq{}) -> #txq{}.
rx1_or_rx2_window(Plan, Delay, Offset, RxQ) ->
    Region = Plan#channel_plan.base_region,
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

%% ------------------------------------------------------------------
%% rx2 Functions
%% ------------------------------------------------------------------

-spec rx2_datarate(#channel_plan{}) -> integer().
rx2_datarate(Plan) ->
    Plan#channel_plan.rx2_datarate.

-spec rx2_tuple(#channel_plan{}) -> {float(), atom()}.
rx2_tuple(Plan) ->
    RX2_Freq = Plan#channel_plan.rx2_freq,
    DRIndex = Plan#channel_plan.rx2_datarate,
    List = (Plan#channel_plan.data_rates),
    DRAtom = lists:nth(DRIndex, List),
    {RX2_Freq, DRAtom}.

%% ------------------------------------------------------------------
%% Frequency and Channel Functions
%% ------------------------------------------------------------------

-spec freq_to_channel(#channel_plan{}, number()) -> integer().
freq_to_channel(Plan, Freq0) ->
    List = (Plan#channel_plan.u_channels),
    Freq1 = nearest(Freq0, List),
    Channel = index_of(Freq1, List, 0),
    Channel.

-spec channel_to_freq(#channel_plan{}, integer()) -> number().
channel_to_freq(Plan, Ch) ->
    Ch2 = Ch rem Plan#channel_plan.channel_count,
    List = (Plan#channel_plan.u_channels),
    Freq = lists:nth(Ch2 + 1, List),
    Freq.

-spec up_to_down_freq(#channel_plan{}, number()) -> number().
up_to_down_freq(Plan, Freq0) ->
    UList = (Plan#channel_plan.u_channels),
    Freq1 = nearest(Freq0, UList),
    % io:format("Freq=~w UList=~w~n", [Freq, UList]),
    UChannel = index_of(Freq1, UList, 0),
    DList = (Plan#channel_plan.d_channels),
    DownFreq = lists:nth(UChannel + 1, DList),
    DownFreq.

%% ------------------------------------------------------------------
%% TX Power Functions
%% ------------------------------------------------------------------

-spec max_tx_power(#channel_plan{}) -> pos_integer().
max_tx_power(Plan) ->
    Plan#channel_plan.max_eirp_db.

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

%% ------------------------------------------------------------------
%% Utility Functions
%% ------------------------------------------------------------------

index_of(Value, List, Default) ->
    Map = lists:zip(List, lists:seq(1, length(List))),
    case lists:keyfind(Value, 1, Map) of
        {Value, Index} -> Index - 1;
        false -> Default
    end.

-spec nearest(float() | number(), [float()]) -> float().
nearest(F, List) ->
    Near = fun(A, B) ->
        AbsA = abs(A - F),
        AbsB = abs(B - F),
        R = AbsA < AbsB,
        case R of
            true -> A;
            false -> B
        end
    end,
    lists:foldl(Near, 1.0, List).

%% ------------------------------------------------------------------
%% Plan Record Functions
%%
%% u_channels frequencies are defined here.  Refer for source of truth.
%% https://github.com/helium/sx1302_hal/blob/helium/hotspot/packet_forwarder/
%%
%% ------------------------------------------------------------------

plan_eu868_A() ->
    Plan = #channel_plan{
        channel_plan_id = 1,
        plan_name = 'EU868_A',
        base_region = 'EU868',
        dynamic_plan = true,
        float_precision = 1,
        min_freq = 863.0,
        max_freq = 870.0,
        u_channels = [867.1, 867.3, 867.5, 867.7, 867.9, 868.1, 868.3, 868.5],
        d_channels = [867.1, 867.3, 867.5, 867.7, 867.9, 868.1, 868.3, 868.5],
        channel_count = 8,
        bank_offset = 0,
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
        uplink_dwell_time = 0,
        downlink_dwell_time = 0,
        tx_param_setup_allowed = false,
        % max_eirp_db = 16,
        max_eirp_db = 20,
        rx1_offset = {0, 5},
        rx2_datarate = 0,
        rx2_freq = 869.525,
        beacon_freq = 869.525,
        pingslot_freq = 869.525
    },
    Plan.

plan_eu433_A() ->
    Plan = #channel_plan{
        channel_plan_id = 1,
        plan_name = 'EU433_A',
        base_region = 'EU433',
        dynamic_plan = true,
        float_precision = 3,
        min_freq = 433.175,
        max_freq = 434.665,
        u_channels = [433.175, 433.375, 433.575],
        d_channels = [433.175, 433.375, 433.575],
        channel_count = 3,
        bank_offset = 0,
        join_channels = {0, 2},
        data_rates = [
            'SF12BW125',
            'SF11BW125',
            'SF10BW125',
            'SF9BW125',
            'SF8BW125',
            'SF7BW125',
            'SF7BW250',
            'FSK50'
        ],
        tx_power = [0, -2, -4, -6, -8, -10],
        join_dr = {0, 5},
        mandatory_dr = {0, 5},
        optional_dr = {6, 7},
        max_duty_cycle = 10,
        uplink_dwell_time = 0,
        downlink_dwell_time = 0,
        tx_param_setup_allowed = false,
        max_eirp_db = 12,
        rx1_offset = {0, 5},
        rx2_datarate = 0,
        rx2_freq = 434.665,
        beacon_freq = 434.665,
        pingslot_freq = 434.665
    },
    Plan.

plan_us915_SB2() ->
    Plan = #channel_plan{
        channel_plan_id = 2,
        plan_name = 'US915_SB2',
        base_region = 'US915',
        dynamic_plan = false,
        float_precision = 1,
        min_freq = 902.0,
        max_freq = 928.0,
        %% US915's subbank two set of channels
        u_channels = [903.9, 904.1, 904.3, 904.5, 904.7, 904.9, 905.1, 905.3],
        %% The eight US915 downlink channels are hard-coded in the spec
        d_channels = [923.3, 923.9, 924.5, 925.1, 925.7, 926.3, 926.9, 927.5],
        channel_count = 8,
        bank_offset = 8,
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
            'SF7BW500'
        ],
        %% tx_power = [0,-2,-4,-6,-8,-10,-12,-14,-16,-18,-20,-22,-24,-26,-28,0],
        tx_power = [0, -2, -4, -6, -8, -10, -12, -14, -16, -18, -20],
        join_dr = {2, 4},
        mandatory_dr = {0, 4},
        optional_dr = {5, 6},
        max_duty_cycle = 10000,
        uplink_dwell_time = 401,
        downlink_dwell_time = 401,
        tx_param_setup_allowed = false,
        max_eirp_db = 30,
        rx1_offset = {0, 3},
        rx2_datarate = 8,
        rx2_freq = 923.3,
        beacon_freq = 923.3,
        pingslot_freq = 923.3
    },
    Plan.

plan_au915_SB2() ->
    Plan = #channel_plan{
        channel_plan_id = 5,
        plan_name = 'AU915_SB2',
        base_region = 'AU915',
        dynamic_plan = false,
        float_precision = 1,
        min_freq = 915.0,
        max_freq = 928.0,
        %% AU915's subbank two set of channels
        u_channels = [916.8, 917.0, 917.2, 917.4, 917.6, 917.8, 918.0, 918.2],
        %% The eight AU915 downlink channels are hard-coded in the spec
        d_channels = [923.3, 923.9, 924.5, 925.1, 925.7, 926.3, 926.9, 927.5],
        channel_count = 8,
        bank_offset = 8,
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
            'SF7BW500'
        ],
        %% tx_power = [0,-2,-4,-6,-8,-10,-12,-14,-16,-18,-20,-22,-24,-26,-28,0],
        tx_power = [0, -2, -4, -6, -8, -10, -12, -14, -16, -18, -20],
        join_dr = {2, 5},
        mandatory_dr = {0, 6},
        optional_dr = {7, 7},
        max_duty_cycle = 1,
        uplink_dwell_time = 400,
        downlink_dwell_time = 400,
        tx_param_setup_allowed = true,
        max_eirp_db = 30,
        rx1_offset = {0, 5},
        rx2_datarate = 8,
        rx2_freq = 923.3,
        beacon_freq = 923.3,
        pingslot_freq = 923.3
    },
    Plan.

plan_cn470_A() ->
    Plan = #channel_plan{
        channel_plan_id = 6,
        plan_name = 'CN470_A',
        base_region = 'CN470',
        dynamic_plan = true,
        float_precision = 1,
        min_freq = 470.0,
        max_freq = 510.0,
        u_channels = [486.3, 486.5, 486.7, 486.9, 487.1, 487.3, 487.5, 487.7],
        d_channels = [506.7, 506.9, 507.1, 507.3, 507.5, 507.7, 507.9, 508.1],
        channel_count = 8,
        bank_offset = 0,
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
        uplink_dwell_time = 1000,
        downlink_dwell_time = 1000,
        tx_param_setup_allowed = false,
        max_eirp_db = 19,
        rx1_offset = {0, 7},
        rx2_datarate = 0,
        rx2_freq = 505.3,
        beacon_freq = 508.3,
        pingslot_freq = 508.3
    },
    Plan.

plan_as923_A() ->
    Plan = #channel_plan{
        channel_plan_id = 7,
        plan_name = 'AS923_1A',
        base_region = 'AS923',
        dynamic_plan = true,
        float_precision = 1,
        min_freq = 915.0,
        max_freq = 928.0,
        u_channels = [923.2, 923.4, 923.6, 923.8, 924.0, 924.2, 924.4, 924.6],
        d_channels = [923.2, 923.4, 923.6, 923.8, 924.0, 924.2, 924.4, 924.6],
        channel_count = 8,
        bank_offset = 0,
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
        uplink_dwell_time = 400,
        downlink_dwell_time = 400,
        tx_param_setup_allowed = true,
        max_eirp_db = 16,
        rx1_offset = {0, 7},
        rx2_datarate = 2,
        rx2_freq = 923.2,
        beacon_freq = 923.4,
        pingslot_freq = 923.4
    },
    Plan.

plan_as923_1A() ->
    Plan = #channel_plan{
        channel_plan_id = 7,
        plan_name = 'AS923_1A',
        base_region = 'AS923_1',
        dynamic_plan = true,
        float_precision = 1,
        min_freq = 915.0,
        max_freq = 928.0,
        u_channels = [923.2, 923.4, 923.6, 923.8, 924.0, 924.2, 924.4, 924.6],
        d_channels = [923.2, 923.4, 923.6, 923.8, 924.0, 924.2, 924.4, 924.6],
        channel_count = 8,
        bank_offset = 0,
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
        uplink_dwell_time = 400,
        downlink_dwell_time = 400,
        tx_param_setup_allowed = true,
        max_eirp_db = 16,
        rx1_offset = {0, 7},
        rx2_datarate = 2,
        rx2_freq = 923.2,
        beacon_freq = 923.4,
        pingslot_freq = 923.4
    },
    Plan.

%%
%% Defined from https://docs.google.com/spreadsheets/d/1Mw3qtSNLz4kSjJXZwptnw_pqbYAaxPrpXLsVKQrOY_U
%% and https://docs.google.com/document/d/1ImdqnNkD7BDE8ocKvUI1M8Bke8GtTuFsMCcFcEGTEQk
%%
plan_as923_1B() ->
    Plan = #channel_plan{
        channel_plan_id = 7,
        plan_name = 'AS923_1B',
        base_region = 'AS923_1',
        dynamic_plan = true,
        float_precision = 1,
        min_freq = 915.0,
        max_freq = 923.0,
        u_channels = [922.0, 922.2, 922.4, 922.6, 922.8, 923.0, 923.2, 923.4],
        d_channels = [915.2, 915.4, 915.6, 915.8, 916.0, 916.2, 916.4, 916.6],
        channel_count = 8,
        bank_offset = 0,
        join_channels = {6, 7},
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
        uplink_dwell_time = 400,
        downlink_dwell_time = 400,
        tx_param_setup_allowed = true,
        max_eirp_db = 16,
        rx1_offset = {0, 7},
        rx2_datarate = 2,
        rx2_freq = 923.2,
        beacon_freq = 923.4,
        pingslot_freq = 923.4
    },
    Plan.

plan_as923_2A() ->
    Plan = #channel_plan{
        channel_plan_id = 8,
        plan_name = 'AS923_2A',
        base_region = 'AS923_2',
        dynamic_plan = true,
        float_precision = 1,
        min_freq = 920.0,
        max_freq = 923.0,
        u_channels = [921.4, 921.6, 921.8, 922.0, 922.2, 922.4, 922.6, 922.8],
        d_channels = [921.4, 921.6, 921.8, 922.0, 922.2, 922.4, 922.6, 922.8],
        channel_count = 8,
        bank_offset = 0,
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
        uplink_dwell_time = 400,
        downlink_dwell_time = 400,
        tx_param_setup_allowed = true,
        max_eirp_db = 16,
        rx1_offset = {0, 7},
        rx2_datarate = 2,
        rx2_freq = 921.4,
        beacon_freq = 921.6,
        pingslot_freq = 921.6
    },
    Plan.

plan_as923_3A() ->
    Plan = #channel_plan{
        channel_plan_id = 9,
        plan_name = 'AS923_3A',
        base_region = 'AS923_3',
        dynamic_plan = true,
        float_precision = 1,
        min_freq = 915.0,
        max_freq = 921.0,
        u_channels = [916.6, 916.8, 917.0, 917.2, 917.4, 917.6, 917.8, 918.0],
        d_channels = [916.6, 916.8, 917.0, 917.2, 917.4, 917.6, 917.8, 918.0],
        channel_count = 8,
        bank_offset = 0,
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
        uplink_dwell_time = 400,
        downlink_dwell_time = 400,
        tx_param_setup_allowed = true,
        max_eirp_db = 16,
        rx1_offset = {0, 7},
        rx2_datarate = 2,
        rx2_freq = 916.6,
        beacon_freq = 916.8,
        pingslot_freq = 916.8
    },
    Plan.

plan_as923_4A() ->
    Plan = #channel_plan{
        channel_plan_id = 13,
        plan_name = 'AS923_4A',
        base_region = 'AS923_4',
        dynamic_plan = true,
        float_precision = 1,
        min_freq = 917.0,
        max_freq = 920.0,
        u_channels = [917.3, 917.5, 917.7, 917.9, 918.1, 918.3, 918.5, 918.7],
        d_channels = [917.3, 917.5, 917.7, 917.9, 918.1, 918.3, 918.5, 918.7],
        channel_count = 8,
        bank_offset = 0,
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
        uplink_dwell_time = 400,
        downlink_dwell_time = 400,
        tx_param_setup_allowed = true,
        max_eirp_db = 16,
        rx1_offset = {0, 7},
        rx2_datarate = 2,
        rx2_freq = 917.3,
        beacon_freq = 917.5,
        pingslot_freq = 917.5
    },
    Plan.

plan_kr920_A() ->
    Plan = #channel_plan{
        channel_plan_id = 10,
        plan_name = 'KR920_A',
        base_region = 'KR920',
        dynamic_plan = true,
        float_precision = 1,
        min_freq = 920.9,
        max_freq = 923.3,
        u_channels = [922.1, 922.3, 922.5, 922.7, 922.9, 923.1, 923.3],
        d_channels = [922.1, 922.3, 922.5, 922.7, 922.9, 923.1, 923.3],
        channel_count = 7,
        bank_offset = 0,
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
        uplink_dwell_time = 0,
        downlink_dwell_time = 0,
        tx_param_setup_allowed = false,
        max_eirp_db = 14,
        rx1_offset = {0, 5},
        rx2_datarate = 0,
        rx2_freq = 921.9,
        beacon_freq = 923.1,
        pingslot_freq = 923.1
    },
    Plan.

plan_in865_A() ->
    Plan = #channel_plan{
        channel_plan_id = 11,
        plan_name = 'IN865_A',
        base_region = 'IN865',
        dynamic_plan = true,
        float_precision = 4,
        min_freq = 865.0000,
        max_freq = 867.0000,
        u_channels = [865.0625, 865.4025, 865.985],
        d_channels = [865.0625, 865.4025, 865.985],
        channel_count = 3,
        bank_offset = 0,
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
        uplink_dwell_time = 0,
        downlink_dwell_time = 0,
        tx_param_setup_allowed = false,
        max_eirp_db = 30,
        rx1_offset = {0, 7},
        rx2_datarate = 2,
        rx2_freq = 866.5500,
        beacon_freq = 866.5500,
        pingslot_freq = 866.5500
    },
    Plan.

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

uplink_to_downlink_rounding_test() ->
    #channel_plan{u_channels = UChans, d_channels = DChans} =
        Plan = lora_plan:region_to_plan('US915'),

    RoundingError = 0.00000000094,
    lists:foreach(
        fun({Up, Down}) ->
            JsonUp = Up - RoundingError,
            ?assertEqual(up_to_down_freq(Plan, JsonUp), Down)
        end,
        lists:zip(UChans, DChans)
    ),

    ok.

uplink_to_downlink_rounding_2_test() ->
    #channel_plan{u_channels = UChans, d_channels = DChans} =
        Plan = lora_plan:region_to_plan('IN865'),

    RoundingError = 0.00000000094,
    lists:foreach(
        fun({Up, Down}) ->
            JsonUp = Up - RoundingError,
            ?assertEqual(up_to_down_freq(Plan, JsonUp), Down)
        end,
        lists:zip(UChans, DChans)
    ),

    ok.

up_down_test() ->
    UpF = 903.79,
    DownF = up_to_down_freq(plan_us915_SB2(), UpF),
    ?assertEqual(923.3, DownF),
    UpF1 = 904.11,
    DownF1 = up_to_down_freq(plan_us915_SB2(), UpF1),
    ?assertEqual(923.9, DownF1).

% valid_round(F1, F2, Precision) ->
%     R2 = round_frequency(F2, Precision),
%     % io:format("F1=~w F2=~w R2=~w~n", [F1, F2, R2]),
%     ?assertEqual(F1, R2).

% -spec round_frequency(float() | number(), integer()) -> float().
% round_frequency(Value, Precision) ->
%     list_to_float(float_to_list(Value, [{decimals, Precision}, compact])).

valid_frequency(Expect, Actual, List) ->
    F = nearest(Actual, List),
    ?assertEqual(Expect, F).

round_00_test() ->
    FList = [923.2, 923.21, 923.24, 923.19, 923.151, 923.2000000001, 923.1999999999],
    Plan = plan_as923_1B(),
    CList = Plan#channel_plan.u_channels,
    [valid_frequency(923.2, X, CList) || X <- FList].

round_01_test() ->
    FList = [923.0, 923.01, 923.04, 923.05, 923.049, 923.0000000001, 923.04999999999],
    Plan = plan_as923_1B(),
    CList = Plan#channel_plan.u_channels,
    [valid_frequency(923.0, X, CList) || X <- FList].

round_02_test() ->
    FList = [
        865.0, 865.1, 865.2, 865.0625, 865.06251, 865.06249, 865.0625000000001, 865.0624999999999
    ],
    Plan = plan_in865_A(),
    CList = Plan#channel_plan.u_channels,
    [valid_frequency(865.0625, X, CList) || X <- FList].

round_03_test() ->
    FList = [866.5500, 866.55001, 866.54999, 866.5500000000001, 866.5499999999999],
    CList = [860.123, 866.5500, 870.123],
    [valid_frequency(866.5500, X, CList) || X <- FList].

valid_uplink_freq(Plan, Freq) when Plan#channel_plan.base_region == 'EU433' ->
    case Freq of
        433.175 -> true;
        433.375 -> true;
        433.575 -> true;
        _ -> false
    end;
valid_uplink_freq(Plan, Freq) ->
    Region = Plan#channel_plan.base_region,
    F0 = lora_region:uch2f(Region, 0),
    case Freq of
        F0 ->
            true;
        _ ->
            Ch = lora_region:f2uch(Region, Freq),
            % Freq2 = lora_region:uch2f(Region, Ch),
            % io:format("Freq=~w Ch=~w Freq2=~w ~n", [Freq, Ch, Freq2]),
            (Ch > 0)
    end.

seek_freq(Plan, Freq) ->
    Region = Plan#channel_plan.base_region,
    lora_region:f2uch(Region, Freq).

validate_channel(Plan, Ch) ->
    % Region = Plan#channel_plan.base_region,
    % io:format("Region=~w~n", [Region]),
    F1 = channel_to_freq(Plan, Ch),
    Ch2 = freq_to_channel(Plan, F1),
    F2 = channel_to_freq(Plan, Ch2),
    ?assert(valid_uplink_freq(Plan, F2)),
    _OldCh = seek_freq(Plan, F2),
    % io:format("Ch=~w F1=~w F2=~w OldCh=~w~n", [Ch, F1, F2, OldCh]),
    ?assertEqual(F1, F2).

validate_channels(Plan) ->
    % Region = Plan#channel_plan.base_region,
    ChannelList = lists:seq(0, 16),
    [validate_channel(Plan, X) || X <- ChannelList].

validate_tx_power(Plan) ->
    Region = Plan#channel_plan.base_region,
    PowerTable = lora_region:uplink_power_table(Region),
    PT0 = [X || {_I, X} <- PowerTable],
    PT1 = tx_power_list(Plan),
    % io:format("Region=~w~n", [Region]),
    % io:format("PowerTable=~w~n", [PowerTable]),
    % io:format("PT0=~w~n", [PT0]),
    % io:format("PT1=~w~n", [PT1]),
    % io:format("Plan#channel_plan.tx_powers=~w~n", [Plan#channel_plan.tx_power]),
    ?assertEqual(PT0, PT1).

validate_downlink_size(Plan, _RxQ) when Plan#channel_plan.base_region == 'IN865' ->
    ?assert(true);
validate_downlink_size(Plan, _RxQ) when Plan#channel_plan.base_region == 'KR920' ->
    ?assert(true);
validate_downlink_size(Plan, DataRateAtom) ->
    Region = Plan#channel_plan.base_region,
    M1 = max_uplink_payload_size(Plan, DataRateAtom),
    DRIdx = datarate_to_index(Plan, DataRateAtom),
    case DRIdx of
        15 ->
            ?assertEqual(true, true);
        _ ->
            DRAtom = datarate_to_atom(Plan, DRIdx),
            ?assertEqual(DRAtom, DataRateAtom),
            M2 = lora_region:max_payload_size(Region, DRIdx),
            % io:format("DRAtom=~w DR~w ~w~n", [DRAtom, DRIdx, M2]),
            ?assertEqual(M2, M1)
    end.

validate_payload_size(Plan) ->
    validate_downlink_size(Plan, 'SF12BW125'),
    validate_downlink_size(Plan, 'SF11BW125'),
    validate_downlink_size(Plan, 'SF10BW125'),
    validate_downlink_size(Plan, 'SF9BW125'),
    validate_downlink_size(Plan, 'SF8BW125'),
    validate_downlink_size(Plan, 'SF7BW125'),
    validate_downlink_size(Plan, 'SF7BW250'),
    validate_downlink_size(Plan, 'SF12BW500'),
    validate_downlink_size(Plan, 'SF11BW500'),
    validate_downlink_size(Plan, 'SF10BW500').

validate_txq(Plan, TxQ) ->
    _Region = Plan#channel_plan.base_region,
    DRAtom = lora_plan:datarate_to_atom(Plan, TxQ#txq.datr),
    DRIdx = lora_plan:datarate_to_index(Plan, DRAtom),
    DRAtom2 = lora_plan:datarate_to_atom(Plan, DRIdx),
    %% DR = datar_to_dr(Plan, TxQ#txq.datr),
    %% _Tuple = lora_region:dr_to_tuple(Region, DRIdx),
    ?assertEqual(DRAtom, DRAtom2).

print_txq(Plan, TxQ, Enable) ->
    case Enable of
        true ->
            io:format("TxQ Frequency = ~w~n", [TxQ#txq.freq]),
            io:format("TxQ Region = ~w~n", [Plan#channel_plan.base_region]),
            io:format("TxQ DataRate = ~w~n", [datarate_to_atom(Plan, TxQ#txq.datr)]),
            io:format("TxQ DRIndex = ~w~n", [datarate_to_index(Plan, TxQ#txq.datr)]);
        false ->
            ok
    end.

validate_rx2_window(Plan, _RxQ) when Plan#channel_plan.base_region == 'IN865' ->
    ?assert(true);
validate_rx2_window(Plan, _RxQ) when Plan#channel_plan.base_region == 'KR920' ->
    ?assert(true);
validate_rx2_window(Plan, RxQ) ->
    Region = Plan#channel_plan.base_region,
    TxQ_P = rx2_window(Plan, 0, RxQ),
    TxQ_R = lora_region:rx2_window(Region, 0, RxQ),
    ?assertEqual(TxQ_R, TxQ_P),
    validate_txq(Plan, TxQ_P).

validate_join2_window(Plan, _RxQ) when Plan#channel_plan.base_region == 'IN865' ->
    ?assert(true);
validate_join2_window(Plan, _RxQ) when Plan#channel_plan.base_region == 'KR920' ->
    ?assert(true);
validate_join2_window(Plan, RxQ) ->
    Region = Plan#channel_plan.base_region,
    TxQ_P = join2_window(Plan, RxQ),
    TxQ_R = lora_region:join2_window(Region, RxQ),
    ?assertEqual(TxQ_R, TxQ_P),
    validate_txq(Plan, TxQ_P).

validate_rx1_window(Plan, _RxQ) when Plan#channel_plan.base_region == 'IN865' ->
    ?assert(true);
validate_rx1_window(Plan, _RxQ) when Plan#channel_plan.base_region == 'KR920' ->
    ?assert(true);
validate_rx1_window(Plan, RxQ) ->
    Region = Plan#channel_plan.base_region,
    TxQ_P = rx1_window(Plan, 0, 0, RxQ),
    TxQ_R = lora_region:rx1_window(Region, 0, 0, RxQ),
    ?assertEqual(TxQ_R, TxQ_P),
    validate_txq(Plan, TxQ_P).

validate_join1_window(Plan, _RxQ) when Plan#channel_plan.base_region == 'IN865' ->
    ?assert(true);
validate_join1_window(Plan, _RxQ) when Plan#channel_plan.base_region == 'KR920' ->
    ?assert(true);
validate_join1_window(Plan, RxQ) ->
    Region = Plan#channel_plan.base_region,
    TxQ_P = join1_window(Plan, 0, RxQ),
    TxQ_R = lora_region:join1_window(Region, 0, RxQ),
    ?assertEqual(TxQ_R, TxQ_P),
    validate_txq(Plan, TxQ_P).

validate_window(Plan, 'SF11BW125', _Channel) when Plan#channel_plan.base_region == 'US915' ->
    ?assert(true);
validate_window(Plan, 'SF11BW125', _Channel) when Plan#channel_plan.base_region == 'AU915' ->
    ?assert(true);
validate_window(Plan, 'SF12BW125', _Channel) when Plan#channel_plan.base_region == 'US915' ->
    ?assert(true);
validate_window(Plan, 'SF12BW125', _Channel) when Plan#channel_plan.base_region == 'AU915' ->
    ?assert(true);
validate_window(Plan, DataRateAtom, Channel) ->
    % ToDo - Add Channel parameter
    Region = Plan#channel_plan.base_region,
    % io:format("validate_window Region=~w DataRate=~w~n", [Region, DataRateAtom]),
    DataRateStr = datarate_to_binary(Plan, DataRateAtom),
    [JoinChannel_0 | _] = Plan#channel_plan.u_channels,
    J0 = channel_to_freq(Plan, 0),
    ?assertEqual(JoinChannel_0, J0),
    Frequency = channel_to_freq(Plan, Channel),
    % io:format("JoinChannel=~w~n", [JoinChannel_2]),

    Now = os:timestamp(),
    RxQ = #rxq{
        freq = Frequency,
        datr = DataRateStr,
        codr = <<"4/5">>,
        time = calendar:now_to_datetime(Now),
        tmms = 0,
        rssi = 42.2,
        lsnr = 10.1
    },

    validate_rx2_window(Plan, RxQ),
    validate_join2_window(Plan, RxQ),
    validate_rx1_window(Plan, RxQ),
    validate_join1_window(Plan, RxQ),

    TxQ_1 = rx1_window(Plan, 0, 0, RxQ),
    % io:format("TxQ_1=~w~n", [TxQ_1]),
    print_txq(Plan, TxQ_1, false),

    TxQ_2 = rx2_window(Plan, 0, RxQ),
    % io:format("TxQ_2=~w~n", [TxQ_2]),
    _DRIdx_2 = lora_region:datar_to_dr(Region, TxQ_2#txq.datr),
    % io:format("DRIdx_2=~w~n", [DRIdx_2]),
    % ?assertEqual(lora_region:datar_to_dr('US915', TxQ#txq.datr), 8),
    % ?assertEqual(JoinChannel, TxQ_2#txq.freq),

    TxQ_3 = join2_window(Plan, RxQ),
    % io:format("TxQ_3=~w~n", [TxQ_3]),
    _DRIdx_3 = lora_region:datar_to_dr(Region, TxQ_3#txq.datr),
    % io:format("DRIdx_3=~w~n", [DRIdx_3]),
    % ?assertEqual(lora_region:datar_to_dr('US915', TxQ#txq.datr), 8),
    % ?assertEqual(JoinChannel, TxQ_3#txq.freq),
    ok.

validate_snr(Plan, DRIndex) ->
    Region = Plan#channel_plan.base_region,
    MaxUplinkSnr01 = max_uplink_snr(Plan, DRIndex),
    MaxUplinkSnr02 = lora_region:max_uplink_snr(Region, DRIndex),
    ?assertEqual(MaxUplinkSnr02, MaxUplinkSnr01),
    MaxDownlinkSnr01 = max_downlink_snr(Plan, DRIndex, 0),
    MaxDownlinkSnr02 = lora_region:max_downlink_snr(Region, DRIndex, 0),
    ?assertEqual(MaxDownlinkSnr02, MaxDownlinkSnr01).

exercise_snr(Plan) when Plan#channel_plan.base_region == 'US915' ->
    [validate_snr(Plan, X) || X <- [0, 1, 2, 3, 4]];
exercise_snr(Plan) when Plan#channel_plan.base_region == 'AU915' ->
    [validate_snr(Plan, X) || X <- [0, 1, 2, 3, 4]];
exercise_snr(Plan) when Plan#channel_plan.base_region == 'CN470' ->
    [validate_snr(Plan, X) || X <- [0, 1, 2, 3, 4, 5]];
exercise_snr(Plan) ->
    [validate_snr(Plan, X) || X <- [0, 1, 2, 3, 4, 5, 6]].

au915_test() ->
    Plan = plan_au915_SB2(),
    % AU915 supports a 'fat' SF8BW500 DR on frequency 917.5
    validate_window(Plan, 'SF8BW500', 0).

eu868_test() ->
    Plan = plan_eu868_A(),
    % EU868 supports a 'fat' BW250 data rate
    validate_window(Plan, 'SF7BW250', 0).

exercise_window_channel(Plan, Atom) ->
    Channels = lists:seq(0, Plan#channel_plan.channel_count - 1),
    % io:format("Atom=~w Channels=~w~n", [Atom, Channels]),
    [validate_window(Plan, Atom, X) || X <- Channels].

exercise_window(Plan) ->
    {DRMin, DRMax} = Plan#channel_plan.mandatory_dr,
    Atoms = lists:seq(DRMin, DRMax),
    [exercise_window_channel(Plan, Atom) || Atom <- Atoms].

exercise_plan(Plan) ->
    Region = Plan#channel_plan.base_region,
    io:format("Region=~w~n", [Region]),
    exercise_snr(Plan),
    exercise_window(Plan),
    exercise_window_channel(Plan, 'SF7BW125'),
    validate_window(Plan, 'SF7BW125', 0),
    validate_window(Plan, 'SF8BW125', 0),
    validate_window(Plan, 'SF9BW125', 0),
    validate_window(Plan, 'SF10BW125', 0),
    validate_window(Plan, 'SF11BW125', 0),
    validate_window(Plan, 'SF12BW125', 0),
    validate_payload_size(Plan),
    validate_tx_power(Plan),
    validate_channels(Plan).

plan_test() ->
    exercise_plan(plan_us915_SB2()),
    exercise_plan(plan_au915_SB2()),
    exercise_plan(plan_eu868_A()),
    exercise_plan(plan_as923_1A()),
    exercise_plan(plan_in865_A()),
    exercise_plan(plan_cn470_A()),
    exercise_plan(plan_kr920_A()),
    exercise_plan(plan_eu433_A()),
    % exercise_plan(plan_as923_2A()),
    % exercise_plan(plan_as923_3A()),
    % exercise_plan(plan_as923_4A()),
    % exercise_plan(plan_as923_1A()),
    fin.

nearest_test() ->
    R = nearest(923.81, [923.2, 923.4, 923.6, 923.8, 924.0, 924.2, 924.4, 924.6]),
    io:format("R=~w~n", [R]),
    R2 = nearest(923.9, [923.2, 923.4, 923.6, 923.8, 924.0, 924.2, 924.4, 924.6]),
    io:format("R=~w~n", [R2]).

-endif.
%% end of file
