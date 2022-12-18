-module(lora_plan_tests).
%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
%-define(EUNIT, 1).
-ifdef(EUNIT).
-include("lora.hrl").
-include_lib("eunit/include/eunit.hrl").

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

uplink_to_downlink_rounding_test() ->
    #channel_plan{u_channels = UChans, d_channels = DChans} =
        Plan = lora_plan:region_to_plan('US915'),

    RoundingError = 0.00000000094,
    lists:foreach(
        fun({Up, Down}) ->
            JsonUp = Up - RoundingError,
            ?assertEqual(lora_plan:up_to_down_freq(Plan, JsonUp), Down)
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
            ?assertEqual(lora_plan:up_to_down_freq(Plan, JsonUp), Down)
        end,
        lists:zip(UChans, DChans)
    ),

    ok.

up_down_test() ->
    UpF = 903.79,
    DownF = lora_plan:up_to_down_freq(lora_plan:plan_us915_SB2(), UpF),
    ?assertEqual(923.3, DownF),
    UpF1 = 904.11,
    DownF1 = lora_plan:up_to_down_freq(lora_plan:plan_us915_SB2(), UpF1),
    ?assertEqual(923.9, DownF1).

round_00_test() ->
    FList = [923.2, 923.21, 923.24, 923.19, 923.151, 923.2000000001, 923.1999999999],
    Plan = lora_plan:plan_as923_1B(),
    CList = Plan#channel_plan.u_channels,
    [valid_round(923.2, F, 1) || F <- FList],
    [valid_frequency(923.2, X, CList) || X <- FList].

round_01_test() ->
    FList = [923.0, 923.01, 923.04, 923.05, 923.049, 923.0000000001, 923.04999999999],
    Plan = lora_plan:plan_as923_1B(),
    CList = Plan#channel_plan.u_channels,
    [valid_frequency(923.0, X, CList) || X <- FList].

round_02_test() ->
    FList = [
        865.0, 865.1, 865.2, 865.0625, 865.06251, 865.06249, 865.0625000000001, 865.0624999999999
    ],
    Plan = lora_plan:plan_in865_A(),
    CList = Plan#channel_plan.u_channels,
    [valid_frequency(865.0625, X, CList) || X <- FList].

round_03_test() ->
    FList = [866.5500, 866.55001, 866.54999, 866.5500000000001, 866.5499999999999],
    CList = [860.123, 866.5500, 870.123],
    [valid_frequency(866.5500, X, CList) || X <- FList].

plan_test() ->
    exercise_plan(lora_plan:plan_us915_SB2()),
    exercise_plan(lora_plan:plan_au915_SB2()),
    exercise_plan(lora_plan:plan_au915_SB5()),
    exercise_plan(lora_plan:plan_au915_DP()),
    exercise_plan(lora_plan:plan_eu868_A()),
    exercise_plan(lora_plan:plan_as923_1A()),
    exercise_plan(lora_plan:plan_in865_A()),
    exercise_plan(lora_plan:plan_cn470_A()),
    exercise_plan(lora_plan:plan_kr920_A()),
    exercise_plan(lora_plan:plan_eu433_A()),
    exercise_plan(lora_plan:plan_as923_2A()),
    exercise_plan(lora_plan:plan_as923_3A()),
    exercise_plan(lora_plan:plan_as923_4A()),
    fin.

au915_test() ->
    Plan = lora_plan:plan_au915_SB2(),
    % AU915 supports a 'fat' SF8BW500 DR on frequency 917.5
    validate_window(Plan, 'SF8BW500', 0).

eu868_test() ->
    Plan = lora_plan:plan_eu868_A(),
    % EU868 supports a 'fat' BW250 data rate
    validate_window(Plan, 'SF7BW250', 0).

nearest_test() ->
    R = lora_plan:nearest(923.81, [923.2, 923.4, 923.6, 923.8, 924.0, 924.2, 924.4, 924.6]),
    io:format("R=~w~n", [R]),
    R2 = lora_plan:nearest(923.9, [923.2, 923.4, 923.6, 923.8, 924.0, 924.2, 924.4, 924.6]),
    io:format("R=~w~n", [R2]).

datarate_test() ->
    Plan = lora_plan:plan_us915_SB2(),
    DataRateAtom = 'SF7BW125',
    DataRateBinary = <<"SF7BW125">>,
    DataRateString = "SF7BW125",
    DataRateIndex = 3,
    List = [DataRateAtom, DataRateBinary, DataRateString, DataRateIndex],
    [?assertEqual(DataRateAtom, lora_plan:datarate_to_atom(Plan, X)) || X <- List],
    [?assertEqual(DataRateBinary, lora_plan:datarate_to_binary(Plan, X)) || X <- List],
    [?assertEqual(DataRateString, lora_plan:datarate_to_string(Plan, X)) || X <- List],
    [?assertEqual(DataRateIndex, lora_plan:datarate_to_index(Plan, X)) || X <- List].

dualplan_test() ->
    DR = 'SF7BW125',
    ?assertEqual(true, lora_plan:valid_region('US915')),
    ?assertEqual(true, lora_plan:valid_region('AS923_1')),
    ?assertEqual(true, lora_plan:valid_region('AU915_SB5')),
    ?assertEqual(true, lora_plan:valid_region('AU915_DP')),
    ?assertEqual(true, lora_plan:valid_region('AS923_1B')),
    ?assertEqual(false, lora_plan:valid_region('ZZ915')),
    ?assertEqual('US915', lora_plan:dualplan_region('US915', 923.2, DR)),
    ?assertEqual('AS923_1', lora_plan:dualplan_region('AS923_1', 923.2, DR)),
    ?assertEqual('AS923_1', lora_plan:dualplan_region('AS923_1', 923.4, DR)),
    ?assertEqual('AU915_SB5', lora_plan:dualplan_region('AS923_1', 923.6, DR)),
    ?assertEqual('AU915_SB5', lora_plan:dualplan_region('AS923_1', 923.61, DR)),
    ?assertEqual('AU915_SB5', lora_plan:dualplan_region('AS923_1', 923.59, DR)),
    ?assertEqual('AS923_1B', lora_plan:dualplan_region('AS923_1B', 923.6, DR)),
    ?assertEqual('AU915_DP', lora_plan:dualplan_region('AS923_1B', 923.0, DR)),
    ?assertEqual('AU915_DP', lora_plan:dualplan_region('AS923_1B', 923.01, DR)),
    ?assertEqual('AU915_DP', lora_plan:dualplan_region('AS923_1B', 922.96, DR)),
    ?assertEqual('AS923_1', lora_plan:dualplan_region('AS923_1', 923.0, DR)),
    ?assertEqual('AS923_1', lora_plan:dualplan_region('AS923_1', 924.8, DR)).

%%
%% Test Helper Functions
%%

valid_round(F1, F2, Precision) ->
    R2 = lora_plan:round_frequency(F2, Precision),
    % io:format("F1=~w F2=~w R2=~w~n", [F1, F2, R2]),
    ?assertEqual(F1, R2).

valid_frequency(Expect, Actual, List) ->
    F = lora_plan:nearest(Actual, List),
    ?assertEqual(Expect, F).

valid_uplink_freq(Plan, Freq) when Plan#channel_plan.base_region == 'EU433' ->
    case Freq of
        433.175 -> true;
        433.375 -> true;
        433.575 -> true;
        _ -> false
    end;
valid_uplink_freq(Plan, Freq) ->
    Region = Plan#channel_plan.base_region,
    F0 = test_region:uch2f(Region, 0),
    case Freq of
        F0 ->
            true;
        _ ->
            Ch = test_region:f2uch(Region, Freq),
            % Freq2 = test_region:uch2f(Region, Ch),
            % io:format("Freq=~w Ch=~w Freq2=~w ~n", [Freq, Ch, Freq2]),
            (Ch > 0)
    end.

seek_freq(Plan, Freq) ->
    Region = Plan#channel_plan.base_region,
    test_region:f2uch(Region, Freq).

validate_channel(Plan, Ch) ->
    % Region = Plan#channel_plan.base_region,
    % io:format("Region=~w~n", [Region]),
    F1 = lora_plan:channel_to_freq(Plan, Ch),
    Ch2 = lora_plan:freq_to_channel(Plan, F1),
    F2 = lora_plan:channel_to_freq(Plan, Ch2),
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
    PowerTable = test_region:uplink_power_table(Region),
    PT0 = [X || {_I, X} <- PowerTable],
    PT1 = lora_plan:tx_power_list(Plan),
    % io:format("Region=~w~n", [Region]),
    % io:format("PowerTable=~w~n", [PowerTable]),
    % io:format("PT0=~w~n", [PT0]),
    % io:format("PT1=~w~n", [PT1]),
    % io:format("Plan#channel_plan.tx_powers=~w~n", [Plan#channel_plan.tx_power]),
    ?assertEqual(PT0, PT1).

validate_downlink_size(Plan, DataRateAtom) ->
    M1 = lora_plan:max_uplink_payload_size(Plan, DataRateAtom),
    DRIdx = lora_plan:datarate_to_index(Plan, DataRateAtom),
    case DRIdx of
        15 ->
            ?assertEqual(true, true);
        _ ->
            DRAtom = lora_plan:datarate_to_atom(Plan, DRIdx),
            ?assertEqual(DRAtom, DataRateAtom),
            UplinkDwellTime = Plan#channel_plan.uplink_dwell_time,
            M2 = lora_plan:dwelltime_payload_size(DRAtom, UplinkDwellTime),
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
    ?assertEqual(DRAtom, DRAtom2).

print_txq(Plan, TxQ, Enable) ->
    case Enable of
        true ->
            io:format("TxQ Frequency = ~w~n", [TxQ#txq.freq]),
            io:format("TxQ Region = ~w~n", [Plan#channel_plan.base_region]),
            io:format("TxQ DataRate = ~w~n", [lora_plan:datarate_to_atom(Plan, TxQ#txq.datr)]),
            io:format("TxQ DRIndex = ~w~n", [lora_plan:datarate_to_index(Plan, TxQ#txq.datr)]);
        false ->
            ok
    end.

validate_rx2_window(Plan, RxQ) ->
    Region = Plan#channel_plan.base_region,
    TxQ_P = lora_plan:rx2_window(Plan, 0, RxQ),
    TxQ_R = test_region:rx2_window(Region, 0, RxQ),
    ?assertEqual(TxQ_R, TxQ_P),
    validate_txq(Plan, TxQ_P).

validate_join2_window(Plan, RxQ) ->
    Region = Plan#channel_plan.base_region,
    TxQ_P = lora_plan:join2_window(Plan, RxQ),
    TxQ_R = test_region:join2_window(Region, RxQ),
    ?assertEqual(TxQ_R, TxQ_P),
    validate_txq(Plan, TxQ_P).

validate_rx1_window(Plan, RxQ) ->
    Region = Plan#channel_plan.base_region,
    TxQ_P = lora_plan:rx1_window(Plan, 0, 0, RxQ),
    TxQ_R = test_region:rx1_window(Region, 0, 0, RxQ),
    ?assertEqual(TxQ_R, TxQ_P),
    validate_txq(Plan, TxQ_P).

validate_join1_window(Plan, RxQ) ->
    Region = Plan#channel_plan.base_region,
    TxQ_P = lora_plan:join1_window(Plan, 0, RxQ),
    TxQ_R = test_region:join1_window(Region, 0, RxQ),
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
validate_window(Plan, _DataRateAtom, _Channel) when Plan#channel_plan.base_region == 'AS923_2' ->
    ?assert(true);
validate_window(Plan, _DataRateAtom, _Channel) when Plan#channel_plan.base_region == 'AS923_3' ->
    ?assert(true);
validate_window(Plan, _DataRateAtom, _Channel) when Plan#channel_plan.base_region == 'AS923_4' ->
    ?assert(true);
validate_window(Plan, _DataRateAtom, _Channel) when Plan#channel_plan.base_region == 'IN865' ->
    ?assert(true);
validate_window(Plan, _DataRateAtom, _Channel) when Plan#channel_plan.base_region == 'KR920' ->
    ?assert(true);
validate_window(Plan, DataRateAtom, Channel) ->
    Region = Plan#channel_plan.base_region,
    % io:format("validate_window Region=~w DataRate=~w Channel=~w~n",
    % [Region, DataRateAtom, Channel]),
    DataRateStr = lora_plan:datarate_to_binary(Plan, DataRateAtom),
    [JoinChannel_0 | _] = Plan#channel_plan.u_channels,
    J0 = lora_plan:channel_to_freq(Plan, 0),
    ?assertEqual(JoinChannel_0, J0),
    Frequency = lora_plan:channel_to_freq(Plan, Channel),

    Now = os:timestamp(),
    RxQ = #rxq{
        freq = Frequency,
        datr = DataRateStr,
        codr = <<"4/5">>,
        time = calendar:now_to_datetime(Now),
        tmms = 0,
        rssi = 42.2,
        lsnr = lora_plan:max_uplink_snr(Plan, DataRateAtom)
    },

    validate_rx2_window(Plan, RxQ),
    validate_join2_window(Plan, RxQ),
    validate_rx1_window(Plan, RxQ),
    validate_join1_window(Plan, RxQ),

    TxQ_1 = lora_plan:rx1_window(Plan, 0, 0, RxQ),
    print_txq(Plan, TxQ_1, false),

    TxQ_2 = lora_plan:rx2_window(Plan, 0, RxQ),
    print_txq(Plan, TxQ_2, false),

    TxQ_3 = lora_plan:join2_window(Plan, RxQ),
    print_txq(Plan, TxQ_3, false),
    _DRIdx_3 = test_region:datar_to_dr(Region, TxQ_3#txq.datr),
    ok.

validate_snr(Plan, DRIndex) ->
    Region = Plan#channel_plan.base_region,
    MaxUplinkSnr01 = lora_plan:max_uplink_snr(Plan, DRIndex),
    MaxUplinkSnr02 = test_region:max_uplink_snr(Region, DRIndex),
    ?assertEqual(MaxUplinkSnr02, MaxUplinkSnr01),
    MaxDownlinkSnr01 = lora_plan:max_downlink_snr(Plan, DRIndex, 0),
    MaxDownlinkSnr02 = test_region:max_downlink_snr(Region, DRIndex, 0),
    ?assertEqual(MaxDownlinkSnr02, MaxDownlinkSnr01).

exercise_snr(Plan) when Plan#channel_plan.base_region == 'US915' ->
    [validate_snr(Plan, X) || X <- [0, 1, 2, 3, 4]];
exercise_snr(Plan) when Plan#channel_plan.base_region == 'AU915' ->
    [validate_snr(Plan, X) || X <- [0, 1, 2, 3, 4]];
exercise_snr(Plan) when Plan#channel_plan.base_region == 'CN470' ->
    [validate_snr(Plan, X) || X <- [0, 1, 2, 3, 4, 5]];
exercise_snr(Plan) when Plan#channel_plan.base_region == 'IN865' ->
    [validate_snr(Plan, X) || X <- [0, 1, 2, 3, 4, 5]];
exercise_snr(Plan) when Plan#channel_plan.base_region == 'KR920' ->
    [validate_snr(Plan, X) || X <- [0, 1, 2, 3, 4, 5]];
exercise_snr(Plan) ->
    [validate_snr(Plan, X) || X <- [0, 1, 2, 3, 4, 5, 6]].

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

-endif.

%% end of file
