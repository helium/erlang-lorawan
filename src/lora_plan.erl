-module(lora_plan).

-export([
    freq_to_channel/2,
    channel_to_freq/2,
    tx_power/2,
    tx_power_list/1,
    region_to_plan/1,
    rx2_datarate/1,
    max_payload_size/1
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

-spec max_payload_size(integer()) -> integer().
max_payload_size(_DataRateID) ->
    250.

-spec rx2_datarate(#channel_plan{}) -> integer().
rx2_datarate(Plan) ->
    Plan#channel_plan.rx2_datarate.

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

%-spec tx_power(#channel_plan{}, integer()) -> float().
tx_power(Plan, Index) when Index < 16 ->
    List = (Plan#channel_plan.tx_power),
    Offset = lists:nth(Index, List),
    ComputedPower = Plan#channel_plan.max_eirp_db + Offset,
    ComputedPower.

%-spec tx_power_list(#channel_plan{}) -> [float()].
tx_power_list(Plan) ->
    List = (Plan#channel_plan.tx_power),
    [Plan#channel_plan.max_eirp_db + Offset || Offset <- List].

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
        data_rates = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11],
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
        data_rates = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11],
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
        data_rates = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11],
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
        data_rates = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11],
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
        data_rates = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11],
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
        data_rates = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11],
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
        data_rates = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11],
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

% tx_power_1() ->
% 	#tx_power{
%    	id = 1,
%     	eirp = 16
%    }.

% dr_list() ->
%    Data_Rates = [
% 			#data_rate{id = 0, name = 'SF12BW125', max_size = 59, no_repeater_size = 59, bit_rate = 250},
% 			#data_rate{id = 1, name = 'SF11BW125', max_size = 59, no_repeater_size = 59, bit_rate = 440},
% 			#data_rate{id = 2, name = 'SF10BW125', max_size = 59, no_repeater_size = 59, bit_rate = 980},
% 			#data_rate{id = 3, name = 'SF9BW125', max_size = 123, no_repeater_size = 123, bit_rate = 1760},
% 			#data_rate{id = 4, name = 'SF8BW125', max_size = 239, no_repeater_size = 230, bit_rate = 3125},
% 			#data_rate{id = 5, name = 'SF7BW125', max_size = 230, no_repeater_size = 230, bit_rate = 5470},
% #data_rate{id = 6, name = 'SF7BW250', max_size = 230, no_repeater_size = 230, bit_rate = 11000},
% 			#data_rate{id = 7, name = 'FSK50', max_size = 230, no_repeater_size = 230, bit_rate = 50000},
% 			#data_rate{id = 8, name = 'CR13BW137', max_size = 58, no_repeater_size = 58, bit_rate = 162},
% 			#data_rate{id = 9, name = 'CR23BW137', max_size = 123, no_repeater_size = 123, bit_rate = 325},
% 			#data_rate{id = 10, name = 'CR13BW336', max_size = 58, no_repeater_size = 58, bit_rate = 162},
% 			#data_rate{id = 11, name = 'CR23BW336', max_size = 123, no_repeater_size = 123, bit_rate = 325}
%    ],
%    Data_Rates.

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
