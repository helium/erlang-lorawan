-module(lora_channel).

-export([
    freq_to_channel/2,
    channel_to_freq/2
]).

-include("lorawan.hrl").

-spec freq_to_channel(#channel_plan{}, number()) -> integer().
freq_to_channel(Plan, Freq) ->
	List = (Plan#channel_plan.channels),
	Channel = index_of(Freq, List),
	Channel.

-spec channel_to_freq(#channel_plan{}, integer()) -> number().
channel_to_freq(Plan, Ch) ->
	List = (Plan#channel_plan.channels),
	Freq = lists:nth(Ch, List),
	Freq.

index_of(Value, List) ->
   Map = lists:zip(List, lists:seq(1, length(List))),
   case lists:keyfind(Value, 1, Map) of
      {Value, Index} -> Index;
      false -> notfound
   end.

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
%%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

tx_power_1() ->
	#tx_power{
   	id = 1,
    	eirp = 16
   }.

dr_list() ->
   Data_Rates = [
			#data_rate{id = 0, name = 'SF12BW125', max_size = 59, no_repeater_size = 59, bit_rate = 250},
			#data_rate{id = 1, name = 'SF11BW125', max_size = 59, no_repeater_size = 59, bit_rate = 440},
			#data_rate{id = 2, name = 'SF10BW125', max_size = 59, no_repeater_size = 59, bit_rate = 980},
			#data_rate{id = 3, name = 'SF9BW125', max_size = 123, no_repeater_size = 123, bit_rate = 1760},
			#data_rate{id = 4, name = 'SF8BW125', max_size = 239, no_repeater_size = 230, bit_rate = 3125},
			#data_rate{id = 5, name = 'SF7BW125', max_size = 230, no_repeater_size = 230, bit_rate = 5470},
			#data_rate{id = 6, name = 'SF7BW250', max_size = 230, no_repeater_size = 230, bit_rate = 11000},
			#data_rate{id = 7, name = 'FSK50', max_size = 230, no_repeater_size = 230, bit_rate = 50000},
			#data_rate{id = 8, name = 'CR13BW137', max_size = 58, no_repeater_size = 58, bit_rate = 162},
			#data_rate{id = 9, name = 'CR23BW137', max_size = 123, no_repeater_size = 123, bit_rate = 325},
			#data_rate{id = 10, name = 'CR13BW336', max_size = 58, no_repeater_size = 58, bit_rate = 162},
			#data_rate{id = 11, name = 'CR23BW336', max_size = 123, no_repeater_size = 123, bit_rate = 325}
   ],
   Data_Rates.

plan_eu868() ->
   EU868 = #channel_plan{
   	id = 1,
    	name = 'EU868',
    	region = 'EU868',
    	dynamic_plan = true,
    	min_freq = 863.0,
    	max_freq = 870.0,
    	%% channels = [867.1, 867.3, 867.5, 867.7, 867.9, 868.1, 868.3, 868.5],
    	channels = [868.1, 868.3, 868.5, 864.3, 864.5, 864.7, 864.9, 865.1],
    	channel_count = 8,
    	join_channels = {0, 2},
    	data_rates = [1,2,3,4,5,6,7,8,9,10,11],
    	tx_powers = [16,14,12,10,8,6,4,2],
		join_dr = {0, 5},
		mandatory_dr = {0, 5},
		optional_dr = {6, 7},
		max_duty_cycle = 1,
		dwell_time_limit = 0,
		tx_param_setup_allowed = false,
		max_eirp_db = 16,
		default_rx1_offset = 0,
		allowed_rx1_offset = 7,
		default_rx2_datarate = 0,
		default_rx2_freq = 869.525,
		default_beacon_freq = 869.525,
		default_pingslot_freq = 869.525
   },
   EU868.

validate_channels(Region, List) ->
	F0 = lists:nth(1, List),
	Freq0 = lora_region:uch2f(Region, 0),
	?assertEqual(Freq0, F0),
	F1 = lists:nth(2, List),
	Freq1 = lora_region:uch2f(Region, 1),
	?assertEqual(Freq1, F1),
	F2 = lists:nth(3, List),
	Freq2 = lora_region:uch2f(Region, 2),
	?assertEqual(Freq2, F2),
	F3 = lists:nth(4, List),
	Freq3 = lora_region:uch2f(Region, 3),
	?assertEqual(Freq3, F3),
	F4 = lists:nth(5, List),
	Freq4 = lora_region:uch2f(Region, 4),
	?assertEqual(Freq4, F4),
	F5 = lists:nth(6, List),
	Freq5 = lora_region:uch2f(Region, 5),
	?assertEqual(Freq5, F5),
	F6 = lists:nth(7, List),
	Freq6 = lora_region:uch2f(Region, 6),
	?assertEqual(Freq6, F6),
	F7 = lists:nth(8, List),
	Freq7 = lora_region:uch2f(Region, 7),
	?assertEqual(Freq7, F7),
	fin.

exercise_plan(Plan) ->
	Region = Plan#channel_plan.region,
	validate_channels(Region, Plan#channel_plan.channels).

payload_util_test() ->

	Plan = plan_eu868(),

	Freq = lora_region:uch2f('EU868', 0),
	io:format("Freq=~w~n", [Freq]),

	exercise_plan(Plan),

	fin.

%% -endif.
%% end of file