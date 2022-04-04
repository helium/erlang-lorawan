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
    	f_channels = [868.1, 868.3, 868.5, 867.1, 867.3, 867.5, 867.7, 867.9],
    	u_channels = [868.1, 868.3, 868.5, 864.3, 864.5, 864.7, 864.9, 865.1],
    	d_channels = [868.1, 868.3, 868.5, 868.7, 868.9, 869.1, 869.3, 869.5],
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

validate_u_channels(Region, List) ->
	TList = [
      lora_region:uch2f(Region, F)
      || F <- [0, 1, 2, 3, 4, 5, 6, 7]
   ],
	?assertEqual(List, TList),
	fin.

validate_d_channels(Region, List) ->
	TList = [
      lora_region:dch2f(Region, F)
      || F <- [0, 1, 2, 3, 4, 5, 6, 7]
   ],
	?assertEqual(List, TList),
	fin.

validate_u_frequences(Region, List) ->
	TList = [
      lora_region:f2uch(Region, F)
      || F <- List
   ],
	?assertEqual([0,1,2,-11,-10,-9,-8,-7], TList),
	fin.

validate_d_frequences(Region, List) ->
	TList = [
      lora_region:f2dch(Region, F)
      || F <- List
   ],
	?assertEqual([0,1,2,3,4,5,6,7], TList),
	fin.

exercise_plan(Plan) ->
	Region = Plan#channel_plan.region,
	validate_u_channels(Region, Plan#channel_plan.u_channels),
	validate_d_channels(Region, Plan#channel_plan.d_channels),
	validate_u_frequences(Region, Plan#channel_plan.u_channels),
	validate_d_frequences(Region, Plan#channel_plan.d_channels),
	fin.

payload_util_test() ->

	Plan = plan_eu868(),

	Freq = lora_region:uch2f('EU868', 0),
	io:format("Freq=~w~n", [Freq]),

	exercise_plan(Plan),

	fin.

%% -endif.
%% end of file