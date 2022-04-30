-module(lora_chmask).

-export([
    make_link_adr_req/3,
    join_cf_list/1
]).

%% -------------------------------------------------------------------
%% CFList functions
%% -------------------------------------------------------------------

-spec join_cf_list(atom()) -> binary().
join_cf_list('US915') ->
    %% https://lora-alliance.org/wp-content/uploads/2021/05/RP-2-1.0.3.pdf
    %% Page 33
    Chans = [{8, 15}],
    ChMaskTable = [
        {2, mask, build_chmask(Chans, {0, 15})},
        {2, mask, build_chmask(Chans, {16, 31})},
        {2, mask, build_chmask(Chans, {32, 47})},
        {2, mask, build_chmask(Chans, {48, 63})},
        {2, mask, build_chmask(Chans, {64, 71})},
        {2, rfu, 0},
        {3, rfu, 0},
        {1, cf_list_type, 1}
    ],
    cf_list_for_channel_mask_table(ChMaskTable);
join_cf_list('EU868') ->
    %% In this case the CFList is a list of five channel frequencies for the channels
    %% three to seven whereby each frequency is encoded as a 24 bits unsigned integer
    %% (three octets). All these channels are usable for DR0 to DR5 125kHz LoRa
    %% modulation. The list of frequencies is followed by a single CFListType octet
    %% for a total of 16 octets. The CFListType SHALL be equal to zero (0) to indicate
    %% that the CFList contains a list of frequencies.
    %%
    %% The actual channel frequency in Hz is 100 x frequency whereby values representing
    %% frequencies below 100 MHz are reserved for future use.
    cflist_for_frequencies([8671000, 8673000, 8675000, 8677000, 8679000]);
join_cf_list('AS923_1') ->
    cflist_for_frequencies([9236000, 9238000, 9240000, 9242000, 9244000]);
join_cf_list('AS923_2') ->
    cflist_for_frequencies([9218000, 9220000, 9222000, 9224000, 9226000]);
join_cf_list('AS923_3') ->
    cflist_for_frequencies([9170000, 9172000, 9174000, 9176000, 9178000]);
join_cf_list('AS923_4') ->
    cflist_for_frequencies([9177000, 9179000, 9181000, 9183000, 9185000]);
join_cf_list(_Region) ->
    <<>>.

-spec cflist_for_frequencies(list(non_neg_integer())) -> binary().
cflist_for_frequencies(Frequencies) ->
    Channels = <<
        <<X:24/integer-unsigned-little>>
     || X <- Frequencies
    >>,
    <<Channels/binary, 0:8/integer>>.

-spec cf_list_for_channel_mask_table([
    {ByteSize :: pos_integer(), Type :: atom(), Value :: non_neg_integer()}
]) -> binary().
cf_list_for_channel_mask_table(ChMaskTable) ->
    <<<<Val:Size/little-unit:8>> || {Size, _, Val} <- ChMaskTable>>.

%% ------------------------------------------------------------------
%% @doc Top Level Region
%% AS923 has sub-regions. Besides for the cflist during joining,
%% they should be treated the same.
%% @end
%% ------------------------------------------------------------------
-spec top_level_region(atom()) -> atom().
top_level_region('AS923_1') -> 'AS923';
top_level_region('AS923_2') -> 'AS923';
top_level_region('AS923_3') -> 'AS923';
top_level_region('AS923_4') -> 'AS923';
top_level_region(Region) -> Region.

-spec make_link_adr_req(atom(), tuple(), list()) -> list().
make_link_adr_req(Region, Tuple, FOptsOut) ->
    TopLevelRegion = top_level_region(Region),
    make_link_adr_req_(TopLevelRegion, Tuple, FOptsOut).

%% link_adr_req command

make_link_adr_req_(Region, {0, <<"NoChange">>, Chans}, FOptsOut) when
    Region == 'US915'; Region == 'AU915'
->
    case all_bit({0, 63}, Chans) of
        true ->
            [
                {link_adr_req, 16#F, 16#F, build_chmask(Chans, {64, 71}), 6, 0}
                | FOptsOut
            ];
        false ->
            [
                {link_adr_req, 16#F, 16#F, build_chmask(Chans, {64, 71}), 7, 0}
                | append_mask(Region, 3, {0, <<"NoChange">>, Chans}, FOptsOut)
            ]
    end;
make_link_adr_req_(Region, {TXPower, DataRate, Chans}, FOptsOut) when
    Region == 'US915'; Region == 'AU915'
->
    Plan = lora_plan:region_to_plan(Region),
    DataRateAtom = lora_plan:datarate_to_atom(DataRate),
    DataRateIdx = lora_plan:datarate_to_index(Plan, DataRateAtom),
    case all_bit({0, 63}, Chans) of
        true ->
            [
                {link_adr_req, DataRateIdx, TXPower, build_chmask(Chans, {64, 71}), 6, 0}
                | FOptsOut
            ];
        false ->
            [
                {link_adr_req, DataRateIdx, TXPower, build_chmask(Chans, {64, 71}), 7, 0}
                | append_mask(Region, 3, {TXPower, DataRate, Chans}, FOptsOut)
            ]
    end;
make_link_adr_req_(Region, {TXPower, DataRate, Chans}, FOptsOut) when Region == 'CN470' ->
    Plan = lora_plan:region_to_plan(Region),
    DataRateAtom = lora_plan:datarate_to_atom(DataRate),
    DataRateIdx = lora_plan:datarate_to_index(Plan, DataRateAtom),
    case all_bit({0, 95}, Chans) of
        true ->
            [
                {link_adr_req, DataRateIdx, TXPower, 0, 6, 0}
                | FOptsOut
            ];
        false ->
            append_mask(Region, 5, {TXPower, DataRate, Chans}, FOptsOut)
    end;
make_link_adr_req_(Region, {TXPower, DataRate, Chans}, FOptsOut) ->
    Plan = lora_plan:region_to_plan(Region),
    DataRateAtom = lora_plan:datarate_to_atom(DataRate),
    DataRateIdx = lora_plan:datarate_to_index(Plan, DataRateAtom),
    [
        {link_adr_req, DataRateIdx, TXPower, build_chmask(Chans, {0, 15}), 0, 0}
        | FOptsOut
    ].

all_bit(MinMax, Chans) ->
    lists:any(
        fun(Tuple) -> match_whole(MinMax, Tuple) end,
        Chans
    ).

match_whole(MinMax, {A, B}) when B < A ->
    match_whole(MinMax, {B, A});
match_whole({Min, Max}, {A, B}) ->
    (A =< Min) and (B >= Max).

-spec build_chmask(
    list({non_neg_integer(), non_neg_integer()}),
    {non_neg_integer(), non_neg_integer()}
) -> non_neg_integer().
build_chmask(Chans, {Min, Max}) ->
    Bits = Max - Min + 1,
    lists:foldl(
        fun(Tuple, Acc) ->
            <<Num:Bits>> = build_chmask0({Min, Max}, Tuple),
            Num bor Acc
        end,
        0,
        Chans
    ).

build_chmask0(MinMax, {A, B}) when B < A ->
    build_chmask0(MinMax, {B, A});
build_chmask0({Min, Max}, {A, B}) when B < Min; Max < A ->
    %% out of range
    <<0:(Max - Min + 1)>>;
build_chmask0({Min, Max}, {A, B}) ->
    C = max(Min, A),
    D = min(Max, B),
    Bits = Max - Min + 1,
    %% construct the binary
    Bin = <<-1:(D - C + 1), 0:(C - Min)>>,
    case bit_size(Bin) rem Bits of
        0 -> Bin;
        N -> <<0:(Bits - N), Bin/bits>>
    end.

append_mask(_Region, Idx, _, FOptsOut) when Idx < 0 ->
    FOptsOut;
append_mask(Region, Idx, {0, <<"NoChange">>, Chans}, FOptsOut) ->
    append_mask(
        Region,
        Idx - 1,
        {0, <<"NoChange">>, Chans},
        case build_chmask(Chans, {16 * Idx, 16 * (Idx + 1) - 1}) of
            0 ->
                FOptsOut;
            ChMask ->
                [{link_adr_req, 16#F, 16#F, ChMask, Idx, 0} | FOptsOut]
        end
    );
append_mask(Region, Idx, {TXPower, DataRate, Chans}, FOptsOut) ->
    Plan = lora_plan:region_to_plan(Region),
    DataRateAtom = lora_plan:datarate_to_atom(DataRate),
    DataRateIdx = lora_plan:datarate_to_index(Plan, DataRateAtom),
    append_mask(
        Region,
        Idx - 1,
        {TXPower, DataRate, Chans},
        case build_chmask(Chans, {16 * Idx, 16 * (Idx + 1) - 1}) of
            0 ->
                FOptsOut;
            ChMask ->
                [
                    {link_adr_req, DataRateIdx, TXPower, ChMask, Idx, 0}
                    | FOptsOut
                ]
        end
    ).

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

expand_intervals([{A, B} | Rest]) ->
    lists:seq(A, B) ++ expand_intervals(Rest);
expand_intervals([]) ->
    [].

some_bit(MinMax, Chans) ->
    lists:any(
        fun(Tuple) -> match_part(MinMax, Tuple) end,
        Chans
    ).

none_bit(MinMax, Chans) ->
    lists:all(
        fun(Tuple) -> not match_part(MinMax, Tuple) end,
        Chans
    ).

match_part(MinMax, {A, B}) when B < A ->
    match_part(MinMax, {B, A});
match_part({Min, Max}, {A, B}) ->
    (A =< Max) and (B >= Min).

bits_test_() ->
    PlanEU868 = lora_plan:region_to_plan('EU868'),
    PlanUS915 = lora_plan:region_to_plan('US915'),
    [
        ?_assertEqual([0, 1, 2, 5, 6, 7, 8, 9], expand_intervals([{0, 2}, {5, 9}])),
        ?_assertEqual(7, build_chmask([{0, 2}], {0, 15})),
        ?_assertEqual(0, build_chmask([{0, 2}], {16, 31})),
        ?_assertEqual(65535, build_chmask([{0, 71}], {0, 15})),
        ?_assertEqual(16#FF00, build_chmask([{8, 15}], {0, 15})),
        ?_assertEqual(16#F800, build_chmask([{11, 15}], {0, 15})),
        ?_assertEqual(16#F000, build_chmask([{12, 15}], {0, 15})),
        ?_assertEqual(16#0F00, build_chmask([{8, 11}], {0, 15})),
        ?_assertEqual(0, build_chmask([{8, 15}], {16, 31})),
        ?_assertEqual(0, build_chmask([{8, 15}], {32, 47})),
        ?_assertEqual(0, build_chmask([{8, 15}], {48, 63})),
        ?_assertEqual(0, build_chmask([{8, 15}], {64, 71})),
        ?_assertEqual(16#1, build_chmask([{64, 64}], {64, 71})),
        ?_assertEqual(16#2, build_chmask([{65, 65}], {64, 71})),
        ?_assertEqual(16#7, build_chmask([{64, 66}], {64, 71})),
        ?_assertEqual(true, some_bit({0, 71}, [{0, 71}])),
        ?_assertEqual(true, all_bit({0, 71}, [{0, 71}])),
        ?_assertEqual(false, none_bit({0, 71}, [{0, 71}])),
        ?_assertEqual(true, some_bit({0, 15}, [{0, 2}])),
        ?_assertEqual(false, all_bit({0, 15}, [{0, 2}])),
        ?_assertEqual(false, none_bit({0, 15}, [{0, 2}])),
        ?_assertEqual(
            [{link_adr_req, lora_plan:datarate_to_index(PlanEU868, 'SF12BW125'), 14, 7, 0, 0}],
            make_link_adr_req('EU868', {14, <<"SF12BW125">>, [{0, 2}]}, [])
        ),
        ?_assertEqual(
            [
                {link_adr_req, lora_plan:datarate_to_index(PlanUS915, 'SF12BW500'), 20, 0, 7, 0},
                {link_adr_req, lora_plan:datarate_to_index(PlanUS915, 'SF12BW500'), 20, 255, 0, 0}
            ],
            make_link_adr_req('US915', {20, <<"SF12BW500">>, [{0, 7}]}, [])
        ),
        ?_assertEqual(
            [
                {link_adr_req, lora_plan:datarate_to_index(PlanUS915, 'SF12BW500'), 20, 2, 7, 0},
                {link_adr_req, lora_plan:datarate_to_index(PlanUS915, 'SF12BW500'), 20, 65280, 0, 0}
            ],
            make_link_adr_req('US915', {20, <<"SF12BW500">>, [{8, 15}, {65, 65}]}, [])
        )
    ].

join_cf_list_test_() ->
    [
        ?_assertEqual(
            %% Active Channels 8-15
            <<0, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1>>,
            join_cf_list('US915')
        ),
        ?_assertEqual(
            %% Freqs 923.6, 923.8, 924.0, 924.2, 924.4
            <<32, 238, 140, 240, 245, 140, 192, 253, 140, 144, 5, 141, 96, 13, 141, 0>>,
            join_cf_list('AS923_1')
        ),
        ?_assertEqual(
            %% Freqs 921.8, 922.0, 922.2, 922.4, 922.6
            <<208, 167, 140, 160, 175, 140, 112, 183, 140, 64, 191, 140, 16, 199, 140, 0>>,
            join_cf_list('AS923_2')
        ),
        ?_assertEqual(
            %% Freqs 917.0, 917.2, 917.4, 917.6, 917.8
            <<80, 236, 139, 32, 244, 139, 240, 251, 139, 192, 3, 140, 144, 11, 140, 0>>,
            join_cf_list('AS923_3')
        ),
        ?_assertEqual(
            %% Freqs 917.7, 917.9, 918.1, 918.3, 918.5
            <<168, 7, 140, 120, 15, 140, 72, 23, 140, 24, 31, 140, 232, 38, 140, 0>>,
            join_cf_list('AS923_4')
        ),
        ?_assertEqual(
            %% Freqs 867.1, 867.3, 867.5, 867.7, 867.9
            <<24, 79, 132, 232, 86, 132, 184, 94, 132, 136, 102, 132, 88, 110, 132, 0>>,
            join_cf_list('EU868')
        )
    ].

-endif.
%% end of file
