-module(lora_chmask).

-export([
    cflist_type_0/1,
    cflist_type_1/1,
    make_link_adr_req/3,
    build_link_adr_req/3,
    join_cf_list/1
]).

-include("lora.hrl").

%% -------------------------------------------------------------------
%% CFList functions
%% -------------------------------------------------------------------

% Join-Accept CFList Type 0

% In this case the CFList is a list of five channel frequencies for the
% channels three to seven whereby each frequency is encoded as a 24
% bits unsigned integer (three octets). All these channels are usable
% for DR0 to DR5 125 kHz LoRa modulation. The list of frequencies is
% followed by a single CFListType octet for a total of 16 octets. The
% CFListType SHALL be equal to zero (0) to indicate that the CFList
% contains a list of frequencies.

% The actual channel frequency in Hz is 100 x frequency whereby values
% representing frequencies below 100 MHz are reserved for future use.
% This allows setting the frequency of a channel anywhere between 100
% MHz to 1.678 GHz in 100 Hz steps. Unused channels have a frequency
% value of 0. The CFList is OPTIONAL, and its presence can be detected
% by the length of the join-accept message. If present, the CFList
% SHALL replace all the previous channels stored in the end-device
% apart from the three default channels. The newly defined channels are
% immediately enabled and usable by the end-device for communication.

-spec cflist_type_0(list(non_neg_integer())) -> binary().
cflist_type_0(Frequencies) ->
    Channels = <<
        <<X:24/integer-unsigned-little>>
     || X <- Frequencies
    >>,
    <<Channels/binary, 0:8/integer>>.

% Both Fixed and Dynamic Channel Plan regions support CFList Type 1.

% If the CFlist is not empty, then the CFListType field SHALL contain the
% value one (0x01) to indicate the CFList contains a series of ChMask
% fields. ChMask0 controls the first 16 channels, ChMask1 the second 16
% channels, up to a maximum of 96 channels.   Within each ChMask field,
% each bit corresponds to a single channel identified by the following
% formula: ChMask-field-number * 16 + ChMask-bit-number = Channel-Id

% End-devices SHALL silently ignore bits set for channels not defined for
% the channel plan they are operating under.  End-devices SHALL silently
% ignore bits set for channels which refer to frequencies not available
% for use in the regulatory region the end-device is currently operating
% in.  If no bits are set in the CFList ChMask fields, the end-device
% SHALL operate on all default channels

-spec cflist_type_1({integer(), integer()}) -> binary().
cflist_type_1(SubBand) ->
    %% https://lora-alliance.org/wp-content/uploads/2021/05/RP-2-1.0.3.pdf
    %% Page 33
    Chans = [SubBand],
    ChMaskTable = [
        {2, mask, build_chmask(Chans, {0, 15})},
        {2, mask, build_chmask(Chans, {16, 31})},
        {2, mask, build_chmask(Chans, {32, 47})},
        {2, mask, build_chmask(Chans, {48, 63})},
        {2, mask, build_chmask(Chans, {64, 79})},
        {2, mask, build_chmask(Chans, {80, 95})},
        {3, rfu, 0},
        {1, cf_list_type, 1}
    ],
    cf_list_for_channel_mask_table(ChMaskTable).

-spec cf_list_for_channel_mask_table([
    {ByteSize :: pos_integer(), Type :: atom(), Value :: non_neg_integer()}
]) -> binary().
cf_list_for_channel_mask_table(ChMaskTable) ->
    <<<<Val:Size/little-unit:8>> || {Size, _, Val} <- ChMaskTable>>.

-spec join_cf_list(atom()) -> binary().
join_cf_list('US915') ->
    cflist_type_1({8, 15});
join_cf_list('AU915') ->
    cflist_type_1({8, 15});
join_cf_list('EU868') ->
    cflist_type_0([8671000, 8673000, 8675000, 8677000, 8679000]);
join_cf_list('EU433') ->
    cflist_type_0([4331750, 4333750, 4335750, 0, 0]);
join_cf_list('CN470') ->
    cflist_type_0([4869000, 4871000, 4873000, 4875000, 4877000]);
join_cf_list('AS923_1') ->
    cflist_type_0([9236000, 9238000, 9240000, 9242000, 9244000]);
join_cf_list('AS923_1B') ->
    cflist_type_0([9220000, 9222000, 9234000, 9228000, 9230000]);
join_cf_list('AS923_2') ->
    cflist_type_0([9218000, 9220000, 9222000, 9224000, 9226000]);
join_cf_list('AS923_3') ->
    cflist_type_0([9170000, 9172000, 9174000, 9176000, 9178000]);
join_cf_list('AS923_4') ->
    cflist_type_0([9177000, 9179000, 9181000, 9183000, 9185000]);
join_cf_list('KR920') ->
    cflist_type_0([9227000, 9229000, 9231000, 9233000, 0]);
join_cf_list('IN865') ->
    cflist_type_0([0, 0, 0, 0, 0]);
join_cf_list(_Region) ->
    <<>>.

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
    DRIndex = lora_plan:datarate_to_index(Plan, DataRate),
    case all_bit({0, 63}, Chans) of
        true ->
            [
                {link_adr_req, DRIndex, TXPower, build_chmask(Chans, {64, 71}), 6, 0}
                | FOptsOut
            ];
        false ->
            [
                {link_adr_req, DRIndex, TXPower, build_chmask(Chans, {64, 71}), 7, 0}
                | append_mask(Region, 3, {TXPower, DataRate, Chans}, FOptsOut)
            ]
    end;
make_link_adr_req_(Region, {TXPower, DataRate, Chans}, FOptsOut) when Region == 'CN470' ->
    Plan = lora_plan:region_to_plan(Region),
    DRIndex = lora_plan:datarate_to_index(Plan, DataRate),
    case all_bit({0, 95}, Chans) of
        true ->
            [
                {link_adr_req, DRIndex, TXPower, 0, 6, 0}
                | FOptsOut
            ];
        false ->
            append_mask(Region, 5, {TXPower, DataRate, Chans}, FOptsOut)
    end;
make_link_adr_req_(Region, {TXPower, DataRate, Chans}, FOptsOut) ->
    Plan = lora_plan:region_to_plan(Region),
    DRIndex = lora_plan:datarate_to_index(Plan, DataRate),
    [
        {link_adr_req, DRIndex, TXPower, build_chmask(Chans, {0, 15}), 0, 0}
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

build_link_adr_req(Plan, {TXPower0, DataRate}, FOptsOut) ->
    Region = Plan#channel_plan.base_region,
    DRIndex0 = lora_plan:datarate_to_index(Plan, DataRate),
    {MinIndex, MaxIndex} = Plan#channel_plan.mask_dr,
    InRange = ((DRIndex0 >= MinIndex) and (DRIndex0 =< MaxIndex)),
    DRIndex1 =
        case InRange of
            true -> DRIndex0;
            false -> 0
        end,
    TXPower1 =
        case InRange of
            true -> TXPower0;
            false -> 0
        end,
    case Region of
        'US915' ->
            %% For US915 and AU915 currently only support subband 2
            Chans = [{8, 15}],
            [
                {link_adr_req, DRIndex1, TXPower1, 0, 7, 0}
                | append_mask(Region, 3, {TXPower1, DataRate, Chans}, FOptsOut)
            ];
        'AU915' ->
            Chans = [{8, 15}],
            [
                {link_adr_req, DRIndex1, TXPower1, 0, 7, 0}
                | append_mask(Region, 3, {TXPower1, DataRate, Chans}, FOptsOut)
            ];
        _ ->
            append_mask(Region, 5, {TXPower1, DRIndex1, [{0, 7}]}, FOptsOut)
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
    DRIndex = lora_plan:datarate_to_index(Plan, DataRate),
    append_mask(
        Region,
        Idx - 1,
        {TXPower, DataRate, Chans},
        case build_chmask(Chans, {16 * Idx, 16 * (Idx + 1) - 1}) of
            0 ->
                FOptsOut;
            ChMask ->
                [
                    {link_adr_req, DRIndex, TXPower, ChMask, Idx, 0}
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
    EU868Index = lora_plan:datarate_to_index(PlanEU868, 'SF12BW125'),
    US915Index = lora_plan:datarate_to_index(PlanUS915, 'SF12BW500'),
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
            [{link_adr_req, EU868Index, 14, 7, 0, 0}],
            make_link_adr_req('EU868', {14, <<"SF12BW125">>, [{0, 2}]}, [])
        ),
        ?_assertEqual(
            [
                {link_adr_req, US915Index, 20, 0, 7, 0},
                {link_adr_req, US915Index, 20, 255, 0, 0}
            ],
            make_link_adr_req('US915', {20, <<"SF12BW500">>, [{0, 7}]}, [])
        ),

        ?_assertEqual(
            [
                {link_adr_req, US915Index, 20, 2, 7, 0},
                {link_adr_req, US915Index, 20, 65280, 0, 0}
            ],
            make_link_adr_req('US915', {20, <<"SF12BW500">>, [{8, 15}, {65, 65}]}, [])
        )
    ].

validate_req(Plan, TxPower, DataRate) ->
    Region = Plan#channel_plan.base_region,
    Chans =
        case Region of
            'US915' -> [{8, 15}];
            'AU915' -> [{8, 15}];
            _ -> [{0, 7}]
        end,
    M1 = make_link_adr_req(Region, {TxPower, DataRate, Chans}, []),
    % io:format("M1=~w~n", [M1]),
    B1 = build_link_adr_req(Plan, {TxPower, DataRate}, []),
    % io:format("B1=~w ~n", [B1]),
    ?_assertEqual(M1, B1).

exercise_req({Region, TxPower, DataRate}) ->
    Plan = lora_plan:region_to_plan(Region),
    DRBinary = lora_plan:datarate_to_binary(Plan, DataRate),
    % io:format("Region=~w TxPower=~w DRBinary=~w~n", [Region, TxPower, DRBinary]),
    validate_req(Plan, TxPower, DRBinary).

exercise_req_test_() ->
    Regions = ['EU868', 'US915', 'AU915', 'CN470', 'AS923_1', 'KR920', 'IN865', 'EU433'],
    Powers = [30, 20, 19, 16, 14, 12, 5, 2],
    DataRates = [0, 1, 2, 3],
    [exercise_req({Region, TX, DR}) || Region <- Regions, TX <- Powers, DR <- DataRates].

link_adr_req_test() ->
    PlanEU868 = lora_plan:region_to_plan('EU868'),
    PlanUS915 = lora_plan:region_to_plan('US915'),
    M1 = make_link_adr_req('EU868', {14, <<"SF12BW125">>, [{0, 7}]}, []),
    % io:format("M1=~w~n", [M1]),
    B1 = build_link_adr_req(PlanEU868, {14, <<"SF12BW125">>}, []),
    % io:format("B1=~w ~n", [B1]),
    ?assertEqual(M1, B1),
    M2 = make_link_adr_req('US915', {20, <<"SF10BW125">>, [{8, 15}]}, []),
    % io:format("M2=~w~n", [M2]),
    B2 = build_link_adr_req(PlanUS915, {20, <<"SF10BW125">>}, []),
    % io:format("B2=~w ~n", [B2]),
    ?assertEqual(M2, B2),
    validate_req(PlanUS915, 20, <<"SF10BW125">>).

join_cf_list_test_() ->
    [
        ?_assertEqual(
            %% Active Channels 8-15
            <<0, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1>>,
            join_cf_list('US915')
        ),
        ?_assertEqual(
            %% Active Channels 8-15
            <<0, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1>>,
            join_cf_list('AU915')
        ),
        ?_assertEqual(
            %% Freqs 923.6, 923.8, 924.0, 924.2, 924.4
            <<32, 238, 140, 240, 245, 140, 192, 253, 140, 144, 5, 141, 96, 13, 141, 0>>,
            join_cf_list('AS923_1')
        ),
        ?_assertEqual(
            %% Freqs 923.6, 923.8, 924.0, 924.2, 924.4
            <<160, 175, 140, 112, 183, 140, 80, 230, 140, 224, 206, 140, 176, 214, 140, 0>>,
            join_cf_list('AS923_1B')
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
        ),
        ?_assertEqual(
            <<136, 75, 74, 88, 83, 74, 40, 91, 74, 248, 98, 74, 200, 106, 74, 0>>,
            join_cf_list('CN470')
        ),
        ?_assertEqual(
            <<248, 202, 140, 200, 210, 140, 152, 218, 140, 104, 226, 140, 0, 0, 0, 0>>,
            join_cf_list('KR920')
        ),
        ?_assertEqual(
            <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
            join_cf_list('IN865')
        )
    ].

-endif.
%% end of file
