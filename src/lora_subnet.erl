%%%-------------------------------------------------------------------
%% @doc
%% == LoRaWAN ==
%% @end
%%%-------------------------------------------------------------------
-module(lorawan).

-export([
    %% public functions
    is_local_devaddr/2,
    devaddr_from_subnet/2,
    subnet_from_devaddr/2,
    %% internal functions
    devaddr/2,
    parse_netid/1,
    netid_class/1,
    addr_len/1,
    addr_bit_len/1,
    netid_type/1,
    nwk_addr/1,
    netid_addr_range/2,
    is_local_netid/2,
    netid_size/1
]).

-type netid() :: non_neg_integer().
-type netclass() :: non_neg_integer().
-type devaddr() :: non_neg_integer().
-type nwkaddr() :: non_neg_integer().
-type subnetaddr() :: non_neg_integer().

-define(RETIRED_NETID, 16#200010).

%%%=============================================================================
%%% Public functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Does this LoRaWAN DevAddr belong to the Helium network?
%% NetIDList contains Helium's ordered list of assigned NetIDs.
%%
%% @end
%%------------------------------------------------------------------------------
-spec is_local_devaddr(devaddr(), [netid()]) -> boolean().
is_local_devaddr(DevAddr, NetIDList) ->
    NetID = the_netid(DevAddr),
    is_local_netid(NetID, NetIDList).

%%------------------------------------------------------------------------------
%% @doc Translate from a Helium subnet address to a LoRaWAN DevAddr.
%% NetIDList contains Helium's ordered list of assigned NetIDs.
%%
%% @end
%%------------------------------------------------------------------------------
-spec devaddr_from_subnet(subnetaddr(), [netid()]) -> devaddr().
devaddr_from_subnet(SubnetAddr, NetIDList) ->
    NetID = subnet_addr_to_netid(SubnetAddr, NetIDList),
    {Lower, _Upper} = netid_addr_range(NetID, NetIDList),
    DevAddr = devaddr(NetID, SubnetAddr - Lower),
    DevAddr.

%%------------------------------------------------------------------------------
%% @doc Translate from a LoRaWAN DevAddr to a Helium subnet address.
%% NetIDList contains Helium's ordered list of assigned NetIDs.
%%
%% @end
%%------------------------------------------------------------------------------
-spec subnet_from_devaddr(devaddr(), [netid()]) -> subnetaddr().
subnet_from_devaddr(DevAddr, NetIDList) ->
    NetID = the_netid(DevAddr),
    {Lower, _Upper} = netid_addr_range(NetID, NetIDList),
    SubnetAddr = Lower + nwk_addr(DevAddr),
    SubnetAddr.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec netid_class(netid()) -> netclass().
netid_class(NetID) ->
    NetClass = NetID bsr 21,
    NetClass.

-spec addr_len(netclass()) -> 7 | 10 | 13 | 15 | 17 | 20 | 24 | 25.
addr_len(NetClass) ->
    case NetClass of
        0 -> 25;
        1 -> 24;
        2 -> 20;
        3 -> 17;
        4 -> 15;
        5 -> 13;
        6 -> 10;
        7 -> 7
    end.

-spec id_len(netclass()) -> 6 | 9 | 11 | 12 | 13 | 15 | 17.
id_len(NetClass) ->
    case NetClass of
        0 -> 6;
        1 -> 6;
        2 -> 9;
        3 -> 11;
        4 -> 12;
        5 -> 13;
        6 -> 15;
        7 -> 17
    end.

-spec is_local_netid(netid(), [netid()]) -> boolean().
is_local_netid(NetID, NetIDList) ->
    case NetID of
        ?RETIRED_NETID ->
            true;
        _ ->
            lists:any(fun(X) -> X == NetID end, NetIDList)
    end.

-spec devaddr(netid(), nwkaddr()) -> devaddr().
devaddr(NetID, NwkAddr) ->
    NetClass = NetID bsr 21,
    ID = NetID band 2#111111111111111111111,
    % <<ID:21/integer-unsigned, NetClass:3/integer-unsigned, _Ignore:8/integer-unsigned>> = NetID,
    Addr0 = var_net_class(NetClass) bor ID,
    DevAddr = var_netid(NetClass, Addr0) bor NwkAddr,
    DevAddr.

-spec subnet_addr_to_netid(subnetaddr(), [netid()]) -> netid().
subnet_addr_to_netid(NwkAddr, NetIDList) ->
    subnet_addr_to_netid_search(NwkAddr, NetIDList, NetIDList).

subnet_addr_to_netid_search(_, [], _) ->
    0;
subnet_addr_to_netid_search(NwkAddr, SList, NetIDList) ->
    [H | T] = SList,
    WithinRange = subnet_addr_within_range(NwkAddr, H, NetIDList),
    case WithinRange of
        true ->
            H;
        false ->
            subnet_addr_to_netid_search(NwkAddr, T, NetIDList)
    end.

-spec subnet_addr_within_range(subnetaddr(), netid(), [netid()]) -> boolean().
subnet_addr_within_range(Addr, NetID, NetIDList) ->
    {Lower, Upper} = netid_addr_range(NetID, NetIDList),
    (Addr >= Lower) and (Addr < Upper).

-spec var_net_class(netclass()) -> non_neg_integer().
var_net_class(NetClass) when NetClass =< 7 ->
    IDLen = id_len(NetClass),
    case NetClass of
        0 -> 0;
        1 -> 2#10 bsl IDLen;
        2 -> 2#110 bsl IDLen;
        3 -> 2#1110 bsl IDLen;
        4 -> 2#11110 bsl IDLen;
        5 -> 2#111110 bsl IDLen;
        6 -> 2#1111110 bsl IDLen;
        7 -> 2#11111110 bsl IDLen
    end.

-spec var_netid(netclass(), netid()) -> non_neg_integer().
var_netid(NetClass, NetID) when NetClass =< 7 ->
    NetID bsl addr_len(NetClass).

-spec the_netid(number() | binary()) -> netid().
the_netid(DevNum) when erlang:is_number(DevNum) ->
    the_netid(<<DevNum:32/integer-unsigned>>);
the_netid(DevAddr) ->
    Type = netid_type(DevAddr),
    ID = parse_id(DevAddr, Type + 1, id_len(Type)),
    NetID = ID bor (Type bsl 21),
    NetID.

-spec parse_netid(number() | binary()) -> {ok, netid()} | {error, invalid_netid_type}.
parse_netid(DevNum) when erlang:is_number(DevNum) ->
    parse_netid(<<DevNum:32/integer-unsigned>>);
parse_netid(DevAddr) ->
    try
        Type = netid_type(DevAddr),
        ID = parse_id(DevAddr, Type + 1, id_len(Type)),
        {ok, ID bor (Type bsl 21)}
    catch
        throw:invalid_netid_type:_ ->
            {error, invalid_netid_type}
    end.

-spec addr_bit_len(number() | binary()) -> 7 | 10 | 13 | 15 | 17 | 20 | 24 | 25.
addr_bit_len(DevNum) when erlang:is_number(DevNum) ->
    addr_bit_len(<<DevNum:32/integer-unsigned>>);
addr_bit_len(DevAddr) ->
    NetID = the_netid(DevAddr),
    addr_len(netid_class(NetID)).

-spec parse_id(binary(), non_neg_integer(), non_neg_integer()) -> netid().
parse_id(DevAddr, PrefixLength, NwkIDBits) ->
    <<DevAddrNum:32/integer-unsigned>> = DevAddr,
    uint32(uint32(DevAddrNum bsl PrefixLength) bsr (32 - NwkIDBits)).

-spec netid_type(number() | binary()) -> 0..7.
netid_type(NetID) when erlang:is_number(NetID) ->
    netid_type(<<NetID:32/integer-unsigned>>);
netid_type(<<First:8/integer-unsigned, _/binary>>) ->
    netid_type(First, 7).

-spec netid_type(non_neg_integer(), non_neg_integer()) -> 0..7.
netid_type(_, -1) ->
    throw(invalid_netid_type);
netid_type(Prefix, Index) ->
    case Prefix band (1 bsl Index) of
        0 -> 7 - Index;
        _ -> netid_type(Prefix, Index - 1)
    end.

-spec nwk_addr(devaddr()) -> nwkaddr().
nwk_addr(DevAddr) ->
    NetID = the_netid(DevAddr),
    AddrBitLen = addr_len(netid_class(NetID)),
    IgnoreLen = 32 - AddrBitLen,
    DevAddr2 = <<DevAddr:32/integer-unsigned>>,
    <<_:IgnoreLen, NwkAddr:AddrBitLen/integer-unsigned>> = DevAddr2,
    NwkAddr.

-spec netid_addr_range(netid(), [netid()]) -> {non_neg_integer(), non_neg_integer()}.
netid_addr_range(NetID, NetIDList0) ->
    FoundNetID = lists:any(fun(X) -> X == NetID end, NetIDList0),
    case FoundNetID of
        false ->
            {0, 0};
        true ->
            NetIDList = lists:takewhile(fun(X) -> X =/= NetID end, NetIDList0),
            Lower = lists:foldl(fun(X, Sum) -> netid_size(X) + Sum end, 0, NetIDList),
            NetIDPlusList = NetIDList ++ [NetID],
            Upper = lists:foldl(fun(X, Sum) -> netid_size(X) + Sum end, 0, NetIDPlusList),
            {Lower, Upper}
    end.

-spec netid_size(netid()) -> non_neg_integer().
netid_size(NetID) ->
    Size = 1 bsl addr_len(netid_class(NetID)),
    Size.

-spec uint32(integer()) -> integer().
uint32(Num) ->
    Num band 16#FFFFFFFF.

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

id_test() ->
    %% CP data
    ?assertEqual(
        {ok, 16#00002D}, lorawan:parse_netid(<<91, 255, 255, 255>>), "[45] == 2D == 45 type 0"
    ),
    ?assertEqual(
        {ok, 16#20002D}, lorawan:parse_netid(<<173, 255, 255, 255>>), "[45] == 2D == 45 type 1"
    ),
    ?assertEqual(
        {ok, 16#40016D}, lorawan:parse_netid(<<214, 223, 255, 255>>), "[1,109] == 16D == 365 type 2"
    ),
    ?assertEqual(
        {ok, 16#6005B7},
        lorawan:parse_netid(<<235, 111, 255, 255>>),
        "[5,183] == 5B7 == 1463 type 3"
    ),
    ?assertEqual(
        {ok, 16#800B6D},
        lorawan:parse_netid(<<245, 182, 255, 255>>),
        "[11, 109] == B6D == 2925 type 4"
    ),
    ?assertEqual(
        {ok, 16#A016DB},
        lorawan:parse_netid(<<250, 219, 127, 255>>),
        "[22,219] == 16DB == 5851 type 5"
    ),
    ?assertEqual(
        {ok, 16#C05B6D},
        lorawan:parse_netid(<<253, 109, 183, 255>>),
        "[91, 109] == 5B6D == 23405 type 6"
    ),
    ?assertEqual(
        {ok, 16#E16DB6},
        lorawan:parse_netid(<<254, 182, 219, 127>>),
        "[1,109,182] == 16DB6 == 93622 type 7"
    ),
    ?assertEqual(
        {error, invalid_netid_type},
        lorawan:parse_netid(<<255, 255, 255, 255>>),
        "Invalid DevAddr"
    ),

    % Actility spreadsheet examples
    ?assertEqual({ok, 0}, lorawan:parse_netid(<<0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:25>>)),
    ?assertEqual({ok, 1}, lorawan:parse_netid(<<0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 1:1, 0:25>>)),
    ?assertEqual({ok, 2}, lorawan:parse_netid(<<0:1, 0:1, 0:1, 0:1, 0:1, 1:1, 0:1, 0:25>>)),

    %% Mis-parsed as netid 4 of type 3
    ?assertEqual(
        {ok, 16#600004}, lorawan:parse_netid(<<224, 9, 171, 205>>), "hex_to_binary(<<'E009ABCD'>>)"
    ),
    %% Valid DevAddr, NetID not assigned
    ?assertEqual(
        {ok, 16#20002D},
        lorawan:parse_netid(<<173, 255, 255, 255>>),
        "hex_to_binary(<<'ADFFFFFF'>>)"
    ),
    %% Less than 32 bit number
    ?assertEqual({ok, 0}, lorawan:parse_netid(46377)),

    % Louis test data
    ?assertEqual({ok, 16#600002}, lorawan:parse_netid(<<224, 4, 0, 1>>)),
    ?assertEqual({ok, 16#600002}, lorawan:parse_netid(<<224, 5, 39, 132>>)),
    ?assertEqual({ok, 16#000002}, lorawan:parse_netid(<<4, 16, 190, 163>>)),
    ok.

create_netid(NetClass, ID) ->
    NetIDBin = <<0:8/integer-unsigned, NetClass:3/integer-unsigned, ID:21/integer-unsigned>>,
    <<NetID:32/integer-unsigned>> = NetIDBin,
    NetID.

mock_random_netids() ->
    Len = rand:uniform(10),
    [create_netid(rand:uniform(7), rand:uniform(64)) || _ <- lists:seq(1, Len)].

mock_netid_list() ->
    [16#E00001, 16#C00035, 16#60002D].

insert_item(Item, List, Pos) ->
    {A, B} = lists:split(Pos, List),
    NewList = A ++ [Item] ++ B,
    NewList.

insert_rand(Item, List) ->
    Pos = rand:uniform(length(List)),
    {A, B} = lists:split(Pos, List),
    NewList = A ++ [Item] ++ B,
    NewList.

exercise_subnet(DevAddr, NetIDList) ->
    SubnetAddr = lorawan:subnet_from_devaddr(DevAddr, NetIDList),
    DevAddr2 = lorawan:devaddr_from_subnet(SubnetAddr, NetIDList),
    ?assertEqual(DevAddr, DevAddr2),
    ok.

exercise_subnet(DevAddr) ->
    {ok, NetID} = lorawan:parse_netid(DevAddr),
    exercise_subnet(DevAddr, insert_item(NetID, mock_netid_list(), 0)),
    exercise_subnet(DevAddr, insert_item(NetID, mock_netid_list(), 1)),
    exercise_subnet(DevAddr, insert_item(NetID, mock_netid_list(), 2)),
    exercise_subnet(DevAddr, insert_item(NetID, mock_netid_list(), 3)),
    ok.

random_subnet(DevAddr) ->
    {ok, NetID} = lorawan:parse_netid(DevAddr),
    [exercise_subnet(DevAddr, insert_rand(NetID, mock_random_netids())) || _ <- lists:seq(1, 400)],
    ok.

exercise_devaddr(NetID, Addr, _IDLen, AddrLen) ->
    DevAddr = lorawan:devaddr(NetID, Addr),
    NetIDType = lorawan:netid_type(DevAddr),
    ?assert(NetIDType =< 7),
    {ok, NetID0} = lorawan:parse_netid(DevAddr),
    ?assertEqual(NetID, NetID0),
    AddrBitLen = lorawan:addr_bit_len(DevAddr),
    ?assertEqual(AddrLen, AddrBitLen),
    NwkAddr = lorawan:nwk_addr(DevAddr),
    ?assertEqual(Addr, NwkAddr),
    exercise_subnet(DevAddr),
    random_subnet(DevAddr),
    ok.

exercise_netid(NetClass, ID, IDLen, AddrLen) ->
    NetIDBin = <<0:8/integer-unsigned, NetClass:3/integer-unsigned, ID:21/integer-unsigned>>,
    <<NetID:32/integer-unsigned>> = NetIDBin,
    NetAddrLen = lorawan:addr_len(NetClass),
    ?assert(NetAddrLen == AddrLen),
    MaxNetSize = lorawan:netid_size(NetID),
    exercise_devaddr(NetID, 0, IDLen, AddrLen),
    exercise_devaddr(NetID, 1, IDLen, AddrLen),
    exercise_devaddr(NetID, 8, IDLen, AddrLen),
    exercise_devaddr(NetID, 16, IDLen, AddrLen),
    exercise_devaddr(NetID, 32, IDLen, AddrLen),
    exercise_devaddr(NetID, 33, IDLen, AddrLen),
    exercise_devaddr(NetID, 64, IDLen, AddrLen),
    exercise_devaddr(NetID, MaxNetSize - 1, IDLen, AddrLen),
    ok.

devaddr_exercise_test() ->
    exercise_netid(7, 2, 17, 7),
    exercise_netid(6, 2, 15, 10),
    exercise_netid(5, 2, 13, 13),
    exercise_netid(4, 2, 12, 15),
    exercise_netid(3, 2, 11, 17),
    exercise_netid(2, 2, 9, 20),
    exercise_netid(1, 2, 6, 24),
    exercise_netid(0, 2, 6, 25),
    ok.

netid_test() ->
    LegacyDevAddr = <<$H:7, 0:25>>,
    LegacyNum = 16#90000000,
    _LegacyID = 8,
    %% 16#200010,
    LegacyNetID = ?RETIRED_NETID,
    <<H1:7, _/bitstring>> = LegacyDevAddr,
    <<H2:7, _:25>> = LegacyDevAddr,
    H3 = <<LegacyNum:32/integer-unsigned>>,
    ?assertEqual(H1, H2),
    ?assertEqual(H3, LegacyDevAddr),

    NetID00 = 16#E00001,
    NetID01 = 16#C00035,
    %% Official NetID assigned to Helium by LoRa Alliance
    NetID02 = 16#60002D,
    NetIDExt = 16#C00050,

    %% DevAddr with legacy Helium NetID
    DevAddr00 = 16#90000000,
    %% DevAddr with class 6 NetID
    DevAddr01 = 16#FC00D410,
    %% DevAddr with class 3 NetID
    DevAddr02 = 16#E05A0008,

    NetWidth0 = lorawan:addr_len(lorawan:netid_class(NetID00)),
    ?assertEqual(7, NetWidth0),
    NetWidth1 = lorawan:addr_len(lorawan:netid_class(NetID01)),
    ?assertEqual(10, NetWidth1),
    NetWidth2 = lorawan:addr_len(lorawan:netid_class(NetID02)),
    ?assertEqual(17, NetWidth2),
    NetSize0 = lorawan:netid_size(NetID00),
    ?assertEqual(128, NetSize0),
    NetSize1 = lorawan:netid_size(NetID01),
    ?assertEqual(1024, NetSize1),
    NetSize2 = lorawan:netid_size(NetID02),
    ?assertEqual(131072, NetSize2),

    NetIDList = [NetID00, NetID01, NetID02],
    LocalTrue = lorawan:is_local_netid(NetID01, NetIDList),
    LocalFalse = lorawan:is_local_netid(NetIDExt, NetIDList),
    LegacyLocal = lorawan:is_local_netid(LegacyNetID, NetIDList),
    ?assertEqual(true, LocalTrue),
    ?assertEqual(false, LocalFalse),
    ?assertEqual(true, LegacyLocal),

    DevAddrLegacy = lorawan:devaddr(LegacyNetID, 0),
    ?assertEqual(DevAddr00, DevAddrLegacy),
    DevAddr1 = lorawan:devaddr(NetID01, 16),
    ?assertEqual(DevAddr01, DevAddr1),
    DevAddr2 = lorawan:devaddr(NetID02, 8),
    ?assertEqual(DevAddr02, DevAddr2),

    NetIDType00 = lorawan:netid_type(DevAddr00),
    ?assertEqual(1, NetIDType00),
    NetIDType01 = lorawan:netid_type(DevAddr01),
    ?assertEqual(6, NetIDType01),
    NetIDType02 = lorawan:netid_type(DevAddr02),
    ?assertEqual(3, NetIDType02),

    NetIDType0 = lorawan:netid_type(DevAddrLegacy),
    ?assertEqual(1, NetIDType0),
    NetIDType1 = lorawan:netid_type(DevAddr1),
    ?assertEqual(6, NetIDType1),
    NetIDType2 = lorawan:netid_type(DevAddr2),
    ?assertEqual(3, NetIDType2),

    {ok, NetID_0} = lorawan:parse_netid(DevAddr00),
    ?assertEqual(NetID_0, LegacyNetID),
    {ok, NetID_1} = lorawan:parse_netid(16#FC00D410),
    ?assertEqual(NetID_1, 16#C00035),
    {ok, NetID_1} = lorawan:parse_netid(DevAddr01),
    ?assertEqual(NetID_1, NetID01),
    {ok, NetID_2} = lorawan:parse_netid(DevAddr02),
    ?assertEqual(NetID_2, NetID02),

    {ok, NetID0} = lorawan:parse_netid(DevAddrLegacy),
    ?assertEqual(NetID0, LegacyNetID),
    {ok, NetID1} = lorawan:parse_netid(DevAddr1),
    ?assertEqual(NetID1, NetID01),
    {ok, NetID2} = lorawan:parse_netid(DevAddr2),
    ?assertEqual(NetID2, NetID02),

    Width_0 = lorawan:addr_bit_len(DevAddr00),
    ?assertEqual(24, Width_0),
    Width_1 = lorawan:addr_bit_len(DevAddr01),
    ?assertEqual(10, Width_1),
    Width_2 = lorawan:addr_bit_len(DevAddr02),
    ?assertEqual(17, Width_2),

    Width0 = lorawan:addr_bit_len(DevAddrLegacy),
    ?assertEqual(24, Width0),
    Width1 = lorawan:addr_bit_len(DevAddr1),
    ?assertEqual(10, Width1),
    Width2 = lorawan:addr_bit_len(DevAddr2),
    ?assertEqual(17, Width2),

    NwkAddr0 = lorawan:nwk_addr(DevAddr00),
    ?assertEqual(0, NwkAddr0),
    NwkAddr1 = lorawan:nwk_addr(DevAddr01),
    ?assertEqual(16, NwkAddr1),
    NwkAddr2 = lorawan:nwk_addr(DevAddr02),
    ?assertEqual(8, NwkAddr2),

    %% Backwards DevAddr compatibility test
    %% DevAddr00 is a legacy Helium Devaddr.  The NetID is retired.
    %% By design we do compute a proper subnet (giving us a correct OUI route),
    %% but if we compute the associated DevAddr for this subnet (for the Join request)
    %% we'll get a new DevAddr associated with a current and proper NetID.
    %% In other words, DevAddr00 is not equal to DevAddr000.
    Subnet0 = lorawan:subnet_from_devaddr(DevAddr00, NetIDList),
    io:format("Subnet0 ~8.16.0B~n", [Subnet0]),
    ?assertEqual(0, Subnet0),
    DevAddr000 = lorawan:devaddr_from_subnet(Subnet0, NetIDList),
    io:format("DevAddr00 ~8.16.0B~n", [DevAddr00]),
    io:format("DevAddr000 ~8.16.0B~n", [DevAddr000]),
    %% By design the reverse DevAddr will have a correct NetID
    ?assertNotEqual(DevAddr000, DevAddr00),
    ?assertEqual(16#FE000080, DevAddr000),
    {ok, DevAddr000NetID} = lorawan:parse_netid(DevAddr000),
    ?assertEqual(NetID00, DevAddr000NetID),
    NwkAddr000 = lorawan:nwk_addr(DevAddr000),
    ?assertEqual(NwkAddr0, NwkAddr000),

    Subnet1 = lorawan:subnet_from_devaddr(DevAddr01, NetIDList),
    io:format("Subnet1 ~8.16.0B~n", [Subnet1]),
    ?assertEqual((1 bsl 7) + 16, Subnet1),
    DevAddr001 = lorawan:devaddr_from_subnet(Subnet1, NetIDList),
    io:format("DevAddr01 ~8.16.0B~n", [DevAddr01]),
    io:format("DevAddr001 ~8.16.0B~n", [DevAddr001]),
    ?assertEqual(DevAddr001, DevAddr01),

    Subnet1 = lorawan:subnet_from_devaddr(DevAddr01, NetIDList),
    ?assertEqual((1 bsl 7) + 16, Subnet1),
    DevAddr001 = lorawan:devaddr_from_subnet(Subnet1, NetIDList),
    ?assertEqual(DevAddr001, DevAddr01),

    Subnet2 = lorawan:subnet_from_devaddr(DevAddr02, NetIDList),
    ?assertEqual((1 bsl 7) + (1 bsl 10) + 8, Subnet2),
    DevAddr002 = lorawan:devaddr_from_subnet(Subnet2, NetIDList),
    ?assertEqual(DevAddr002, DevAddr02),

    ok.

-endif.
