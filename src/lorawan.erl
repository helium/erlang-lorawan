%%%-------------------------------------------------------------------
%% @doc
%% == LoRaWAN ==
%% @end
%%%-------------------------------------------------------------------
-module(lorawan).

-export([
    devaddr/2,
    devaddr_from_subnet/2,
    subnet_from_devaddr/2,
    netid/1,
    addr_bit_len/1,
    netid_type/1,
    nwk_addr/1,
    netid_addr_range/2,
    is_local_netid/2
]).

-type netid() :: non_neg_integer().
-type netclass() :: non_neg_integer().
-type devaddr() :: non_neg_integer().
-type nwkaddr() :: non_neg_integer().
-type subnetaddr() :: non_neg_integer().

-define(RETIRED_NETID, 16#200010).

-spec devaddr_from_subnet(subnetaddr(), [netid()]) -> devaddr().
devaddr_from_subnet(SubnetAddr, NetIDList) ->
	io:format("devaddr_from_subnet ~8.16.0B~n", [SubnetAddr]),
    NetID = subnet_addr_to_netid(SubnetAddr, NetIDList),
    io:format("NetID ~8.16.0B~n", [NetID]),
    {Lower, Upper} = netid_addr_range(NetID, NetIDList),
    io:format("Lower ~8.16.0B~n", [Lower]),
    io:format("Upper ~8.16.0B~n", [Upper]),
    DevAddr = devaddr(NetID, SubnetAddr - Lower),
    DevAddr.

-spec subnet_from_devaddr(devaddr(), [netid()]) -> subnetaddr().
subnet_from_devaddr(DevAddr, NetIDList) ->
    {ok, NetID} = netid(DevAddr),
    {Lower, _Upper} = netid_addr_range(NetID, NetIDList),
    SubnetAddr = Lower + nwk_addr(DevAddr),
    SubnetAddr.

-spec devaddr(netid(), nwkaddr()) -> devaddr().
devaddr(NetID, NwkAddr) ->
	io:format("devaddr start~n", []),
	io:format("NetID ~8.16.0B~n", [NetID]),
	io:format("NwkAddr ~8.16.0B~n", [NwkAddr]),
    NetClass = NetID bsr 21,
    io:format("NetClass ~8.16.0B~n", [NetClass]),
    ID = NetID band 2#111111111111111111111,
    io:format("ID ~8.16.0B~n", [ID]),
    % <<ID:21/integer-unsigned, NetClass:3/integer-unsigned, _Ignore:8/integer-unsigned>> = NetID,
    Addr0 = var_net_class(NetClass) bor ID,
    io:format("Addr0 ~8.16.0B~n", [Addr0]),
    DevAddr = var_netid(NetClass, Addr0) bor NwkAddr,
    io:format("DevAddr ~8.16.0B~n", [DevAddr]),
    io:format("DevAddr end~n", []),
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

-spec is_local_netid(netid(), [netid()]) -> boolean().
is_local_netid(NetID, NetIDList) ->
    case NetID of
        ?RETIRED_NETID ->
            true;
        _ ->
            lists:any(fun(X) -> X == NetID end, NetIDList)
    end.

-spec var_net_class(netclass()) -> non_neg_integer().
var_net_class(NetClass) ->
    case NetClass of
        0 -> 0;
        1 -> 2#10 bsl 6;
        2 -> 2#110 bsl 9;
        3 -> 2#1110 bsl 11;
        4 -> 2#11110 bsl 12;
        5 -> 2#111110 bsl 13;
        6 -> 2#1111110 bsl 15;
        7 -> 2#11111110 bsl 17
    end.

-spec var_netid(netclass(), netid()) -> non_neg_integer().
var_netid(NetClass, NetID) ->
    case NetClass of
        0 -> NetID bsl 25;
        1 -> NetID bsl 24;
        2 -> NetID bsl 20;
        3 -> NetID bsl 17;
        4 -> NetID bsl 15;
        5 -> NetID bsl 13;
        6 -> NetID bsl 10;
        7 -> NetID bsl 7
    end.

-spec netid(number() | binary()) -> {ok, netid()} | {error, invalid_netid_type}.
netid(DevNum) when erlang:is_number(DevNum) ->
    netid(<<DevNum:32/integer-unsigned>>);
netid(DevAddr) ->
    try
        Type = netid_type(DevAddr),
        NetID =
            case Type of
                0 -> get_netid(DevAddr, 1, 6);
                1 -> get_netid(DevAddr, 2, 6);
                2 -> get_netid(DevAddr, 3, 9);
                3 -> get_netid(DevAddr, 4, 11);
                4 -> get_netid(DevAddr, 5, 12);
                5 -> get_netid(DevAddr, 6, 13);
                6 -> get_netid(DevAddr, 7, 15);
                7 -> get_netid(DevAddr, 8, 17)
            end,
        {ok, NetID bor (Type bsl 21)}
    catch
        throw:invalid_netid_type:_ ->
            {error, invalid_netid_type}
    end.

-spec addr_bit_len(number() | binary()) -> 7 | 10 | 13 | 15 | 17 | 20 | 24 | 25.
addr_bit_len(DevNum) when erlang:is_number(DevNum) ->
    addr_bit_len(<<DevNum:32/integer-unsigned>>);
addr_bit_len(DevAddr) ->
    Type = netid_type(DevAddr),
    case Type of
        0 -> 25;
        1 -> 24;
        2 -> 20;
        3 -> 17;
        4 -> 15;
        5 -> 13;
        6 -> 10;
        7 -> 7
    end.

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

-spec get_netid(binary(), non_neg_integer(), non_neg_integer()) -> netid().
get_netid(DevAddr, PrefixLength, NwkIDBits) ->
    <<Temp:32/integer-unsigned>> = DevAddr,
    %% Remove type prefix
    One = uint32(Temp bsl PrefixLength),
    %% Remove NwkAddr suffix
    Two = uint32(One bsr (32 - NwkIDBits)),

    IgnoreSize = 32 - NwkIDBits,
    <<_:IgnoreSize, NetID:NwkIDBits/integer-unsigned>> = <<Two:32/integer-unsigned>>,
    NetID.

-spec nwk_addr(devaddr()) -> nwkaddr().
nwk_addr(DevAddr) ->
    AddrBitLen = addr_bit_len(DevAddr),
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

-spec netid_width(netid()) -> 7 | 10 | 13 | 15 | 17 | 20 | 24 | 25.
netid_width(NetID) ->
    NetClass = NetID bsr 21,
    % <<_ID:21, NetClass:3, _Ignore:8>> = NetID,
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

-spec netid_size(netid()) -> non_neg_integer().
netid_size(NetID) ->
    Size = 1 bsl netid_width(NetID),
    Size.

-spec uint32(integer()) -> integer().
uint32(Num) ->
    Num band 16#FFFFFFFF.

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

net_id_test() ->
    %% CP data
    ?assertEqual({ok, 16#00002D}, netid(<<91, 255, 255, 255>>), "[45] == 2D == 45 type 0"),
    ?assertEqual({ok, 16#20002D}, netid(<<173, 255, 255, 255>>), "[45] == 2D == 45 type 1"),
    ?assertEqual({ok, 16#40016D}, netid(<<214, 223, 255, 255>>), "[1,109] == 16D == 365 type 2"),
    ?assertEqual({ok, 16#6005B7}, netid(<<235, 111, 255, 255>>), "[5,183] == 5B7 == 1463 type 3"),
    ?assertEqual(
        {ok, 16#800B6D},
        netid(<<245, 182, 255, 255>>),
        "[11, 109] == B6D == 2925 type 4"
    ),
    ?assertEqual(
        {ok, 16#A016DB},
        netid(<<250, 219, 127, 255>>),
        "[22,219] == 16DB == 5851 type 5"
    ),
    ?assertEqual(
        {ok, 16#C05B6D},
        netid(<<253, 109, 183, 255>>),
        "[91, 109] == 5B6D == 23405 type 6"
    ),
    ?assertEqual(
        {ok, 16#E16DB6},
        netid(<<254, 182, 219, 127>>),
        "[1,109,182] == 16DB6 == 93622 type 7"
    ),
    ?assertEqual(
        {error, invalid_netid_type},
        netid(<<255, 255, 255, 255>>),
        "Invalid DevAddr"
    ),

    % Actility spreadsheet examples
    ?assertEqual({ok, 0}, netid(<<0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:25>>)),
    ?assertEqual({ok, 1}, netid(<<0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 1:1, 0:25>>)),
    ?assertEqual({ok, 2}, netid(<<0:1, 0:1, 0:1, 0:1, 0:1, 1:1, 0:1, 0:25>>)),

    %% Mis-parsed as netid 4 of type 3
    ?assertEqual({ok, 16#600004}, netid(<<224, 9, 171, 205>>), "hex_to_binary(<<'E009ABCD'>>)"),
    %% Valid DevAddr, NetID not assigned
    ?assertEqual({ok, 16#20002D}, netid(<<173, 255, 255, 255>>), "hex_to_binary(<<'ADFFFFFF'>>)"),
    %% Less than 32 bit number
    ?assertEqual({ok, 0}, netid(46377)),

    % Louis test data
    ?assertEqual({ok, 16#600002}, netid(<<224, 4, 0, 1>>)),
    ?assertEqual({ok, 16#600002}, netid(<<224, 5, 39, 132>>)),
    ?assertEqual({ok, 16#000002}, netid(<<4, 16, 190, 163>>)),
    ok.

% validate_devaddr(NetClass, ID, AddrLen, AddrSize) ->
% 	NetIDBin = <<NetClass:3, ID:21>>,
% 	<<NetID:32/integer-unsigned>> = NetIDBin,
% 	NetAddrLen = netid_width(NetID),
% 	?assertEqual(AddrLen, NetAddrLen),
% 	NetAddrSize = netid_size(NetID),
% 	?assertEqual(AddrSize, NetAddrSize),
% 	ok.

mock_netid_list() ->
    [ 16#E00001, 16#C00035, 16#60002D ].

insert_item(Item, List, Pos) ->
	{A, B} = lists:split(Pos, List),
	NewList = A ++ [Item] ++ B,
	NewList.

exercise_subnet(DevAddr, NetIDList) ->
    SubnetAddr = subnet_from_devaddr(DevAddr, NetIDList),
    DevAddr2 = devaddr_from_subnet(SubnetAddr, NetIDList),
    ?assertEqual(DevAddr, DevAddr2),
    ok.

exercise_subnet(DevAddr) ->
	{ok, NetID} = netid(DevAddr),
	exercise_subnet(DevAddr, insert_item(NetID, mock_netid_list(), 0)),
	exercise_subnet(DevAddr, insert_item(NetID, mock_netid_list(), 1)),
	exercise_subnet(DevAddr, insert_item(NetID, mock_netid_list(), 2)),
	exercise_subnet(DevAddr, insert_item(NetID, mock_netid_list(), 3)),
    ok.

exercise_devaddr(NetID, Addr) ->
	DevAddr = devaddr(NetID, Addr),
	NetIDType = netid_type(DevAddr),
    ?assert(NetIDType =< 7),
    {ok, NetID0} = netid(DevAddr),
    ?assertEqual(NetID, NetID0),
    AddrBitLen = addr_bit_len(DevAddr),
    NwkAddr = nwk_addr(DevAddr),
    ?assertEqual(Addr, NwkAddr),
    exercise_subnet(DevAddr),
    {DevAddr, AddrBitLen}.

exercise_netid(NetClass, ID) ->
	NetIDBin = <<0:8/integer-unsigned, NetClass:3/integer-unsigned, ID:21/integer-unsigned>>,
	<<NetID:32/integer-unsigned>> = NetIDBin,
	NetAddrLen = netid_width(NetID),
	?assert((NetAddrLen >= 7) and (NetAddrLen =< 25)),
	MaxNetSize = netid_size(NetID),
	exercise_devaddr(NetID, 0),
	exercise_devaddr(NetID, 1),
	exercise_devaddr(NetID, 8),
	exercise_devaddr(NetID, 16),
	exercise_devaddr(NetID, 32),
	exercise_devaddr(NetID, 33),
	exercise_devaddr(NetID, 64),
	exercise_devaddr(NetID, MaxNetSize - 1),
	ok.

netid_exercise_test() ->
	exercise_netid(7, 2),
	exercise_netid(6, 2),
	exercise_netid(5, 2),
	exercise_netid(4, 2),
	exercise_netid(3, 2),
	exercise_netid(2, 2),
	exercise_netid(1, 2),
 	exercise_netid(0, 2),
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
    NetID02 = 16#60002D,
    NetIDExt = 16#C00050,

    %% Class 6
    DevAddr00 = 16#90000000,
    DevAddr01 = 16#FC00D410,
    DevAddr02 = 16#E05A0008,

    NetWidth0 = netid_width(NetID00),
    ?assertEqual(7, NetWidth0),
    NetWidth1 = netid_width(NetID01),
    ?assertEqual(10, NetWidth1),
    NetWidth2 = netid_width(NetID02),
    ?assertEqual(17, NetWidth2),
    NetSize0 = netid_size(NetID00),
    ?assertEqual(128, NetSize0),
    NetSize1 = netid_size(NetID01),
    ?assertEqual(1024, NetSize1),
    NetSize2 = netid_size(NetID02),
    ?assertEqual(131072, NetSize2),

    NetIDList = [NetID00, NetID01, NetID02],
    LocalTrue = is_local_netid(NetID01, NetIDList),
    LocalFalse = is_local_netid(NetIDExt, NetIDList),
    LegacyLocal = is_local_netid(LegacyNetID, NetIDList),
    ?assertEqual(true, LocalTrue),
    ?assertEqual(false, LocalFalse),
    ?assertEqual(true, LegacyLocal),

    DevAddrLegacy = devaddr(LegacyNetID, 0),
    ?assertEqual(DevAddr00, DevAddrLegacy),
    DevAddr1 = devaddr(NetID01, 16),
    ?assertEqual(DevAddr01, DevAddr1),
    DevAddr2 = devaddr(NetID02, 8),
    ?assertEqual(DevAddr02, DevAddr2),

    NetIDType00 = netid_type(DevAddr00),
    ?assertEqual(1, NetIDType00),
    NetIDType01 = netid_type(DevAddr01),
    ?assertEqual(6, NetIDType01),
    NetIDType02 = netid_type(DevAddr02),
    ?assertEqual(3, NetIDType02),

    NetIDType0 = netid_type(DevAddrLegacy),
    ?assertEqual(1, NetIDType0),
    NetIDType1 = netid_type(DevAddr1),
    ?assertEqual(6, NetIDType1),
    NetIDType2 = netid_type(DevAddr2),
    ?assertEqual(3, NetIDType2),

    {ok, NetID_0} = netid(DevAddr00),
    ?assertEqual(NetID_0, LegacyNetID),
    {ok, NetID_1} = netid(16#FC00D410),
    ?assertEqual(NetID_1, 16#C00035),
    {ok, NetID_1} = netid(DevAddr01),
    ?assertEqual(NetID_1, NetID01),
    {ok, NetID_2} = netid(DevAddr02),
    ?assertEqual(NetID_2, NetID02),

    {ok, NetID0} = netid(DevAddrLegacy),
    ?assertEqual(NetID0, LegacyNetID),
    {ok, NetID1} = netid(DevAddr1),
    ?assertEqual(NetID1, NetID01),
    {ok, NetID2} = netid(DevAddr2),
    ?assertEqual(NetID2, NetID02),

    Width_0 = addr_bit_len(DevAddr00),
    ?assertEqual(24, Width_0),
    Width_1 = addr_bit_len(DevAddr01),
    ?assertEqual(10, Width_1),
    Width_2 = addr_bit_len(DevAddr02),
    ?assertEqual(17, Width_2),

    Width0 = addr_bit_len(DevAddrLegacy),
    ?assertEqual(24, Width0),
    Width1 = addr_bit_len(DevAddr1),
    ?assertEqual(10, Width1),
    Width2 = addr_bit_len(DevAddr2),
    ?assertEqual(17, Width2),

    NwkAddr0 = nwk_addr(DevAddr00),
    ?assertEqual(0, NwkAddr0),
    NwkAddr1 = nwk_addr(DevAddr01),
    ?assertEqual(16, NwkAddr1),
    NwkAddr2 = nwk_addr(DevAddr02),
    ?assertEqual(8, NwkAddr2),

    % Subnet0 = subnet_from_devaddr(DevAddr00, NetIDList),
    % io:format("Subnet0 ~8.16.0B~n", [Subnet0]),
    % ?assertEqual(0, Subnet0),
    % DevAddr000 = devaddr_from_subnet(Subnet0, NetIDList),
    % io:format("DevAddr00 ~8.16.0B~n", [DevAddr00]),
    % io:format("DevAddr000 ~8.16.0B~n", [DevAddr000]),
    % ?assertEqual(DevAddr000, DevAddr00),

    Subnet1 = subnet_from_devaddr(DevAddr01, NetIDList),
    io:format("Subnet1 ~8.16.0B~n", [Subnet1]),
    ?assertEqual((1 bsl 7) + 16, Subnet1),
    DevAddr001 = devaddr_from_subnet(Subnet1, NetIDList),
    io:format("DevAddr01 ~8.16.0B~n", [DevAddr01]),
    io:format("DevAddr001 ~8.16.0B~n", [DevAddr001]),
    ?assertEqual(DevAddr001, DevAddr01),

    Subnet1 = subnet_from_devaddr(DevAddr01, NetIDList),
    ?assertEqual((1 bsl 7) + 16, Subnet1),
    DevAddr001 = devaddr_from_subnet(Subnet1, NetIDList),
    ?assertEqual(DevAddr001, DevAddr01),

    Subnet2 = subnet_from_devaddr(DevAddr02, NetIDList),
    ?assertEqual((1 bsl 7) + (1 bsl 10) + 8, Subnet2),
    DevAddr002 = devaddr_from_subnet(Subnet2, NetIDList),
    ?assertEqual(DevAddr002, DevAddr02),

    ok.

-endif.
