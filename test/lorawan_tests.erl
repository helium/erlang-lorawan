-module(lorawan_tests).

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(RETIRED_NETID, 16#200010).

id_test() ->
    %% CP data
    ?assertEqual({ok, 16#00002D}, lorawan:netid(<<91, 255, 255, 255>>), "[45] == 2D == 45 type 0"),
    ?assertEqual({ok, 16#20002D}, lorawan:netid(<<173, 255, 255, 255>>), "[45] == 2D == 45 type 1"),
    ?assertEqual({ok, 16#40016D}, lorawan:netid(<<214, 223, 255, 255>>), "[1,109] == 16D == 365 type 2"),
    ?assertEqual({ok, 16#6005B7}, lorawan:netid(<<235, 111, 255, 255>>), "[5,183] == 5B7 == 1463 type 3"),
    ?assertEqual(
        {ok, 16#800B6D},
        lorawan:netid(<<245, 182, 255, 255>>),
        "[11, 109] == B6D == 2925 type 4"
    ),
    ?assertEqual(
        {ok, 16#A016DB},
        lorawan:netid(<<250, 219, 127, 255>>),
        "[22,219] == 16DB == 5851 type 5"
    ),
    ?assertEqual(
        {ok, 16#C05B6D},
        lorawan:netid(<<253, 109, 183, 255>>),
        "[91, 109] == 5B6D == 23405 type 6"
    ),
    ?assertEqual(
        {ok, 16#E16DB6},
        lorawan:netid(<<254, 182, 219, 127>>),
        "[1,109,182] == 16DB6 == 93622 type 7"
    ),
    ?assertEqual(
        {error, invalid_netid_type},
        lorawan:netid(<<255, 255, 255, 255>>),
        "Invalid DevAddr"
    ),

    % Actility spreadsheet examples
    ?assertEqual({ok, 0}, lorawan:netid(<<0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:25>>)),
    ?assertEqual({ok, 1}, lorawan:netid(<<0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 1:1, 0:25>>)),
    ?assertEqual({ok, 2}, lorawan:netid(<<0:1, 0:1, 0:1, 0:1, 0:1, 1:1, 0:1, 0:25>>)),

    %% Mis-parsed as netid 4 of type 3
    ?assertEqual({ok, 16#600004}, lorawan:netid(<<224, 9, 171, 205>>), "hex_to_binary(<<'E009ABCD'>>)"),
    %% Valid DevAddr, NetID not assigned
    ?assertEqual({ok, 16#20002D}, lorawan:netid(<<173, 255, 255, 255>>), "hex_to_binary(<<'ADFFFFFF'>>)"),
    %% Less than 32 bit number
    ?assertEqual({ok, 0}, lorawan:netid(46377)),

    % Louis test data
    ?assertEqual({ok, 16#600002}, lorawan:netid(<<224, 4, 0, 1>>)),
    ?assertEqual({ok, 16#600002}, lorawan:netid(<<224, 5, 39, 132>>)),
    ?assertEqual({ok, 16#000002}, lorawan:netid(<<4, 16, 190, 163>>)),
    ok.

create_netid(NetClass, ID) ->
    NetIDBin = <<0:8/integer-unsigned, NetClass:3/integer-unsigned, ID:21/integer-unsigned>>,
    <<NetID:32/integer-unsigned>> = NetIDBin,
    NetID.

mock_random_netids() ->
    Len = rand:uniform(10),
    [create_netid(rand:uniform(7), rand:uniform(64)) || _ <- lists:seq(1, Len)].

mock_netid_list() ->
    [ 16#E00001, 16#C00035, 16#60002D ].

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
	{ok, NetID} = lorawan:netid(DevAddr),
	exercise_subnet(DevAddr, insert_item(NetID, mock_netid_list(), 0)),
	exercise_subnet(DevAddr, insert_item(NetID, mock_netid_list(), 1)),
	exercise_subnet(DevAddr, insert_item(NetID, mock_netid_list(), 2)),
	exercise_subnet(DevAddr, insert_item(NetID, mock_netid_list(), 3)),
    ok.

random_subnet(DevAddr) ->
    {ok, NetID} = lorawan:netid(DevAddr),
    [exercise_subnet(DevAddr, insert_rand(NetID, mock_random_netids())) || _ <- lists:seq(1, 400)],
    ok.

exercise_devaddr(NetID, Addr, _IDLen, AddrLen) ->
	DevAddr = lorawan:devaddr(NetID, Addr),
	NetIDType = lorawan:netid_type(DevAddr),
    ?assert(NetIDType =< 7),
    {ok, NetID0} = lorawan:netid(DevAddr),
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
	NetAddrLen = lorawan:addr_len(NetID),
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
    NetID02 = 16#60002D,
    NetIDExt = 16#C00050,

    %% Class 6
    DevAddr00 = 16#90000000,
    DevAddr01 = 16#FC00D410,
    DevAddr02 = 16#E05A0008,

    NetWidth0 = lorawan:addr_len(NetID00),
    ?assertEqual(7, NetWidth0),
    NetWidth1 = lorawan:addr_len(NetID01),
    ?assertEqual(10, NetWidth1),
    NetWidth2 = lorawan:addr_len(NetID02),
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

    {ok, NetID_0} = lorawan:netid(DevAddr00),
    ?assertEqual(NetID_0, LegacyNetID),
    {ok, NetID_1} = lorawan:netid(16#FC00D410),
    ?assertEqual(NetID_1, 16#C00035),
    {ok, NetID_1} = lorawan:netid(DevAddr01),
    ?assertEqual(NetID_1, NetID01),
    {ok, NetID_2} = lorawan:netid(DevAddr02),
    ?assertEqual(NetID_2, NetID02),

    {ok, NetID0} = lorawan:netid(DevAddrLegacy),
    ?assertEqual(NetID0, LegacyNetID),
    {ok, NetID1} = lorawan:netid(DevAddr1),
    ?assertEqual(NetID1, NetID01),
    {ok, NetID2} = lorawan:netid(DevAddr2),
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
    %% we'll get a new one associated with a current and proper NetID
    Subnet0 = lorawan:subnet_from_devaddr(DevAddr00, NetIDList),
    io:format("Subnet0 ~8.16.0B~n", [Subnet0]),
    ?assertEqual(0, Subnet0),
    DevAddr000 = lorawan:devaddr_from_subnet(Subnet0, NetIDList),
    io:format("DevAddr00 ~8.16.0B~n", [DevAddr00]),
    io:format("DevAddr000 ~8.16.0B~n", [DevAddr000]),
    %% By design the reverse DevAddr will have a correct NetID
    ?assertNotEqual(DevAddr000, DevAddr00),
    ?assertEqual(16#FE000080, DevAddr000),
    {ok, DevAddr000NetID} = lorawan:netid(DevAddr000),
    ?assertEqual(NetID00, DevAddr000NetID),

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