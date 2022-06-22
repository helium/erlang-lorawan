%%%-------------------------------------------------------------------
%% @doc
%% == LoRaWAN ==
%% @end
%%%-------------------------------------------------------------------
-module(lora_subnet_tests).

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
%-define(EUNIT, 1).
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

-define(RETIRED_NETID, 16#200010).

helium_id_test() ->
    %% The LoRaWAN protocol is little endian
    %% devaddr values are little endian
    %% netid values are big endian
    A1 = lora_subnet:swap_four_bytes(<<8, 7, 0, 72>>),
    B1 = lora_subnet:swap_four_bytes(A1),
    ?assertEqual(B1, <<8, 7, 0, 72>>),
    A2 = lora_subnet:swap_four_bytes(16#08080048),
    % io:format("A2 ~8.16.0B~n", [A2]),
    % io:format("A2 ~w~n", [A2]),
    B2 = lora_subnet:swap_four_bytes(A2),
    ?assertEqual(B2, 16#08080048),
    %% Helium ID language constructs
    ?assertEqual($H bsr 1, 36),
    ?assertEqual(
        {ok, 16#000024}, lora_subnet:parse_netid(<<8, 7, 0, 72>>), "[36] == 0x24 == type 0"
    ),
    ?assertEqual(
        {ok, 36}, lora_subnet:parse_netid(<<8, 7, 0, 72>>)
    ),
    ?assertEqual(
        {ok, 36}, lora_subnet:parse_netid(16#08070048)
    ),
    ?assertEqual(
        {ok, 36}, lora_subnet:parse_netid(134742088)
    ),
    <<_:25/integer-unsigned-little, DevAddrPrefix_0:7/integer>> = <<8, 7, 0, 72>>,
    ?assertEqual(DevAddrPrefix_0, $H),
    <<DevAddrPrefix_1:8, _/binary>> = <<72, 0, 7, 8>>,
    ?assertEqual(DevAddrPrefix_1, $H),
    <<I:32/integer-unsigned>> = <<72, 0, 7, 8>>,
    ?assertEqual(I, 1207961352),
    ?assertEqual(I, 16#48000708),
    LittleEndian = <<I:4/little-signed-integer-unit:8>>,
    ?assertEqual(<<8, 7, 0, 72>>, LittleEndian),
    ok.

id_test() ->
    %% CP data
    ?assertEqual(
        {ok, 16#00002D}, lora_subnet:parse_netid(<<255, 255, 255, 91>>), "[45] == 2D == 45 type 0"
    ),
    ?assertEqual(
        {ok, 16#20002D}, lora_subnet:parse_netid(<<255, 255, 255, 173>>), "[45] == 2D == 45 type 1"
    ),
    ?assertEqual(
        {ok, 16#40016D},
        lora_subnet:parse_netid(<<255, 244, 223, 214>>),
        "[1,109] == 16D == 365 type 2"
    ),
    ?assertEqual(
        {ok, 16#6005B7},
        lora_subnet:parse_netid(<<255, 255, 111, 235>>),
        "[5,183] == 5B7 == 1463 type 3"
    ),
    ?assertEqual(
        {ok, 16#800B6D},
        lora_subnet:parse_netid(<<255, 255, 182, 245>>),
        "[11, 109] == B6D == 2925 type 4"
    ),
    ?assertEqual(
        {ok, 16#A016DB},
        lora_subnet:parse_netid(<<255, 127, 219, 250>>),
        "[22,219] == 16DB == 5851 type 5"
    ),
    ?assertEqual(
        {ok, 16#C05B6D},
        lora_subnet:parse_netid(<<255, 183, 109, 253>>),
        "[91, 109] == 5B6D == 23405 type 6"
    ),
    ?assertEqual(
        {ok, 16#E16DB6},
        lora_subnet:parse_netid(<<127, 219, 182, 254>>),
        "[1,109,182] == 16DB6 == 93622 type 7"
    ),
    ?assertEqual(
        {error, invalid_netid_type},
        lora_subnet:parse_netid(<<255, 255, 255, 255>>),
        "Invalid DevAddr"
    ),

    % Actility spreadsheet examples
    ?assertEqual(
        {ok, 0},
        lora_subnet:parse_netid(
            lora_subnet:swap_four_bytes(<<0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:25>>)
        )
    ),
    ?assertEqual(
        {ok, 1},
        lora_subnet:parse_netid(
            lora_subnet:swap_four_bytes(<<0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 1:1, 0:25>>)
        )
    ),
    ?assertEqual(
        {ok, 2},
        lora_subnet:parse_netid(
            lora_subnet:swap_four_bytes(<<0:1, 0:1, 0:1, 0:1, 0:1, 1:1, 0:1, 0:25>>)
        )
    ),
    %% Mis-parsed as netid 4 of type 3
    ?assertEqual(
        {ok, 16#600004},
        lora_subnet:parse_netid(<<205, 171, 9, 224>>),
        "hex_to_binary(<<'E009ABCD'>>)"
    ),
    %% Valid DevAddr, NetID not assigned
    ?assertEqual(
        {ok, 16#20002D},
        lora_subnet:parse_netid(<<255, 255, 255, 173>>),
        "hex_to_binary(<<'ADFFFFFF'>>)"
    ),
    %% Less than 32 bit number
    ?assertEqual({ok, 36}, lora_subnet:parse_netid(16#48)),

    % Louis test data
    ?assertEqual({ok, 16#600002}, lora_subnet:parse_netid(<<1, 0, 4, 224>>)),
    ?assertEqual({ok, 16#600002}, lora_subnet:parse_netid(<<132, 39, 5, 224>>)),
    ?assertEqual({ok, 16#000002}, lora_subnet:parse_netid(<<163, 190, 16, 4>>)),
    ok.

devaddr_test() ->
    RandList = [random_unsigned() || _X <- lists:seq(0, 1000)],
    [valid_devaddr(R) || R <- RandList].

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

    NetWidth0 = lora_subnet:addr_len(lora_subnet:netid_class(NetID00)),
    ?assertEqual(7, NetWidth0),
    NetWidth1 = lora_subnet:addr_len(lora_subnet:netid_class(NetID01)),
    ?assertEqual(10, NetWidth1),
    NetWidth2 = lora_subnet:addr_len(lora_subnet:netid_class(NetID02)),
    ?assertEqual(17, NetWidth2),
    NetSize0 = lora_subnet:netid_size(NetID00),
    ?assertEqual(128, NetSize0),
    NetSize1 = lora_subnet:netid_size(NetID01),
    ?assertEqual(1024, NetSize1),
    NetSize2 = lora_subnet:netid_size(NetID02),
    ?assertEqual(131072, NetSize2),

    NetIDList = [NetID00, NetID01, NetID02],
    LocalTrue = lora_subnet:is_local_netid(NetID01, NetIDList),
    LocalFalse = lora_subnet:is_local_netid(NetIDExt, NetIDList),
    LegacyLocal = lora_subnet:is_local_netid(LegacyNetID, NetIDList),
    ?assertEqual(true, LocalTrue),
    ?assertEqual(false, LocalFalse),
    ?assertEqual(true, LegacyLocal),

    DevAddrLegacy = lora_subnet:devaddr(LegacyNetID, 0),
    ?assertEqual(DevAddr00, DevAddrLegacy),
    DevAddr1 = lora_subnet:devaddr(NetID01, 16),
    ?assertEqual(DevAddr01, DevAddr1),
    DevAddr2 = lora_subnet:devaddr(NetID02, 8),
    ?assertEqual(DevAddr02, DevAddr2),

    NetIDType00 = lora_subnet:netid_type(DevAddr00),
    ?assertEqual(1, NetIDType00),
    NetIDType01 = lora_subnet:netid_type(DevAddr01),
    ?assertEqual(6, NetIDType01),
    NetIDType02 = lora_subnet:netid_type(DevAddr02),
    ?assertEqual(3, NetIDType02),

    NetIDType0 = lora_subnet:netid_type(DevAddrLegacy),
    ?assertEqual(1, NetIDType0),
    NetIDType1 = lora_subnet:netid_type(DevAddr1),
    ?assertEqual(6, NetIDType1),
    NetIDType2 = lora_subnet:netid_type(DevAddr2),
    ?assertEqual(3, NetIDType2),

    {ok, NetID_0} = parse_netid_be(DevAddr00),
    ?assertEqual(NetID_0, LegacyNetID),
    {ok, NetID_1} = parse_netid_be(16#FC00D410),
    ?assertEqual(NetID_1, 16#C00035),
    {ok, NetID_1} = parse_netid_be(DevAddr01),
    ?assertEqual(NetID_1, NetID01),
    {ok, NetID_2} = parse_netid_be(DevAddr02),
    ?assertEqual(NetID_2, NetID02),

    {ok, NetID0} = parse_netid_be(DevAddrLegacy),
    ?assertEqual(NetID0, LegacyNetID),
    {ok, NetID1} = parse_netid_be(DevAddr1),
    ?assertEqual(NetID1, NetID01),
    {ok, NetID2} = parse_netid_be(DevAddr2),
    ?assertEqual(NetID2, NetID02),

    Width_0 = lora_subnet:addr_bit_len(DevAddr00),
    ?assertEqual(24, Width_0),
    Width_1 = lora_subnet:addr_bit_len(DevAddr01),
    ?assertEqual(10, Width_1),
    Width_2 = lora_subnet:addr_bit_len(DevAddr02),
    ?assertEqual(17, Width_2),

    Width0 = lora_subnet:addr_bit_len(DevAddrLegacy),
    ?assertEqual(24, Width0),
    Width1 = lora_subnet:addr_bit_len(DevAddr1),
    ?assertEqual(10, Width1),
    Width2 = lora_subnet:addr_bit_len(DevAddr2),
    ?assertEqual(17, Width2),

    NwkAddr0 = lora_subnet:nwk_addr(DevAddr00),
    ?assertEqual(0, NwkAddr0),
    NwkAddr1 = lora_subnet:nwk_addr(DevAddr01),
    ?assertEqual(16, NwkAddr1),
    NwkAddr2 = lora_subnet:nwk_addr(DevAddr02),
    ?assertEqual(8, NwkAddr2),

    %% Backwards DevAddr compatibility test
    %% DevAddr00 is a legacy Helium Devaddr.  The NetID is retired.
    %% By design we do compute a proper subnet (giving us a correct OUI route),
    %% but if we compute the associated DevAddr for this subnet (for the Join request)
    %% we'll get a new DevAddr associated with a current and proper NetID.
    %% In other words, DevAddr00 is not equal to DevAddr000.
    Subnet0 = subnet_from_devaddr_be(DevAddr00, NetIDList),
    io:format("Subnet0 ~8.16.0B~n", [Subnet0]),
    ?assertEqual(0, Subnet0),
    DevAddr000 = devaddr_from_subnet_be(Subnet0, NetIDList),
    io:format("DevAddr00 ~8.16.0B~n", [DevAddr00]),
    io:format("DevAddr000 ~8.16.0B~n", [DevAddr000]),
    %% By design the reverse DevAddr will have a correct NetID
    ?assertNotEqual(DevAddr000, DevAddr00),
    ?assertEqual(16#FE000080, DevAddr000),
    {ok, DevAddr000NetID} = parse_netid_be(DevAddr000),
    ?assertEqual(NetID00, DevAddr000NetID),
    NwkAddr000 = lora_subnet:nwk_addr(DevAddr000),
    ?assertEqual(NwkAddr0, NwkAddr000),

    Subnet1 = subnet_from_devaddr_be(DevAddr01, NetIDList),
    io:format("Subnet1 ~8.16.0B~n", [Subnet1]),
    ?assertEqual((1 bsl 7) + 16, Subnet1),
    DevAddr001 = devaddr_from_subnet_be(Subnet1, NetIDList),
    io:format("DevAddr01 ~8.16.0B~n", [DevAddr01]),
    io:format("DevAddr001 ~8.16.0B~n", [DevAddr001]),
    ?assertEqual(DevAddr001, DevAddr01),

    Subnet1 = subnet_from_devaddr_be(DevAddr01, NetIDList),
    ?assertEqual((1 bsl 7) + 16, Subnet1),
    DevAddr001 = devaddr_from_subnet_be(Subnet1, NetIDList),
    ?assertEqual(DevAddr001, DevAddr01),

    Subnet2 = subnet_from_devaddr_be(DevAddr02, NetIDList),
    ?assertEqual((1 bsl 7) + (1 bsl 10) + 8, Subnet2),
    DevAddr002 = devaddr_from_subnet_be(Subnet2, NetIDList),
    ?assertEqual(DevAddr002, DevAddr02),

    ok.

%%
%% Test Helper Functions
%%

random_unsigned() ->
    <<A:32/unsigned-integer>> = crypto:strong_rand_bytes(4),
    A.

parse_netid_be(DevAddr0) ->
    DevAddr = lora_subnet:swap_four_bytes(DevAddr0),
    lora_subnet:parse_netid(DevAddr).

subnet_from_devaddr_be(DevAddr0, NetIDList) ->
    DevAddr = lora_subnet:swap_four_bytes(DevAddr0),
    lora_subnet:subnet_from_devaddr(DevAddr, NetIDList).

devaddr_from_subnet_be(SubnetAddr, NetIDList) ->
    DevAddr = lora_subnet:devaddr_from_subnet(SubnetAddr, NetIDList),
    lora_subnet:swap_four_bytes(DevAddr).

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

valid_subnet(DevAddr, NetIDList) ->
    SubnetAddr = subnet_from_devaddr_be(DevAddr, NetIDList),
    DevAddr2 = devaddr_from_subnet_be(SubnetAddr, NetIDList),
    ?assertEqual(DevAddr, DevAddr2).

valid_subnet(DevAddr) ->
    {ok, NetID} = parse_netid_be(DevAddr),
    valid_subnet(DevAddr, insert_item(NetID, mock_netid_list(), 0)),
    valid_subnet(DevAddr, insert_item(NetID, mock_netid_list(), 1)),
    valid_subnet(DevAddr, insert_item(NetID, mock_netid_list(), 2)),
    valid_subnet(DevAddr, insert_item(NetID, mock_netid_list(), 3)),
    ok.

random_subnet(DevAddr) ->
    {ok, NetID} = parse_netid_be(DevAddr),
    [valid_subnet(DevAddr, insert_rand(NetID, mock_random_netids())) || _ <- lists:seq(1, 400)],
    ok.

valid_devaddr(DevAddr) ->
    % io:format("DevAddr ~8.16.0B~n", [DevAddr]),
    DevAddrLE = lora_subnet:swap_four_bytes(DevAddr),
    LowMask = DevAddrLE band 16#FF,
    case LowMask of
        16#FF ->
            % io:format("LowMask ~w~n", [DevAddr]),
            ok;
        _ ->
            {ok, NetID} = parse_netid_be(DevAddr),
            % io:format("NetID ~8.16.0B~n", [NetID]),
            NetIDType = lora_subnet:netid_type(DevAddr),
            % io:format("NetIDType ~8.16.0B~n", [NetIDType]),
            ?assert(NetIDType =< 7),
            NetClass = lora_subnet:netid_class(NetID),
            % io:format("NetClass ~8.16.0B~n", [NetClass]),
            AddrLen = lora_subnet:addr_len(NetClass),
            % io:format("AddrLen ~8.16.0B~n", [AddrLen]),
            IDLen = lora_subnet:id_len(NetClass),
            % io:format("IDLen ~8.16.0B~n", [IDLen]),
            ?assert(AddrLen + IDLen < 32),
            NwkAddr = lora_subnet:nwk_addr(DevAddr),
            % io:format("NwkAddr ~8.16.0B~n", [NwkAddr]),
            ?assert(NwkAddr < (1 bsl AddrLen)),
            valid_subnet(DevAddr),
            ok
    end.

exercise_devaddr(NetID, Addr, _IDLen, AddrLen) ->
    DevAddr = lora_subnet:devaddr(NetID, Addr),
    NetIDType = lora_subnet:netid_type(DevAddr),
    ?assert(NetIDType =< 7),
    {ok, NetID0} = parse_netid_be(DevAddr),
    ?assertEqual(NetID, NetID0),
    AddrBitLen = lora_subnet:addr_bit_len(DevAddr),
    ?assertEqual(AddrLen, AddrBitLen),
    NwkAddr = lora_subnet:nwk_addr(DevAddr),
    ?assertEqual(Addr, NwkAddr),
    valid_subnet(DevAddr),
    random_subnet(DevAddr),
    ok.

exercise_netid(NetClass, ID, IDLen, AddrLen) ->
    NetIDBin = <<0:8/integer-unsigned, NetClass:3/integer-unsigned, ID:21/integer-unsigned>>,
    <<NetID:32/integer-unsigned>> = NetIDBin,
    NetAddrLen = lora_subnet:addr_len(NetClass),
    ?assert(NetAddrLen == AddrLen),
    MaxNetSize = lora_subnet:netid_size(NetID),
    exercise_devaddr(NetID, 0, IDLen, AddrLen),
    exercise_devaddr(NetID, 1, IDLen, AddrLen),
    exercise_devaddr(NetID, 8, IDLen, AddrLen),
    exercise_devaddr(NetID, 16, IDLen, AddrLen),
    exercise_devaddr(NetID, 32, IDLen, AddrLen),
    exercise_devaddr(NetID, 33, IDLen, AddrLen),
    exercise_devaddr(NetID, 64, IDLen, AddrLen),
    exercise_devaddr(NetID, MaxNetSize - 1, IDLen, AddrLen),
    ok.

-endif.
