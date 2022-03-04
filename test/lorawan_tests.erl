-module(lorawan_tests).

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(RETIRED_NETID, 16#200010).

id_test() ->
    %% CP data
    ?assertEqual(
        {ok, 16#00002D}, lora_subnet:parse_netid(<<91, 255, 255, 255>>), "[45] == 2D == 45 type 0"
    ),
    ?assertEqual(
        {ok, 16#20002D}, lora_subnet:parse_netid(<<173, 255, 255, 255>>), "[45] == 2D == 45 type 1"
    ),
    ?assertEqual(
        {ok, 16#40016D}, lora_subnet:parse_netid(<<214, 223, 255, 255>>), "[1,109] == 16D == 365 type 2"
    ),
    ?assertEqual(
        {ok, 16#6005B7},
        lora_subnet:parse_netid(<<235, 111, 255, 255>>),
        "[5,183] == 5B7 == 1463 type 3"
    ),
    ?assertEqual(
        {ok, 16#800B6D},
        lora_subnet:parse_netid(<<245, 182, 255, 255>>),
        "[11, 109] == B6D == 2925 type 4"
    ),
    ?assertEqual(
        {ok, 16#A016DB},
        lora_subnet:parse_netid(<<250, 219, 127, 255>>),
        "[22,219] == 16DB == 5851 type 5"
    ),
    ?assertEqual(
        {ok, 16#C05B6D},
        lora_subnet:parse_netid(<<253, 109, 183, 255>>),
        "[91, 109] == 5B6D == 23405 type 6"
    ),
    ?assertEqual(
        {ok, 16#E16DB6},
        lora_subnet:parse_netid(<<254, 182, 219, 127>>),
        "[1,109,182] == 16DB6 == 93622 type 7"
    ),
    ?assertEqual(
        {error, invalid_netid_type},
        lora_subnet:parse_netid(<<255, 255, 255, 255>>),
        "Invalid DevAddr"
    ),

    % Actility spreadsheet examples
    ?assertEqual({ok, 0}, lora_subnet:parse_netid(<<0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:25>>)),
    ?assertEqual({ok, 1}, lora_subnet:parse_netid(<<0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 1:1, 0:25>>)),
    ?assertEqual({ok, 2}, lora_subnet:parse_netid(<<0:1, 0:1, 0:1, 0:1, 0:1, 1:1, 0:1, 0:25>>)),

    %% Mis-parsed as netid 4 of type 3
    ?assertEqual(
        {ok, 16#600004}, lora_subnet:parse_netid(<<224, 9, 171, 205>>), "hex_to_binary(<<'E009ABCD'>>)"
    ),
    %% Valid DevAddr, NetID not assigned
    ?assertEqual(
        {ok, 16#20002D},
        lora_subnet:parse_netid(<<173, 255, 255, 255>>),
        "hex_to_binary(<<'ADFFFFFF'>>)"
    ),
    %% Less than 32 bit number
    ?assertEqual({ok, 0}, lora_subnet:parse_netid(46377)),

    % Louis test data
    ?assertEqual({ok, 16#600002}, lora_subnet:parse_netid(<<224, 4, 0, 1>>)),
    ?assertEqual({ok, 16#600002}, lora_subnet:parse_netid(<<224, 5, 39, 132>>)),
    ?assertEqual({ok, 16#000002}, lora_subnet:parse_netid(<<4, 16, 190, 163>>)),
    ok.

exercise_netid(NetClass, ID, _IDLen, _AddrLen) ->
    NetIDBin = <<0:8/integer-unsigned, NetClass:3/integer-unsigned, ID:21/integer-unsigned>>,
    <<_NetID:32/integer-unsigned>> = NetIDBin,
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
    _LegacyNetID = ?RETIRED_NETID,
    <<H1:7, _/bitstring>> = LegacyDevAddr,
    <<H2:7, _:25>> = LegacyDevAddr,
    H3 = <<LegacyNum:32/integer-unsigned>>,
    ?assertEqual(H1, H2),
    ?assertEqual(H3, LegacyDevAddr),

    NetID00 = 16#E00001,
    NetID01 = 16#C00035,
    %% Official NetID assigned to Helium by LoRa Alliance
    NetID02 = 16#60002D,
    _NetIDExt = 16#C00050,

    %% DevAddr with legacy Helium NetID
    DevAddr00 = 16#90000000,
    %% DevAddr with class 6 NetID
    DevAddr01 = 16#FC00D410,
    %% DevAddr with class 3 NetID
    DevAddr02 = 16#E05A0008,

    NetIDList = [NetID00, NetID01, NetID02],

    %% Backwards DevAddr compatibility test
    %% DevAddr00 is a legacy Helium Devaddr.  The NetID is retired.
    %% By design we do compute a proper subnet (giving us a correct OUI route),
    %% but if we compute the associated DevAddr for this subnet (for the Join request)
    %% we'll get a new DevAddr associated with a current and proper NetID.
    %% In other words, DevAddr00 is not equal to DevAddr000.
    Subnet0 = lora_subnet:subnet_from_devaddr(DevAddr00, NetIDList),
    io:format("Subnet0 ~8.16.0B~n", [Subnet0]),
    ?assertEqual(0, Subnet0),
    DevAddr000 = lora_subnet:devaddr_from_subnet(Subnet0, NetIDList),
    io:format("DevAddr00 ~8.16.0B~n", [DevAddr00]),
    io:format("DevAddr000 ~8.16.0B~n", [DevAddr000]),
    %% By design the reverse DevAddr will have a correct NetID
    ?assertNotEqual(DevAddr000, DevAddr00),
    ?assertEqual(16#FE000080, DevAddr000),
    {ok, DevAddr000NetID} = lora_subnet:parse_netid(DevAddr000),
    ?assertEqual(NetID00, DevAddr000NetID),

    Subnet1 = lora_subnet:subnet_from_devaddr(DevAddr01, NetIDList),
    io:format("Subnet1 ~8.16.0B~n", [Subnet1]),
    ?assertEqual((1 bsl 7) + 16, Subnet1),
    DevAddr001 = lora_subnet:devaddr_from_subnet(Subnet1, NetIDList),
    io:format("DevAddr01 ~8.16.0B~n", [DevAddr01]),
    io:format("DevAddr001 ~8.16.0B~n", [DevAddr001]),
    ?assertEqual(DevAddr001, DevAddr01),

    Subnet1 = lora_subnet:subnet_from_devaddr(DevAddr01, NetIDList),
    ?assertEqual((1 bsl 7) + 16, Subnet1),
    DevAddr001 = lora_subnet:devaddr_from_subnet(Subnet1, NetIDList),
    ?assertEqual(DevAddr001, DevAddr01),

    Subnet2 = lora_subnet:subnet_from_devaddr(DevAddr02, NetIDList),
    ?assertEqual((1 bsl 7) + (1 bsl 10) + 8, Subnet2),
    DevAddr002 = lora_subnet:devaddr_from_subnet(Subnet2, NetIDList),
    ?assertEqual(DevAddr002, DevAddr02),

    ok.

-endif.
