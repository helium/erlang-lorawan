%%%-------------------------------------------------------------------
%% @doc
%% == LoRaWAN ==
%% @end
%%%-------------------------------------------------------------------
-module(lora_subnet).

-export([
    is_local_devaddr/2,
    parse_netid/1,
    parse_netid/2,
    addr_bit_len/1,
    devaddr_from_subnet/2,
    devaddr_from_netid/2,
    subnet_from_devaddr/2
]).

-ifdef(EUNIT).
-export([
    is_local_netid/2,
    netid_class/1,
    addr_len/1,
    id_len/1,
    netid_type/1,
    nwk_addr/1,
    devaddr/2,
    netid_size/1,
    swap_four_bytes/1
]).
-endif.

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
%% The DevAddr is an unsigned little endian.
%% NetIDList contains Helium's ordered list of assigned NetIDs.
%%
%% @end
%%------------------------------------------------------------------------------
-spec is_local_devaddr(devaddr(), [netid()]) -> boolean().
is_local_devaddr(DevAddr0, NetIDList) ->
    DevAddr = swap_four_bytes(DevAddr0),
    NetID = the_netid(DevAddr),
    is_local_netid(NetID, NetIDList).

%%------------------------------------------------------------------------------
%% @doc Translate from a Helium subnet address to a LoRaWAN DevAddr.
%% The DevAddr is an unsigned little endian.
%% NetIDList contains Helium's ordered list of assigned NetIDs.
%%
%% @end
%%------------------------------------------------------------------------------
-spec devaddr_from_subnet(subnetaddr(), [netid()]) -> devaddr().
devaddr_from_subnet(SubnetAddr, NetIDList) ->
    NetID = subnet_addr_to_netid(SubnetAddr, NetIDList),
    {Lower, _Upper} = netid_addr_range(NetID, NetIDList),
    DevAddr = devaddr(NetID, SubnetAddr - Lower),
    swap_four_bytes(DevAddr).

%%------------------------------------------------------------------------------
%% @doc Construct a DevAddr given a NetID and NwkAddr
%% The DevAddr is an unsigned little endian.
%%
%% @end
%%------------------------------------------------------------------------------
-spec devaddr_from_netid(netid(), nwkaddr()) -> devaddr().
devaddr_from_netid(NetID, NwkAddr) ->
    DevAddr = devaddr(NetID, NwkAddr),
    swap_four_bytes(DevAddr).

%%------------------------------------------------------------------------------
%% @doc Translate from a LoRaWAN DevAddr to a Helium subnet address.
%% The DevAddr is an unsigned little endian.
%% NetIDList contains Helium's ordered list of assigned NetIDs.
%%
%% @end
%%------------------------------------------------------------------------------
-spec subnet_from_devaddr(devaddr(), [netid()]) -> subnetaddr().
subnet_from_devaddr(DevAddr0, NetIDList) ->
    DevAddr = swap_four_bytes(DevAddr0),
    NetID = the_netid(DevAddr),
    {Lower, _Upper} = netid_addr_range(NetID, NetIDList),
    SubnetAddr = Lower + nwk_addr(DevAddr),
    SubnetAddr.

%%------------------------------------------------------------------------------
%% @doc Extract a big endian NetID from the LoRaWAN DevAddr.
%% Specify whether the DevAddr is little or big endian.
%%
%% @end
%%------------------------------------------------------------------------------
-spec parse_netid(DevAddr, Endianness) ->
    {ok, netid()} | {error, invalid_netid_type}
when
    DevAddr :: non_neg_integer() | binary(),
    Endianness :: big | little.
parse_netid(DevAddr, Endianness) ->
    case Endianness of
        big -> parse_netid(swap_four_bytes(DevAddr));
        little -> parse_netid(DevAddr)
    end.

%%------------------------------------------------------------------------------
%% @doc Extract a big endian NetID from the little endian LoRaWAN DevAddr.
%%
%% @end
%%------------------------------------------------------------------------------
-spec parse_netid(non_neg_integer() | binary()) -> {ok, netid()} | {error, invalid_netid_type}.
parse_netid(DevNum) when erlang:is_number(DevNum) ->
    parse_netid(<<DevNum:32/integer-unsigned>>);
parse_netid(DevAddr0) ->
    try
        DevAddr = swap_four_bytes(DevAddr0),
        Type = netid_type(DevAddr),
        ID = parse_id(DevAddr, Type + 1, id_len(Type)),
        {ok, ID bor (Type bsl 21)}
    catch
        throw:invalid_netid_type:_ ->
            {error, invalid_netid_type}
    end.

%%------------------------------------------------------------------------------
%% @doc Convert from big endian to little endian or vice versa.
%%
%% @end
%%------------------------------------------------------------------------------
-spec swap_four_bytes(non_neg_integer() | binary()) -> non_neg_integer() | binary().
swap_four_bytes(Value) when is_binary(Value) ->
    <<I:32/integer-unsigned>> = Value,
    Swapped = <<I:4/little-signed-integer-unit:8>>,
    Swapped;
swap_four_bytes(Value) when is_integer(Value) ->
    Swapped = <<Value:4/little-signed-integer-unit:8>>,
    <<I:32/integer-unsigned>> = Swapped,
    I.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec netid_class(netid()) -> netclass().
netid_class(NetID) ->
    NetClass = NetID bsr 21,
    NetClass.

-spec addr_len(netclass()) -> 7 | 10 | 13 | 15 | 17 | 20 | 24 | 25.
addr_len(0) -> 25;
addr_len(1) -> 24;
addr_len(2) -> 20;
addr_len(3) -> 17;
addr_len(4) -> 15;
addr_len(5) -> 13;
addr_len(6) -> 10;
addr_len(7) -> 7.

-spec id_len(netclass()) -> 6 | 9 | 11 | 12 | 13 | 15 | 17.
id_len(0) -> 6;
id_len(1) -> 6;
id_len(2) -> 9;
id_len(3) -> 11;
id_len(4) -> 12;
id_len(5) -> 13;
id_len(6) -> 15;
id_len(7) -> 17.

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
var_net_class(NetClass) ->
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
            Upper = Lower + netid_size(NetID),
            {Lower, Upper}
    end.

-spec netid_size(netid()) -> non_neg_integer().
netid_size(NetID) ->
    Size = 1 bsl addr_len(netid_class(NetID)),
    Size.

-spec uint32(integer()) -> integer().
uint32(Num) ->
    Num band 16#FFFFFFFF.

%-endif.
