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

-spec devaddr_from_subnet(subnetaddr(), [netid()]) -> devaddr().
devaddr_from_subnet(SubnetAddr, NetIDList) ->
    NetID = subnet_addr_to_netid(SubnetAddr, NetIDList),
    {Lower, _Upper} = netid_addr_range(NetID, NetIDList),
    DevAddr = devaddr(NetID, SubnetAddr - Lower),
    DevAddr.

-spec subnet_from_devaddr(devaddr(), [netid()]) -> subnetaddr().
subnet_from_devaddr(DevAddr, NetIDList) ->
    NetID = the_netid(DevAddr),
    {Lower, _Upper} = netid_addr_range(NetID, NetIDList),
    SubnetAddr = Lower + nwk_addr(DevAddr),
    SubnetAddr.

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

-spec is_local_netid(netid(), [netid()]) -> boolean().
is_local_netid(NetID, NetIDList) ->
    case NetID of
        ?RETIRED_NETID ->
            true;
        _ ->
            lists:any(fun(X) -> X == NetID end, NetIDList)
    end.

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
    ID = get_id(DevAddr, Type + 1, id_len(Type)),
    NetID = ID bor (Type bsl 21),
    NetID.

-spec netid(number() | binary()) -> {ok, netid()} | {error, invalid_netid_type}.
netid(DevNum) when erlang:is_number(DevNum) ->
    netid(<<DevNum:32/integer-unsigned>>);
netid(DevAddr) ->
    try
        Type = netid_type(DevAddr),
        ID = get_id(DevAddr, Type + 1, id_len(Type)),
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

-spec get_id(binary(), non_neg_integer(), non_neg_integer()) -> netid().
get_id(DevAddr, PrefixLength, NwkIDBits) ->
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
