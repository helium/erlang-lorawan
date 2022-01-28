%%%-------------------------------------------------------------------
%% @doc
%% == LoRaWAN ==
%% @end
%%%-------------------------------------------------------------------
-module(lora_decode).

-export([
    %% public functions
    sample1/0,
    base64_to_binary/1,
    payload_mhdr/1
    %% internal functions
]).

%% lorawan message types
-define(JOIN_REQUEST, 2#000).
-define(JOIN_ACCEPT, 2#001).
-define(UNCONFIRMED_UP, 2#010).
-define(UNCONFIRMED_DOWN, 2#011).
-define(CONFIRMED_UP, 2#100).
-define(CONFIRMED_DOWN, 2#101).

-spec base64_to_binary(binary()) -> binary().
base64_to_binary(Data) ->
    base64:decode(Data).

payload_join_request(PhyPayload) ->
    <<?JOIN_REQUEST:3, _MHDRRFU:3, _Major:2, AppEUI:8/binary, DevEUI:8/binary, DevNonce:2/binary, _MIC:4/binary>> = PhyPayload,
    {AppEUI, DevEUI, DevNonce}.

payload_join_accept(PhyPayload) ->
    MacPayload = payload_macpayload(PhyPayload),
    <<JoinNonce:3/binary, NetID:3/binary, DevAddr:4/binary, DLSettings:1/binary, RXDelay:1/binary, CFList/binary>> = MacPayload,
    {JoinNonce, NetID, DevAddr, DLSettings, RXDelay, CFList}.

-spec payload_mhdr(binary()) -> binary().
payload_mhdr(PhyPayload) ->
    <<MHDR:8/integer-unsigned, _/binary>> = PhyPayload,
    <<MHDR>>.

payload_direction(PhyPayload) ->
    <<_Ignore:2/integer-unsigned, DirectionBit:1/integer-unsigned, _Ignore2:5/integer, _/binary>> = PhyPayload,
    case DirectionBit of
        0 -> <<"up">>;
        1 -> <<"down">>
    end.

payload_mic(PhyPayload) ->
    PayloadSize = byte_size(PhyPayload),
    Part = {PayloadSize, -4},
    MIC = binary:part(PhyPayload, Part),
    MIC.

payload_macpayload(PhyPayload) ->
    PayloadSize = byte_size(PhyPayload),
    MacPayloadSize = PayloadSize - 5,
    Part = {1, MacPayloadSize},
    MacPayload = binary:part(PhyPayload, Part),
    MacPayload.

payload_ftype(PhyPayload) ->
    MHDR = payload_mhdr(PhyPayload),
    <<FType:3/integer-unsigned, _RFU:3/integer-unsigned, _Major:2/integer-unsigned>> = MHDR,
    FType.

payload_major(PhyPayload) ->
    MHDR = payload_mhdr(PhyPayload),
    <<_FType:3, _RFU:3, Major:2/integer-unsigned>> = MHDR,
    Major.

payload_fcnt(PhyPayload) ->
    <<_MHDR:8/integer, _DevAddr:32/integer, _FCtrl:8/integer-unsigned, FCnt:16/little-integer-unsigned, _/binary>> = PhyPayload,
    FCnt.

payload_devaddr(PhyPayload) ->
    <<_MHDR:8/integer, DevAddr:32/integer, _/binary>> = PhyPayload,
    DevAddr.

payload_fctrl(PhyPayload) ->
    <<_MHDR:8/integer, _DevAddr:32/integer, FCtrl:8/little-integer-unsigned, _/binary>> = PhyPayload,
    FCtrl.

payload_foptslen(PhyPayload) ->
    <<_MHDR:8/integer, _DevAddr:32/integer, _Ignore:4/integer-unsigned, Foptslen:4/integer-unsigned, _/binary>> = PhyPayload,
    Foptslen.

payload_fopts(PhyPayload) ->
    Len = payload_foptslen(PhyPayload),
    %% Offset == MHDR + DevAddr + FCtrl + FCnt
    Offset = 1 + 4 + 1 + 2,
    Part = {Offset, Len},
    FOpt = binary:part(PhyPayload, Part),
    FOpt.

payload_fhdr(PhyPayload) ->
    FOptsLen = payload_foptslen(PhyPayload),
    FhdrLen = 7 + FOptsLen,
    Part = {1, FhdrLen},
    FHDR = binary:part(PhyPayload, Part),
    FHDR.

%% ==================================================================
%% Tests
%% ==================================================================
%%-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

sample0() ->
    <<"QHcQASaAFAABvRjrSjJcz6vXC2TMw1A=">>.
sample1() ->
    <<"YAQAAEiqLgADUwAAcANTAP8ADY5nmA==">>.
join_request_sample() ->
    <<"ANwAANB+1bNwHm/t9XzurwDIhgMK8sk=">>.
join_accept_sample() ->
    <<"IIE/R/UI/6JnC24j4B+EueJdnEEV8C7qCz3T4gs+ypLa">>.

bin_to_hex(Binary) ->
    [[io_lib:format("~2.16.0B",[X]) || <<X:8>> <= Binary ]].

decode_message_type(Payload) ->
    io:format("~n( MHDR = Ftype[7:5] | RFU[4:2] | Major[1:0] )~n"),
    FType = payload_ftype(Payload),
    io:format("FType = ~w~n", [FType]),
    case FType of
        ?JOIN_REQUEST -> io:format("Message Type = Join Request~n");
        ?JOIN_ACCEPT -> io:format("Message Type = Join Accept~n");
        _ -> io:format("Message Type = Frame~n")
    end,
    Direction = payload_direction(Payload),
    io:format("Direction = ~s~n", [Direction]),
    Major = payload_major(Payload),
    io:format("Major = ~w~n", [Major]),
    fin.

decode_macpayload(Payload) ->
    Bin0 = Payload,
    io:format("~n( PHYPayload = MHDR[1] | MACPayload[..] | MIC[4] )~n"),
    MHDR = payload_mhdr(Bin0),
    %% io:format("Binary ~8.16.0B~n", [MHDR]),
    io:format("MHDR = ~w~n", [MHDR]),
    MacPayload = payload_macpayload(Bin0),
    io:format("MacPayload = ~w~n", [MacPayload]),
    io:format("MacPayload = ~s~n", [bin_to_hex(MacPayload)]),
    MIC = payload_mic(Bin0),
    io:format("MIC = ~s~n", [bin_to_hex(MIC)]),
    fin.

decode_join_request(Payload) ->
    Bin0 = Payload,

    {AppEUI, DevEUI, DevNonce} = payload_join_request(Bin0),
    io:format("AppEUI = ~s~n", [bin_to_hex(AppEUI)]),
    io:format("DevEUI = ~s~n", [bin_to_hex(DevEUI)]),
    io:format("DevNonce = ~s~n", [bin_to_hex(DevNonce)]),
    fin.

decode_join_accept(Payload) ->
    Bin0 = Payload,

    io:format("~n( MACPayload = AppNonce[3] | NetID[3] | DevAddr[4] | DLSettings[1] | RxDelay[1] | CFList[0|15] )~n"),
    {JoinNonce, NetID, DevAddr, DLSettings, RXDelay, CFList} = payload_join_accept(Bin0),
    io:format("JoinNonce = ~s~n", [bin_to_hex(JoinNonce)]),
    io:format("NetID = ~s~n", [bin_to_hex(NetID)]),
    io:format("DevAddr = ~s~n", [bin_to_hex(DevAddr)]),
    io:format("DLSettings = ~s~n", [bin_to_hex(DLSettings)]),
    io:format("RXDelay = ~s~n", [bin_to_hex(RXDelay)]),
    io:format("CFList = ~s~n", [bin_to_hex(CFList)]),
    fin.

decode_frame(Payload) ->
    Bin0 = Payload,
    io:format("~n( MACPayload = FHDR | FPort | FRMPayload )~n"),
    FHDR = payload_fhdr(Bin0),
    io:format("FHDR = ~w~n", [FHDR]),
    io:format("FHDR = ~s~n", [bin_to_hex(FHDR)]),
    io:format("FPort = ~w~n", [0]),
    io:format("FRMPayload = ~w~n", [0]),

    io:format("~n( FHDR = DevAddr[4] | FCtrl[1] | FCnt[2] | FOpts[0..15] )~n"),
    DevAddr = payload_devaddr(Bin0),
    io:format("DevAddr = ~8.16.0B~n", [DevAddr]),
    FCtrl = payload_fctrl(Bin0),
    io:format("FCtrl = ~w~n", [FCtrl]),
    FCnt = payload_fcnt(Bin0),
    io:format("FCnt = ~w~n", [FCnt]),
    FOptsLen = payload_foptslen(Bin0),
    io:format("FOptsLen = ~w~n", [FOptsLen]),
    FOpts = payload_fopts(Bin0),
    io:format("FOpts = ~w~n", [FOpts]),
    io:format("FOpts = ~s~n", [bin_to_hex(FOpts)]),

    FType = payload_ftype(Bin0),
    io:format("~nMessage Type = ~w~n", [FType]),
    Direction = payload_direction(Bin0),
    io:format("Direction = ~s~n", [Direction]),
    FCnt2 = payload_fcnt(Bin0),
    io:format("FCnt = ~w~n", [FCnt2]),
    fin.    

decode_payload(Base64) ->
    io:format("~nAssuming base64-encoded packet~n"),
    io:format("~s~n", [Base64]),
    Bin0 = base64_to_binary(Base64),
    io:format("Binary packet = ~w~n", [Bin0]),

    decode_message_type(Bin0),
    decode_macpayload(Bin0),

    MType = payload_ftype(Bin0),
    case MType of
        ?JOIN_REQUEST -> decode_join_request(Bin0);
        ?JOIN_ACCEPT -> decode_join_accept(Bin0);
        _ -> decode_frame(Bin0)
    end,
    fin.

payload_test() ->
    Pay0 = sample0(),
    Pay1 = sample1(),
    Pay2 = join_request_sample(),
    Pay3 = join_accept_sample(),
    decode_payload(Pay0),
    decode_payload(Pay1),
    decode_payload(Pay2),
    decode_payload(Pay3),
    fin.

%%-endif.