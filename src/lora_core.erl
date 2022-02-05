%%%-------------------------------------------------------------------
%% @doc
%% == LoRaWAN ==
%% @end
%%%-------------------------------------------------------------------
-module(lora_core).

-export([
    %% public functions
    payload_mhdr/1,
    encode_fopts/1,
    encode_fupopts/1,
    frame_to_payload/3,
    payload_to_frame/3
    %% internal functions
]).

-include("lorawan.hrl").

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
    <<_MHDR:8, DevAddr:4/binary, _/binary>> = PhyPayload,
    io:format("payload_devaddr devaddr = ~w~n", [DevAddr]),
    DevAddr.

payload_fctrl(PhyPayload) ->
    <<_MHDR:8/integer, _DevAddr:32/integer, FCtrl:8/little-integer-unsigned, _/binary>> = PhyPayload,
    FCtrl.

payload_fctrl_bits(PhyPayload) ->
    <<_MHDR:8/integer, _DevAddr:32/integer, FCtrlBits:4/little-integer-unsigned, _Foptslen:4/integer-unsigned, _/binary>> = PhyPayload,
    FCtrlBits.

payload_foptslen(PhyPayload) ->
    <<_MHDR:8/integer, _DevAddr:32/integer, _Ignore:4/integer-unsigned, Foptslen:4/integer-unsigned, _/binary>> = PhyPayload,
    Foptslen.

payload_fopts(PhyPayload) ->
    Len = payload_foptslen(PhyPayload),
    %% Offset == MHDR + DevAddr + FCtrl + FCnt
    Offset = 1 + 4 + 1 + 2,
    Part = {Offset, Len},
    FOpts = binary:part(PhyPayload, Part),
    FOpts.

payload_fhdr_len(PhyPayload) ->
    FOptsLen = payload_foptslen(PhyPayload),
    7 + FOptsLen.

payload_fhdr(PhyPayload) ->
    FhdrLen = payload_fhdr_len(PhyPayload),
    Part = {1, FhdrLen},
    binary:part(PhyPayload, Part).

payload_fport(PhyPayload) ->
    FHdrLen = payload_fhdr_len(PhyPayload),
    FPortStart = FHdrLen + 1,
    Part = {FPortStart, 1},
    <<FPort:8/integer-unsigned-little>> = binary:part(PhyPayload, Part),
    FPort.

payload_data(PhyPayload) ->
    MacPayload = payload_macpayload(PhyPayload),
    FhdrLen = payload_fhdr_len(PhyPayload),
    <<_Ignore:FhdrLen/binary, _Ignore2:1/binary, FrmPayload/binary>> = MacPayload,
    FrmPayload.

payload_to_frame(PhyPayload, _NwkSKey, _AppSKey) ->
    MType = payload_ftype(PhyPayload),
    DevAddr = payload_devaddr(PhyPayload),
    FCtrlBits = payload_fctrl_bits(PhyPayload),
    FCnt = payload_fcnt(PhyPayload),
    FOpts = payload_fopts(PhyPayload),
    FPort = payload_fport(PhyPayload),
    Data = payload_data(PhyPayload),
    Frame = #frame{
        mtype = MType,
        devaddr = DevAddr,
        fctrlbits = FCtrlBits,
        fcnt = FCnt,
        fopts = parse_fopts(FOpts),
        fport = FPort,
        data = Data
    },
    Frame.

-spec frame_to_payload(#frame{}, binary(), binary()) -> binary().
frame_to_payload(Frame, NwkSKey, AppSKey) ->
    FOpts = encode_fopts(Frame#frame.fopts),
    io:format("frame_to_payload FOpts = ~w~n", [FOpts]),
    FOptsLen = 0, %% erlang:byte_size(FOpts),
    io:format("frame_to_payload FOptsLen = ~w~n", [FOptsLen]),
    case FOptsLen of
        0 ->
            PktHdr =
                <<(Frame#frame.mtype):3, 0:3, 0:2, (Frame#frame.devaddr)/binary,
                    (Frame#frame.fctrlbits):4, 0:4,
                    (Frame#frame.fcnt):16/integer-unsigned-little>>;
        _ ->
            PktHdr =
                <<(Frame#frame.mtype):3, 0:3, 0:2, (Frame#frame.devaddr)/binary,
                    (Frame#frame.fctrlbits):4, FOptsLen:4,
                    (Frame#frame.fcnt):16/integer-unsigned-little, FOpts:FOptsLen/binary>>
    end,
    PktBody =
        case Frame#frame.data of
            <<>> ->
                %% no payload
                <<>>;
            <<Payload/binary>> when Frame#frame.fport == 0 ->
                lager:debug("port 0 outbound"),
                %% port 0 payload, encrypt with network key
                <<0:8/integer-unsigned,
                    (lorawan_utils:reverse(
                        lorawan_utils:cipher(
                            Payload,
                            NwkSKey,
                            1,
                            Frame#frame.devaddr,
                            Frame#frame.fcnt
                        )
                    ))/binary>>;
            <<Payload/binary>> ->
                lager:debug("port ~p outbound", [Frame#frame.fport]),
                % EncPayload = lora_utils:reverse(
                %     lora_utils:cipher(Payload, AppSKey, 1, Frame#frame.devaddr, Frame#frame.fcnt)
                % ),
                % <<(Frame#frame.fport):8/integer-unsigned, EncPayload/binary>>
                <<(Frame#frame.fport):8/integer-unsigned, (Frame#frame.data)/binary>>
        end,
    Msg = <<PktHdr/binary, PktBody/binary>>,
    MsgSize = byte_size(Msg),
    io:format("frame_to_payload MsgSize = ~w~n", [MsgSize]),
    B0Value = b0(1, Frame#frame.devaddr, Frame#frame.fcnt, MsgSize),
    B0 = <<B0Value/binary, Msg/binary>>,
    MIC = crypto:macN(
        cmac,
        aes_128_cbc,
        NwkSKey,
        B0,
        4
    ),
    <<Msg/binary, MIC/binary>>.

-spec b0(integer(), binary(), integer(), integer()) -> binary().
b0(Dir, DevAddr, FCnt, Len) ->
    io:format("b0 Dir = ~w~n", [Dir]),
    io:format("b0 DevAddr = ~w~n", [DevAddr]),
    io:format("b0 FCnt = ~w~n", [FCnt]),
    io:format("b0 Len = ~w~n", [Len]),
    <<16#49, 0, 0, 0, 0, Dir, DevAddr:4/binary, FCnt:32/little-unsigned-integer, 0, Len>>.
    %% <<16#49, 0, 0, 0, 0, 1, <<1,2,3,4>>:4/binary, 0:32/little-unsigned-integer, 0, 0>>.


fopts_mac_cid(<<>>) ->
    0;
fopts_mac_cid(FOpts) ->
    <<CID:8/integer-unsigned,  _rest/binary>> = FOpts,
    CID.

parse_fopts(<<16#02, Rest/binary>>) ->
    [link_check_req | parse_fopts(Rest)];
parse_fopts(<<16#03, _RFU:5, PowerACK:1, DataRateACK:1, ChannelMaskACK:1, Rest/binary>>) ->
    [{link_adr_ans, PowerACK, DataRateACK, ChannelMaskACK} | parse_fopts(Rest)];
parse_fopts(<<16#04, Rest/binary>>) ->
    [duty_cycle_ans | parse_fopts(Rest)];
parse_fopts(<<16#05, _RFU:5, RX1DROffsetACK:1, RX2DataRateACK:1, ChannelACK:1, Rest/binary>>) ->
    [{rx_param_setup_ans, RX1DROffsetACK, RX2DataRateACK, ChannelACK} | parse_fopts(Rest)];
parse_fopts(<<16#06, Battery:8, _RFU:2, Margin:6/signed, Rest/binary>>) ->
    [{dev_status_ans, Battery, Margin} | parse_fopts(Rest)];
parse_fopts(<<16#07, _RFU:6, DataRateRangeOK:1, ChannelFreqOK:1, Rest/binary>>) ->
    [{new_channel_ans, DataRateRangeOK, ChannelFreqOK} | parse_fopts(Rest)];
parse_fopts(<<16#08, Rest/binary>>) ->
    [rx_timing_setup_ans | parse_fopts(Rest)];
parse_fopts(<<16#09, Rest/binary>>) ->
    [tx_param_setup_ans | parse_fopts(Rest)];
parse_fopts(<<16#0A, _RFU:6, UplinkFreqExists:1, ChannelFreqOK:1, Rest/binary>>) ->
    [{di_channel_ans, UplinkFreqExists, ChannelFreqOK} | parse_fopts(Rest)];
parse_fopts(<<16#0D, Rest/binary>>) ->
    [device_time_req | parse_fopts(Rest)];
parse_fopts(<<>>) ->
    [];
parse_fopts(Unknown) ->
    lager:warning("Unknown command ~p", [lora_utils:binary_to_hex(Unknown)]),
    [].

parse_fdownopts(
    <<16#03, DataRate:4, TXPower:4, ChMask:16/little-unsigned-integer, 0:1, ChMaskCntl:3, NbTrans:4,
        Rest/binary>>
) ->
    [{link_adr_req, DataRate, TXPower, ChMask, ChMaskCntl, NbTrans} | parse_fdownopts(Rest)];
parse_fdownopts(<<16#02, Margin, GwCnt, Rest/binary>>) ->
    [{link_check_ans, Margin, GwCnt} | parse_fdownopts(Rest)];
parse_fdownopts(<<16#04, _RFU:4, MaxDCycle:4, Rest/binary>>) ->
    [{duty_cycle_req, MaxDCycle} | parse_fdownopts(Rest)];
parse_fdownopts(
    <<16#05, _RFU:1, RX1DRoffset:3, RX2DataRate:4, Freq:24/little-unsigned-integer, Rest/binary>>
) ->
    [{rx_param_setup_req, RX1DRoffset, RX2DataRate, Freq} | parse_fdownopts(Rest)];
parse_fdownopts(<<16#06, Rest/binary>>) ->
    [dev_status_req | parse_fdownopts(Rest)];
parse_fdownopts(
    <<16#07, ChIndex:8, Freq:24/little-unsigned-integer, MaxDr:4, MinDr:4, Rest/binary>>
) ->
    [{new_channel_req, ChIndex, Freq, MaxDr, MinDr} | parse_fdownopts(Rest)];
parse_fdownopts(<<16#08, _RFU:4, Delay:4, Rest/binary>>) ->
    [{rx_timing_setup_req, Delay} | parse_fdownopts(Rest)];
parse_fdownopts(<<16#09, _RFU:2, DownlinkDwellTime:1, UplinkDwellTime:1, MaxEIRP:4, Rest/binary>>) ->
    [{tx_param_setup_req, DownlinkDwellTime, UplinkDwellTime, MaxEIRP} | parse_fdownopts(Rest)];
parse_fdownopts(
    <<16#0A, ChIndex:8, Freq:24/little-unsigned-integer, MaxDr:4, MinDr:4, Rest/binary>>
) ->
    [{dl_channel_req, ChIndex, Freq, MaxDr, MinDr} | parse_fdownopts(Rest)];
parse_fdownopts(<<16#0D, A:32/little-unsigned-integer, B:8/little-unsigned-integer, Rest/binary>>) ->
    [{device_time_ans, A, B} | parse_fdownopts(Rest)];
parse_fdownopts(<<>>) ->
    [];
parse_fdownopts(Unknown) ->
    lager:warning("Unknown downlink command ~p", [lora_utils:binary_to_hex(Unknown)]),
    [].

encode_fopts([{link_check_ans, Margin, GwCnt} | Rest]) ->
    <<16#02, Margin, GwCnt, (encode_fopts(Rest))/binary>>;
encode_fopts([{link_adr_req, DataRate, TXPower, ChMask, ChMaskCntl, NbRep} | Rest]) ->
    <<16#03, DataRate:4, TXPower:4, ChMask:16/little-unsigned-integer, 0:1, ChMaskCntl:3, NbRep:4,
        (encode_fopts(Rest))/binary>>;
encode_fopts([{duty_cycle_req, MaxDCycle} | Rest]) ->
    <<16#04, 0:4, MaxDCycle:4, (encode_fopts(Rest))/binary>>;
encode_fopts([{rx_param_setup_req, RX1DROffset, RX2DataRate, Frequency} | Rest]) ->
    <<16#05, 0:1, RX1DROffset:3, RX2DataRate:4, Frequency:24/little-unsigned-integer,
        (encode_fopts(Rest))/binary>>;
encode_fopts([dev_status_req | Rest]) ->
    <<16#06, (encode_fopts(Rest))/binary>>;
encode_fopts([{new_channel_req, ChIndex, Freq, MaxDR, MinDR} | Rest]) ->
    <<16#07, ChIndex, Freq:24/little-unsigned-integer, MaxDR:4, MinDR:4,
        (encode_fopts(Rest))/binary>>;
encode_fopts([{rx_timing_setup_req, Delay} | Rest]) ->
    <<16#08, 0:4, Delay:4, (encode_fopts(Rest))/binary>>;
encode_fopts([{tx_param_setup_req, DownDwell, UplinkDwell, MaxEIRP} | Rest]) ->
    <<16#09, 0:2, DownDwell:1, UplinkDwell:1, MaxEIRP:4, (encode_fopts(Rest))/binary>>;
encode_fopts([{di_channel_req, ChIndex, Freq} | Rest]) ->
    <<16#0A, ChIndex, Freq:24/little-unsigned-integer, (encode_fopts(Rest))/binary>>;
encode_fopts([{device_time_ans, MsSinceEpoch} | Rest]) ->
    % 0.5^8
    Ms = trunc((MsSinceEpoch rem 1000) / 3.90625),
    <<16#0D, (MsSinceEpoch div 1000):32/little-unsigned-integer, Ms, (encode_fopts(Rest))/binary>>;
encode_fopts([]) ->
    <<>>.

encode_fupopts([link_check_req | Rest]) ->
    <<16#02, (encode_fupopts(Rest))/binary>>;
encode_fupopts([{link_adr_ans, PowerACK, DataRateACK, ChannelMaskACK} | Rest]) ->
    <<16#03, 0:5, PowerACK:1, DataRateACK:1, ChannelMaskACK:1, (encode_fupopts(Rest))/binary>>;
encode_fupopts([duty_cycle_ans | Rest]) ->
    <<16#04, (encode_fupopts(Rest))/binary>>;
encode_fupopts([{rx_param_setup_ans, RX1DROffsetACK, RX2DataRateACK, ChannelACK} | Rest]) ->
    <<16#05, 0:5, RX1DROffsetACK:1, RX2DataRateACK:1, ChannelACK:1, (encode_fupopts(Rest))/binary>>;
encode_fupopts([{dev_status_ans, Battery, Margin} | Rest]) ->
    <<16#06, Battery:8, 0:2, Margin:6, (encode_fupopts(Rest))/binary>>;
encode_fupopts([{new_channel_ans, DataRateRangeOK, ChannelFreqOK} | Rest]) ->
    <<16#07, 0:6, DataRateRangeOK:1, ChannelFreqOK:1, (encode_fupopts(Rest))/binary>>;
encode_fupopts([rx_timing_setup_ans | Rest]) ->
    <<16#08, (encode_fupopts(Rest))/binary>>;
encode_fupopts([tx_param_setup_ans | Rest]) ->
    <<16#09, (encode_fupopts(Rest))/binary>>;
encode_fupopts([{di_channel_ans, UplinkFreqExists, ChannelFreqOK} | Rest]) ->
    <<16#0A, 0:6, UplinkFreqExists:1, ChannelFreqOK:1, (encode_fupopts(Rest))/binary>>;
encode_fupopts([device_time_req | Rest]) ->
    <<16#0D, (encode_fupopts(Rest))/binary>>;
encode_fupopts([_ | Rest]) ->
    <<(encode_fupopts(Rest))/binary>>;
encode_fupopts([]) ->
    <<>>.

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
    MType = lora_utils:mtype(FType),
    io:format("Message Type = ~s~n", [MType]),
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
    FPort = payload_fport(Bin0),
    FrmPayload = payload_data(Bin0),
    io:format("FHDR = ~w~n", [FHDR]),
    io:format("FHDR = ~s~n", [bin_to_hex(FHDR)]),
    io:format("FPort = ~w~n", [FPort]),
    io:format("FRMPayload = ~w~n", [FrmPayload]),

    io:format("~n( FHDR = DevAddr[4] | FCtrl[1] | FCnt[2] | FOpts[0..15] )~n"),
    DevAddr = payload_devaddr(Bin0),
    % io:format("DevAddr = ~8.16.0B~n", [DevAddr]),
    io:format("DevAddr = ~w~n", [DevAddr]),
    FCtrl = payload_fctrl(Bin0),
    io:format("FCtrl = ~w~n", [FCtrl]),
    FCtrlBits = payload_fctrl_bits(Bin0),
    io:format("FCtrlBits = ~w~n", [FCtrlBits]),
    FCnt = payload_fcnt(Bin0),
    io:format("FCnt = ~w~n", [FCnt]),
    FOptsLen = payload_foptslen(Bin0),
    io:format("FOptsLen = ~w~n", [FOptsLen]),
    FOpts = payload_fopts(Bin0),
    io:format("FOpts = ~w~n", [FOpts]),
    io:format("FOpts = ~s~n", [bin_to_hex(FOpts)]),

    CID = fopts_mac_cid(FOpts),
    io:format("CID = ~w~n", [CID]),

    Direction = payload_direction(Payload),
    case Direction of
        <<"up">> ->
            ParsedFOpts = parse_fopts(FOpts),
            io:format("ParsedFOpts = ~w~n", [ParsedFOpts]);
        <<"down">> ->
            ParsedFOptsDown = parse_fdownopts(FOpts),
            io:format("ParsedFOptsDown = ~w~n", [ParsedFOptsDown])
    end,

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

payload_0_test() ->
    Pay0 = sample0(),
    decode_payload(Pay0),
    Bin0 = base64_to_binary(Pay0),
    io:format("bin0 = ~w~n", [Bin0]),
    Frame0 = payload_to_frame(Bin0, <<1:128>>, <<2:128>>),
    io:format("frame = ~w~n", [Frame0]),
    Bin1 = frame_to_payload(Frame0, <<1:128>>, <<2:128>>),
    io:format("bin0 = ~w~n", [Bin0]),
    io:format("bin1 = ~w~n", [Bin1]),
    fin.

payload_1_test() ->
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