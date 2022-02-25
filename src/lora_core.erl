%%%-------------------------------------------------------------------
%% @doc
%% == LoRaWAN ==
%% @end
%%%-------------------------------------------------------------------
-module(lora_core).

-export([
    %% public functions
    payload_mhdr/1,
    payload_to_frame/3,
    frame_to_payload/3,
    encode_fopts/1,
    encode_fupopts/1
    %% internal functions
]).

-include("lorawan.hrl").

%%
%% Public
%%

-spec payload_mhdr(binary()) -> binary().
payload_mhdr(PhyPayload) ->
    <<MHDR:8/integer-unsigned, _/binary>> = PhyPayload,
    <<MHDR>>.

-spec payload_to_frame(binary(), binary(), binary()) -> #frame{}.
payload_to_frame(PhyPayload, NwkSKey, AppSKey) ->
    MType = payload_ftype(PhyPayload),
    case MType of
        ?JOIN_REQUEST ->
            payload_to_join_req_frame(PhyPayload, NwkSKey, AppSKey);
        ?JOIN_ACCEPT ->
            payload_to_join_resp_frame(PhyPayload, NwkSKey, AppSKey);
        _ ->
            payload_to_data_frame(PhyPayload, NwkSKey, AppSKey)
    end.

-spec frame_to_payload(#frame{}, binary(), binary()) -> binary().
frame_to_payload(Frame, NwkSKey, AppSKey) ->
    <<MType:2, _DirectionBit:1, 0:5>> = <<(Frame#frame.mtype):3, 0:5>>,
    case MType of
        0 -> join_frame_to_payload(Frame, NwkSKey, AppSKey);
        _ -> data_frame_to_payload(Frame, NwkSKey, AppSKey)
    end.

%%
%% Internal
%%

payload_join_request(PhyPayload) ->
    <<?JOIN_REQUEST:3, _MHDRRFU:3, _Major:2, AppEUI:8/binary, DevEUI:8/binary, DevNonce:2/binary, _MIC:4/binary>> = PhyPayload,
    {AppEUI, DevEUI, DevNonce}.

payload_join_accept(PhyPayload) ->
    MacPayload = payload_macpayload(PhyPayload),
    <<JoinNonce:3/binary, NetID:3/binary, DevAddr:4/binary, DLSettings:1/binary, RXDelay:1/binary, CFList/binary>> = MacPayload,
    {JoinNonce, NetID, DevAddr, DLSettings, RXDelay, CFList}.

payload_dirbit(PhyPayload) ->
    <<_Ignore:2/integer-unsigned, DirectionBit:1/integer-unsigned, _Ignore2:5/integer, _/binary>> = PhyPayload,
    DirectionBit.

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

payload_rfu(PhyPayload) ->
    MHDR = payload_mhdr(PhyPayload),
    <<_FType:3, RFU:3, _Major:2/integer-unsigned>> = MHDR,
    RFU.

payload_major(PhyPayload) ->
    MHDR = payload_mhdr(PhyPayload),
    <<_FType:3, _RFU:3, Major:2/integer-unsigned>> = MHDR,
    Major.

payload_fcnt(PhyPayload) ->
    <<_MHDR:8/integer, _DevAddr:32/integer, _FCtrl:8/integer-unsigned, FCnt:16/little-integer-unsigned, _/binary>> = PhyPayload,
    FCnt.

payload_devaddr(PhyPayload) ->
    <<_MHDR:8, DevAddr:4/binary, _/binary>> = PhyPayload,
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
    PayloadLen = byte_size(MacPayload) - FhdrLen,
    case PayloadLen of
        0 -> <<>>;
        _ ->
            <<_Ignore:FhdrLen/binary, _Ignore2:1/binary, FrmPayload/binary>> = MacPayload,
            FrmPayload
    end.

payload_to_join_req_frame(PhyPayload, _NwkSKey, _AppSKey) ->
    MType = payload_ftype(PhyPayload),
    RFU = payload_rfu(PhyPayload),
    Major = payload_major(PhyPayload),
    MacPayload = payload_macpayload(PhyPayload),
    Frame = #frame{
        mtype = MType,
        rfu = RFU,
        major = Major,
        devaddr = 0,
        fctrlbits = 0,
        fcnt = 0,
        fopts = <<>>,
        fport = 0,
        data = MacPayload
    },
    Frame.

payload_to_join_resp_frame(PhyPayload, _NwkSKey, AppKey) ->
    % io:format("payload_to_join_resp_frame~n"),
    MType = payload_ftype(PhyPayload),
    RFU = payload_rfu(PhyPayload),
    Major = payload_major(PhyPayload),
    <<_PktHdr:8, PktBody/binary>> = PhyPayload,
    % io:format("payload_to_join_resp_frame PktBody=~w~n", [PktBody]),
    DecryptedReply = crypto:crypto_one_time(aes_128_ecb, AppKey, <<0:128>>, PktBody, true),
    % io:format("payload_to_join_resp_frame DecryptedReply=~w~n", [DecryptedReply]),
    Frame = #frame{
        mtype = MType,
        rfu = RFU,
        major = Major,
        devaddr = 0,
        fctrlbits = 0,
        fcnt = 0,
        fopts = <<>>,
        fport = 0,
        data = DecryptedReply
    },
    Frame.

payload_to_data_frame(PhyPayload, NwkSKey, AppSKey) ->
    MType = payload_ftype(PhyPayload),
    RFU = payload_rfu(PhyPayload),
    Major = payload_major(PhyPayload),
    DevAddr = payload_devaddr(PhyPayload),
    FCtrlBits = payload_fctrl_bits(PhyPayload),
    FCnt = payload_fcnt(PhyPayload),
    FOpts = payload_fopts(PhyPayload),
    FPort = payload_fport(PhyPayload),
    Data = payload_data(PhyPayload),
    PayloadMIC = payload_mic(PhyPayload),
    DirBit = payload_dirbit(PhyPayload),
    Frame0 = #frame{
        mtype = MType,
        rfu = RFU,
        major = Major,
        devaddr = DevAddr,
        fctrlbits = FCtrlBits,
        fcnt = FCnt,
        fopts = FOpts,
        fport = FPort,
        data = Data
    },
    {Msg, _MIC} = data_frame_to_message(Frame0, NwkSKey, AppSKey),
    TrueFCnt = compute_true_fcnt(Msg, NwkSKey, DirBit, DevAddr, FCnt, PayloadMIC ),
    info_lager_if(TrueFCnt =/= FCnt, "mic=~w true_fcnt=~w~n", [PayloadMIC, TrueFCnt]),
    Frame1 = #frame{
        mtype = MType,
        rfu = RFU,
        major = Major,
        devaddr = DevAddr,
        fctrlbits = FCtrlBits,
        fcnt = TrueFCnt,
        fopts = FOpts,
        fport = FPort,
        data = Data
    },
    Frame1.

info_lager_if(Statement, Format, Value) ->
    case Statement of
        true -> lager:info(Format, Value);
        false -> ok
    end.

compute_true_fcnt(_Msg, _NwkSKey, _DirBit, _DevAddr, FCnt, _PayloadMIC) when FCnt > 16#90000000 ->
    FCnt band 16#FFFF;
compute_true_fcnt(Msg, NwkSKey, DirBit, DevAddr, FCnt, PayloadMIC) ->
    MsgSize = byte_size(Msg),
    B0Value = b0(DirBit, DevAddr, FCnt, MsgSize),
    B0 = <<B0Value/binary, Msg/binary>>,
    MIC = crypto:macN(
        cmac,
        aes_128_cbc,
        NwkSKey,
        B0,
        4
    ),
    case MIC of
        PayloadMIC ->
            FCnt;
        _ ->
            NewFCnt = FCnt + 16#10000,
            compute_true_fcnt(Msg, NwkSKey, DirBit, DevAddr, NewFCnt, PayloadMIC)
    end.

-spec join_frame_to_payload(#frame{}, binary(), binary()) -> binary().
join_frame_to_payload(Frame, _NwkSKey, AppKey) ->
    PktHdr = <<(Frame#frame.mtype):3, (Frame#frame.rfu):3, (Frame#frame.major):2>>,
    % io:format("frame_to_payload PktHdr = ~w~n", [PktHdr]),
    PktBody = <<(Frame#frame.data)/binary>>,
    % io:format("frame_to_payload PktBody = ~w~n", [PktBody]),
    Msg = <<PktHdr/binary, PktBody/binary>>,
    % io:format("frame_to_payload Msg = ~w~n", [Msg]),
    case Frame#frame.mtype of
        ?JOIN_REQUEST ->
            MIC = crypto:macN( cmac, aes_128_cbc, AppKey, Msg, 4 ),
            <<Msg/binary, MIC/binary>>;
        ?JOIN_ACCEPT ->
            ReplyMIC = crypto:macN(cmac, aes_128_cbc, AppKey, <<PktHdr/binary, PktBody/binary>>, 4),
            % EncryptedReply = crypto:block_decrypt(
            %     aes_ecb,
            %     AppKey,
            %     %% lora_utils:padded(16, <<PktBody/binary, ReplyMIC/binary>>)
            %     <<PktBody/binary, ReplyMIC/binary>>
            % ),
            EncryptedReply = crypto:crypto_one_time(
                aes_128_ecb,
                AppKey,
                <<0:128>>,
                 <<PktBody/binary, ReplyMIC/binary>>, false),
            <<PktHdr/binary, EncryptedReply/binary>>
    end.

-spec data_frame_to_message(#frame{}, binary(), binary()) -> binary().
data_frame_to_message(Frame, NwkSKey, _AppSKey) ->
    FOpts = Frame#frame.fopts,
    % io:format("frame_to_payload FOpts = ~w~n", [FOpts]),
    FOptsLen = erlang:byte_size(FOpts),
    % io:format("frame_to_payload FOptsLen = ~w~n", [FOptsLen]),
    <<_Ignore:2, DirectionBit:1, 0:5>> = <<(Frame#frame.mtype):3, 0:5>>,
    case FOptsLen of
        0 ->
            PktHdr =
                <<(Frame#frame.mtype):3, (Frame#frame.rfu):3, (Frame#frame.major):2,
                    (Frame#frame.devaddr)/binary,
                    (Frame#frame.fctrlbits):4, 0:4,
                    (Frame#frame.fcnt):16/integer-unsigned-little>>;
        _ ->
            PktHdr =
                <<(Frame#frame.mtype):3, (Frame#frame.rfu):3, (Frame#frame.major):2,
                    (Frame#frame.devaddr)/binary,
                    (Frame#frame.fctrlbits):4, FOptsLen:4,
                    (Frame#frame.fcnt):16/integer-unsigned-little, FOpts:FOptsLen/binary>>
    end,
    % io:format("frame_to_payload PktHdr = ~w~n", [PktHdr]),
    PktBody =
        case Frame#frame.data of
            <<>> ->
                %% no payload
                <<>>;
            <<Payload/binary>> when Frame#frame.fport == 0 ->
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
            <<_Payload/binary>> ->
                % io:format("frame_to_payload Payload = ~w~n", [Payload]),
                % EncPayload = lora_utils:reverse(
                %     lora_utils:cipher(Payload, AppSKey, 1, Frame#frame.devaddr, Frame#frame.fcnt)
                % ),
                % <<(Frame#frame.fport):8/integer-unsigned, EncPayload/binary>>
                <<(Frame#frame.fport):8/integer-unsigned, (Frame#frame.data)/binary>>
        end,
    Msg = <<PktHdr/binary, PktBody/binary>>,
    MsgSize = byte_size(Msg),
    % io:format("frame_to_payload MsgSize = ~w~n", [MsgSize]),
    B0Value = b0(DirectionBit, Frame#frame.devaddr, Frame#frame.fcnt, MsgSize),
    % io:format("frame_to_payload B0Value = ~w~n", [B0Value]),
    B0 = <<B0Value/binary, Msg/binary>>,
    % io:format("frame_to_payload B0 = ~w~n", [B0]),
    MIC = crypto:macN(
        cmac,
        aes_128_cbc,
        NwkSKey,
        B0,
        4
    ),
    {Msg, MIC}.

-spec data_frame_to_payload(#frame{}, binary(), binary()) -> binary().
data_frame_to_payload(Frame, NwkSKey, AppSKey) ->
    {Msg, MIC} = data_frame_to_message(Frame, NwkSKey, AppSKey),
    <<Msg/binary, MIC/binary>>.

% encrypt_payload(Payload, FPort, Dir, DevAddr, FCnt, NwkSKey) ->
%     <<0:8/integer-unsigned,
%         (lorawan_utils:reverse(
%             lora_utils:cipher(
%                 Payload,
%                 NwkSKey,
%                 Dir,
%                 DevAddr,
%                 FCnt
%             )
%         ))/binary>>.

-spec b0(integer(), binary(), integer(), integer()) -> binary().
b0(DirBit, DevAddr, FCnt, MsgLen) ->
    % io:format("b0 Dir = ~w~n", [Dir]),
    % io:format("b0 DevAddr = ~w~n", [DevAddr]),
    % io:format("b0 FCnt = ~w~n", [FCnt]),
    % io:format("b0 Len = ~w~n", [Len]),
    <<16#49, 0, 0, 0, 0, DirBit, DevAddr:4/binary, FCnt:32/little-unsigned-integer, 0, MsgLen>>.

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

%% https://lorawan-packet-decoder-0ta6puiniaut.runkit.sh/?
sample_00() ->
    {<<"QHcQASaAFAABvRjrSjJcz6vXC2TMw1A=">>, <<1:128>>, <<2:128>>}.
sample_01() ->
    {<<"YAQAAEiqLgADUwAAcANTAP8ADY5nmA==">>, <<1:128>>, <<2:128>>}.
%% https://github.com/anthonykirby/lora-packet/issues/35
sample_02() ->
    {<<"YGcXASaKCwADQAIAcQM1AP8BbePzEg==">>, <<"4BBA414130E0A0C87FE0A7EAA257E9BD">>, <<"7679A920DC79C0DECC693E34E670B11F">>}.
%% https://github.com/JiapengLi/lorawan-parser
sample_03() ->
    {<<"4011111101A05904020FA09D7C61F3FAB7">>, <<"2B7E151628AED2A6ABF7158809CF4F3C">>, <<"2B7E151628AED2A6ABF7158809CF4F3C">>}.
sample_downlink() ->
    {<<"60A5280126000200011D8B658839">>,<<"15641BC99EBBD238E5D9D83D3D5313C5">>,<<"B184F94678DD69F3C83C2525CD3938B3">>}.
sample_uplink() ->
    {<<"QK4TBCaAAAABb4ldmIEHFOMmgpU=">>,<<"99D58493D1205B43EFF938F0F66C339E">>,<<"0A501524F8EA5FCBF9BDB5AD7D126F75">>}.
sample_uplink_2() ->
    {<<"40531E012680664601457090ED25">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>,<<"F1B0B1D3CC529C55C3019A46EF4582EA">>}.
sample_join_request_01() ->
    {<<"ANwAANB+1bNwHm/t9XzurwDIhgMK8sk=">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>,<<"B6B53F4A168A7A88BDF7EA135CE9CFCA">>}.
sample_join_request_02() ->
    %% {<<1, 1, 1, 2, 2, 2, 4, 3, 2, 1, 103, 9>>,<<1:128>>,<<"B6B53F4A168A7A88BDF7EA135CE9CFCA">>}.
    {<<"AQEBAgICBAMCAWcJ">>, <<"7A47F143D7CEF033DFA0D4B75E04A316">>,<<"B6B53F4A168A7A88BDF7EA135CE9CFCA">>}.
% sample_join_request_01_2() ->
%     {<<"20fd5ef68da6f52e331b53d546fdb2ad3c9801c93fc961e78e7e3c23af31422392">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>,<<"B6B53F4A168A7A88BDF7EA135CE9CFCA">>}.
join_accept_sample() ->
    {<<"IIE/R/UI/6JnC24j4B+EueJdnEEV8C7qCz3T4gs+ypLa">>, <<1:128>>, <<2:128>>}.
join_accept_sample_2() ->
    {<<"204dd85ae608b87fc4889970b7d2042c9e72959b0057aed6094b16003df12de145">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>,<<"B6B53F4A168A7A88BDF7EA135CE9CFCA">>}.

%% Downlink 735 is response to Uplink 729 and so on ...
bw_uplink_730() ->
    {<<"gAgIAEiA2gII4zMzRo60VYTaMtzWM1+OqOgXImp2Yn8y0ZSiyeUd0PQU">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_731() ->
    {<<"gAgIAEiE2wIDBwMHCI+oAgzOOB9eORJJ2bnc9ExBcN15jaB9WusAGRUf9sO80Q==">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_732() ->
    {<<"gAgIAEiE3AIDBwMHCE8l9SmHWFuDojFbgK/TIyK+xIHOApOu82dFapAm6mW8iw==">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_733() ->
    {<<"gAgIAEiE3QIDBwMHCFpr43k/wLqrdanhaLVwPKcaGf9fXZ1Pe3biYey9bGLC1w==">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_734() ->
    {<<"gAgIAEiE3gIDBwMHCAdYAX23OyS5uN11qprPs2K7rdlMgzBeiLGHdA2oPAiNvA==">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_735() ->
    {<<"gAgIAEiA3wIIKIbvBQVZchE30o5uGhGpo2bjh1sBU8Gpvr4GSoivH6xu">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_736() ->
    {<<"gAgIAEiA4AIIlbTywENIY+CQWL/cxgkE5vcUkRbdwvo4kERWoIRJliXD">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_737() ->
    {<<"gAgIAEiA4QIIPQVuiboLI7rYRmCyMZP7Cc40EW0lQKF2AEfSRsrXdsct">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.

bw_downlink_735() ->
    {<<"YAgIAEig3wJMnXTA">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_736() ->
    {<<"YAgIAEiq4AIDUwAAcANTAP8AUznkEw==">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_737() ->
    {<<"YAgIAEiq4QIDVgAAcANWAP8AH8sRww==">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_738() ->
    {<<"YAgIAEiq4gIDWQAAcANZAP8A6W+BXA==">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_739() ->
    {<<"YAgIAEiq4wIDWgAAcANaAP8ADAj3ZA==">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_740() ->
    {<<"YAgIAEig5ALNptk3">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_741() ->
    {<<"YAgIAEig5QLTvXZe">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_742() ->
    {<<"YAgIAEig5gJuAqE+">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.

bw_uplink_33() ->
    {<<"gAkIAEiAIQAIBgj52JuVE//FrfGGAmfZsEwYv1+p4tMeM1wMzh6Yw2bo">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_34() ->
    {<<"gAkIAEiAIgAITgDyAIvtGo/HMM1pedVoQ5ggrKZyAZKkswbVzzagbmPZ">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_35() ->
    {<<"gAkIAEiEIwADBwMHCHsvuYuPkx12oA4OcxesC/E4XI58lltj5NSSv2uwEgpnUw==">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_36() ->
    {<<"gAkIAEiEJAADBwMHCNj9XsLmGyQm4/RUiRnJgScChkn2QtFR7SuTavV7sLyqYg==">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_37() ->
    {<<"gAkIAEiEJQADBwMHCJS8Svd7K8TE/YHNjwdHkTHpUf8bSOC9naYOC85HQFEakw==">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_38() ->
    {<<"gAkIAEiAJgAI7sh5/1cdTG1wYlR+GqIf7gIpkF+R2X9Uvx/YctUfslkG">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_39() ->
    {<<"gAkIAEiAJwAI+/LDyTKbsXVqIVwgjfCBM/0xuGP2v0czYIf31MYWl3lc">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_40() ->
    {<<"gAkIAEiAKAAIaZPDi0SPhi47Hj55xC9SuMiRJzu4vaxZR+1topHPBtFO">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_41() ->
    {<<"gAkIAEiAKQAIiOJk8V/gGmPehKWqH96pJULIGKhOxeTKgRlP9mPu2n41">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.

bw_downlink_36() ->
    {<<"YAkIAEigJACIniGz">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_37() ->
    {<<"YAkIAEiqJQADUwAAcANTAP8AZZy0rg==">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_38() ->
    {<<"YAkIAEiqJgADVwAAcANXAP8A/XbMjg==">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_39() ->
    {<<"YAkIAEiqJwADWgAAcANaAP8ASHIRjQ==">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_40() ->
    {<<"YAkIAEigKAC8OJYo">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_41() ->
    {<<"YAkIAEigKQDGgxVm">>,<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,<<"7A47F143D7CEF033DFA0D4B75E04A316">>}.


is_hex_string(HexBin0) ->
    try
        HexBin1 = string:uppercase(HexBin0),
        Bin1 = lora_utils:hex_to_binary(HexBin1),
        HexBin2 = lora_utils:binary_to_hex(Bin1),
        %% OTP 24 (use new API when available)
        % Bin1 = binary:decode_hex(HexBin0),
        % HexBin1 = binary:encode_hex(Bin1),
        case HexBin1 of
            HexBin2 -> true;
            _ -> false
        end
    catch
        _Exception:_Reason ->
            % io:format("is_hex_string Exception=~w Reason=~w~n", [Exception, Reason]),
            false
    end.

string_to_binary(String) ->
    UpperCase = string:uppercase(String),
    case is_hex_string(UpperCase) of
        true ->
            lora_utils:hex_to_binary(UpperCase);
        false ->
            base64:decode(String)
    end.

% bin_to_hex2(Bin) ->
%     [begin if N < 10 -> 48 + N; true -> 87 + N end end || <<N:4>> <= Bin].

bin_to_hex(Bin) ->
    [io_lib:format("~2.16.0B",[X]) || <<X:8>> <= Bin].

decode_message_type(Payload) ->
    io:format("~n( MHDR = Ftype[7:5] | RFU[4:2] | Major[1:0] )~n"),
    FType = payload_ftype(Payload),
    io:format("FType = ~w~n", [FType]),
    MType = lora_utils:mtype(FType),
    io:format("Message Type = ~s~n", [MType]),
    Direction = payload_dirbit(Payload),
    io:format("Direction = ~s~n", [lora_utils:dir_string(Direction)]),
    Major = payload_major(Payload),
    io:format("Major = ~w~n", [Major]),
    fin.

decode_macpayload(Payload) ->
    Bin0 = Payload,
    io:format("~n( PHYPayload = MHDR[1] | MACPayload[..] | MIC[4] )~n"),
    MHDR = payload_mhdr(Bin0),
    %% io:format("Binary ~8.16.0B~n", [MHDR]),
    io:format("MHDR = ~s~n", [bin_to_hex(MHDR)]),
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
    io:format("Encrypted FRMPayload = ~s~n", [bin_to_hex(FrmPayload)]),

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

    Direction = payload_dirbit(Payload),
    case Direction of
        0 ->
            ParsedFOpts = parse_fopts(FOpts),
            io:format("ParsedFOpts = ~w~n", [ParsedFOpts]);
        1 ->
            ParsedFOptsDown = parse_fdownopts(FOpts),
            io:format("ParsedFOptsDown = ~w~n", [ParsedFOptsDown])
    end,

    FType = payload_ftype(Bin0),
    io:format("~nMessage Type = ~w~n", [FType]),
    Direction = payload_dirbit(Bin0),
    io:format("Direction = ~s~n", [lora_utils:dir_string(Direction)]),
    FCnt2 = payload_fcnt(Bin0),
    io:format("FCnt = ~w~n", [FCnt2]),
    fin.    

decode_payload(String) ->
    case is_hex_string(String) of
        true ->
            io:format("~nAssuming hex-encoded packet~n");
        false ->
            io:format("~nAssuming base64-encoded packet~n")
    end,
    io:format("~s~n", [String]),
    Bin0 = string_to_binary(String),
    io:format("Binary packet = ~w~n", [Bin0]),

    decode_macpayload(Bin0),
    decode_message_type(Bin0),

    MType = payload_ftype(Bin0),
    case MType of
        ?JOIN_REQUEST -> decode_join_request(Bin0);
        ?JOIN_ACCEPT -> decode_join_accept(Bin0);
        _ -> decode_frame(Bin0)
    end,
    fin.

payload_util_test() ->
    ValidHex00 = is_hex_string(<<"a0">>),
    ?assertEqual(true, ValidHex00),
    {Pay0,_Key0,_AppKey} = sample_downlink(),
    ValidHex0 = is_hex_string(Pay0),
    ?assertEqual(true, ValidHex0),
    {Sample0,_,_} = sample_00(),
    ValidHex1 = is_hex_string(Sample0),
    ?assertEqual(false, ValidHex1),
    Bin0 = string_to_binary(Pay0),
    io:format("bin0 = ~w~n", [Bin0]),
    Bin1 = string_to_binary(Sample0),
    io:format("bin1 = ~w~n", [Bin1]),
    {Pay2,_Key2,_AppKey2} = join_accept_sample_2(),
    ValidHex2 = is_hex_string(Pay2),
    ?assertEqual(true, ValidHex2),
    Bin2 = string_to_binary(Pay2),
    io:format("bin2 = ~w~n", [Bin2]),
    fin.

payload_decode_test() ->
    {Pay0,_Key0,_AppKey} = sample_downlink(),
    decode_payload(Pay0),
    fin.

decode_encode(Sample) ->
    {Pay0,Key0,AppKey0} = Sample(),
    % decode_payload(Pay0),
    Bin0 = string_to_binary(Pay0),
    % io:format("Key0 = ~w~n", [Key0]),
    % io:format("Key0Size = ~w~n", [byte_size(Key0)]),
    NwkSKey0 = string_to_binary(Key0),
    AppSKey0 = string_to_binary(AppKey0),
    % io:format("NwkSKey0 = ~w~n", [NwkSKey0]),
    % io:format("NwkSKey0Size = ~w~n", [byte_size(NwkSKey0)]),
    ?assertEqual(16, byte_size(NwkSKey0)),
    ?assertEqual(16, byte_size(AppSKey0)),
    Frame0 = payload_to_frame(Bin0, NwkSKey0, AppSKey0),
    % io:format("frame = ~w~n", [Frame0]),
    Bin1 = frame_to_payload(Frame0, NwkSKey0, AppSKey0),
    case Bin0 =/= Bin1 of
        true ->
            lager:warning("Bin0 = ~w~n", [Bin0]),
            lager:warning("Bin1 = ~w~n", [Bin1]),
            Base64 = base64:encode(Bin1),
            lager:info("Base64 = ~s~n", [Base64]),
            decode_payload(Pay0);
        false ->
            ok
    end,
    ?assert(Bin0 =:= Bin1), 
    fin.

encode_decode(Frame0) ->
    Key0 = <<1:128>>,
    AppKey = <<2:128>>,
    Bin0 = frame_to_payload(Frame0, Key0, AppKey),
    Frame0 = payload_to_frame(Bin0, Key0, AppKey),
    Bin1 = frame_to_payload(Frame0, Key0, AppKey),
    case Bin0 =/= Bin1 of
        true ->
            lager:warn("bin0 = ~w~n", [Bin0]),
            lager:warn("bin1 = ~w~n", [Bin1]);
        false ->
            ok
    end,
    ?assert(Bin0 =:= Bin1),
    fin.

payload_all_test() ->
    payload_util_test(),
    payload_decode_test(),
    decode_encode(fun sample_downlink/0),
    decode_encode(fun sample_uplink/0),
    decode_encode(fun sample_uplink_2/0),
    decode_encode(fun sample_join_request_01/0),
    decode_encode(fun join_accept_sample_2/0),
    decode_encode(fun sample_02/0),
    decode_encode(fun sample_03/0),
    exercise_test(),
    fin.

payload_00_test() ->
    decode_encode(fun sample_downlink/0),
    fin.

payload_01_test() ->
    decode_encode(fun sample_uplink/0),
    fin.

payload_02_test() ->
    decode_encode(fun sample_uplink_2/0),
    fin.

payload_03_test() ->
    decode_encode(fun sample_join_request_01/0),
    fin.

payload_04_test() ->
    decode_encode(fun join_accept_sample_2/0),
    fin.

payload_05_test() ->
    decode_encode(fun sample_02/0),
    fin.

payload_06_test() ->
    decode_encode(fun sample_03/0),
    fin.

payload_07_test() ->
    decode_encode(fun sample_join_request_02/0),
    fin.

payload_08_test() ->
    decode_encode(fun bw_uplink_730/0),
    decode_encode(fun bw_uplink_731/0),
    decode_encode(fun bw_uplink_732/0),
    decode_encode(fun bw_uplink_733/0),
    decode_encode(fun bw_uplink_734/0),
    decode_encode(fun bw_uplink_735/0),
    decode_encode(fun bw_uplink_736/0),
    decode_encode(fun bw_uplink_737/0),
    fin.

payload_09_test() ->
    decode_encode(fun bw_downlink_735/0),
    decode_encode(fun bw_downlink_736/0),
    decode_encode(fun bw_downlink_737/0),
    decode_encode(fun bw_downlink_738/0),
    decode_encode(fun bw_downlink_739/0),
    decode_encode(fun bw_downlink_740/0),
    decode_encode(fun bw_downlink_741/0),
    decode_encode(fun bw_downlink_742/0),
    fin.

payload_10_test() ->
    decode_encode(fun bw_uplink_33/0),
    decode_encode(fun bw_uplink_34/0),
    decode_encode(fun bw_uplink_35/0),
    decode_encode(fun bw_uplink_36/0),
    decode_encode(fun bw_uplink_37/0),
    decode_encode(fun bw_uplink_38/0),
    decode_encode(fun bw_uplink_39/0),
    decode_encode(fun bw_uplink_40/0),
    decode_encode(fun bw_uplink_41/0),
    fin.

payload_11_test() ->
    decode_encode(fun bw_downlink_36/0),
    decode_encode(fun bw_downlink_37/0),
    decode_encode(fun bw_downlink_38/0),
    decode_encode(fun bw_downlink_39/0),
    decode_encode(fun bw_downlink_40/0),
    decode_encode(fun bw_downlink_41/0),
    fin.

exercise_test() ->
    Frame0 = #frame{
        mtype = ?CONFIRMED_UP,
        rfu = 0,
        major = 0,
        devaddr = <<1,2,3,4>>,
        fctrlbits = 0,
        fcnt = 1,
        fopts = <<>>,
        fport = 1,
        data = <<1,2,3,4>>
    },
    encode_decode(Frame0),
    fin.

payload_1_test() ->
    {Pay0,_,_} = sample_00(),
    {Pay1,_,_} = sample_01(),
    {Pay2,_,_} = sample_join_request_01(),
    {Pay3,_,_} = join_accept_sample(),
    decode_payload(Pay0),
    decode_payload(Pay1),
    decode_payload(Pay2),
    decode_payload(Pay3),
    fin.

%%-endif.