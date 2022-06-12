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
    encode_fupopts/1,
    %% internal functions
    payload_join_request/1,
    payload_join_accept/1,
    payload_fctrl/1,
    payload_fhdr/1,
    fopts_mac_cid/1,
    parse_fopts/1,
    parse_fdownopts/1,
    payload_ftype/1,
    payload_macpayload/1,
    payload_fctrl_bits/1,
    payload_mic/1,
    payload_dirbit/1,
    payload_fcnt/1,
    payload_major/1,
    payload_fopts/1,
    payload_fport/1,
    payload_data/1,
    payload_devaddr/1,
    payload_foptslen/1
]).

-include("lora.hrl").

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
    <<?JOIN_REQUEST:3, _MHDRRFU:3, _Major:2, AppEUI:8/binary, DevEUI:8/binary, DevNonce:2/binary,
        _MIC:4/binary>> = PhyPayload,
    {AppEUI, DevEUI, DevNonce}.

payload_join_accept(PhyPayload) ->
    MacPayload = payload_macpayload(PhyPayload),
    <<JoinNonce:3/binary, NetID:3/binary, DevAddr:4/binary, DLSettings:1/binary, RXDelay:1/binary,
        CFList/binary>> = MacPayload,
    {JoinNonce, NetID, DevAddr, DLSettings, RXDelay, CFList}.

payload_dirbit(PhyPayload) ->
    <<_Ignore:2/integer-unsigned, DirectionBit:1/integer-unsigned, _Ignore2:5/integer, _/binary>> =
        PhyPayload,
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
    <<_MHDR:8/integer, _DevAddr:32/integer, _FCtrl:8/integer-unsigned,
        FCnt:16/little-integer-unsigned, _/binary>> = PhyPayload,
    FCnt.

payload_devaddr(PhyPayload) ->
    <<_MHDR:8, DevAddr:4/binary, _/binary>> = PhyPayload,
    DevAddr.

payload_fctrl(PhyPayload) ->
    <<_MHDR:8/integer, _DevAddr:32/integer, FCtrl:8/little-integer-unsigned, _/binary>> =
        PhyPayload,
    FCtrl.

payload_fctrl_bits(PhyPayload) ->
    <<_MHDR:8/integer, _DevAddr:32/integer, FCtrlBits:4/little-integer-unsigned,
        _Foptslen:4/integer-unsigned, _/binary>> = PhyPayload,
    FCtrlBits.

payload_foptslen(PhyPayload) ->
    <<_MHDR:8/integer, _DevAddr:32/integer, _Ignore:4/integer-unsigned, Foptslen:4/integer-unsigned,
        _/binary>> = PhyPayload,
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
        0 ->
            <<>>;
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
        devaddr = <<0>>,
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
        devaddr = <<0>>,
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
    TrueFCnt = compute_true_fcnt(Msg, NwkSKey, DirBit, DevAddr, FCnt, PayloadMIC),
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

info_lager_if(Statement, _Format, _Value) ->
    case Statement of
        %% true -> lager:info(Format, Value);
        true -> ok;
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
            MIC = crypto:macN(cmac, aes_128_cbc, AppKey, Msg, 4),
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
                <<PktBody/binary, ReplyMIC/binary>>,
                false
            ),
            <<PktHdr/binary, EncryptedReply/binary>>
    end.

-spec data_frame_to_message(#frame{}, binary(), binary()) -> {binary(), binary()}.
data_frame_to_message(Frame, NwkSKey, _AppSKey) ->
    FOpts = Frame#frame.fopts,
    % io:format("frame_to_payload FOpts = ~w~n", [FOpts]),
    FOptsLen = erlang:byte_size(FOpts),
    % io:format("frame_to_payload FOptsLen = ~w~n", [FOptsLen]),
    <<_Ignore:2, DirectionBit:1, 0:5>> = <<(Frame#frame.mtype):3, 0:5>>,
    case FOptsLen of
        0 ->
            PktHdr =
                <<
                    (Frame#frame.mtype):3,
                    (Frame#frame.rfu):3,
                    (Frame#frame.major):2,
                    (Frame#frame.devaddr)/binary,
                    (Frame#frame.fctrlbits):4,
                    0:4,
                    (Frame#frame.fcnt):16/integer-unsigned-little
                >>;
        _ ->
            PktHdr =
                <<
                    (Frame#frame.mtype):3,
                    (Frame#frame.rfu):3,
                    (Frame#frame.major):2,
                    (Frame#frame.devaddr)/binary,
                    (Frame#frame.fctrlbits):4,
                    FOptsLen:4,
                    (Frame#frame.fcnt):16/integer-unsigned-little,
                    FOpts:FOptsLen/binary
                >>
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
                    (lora_utils:reverse(
                        lora_utils:cipher(
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
    <<CID:8/integer-unsigned, _rest/binary>> = FOpts,
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
parse_fopts(_Unknown) ->
    %% lager:warning("Unknown command ~p", [lora_utils:binary_to_hex(Unknown)]),
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
parse_fdownopts(
    <<16#09, _RFU:2, DownlinkDwellTime:1, UplinkDwellTime:1, MaxEIRP:4, Rest/binary>>
) ->
    [{tx_param_setup_req, DownlinkDwellTime, UplinkDwellTime, MaxEIRP} | parse_fdownopts(Rest)];
parse_fdownopts(
    <<16#0A, ChIndex:8, Freq:24/little-unsigned-integer, MaxDr:4, MinDr:4, Rest/binary>>
) ->
    [{dl_channel_req, ChIndex, Freq, MaxDr, MinDr} | parse_fdownopts(Rest)];
parse_fdownopts(
    <<16#0D, A:32/little-unsigned-integer, B:8/little-unsigned-integer, Rest/binary>>
) ->
    [{device_time_ans, A, B} | parse_fdownopts(Rest)];
parse_fdownopts(<<>>) ->
    [];
parse_fdownopts(_Unknown) ->
    %% lager:warning("Unknown downlink command ~p", [lora_utils:binary_to_hex(Unknown)]),
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

%% end of file
