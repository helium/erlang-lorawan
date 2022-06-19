-module(lora_core_tests).
%% ==================================================================
%% EUNIT Tests
%% ==================================================================
-ifdef(EUNIT).

-include("lora.hrl").
-include_lib("eunit/include/eunit.hrl").

payload_util_test() ->
    ValidHex00 = is_hex_string(<<"a0">>),
    ?assertEqual(true, ValidHex00),
    {Pay0, _Key0, _AppKey} = sample_downlink(),
    ValidHex0 = is_hex_string(Pay0),
    ?assertEqual(true, ValidHex0),
    {Sample0, _, _} = sample_00(),
    ValidHex1 = is_hex_string(Sample0),
    ?assertEqual(false, ValidHex1),
    Bin0 = string_to_binary(Pay0),
    io:format("bin0 = ~w~n", [Bin0]),
    Bin1 = string_to_binary(Sample0),
    io:format("bin1 = ~w~n", [Bin1]),
    {Pay2, _Key2, _AppKey2} = join_accept_sample_2(),
    ValidHex2 = is_hex_string(Pay2),
    ?assertEqual(true, ValidHex2),
    Bin2 = string_to_binary(Pay2),
    io:format("bin2 = ~w~n", [Bin2]),
    fin.

payload_decode_test() ->
    {Pay0, _Key0, _AppKey} = sample_downlink(),
    decode_payload(Pay0),
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

% payload_07_test() ->
%     decode_encode(fun sample_join_request_02/0),
%     fin.

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

payload_12_test() ->
    process_payload_sequence(fun bw_uplink_sequence/0),
    fin.

payload_13_test() ->
    process_payload_sequence(fun bw_downlink_sequence/0),
    fin.

payload_14_test() ->
    process_payload_sequence(fun issue_633_sequence/0),
    fin.

payload_15_test() ->
    {Pay0, _, _} = sample_00(),
    {Pay1, _, _} = sample_01(),
    {Pay2, _, _} = sample_join_request_01(),
    {Pay3, _, _} = join_accept_sample(),
    decode_payload(Pay0),
    decode_payload(Pay1),
    decode_payload(Pay2),
    decode_payload(Pay3),
    fin.

exercise_test() ->
    Frame0 = #frame{
        mtype = ?CONFIRMED_UP,
        rfu = 0,
        major = 0,
        devaddr = <<1, 2, 3, 4>>,
        fctrlbits = 0,
        fcnt = 1,
        fopts = <<>>,
        fport = 1,
        data = <<1, 2, 3, 4>>
    },
    encode_decode(Frame0),
    fin.

%% https://lorawan-packet-decoder-0ta6puiniaut.runkit.sh/?
sample_00() ->
    {<<"QHcQASaAFAABvRjrSjJcz6vXC2TMw1A=">>, <<1:128>>, <<2:128>>}.
sample_01() ->
    {<<"YAQAAEiqLgADUwAAcANTAP8ADY5nmA==">>, <<1:128>>, <<2:128>>}.
%% https://github.com/anthonykirby/lora-packet/issues/35
sample_02() ->
    {<<"YGcXASaKCwADQAIAcQM1AP8BbePzEg==">>, <<"4BBA414130E0A0C87FE0A7EAA257E9BD">>,
        <<"7679A920DC79C0DECC693E34E670B11F">>}.
%% https://github.com/JiapengLi/lorawan-parser
sample_03() ->
    {<<"4011111101A05904020FA09D7C61F3FAB7">>, <<"2B7E151628AED2A6ABF7158809CF4F3C">>,
        <<"2B7E151628AED2A6ABF7158809CF4F3C">>}.
sample_downlink() ->
    {<<"60A5280126000200011D8B658839">>, <<"15641BC99EBBD238E5D9D83D3D5313C5">>,
        <<"B184F94678DD69F3C83C2525CD3938B3">>}.
sample_uplink() ->
    {<<"QK4TBCaAAAABb4ldmIEHFOMmgpU=">>, <<"99D58493D1205B43EFF938F0F66C339E">>,
        <<"0A501524F8EA5FCBF9BDB5AD7D126F75">>}.
sample_uplink_2() ->
    {<<"40531E012680664601457090ED25">>, <<"7A47F143D7CEF033DFA0D4B75E04A316">>,
        <<"F1B0B1D3CC529C55C3019A46EF4582EA">>}.
sample_join_request_01() ->
    {<<"ANwAANB+1bNwHm/t9XzurwDIhgMK8sk=">>, <<"7A47F143D7CEF033DFA0D4B75E04A316">>,
        <<"B6B53F4A168A7A88BDF7EA135CE9CFCA">>}.
% sample_join_request_02() ->
% %% {<<1, 1, 1, 2, 2, 2, 4, 3, 2, 1, 103, 9>>,<<1:128>>,<<"B6B53F4A168A7A88BDF7EA135CE9CFCA">>}.
%     {<<"AQEBAgICBAMCAWcJ">>, <<"7A47F143D7CEF033DFA0D4B75E04A316">>,
%         <<"B6B53F4A168A7A88BDF7EA135CE9CFCA">>}.
% sample_join_request_01_2() ->
%     {<<"20fd5ef68da6f52e331b53d546fdb2ad3c9801c93fc961e78e7e3c23af31422392">>,
%      <<"7A47F143D7CEF033DFA0D4B75E04A316">>,<<"B6B53F4A168A7A88BDF7EA135CE9CFCA">>}.
join_accept_sample() ->
    {<<"IIE/R/UI/6JnC24j4B+EueJdnEEV8C7qCz3T4gs+ypLa">>, <<1:128>>, <<2:128>>}.
join_accept_sample_2() ->
    {<<"204dd85ae608b87fc4889970b7d2042c9e72959b0057aed6094b16003df12de145">>,
        <<"7A47F143D7CEF033DFA0D4B75E04A316">>, <<"B6B53F4A168A7A88BDF7EA135CE9CFCA">>}.

%% Downlink 735 is response to Uplink 729 and so on ...
bw_uplink_730() ->
    {<<"gAgIAEiA2gII4zMzRo60VYTaMtzWM1+OqOgXImp2Yn8y0ZSiyeUd0PQU">>,
        <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>, <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_731() ->
    {<<"gAgIAEiE2wIDBwMHCI+oAgzOOB9eORJJ2bnc9ExBcN15jaB9WusAGRUf9sO80Q==">>,
        <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>, <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_732() ->
    {<<"gAgIAEiE3AIDBwMHCE8l9SmHWFuDojFbgK/TIyK+xIHOApOu82dFapAm6mW8iw==">>,
        <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>, <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_733() ->
    {<<"gAgIAEiE3QIDBwMHCFpr43k/wLqrdanhaLVwPKcaGf9fXZ1Pe3biYey9bGLC1w==">>,
        <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>, <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_734() ->
    {<<"gAgIAEiE3gIDBwMHCAdYAX23OyS5uN11qprPs2K7rdlMgzBeiLGHdA2oPAiNvA==">>,
        <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>, <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_735() ->
    {<<"gAgIAEiA3wIIKIbvBQVZchE30o5uGhGpo2bjh1sBU8Gpvr4GSoivH6xu">>,
        <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>, <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_736() ->
    {<<"gAgIAEiA4AIIlbTywENIY+CQWL/cxgkE5vcUkRbdwvo4kERWoIRJliXD">>,
        <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>, <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_737() ->
    {<<"gAgIAEiA4QIIPQVuiboLI7rYRmCyMZP7Cc40EW0lQKF2AEfSRsrXdsct">>,
        <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>, <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.

bw_downlink_735() ->
    {<<"YAgIAEig3wJMnXTA">>, <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,
        <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_736() ->
    {<<"YAgIAEiq4AIDUwAAcANTAP8AUznkEw==">>, <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,
        <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_737() ->
    {<<"YAgIAEiq4QIDVgAAcANWAP8AH8sRww==">>, <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,
        <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_738() ->
    {<<"YAgIAEiq4gIDWQAAcANZAP8A6W+BXA==">>, <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,
        <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_739() ->
    {<<"YAgIAEiq4wIDWgAAcANaAP8ADAj3ZA==">>, <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,
        <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_740() ->
    {<<"YAgIAEig5ALNptk3">>, <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,
        <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_741() ->
    {<<"YAgIAEig5QLTvXZe">>, <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,
        <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_742() ->
    {<<"YAgIAEig5gJuAqE+">>, <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,
        <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.

bw_uplink_33() ->
    {<<"gAkIAEiAIQAIBgj52JuVE//FrfGGAmfZsEwYv1+p4tMeM1wMzh6Yw2bo">>,
        <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>, <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_34() ->
    {<<"gAkIAEiAIgAITgDyAIvtGo/HMM1pedVoQ5ggrKZyAZKkswbVzzagbmPZ">>,
        <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>, <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_35() ->
    {<<"gAkIAEiEIwADBwMHCHsvuYuPkx12oA4OcxesC/E4XI58lltj5NSSv2uwEgpnUw==">>,
        <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>, <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_36() ->
    {<<"gAkIAEiEJAADBwMHCNj9XsLmGyQm4/RUiRnJgScChkn2QtFR7SuTavV7sLyqYg==">>,
        <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>, <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_37() ->
    {<<"gAkIAEiEJQADBwMHCJS8Svd7K8TE/YHNjwdHkTHpUf8bSOC9naYOC85HQFEakw==">>,
        <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>, <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_38() ->
    {<<"gAkIAEiAJgAI7sh5/1cdTG1wYlR+GqIf7gIpkF+R2X9Uvx/YctUfslkG">>,
        <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>, <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_39() ->
    {<<"gAkIAEiAJwAI+/LDyTKbsXVqIVwgjfCBM/0xuGP2v0czYIf31MYWl3lc">>,
        <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>, <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_40() ->
    {<<"gAkIAEiAKAAIaZPDi0SPhi47Hj55xC9SuMiRJzu4vaxZR+1topHPBtFO">>,
        <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>, <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_uplink_41() ->
    {<<"gAkIAEiAKQAIiOJk8V/gGmPehKWqH96pJULIGKhOxeTKgRlP9mPu2n41">>,
        <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>, <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.

bw_downlink_36() ->
    {<<"YAkIAEigJACIniGz">>, <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,
        <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_37() ->
    {<<"YAkIAEiqJQADUwAAcANTAP8AZZy0rg==">>, <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,
        <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_38() ->
    {<<"YAkIAEiqJgADVwAAcANXAP8A/XbMjg==">>, <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,
        <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_39() ->
    {<<"YAkIAEiqJwADWgAAcANaAP8ASHIRjQ==">>, <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,
        <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_40() ->
    {<<"YAkIAEigKAC8OJYo">>, <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,
        <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.
bw_downlink_41() ->
    {<<"YAkIAEigKQDGgxVm">>, <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,
        <<"7A47F143D7CEF033DFA0D4B75E04A316">>}.

bw_uplink_sequence() ->
    [
        <<"AFbpuZpW+YFgIgBIADMla0fZRDW+IoI=">>,
        <<"AFbpuZpW+YFgIgBIADMla0fZRDW+IoI=">>,
        <<"gAkIAEiAAAAIs1UE7c7e1tQaEWNVgfswFMVV9PafWOLRGsccnHV+0dVs">>,
        <<"gAkIAEiAAQAI3kzFL/7/fIua67xykNa8uAjTmaOkUDK+AT3YQMOl6ib2">>,
        <<"gAkIAEiAAgAIrl46HUwnhVSrLQPQOUminwm3I2ZvVY2ktCzt/mS7/9Jr">>,
        <<"gAkIAEiAAwAIXukAD9lCJRlJQne1IcelB/9mFmHSA6mLjPrL2Op4KLnU">>,
        <<"gAkIAEiABAAI/0Rvfh3bY05w/eWmnyPNT9UPNuLu3hF3H9ERESky56/v">>,
        <<"gAkIAEiABQAI9KBjNOA+3XsVW6AyHflDLrdVBWYE2EO+POxLNfdRV0TW">>,
        <<"gAkIAEiABgAIv2SlApJCPhpE1xnWM7ssoN9rwbkH0WQZLzO8TuPa2DaE">>,
        <<"gAkIAEiABgAIv2SlApJCPhpE1xnWM7ssoN9rwbkH0WQZLzO8TuPa2DaE">>,
        <<"gAkIAEiABwAIJEgfbbZoGOeNO6b7xMZYfU3ZR2yT2cr2GITSilI8AB1J">>,
        <<"gAkIAEiABwAIJEgfbbZoGOeNO6b7xMZYfU3ZR2yT2cr2GITSilI8AB1J">>,
        <<"gAkIAEiACAAI68hALIVh0SlFFDDRrx5c1Y2Oods3a8jSlMfaojkkeHJs">>,
        <<"gAkIAEiACQAI+n4mMwp6ve2uTXFO52rGn8iq4ByyUOGrWz63xPYtqixZ">>,
        <<"gAkIAEiACgAIYjELRIAT2fubRw9eLzdjDv0yb8FNYg1SOpvfwt6OYQPo">>,
        <<"gAkIAEiACwAIqAQjqAd0NZ6J5VgONIbDZf8PUo+MCpNrO05q7D9Ij/+B">>,
        <<"gAkIAEiADAAIqmwdTlaPOL30xLwBhp1F4sgHa/to+4cfGVFcqy7JUaHQ">>,
        <<"gAkIAEiADQAIfrCpNTBjAeJM4wwjdaTwVh6mYFCbf/nym6KmEBKjMUtK">>,
        <<"gAkIAEiADgAIhq6mX0CeYNGFRuKvQlbC/8UoNhfdMEr8wL8STemZIjnC">>,
        <<"gAkIAEiADwAIb+cvHQ6G5Cn/9tmWq5dsldcT48xg406CMZqaZ7/ki8ST">>,
        <<"gAkIAEiADwAIb+cvHQ6G5Cn/9tmWq5dsldcT48xg406CMZqaZ7/ki8ST">>,
        <<"gAkIAEiAEAAIEX63DZX+wt1UMEwI2qqjj9Q0xG5juj+86omYldPuABKa">>,
        <<"gAkIAEiAEQAI9TO4rTAfSJ2urXm6XjMMRPSPm3lI2D5drRpgyn5ff3F2">>,
        <<"gAkIAEiAEgAIThpGeaMvUbbbYMTl69YXHzFV68n3SiH8uh8mzz8TLqW4">>,
        <<"gAkIAEiAEwAIZirAsuez1N+lmENv3aSoBmcC0m92rD4Z6jDrk49TP3fQ">>,
        <<"gAkIAEiAFAAIk5LbLg4ra3EM+igVXexvL756UQO1e28EyKTS92qsbIly">>,
        <<"gAkIAEiAFQAIwq6wO7oiQrL9zPrv1dYhPwG9UgQ2/3nuz+461u2oiiwe">>,
        <<"gAkIAEiAFgAI152dP6c1OGzo7imr6+8ffISvSIlKEbyn9Qb8s8WmVUjP">>,
        <<"gAkIAEiAFwAIMwca62h9TyefHxq6SL9G19n247G82D6Qhrp70IxDsz89">>,
        <<"gAkIAEiAGAAIdq+i1Jis+2AWm4eUYuXzxBEk/TUOQrQpf4obP5iT2Ftc">>,
        <<"gAkIAEiAGQAIxdcxoILCtdEs/208leWmeUDLlvMCcoqn1ND9MjkbwQ3h">>,
        <<"gAkIAEiAGgAIFpfw/Epklf7r/kCZfTsGXOeXJFdPyMp2i+27fM2CDQ3Z">>,
        <<"gAkIAEiAGwAIm+SjqsBAzqjoXdi9ATwPeCxyyNmIlRSl0lHKn1nv/Xol">>,
        <<"gAkIAEiAHAAIaRWhC8o3mvPVTQSGcQIfSm+nrJl97yM9MJWOqJKlPpXb">>,
        <<"gAkIAEiAHQAIlc/CAnYJ3AoBTd07VRny0ajGhyqTpWvlzBJhFwxsQYtq">>,
        <<"gAkIAEiAHgAIdqi08ZyKNn09vu6ej2orJXmOkcOiLOgMJuOfwCFaEzl4">>,
        <<"gAkIAEiAHwAIjr0jfITDsXedlWEJ+Y0ZF+7tkBGPupgHIU07b3let7rf">>,
        <<"gAkIAEiAIAAID8l+uJQzfdALgUc7s36PwP2YNbcOgoQq/wF4f4uiTGjv">>,
        <<"gAkIAEiAIQAIBgj52JuVE//FrfGGAmfZsEwYv1+p4tMeM1wMzh6xGSTf">>,
        <<"gAkIAEiAIgAITgDyAIvtGo/HMM1pedVoQ5ggrKZyAZKkswbVzzaYsyJb">>,
        <<"gAkIAEiEIwADBwMHCHsvuYuPkx12oA4OcxesC/E4XI58lltj5NSSv2uwyOZWEA==">>,
        <<"gAkIAEiEJAADBwMHCNj9XsLmGyQm4/RUiRnJgScChkn2QtFR7SuTavV7E1rzwQ==">>,
        <<"gAkIAEiEJQADBwMHCJS8Svd7K8TE/YHNjwdHkTHpUf8bSOC9naYOC85Hj2TmAg==">>,
        <<"gAkIAEiAJgAI7sh5/1cdTG1wYlR+GqIf7gIpkF+R2X9Uvx/YctXTiHs3">>,
        <<"gAkIAEiAJwAI+/LDyTKbsXVqIVwgjfCBM/0xuGP2v0czYIf31MYQ3Yz1">>,
        <<"gAkIAEiAKAAIaZPDi0SPhi47Hj55xC9SuMiRJzu4vaxZR+1topHB/zmK">>,
        <<"gAkIAEiAKQAIiOJk8V/gGmPehKWqH96pJULIGKhOxeTKgRlP9mP+S7KN">>,
        <<"gAkIAEiAKgAIil4vr4Oo1mzoHBO93xrp8CKqZyiNuRsyYdwwwJeFxftI">>,
        <<"gAkIAEiAKwAIiHT7chVJoghG2Uh3u3km0jaKg7eF82JWJ6jO3xp+FlBE">>,
        <<"gAkIAEiALAAIGcNxyV0lUialkvU4Id1weCGcgH1G5gmU5vvWLHeCuLiN">>,
        <<"gAkIAEiALQAIkTNPMOmCbEa8d5qyJ/kzx/jQvXn3RykGaMGYtcEk3FJi">>,
        <<"gAkIAEiALgAIQbg0o1BtNrBX3JL+SG2kPgE23rQz2bdtFd0yyfH/lHfC">>,
        <<"gAkIAEiALwAIJGS0fsFM6MoS7sa4Zu94CrXvO2BeeBSD4K5+3YtGr1z0">>,
        <<"gAkIAEiAMAAI3yhQWiKcjR69XiRJ7p7axh17uAUbbnpi4/1VtIaEvSbV">>,
        <<"gAkIAEiAMQAIXPHWGqr6Fd20fzeD3dPPBnhj6QberFMJCVzudjewlbos">>,
        <<"gAkIAEiAMgAI0v2oCxLpKrp7/QVNMXBav2OsOeAbbdCdB4FlMJDoqBBr">>,
        <<"gAkIAEiAMwAIbOvNN9litNXDM8b8VEJ5PV1vfh2Hxxz6OUZyw5L2yShT">>,
        <<"gAkIAEiANAAIdN3smyneQCS+cwVaVaJShdmPwk+PPX5GM56MNDnW4j6U">>,
        <<"gAkIAEiANQAI/0wyVV+IinEix6KBajELnVpYjXhHkaX6QVQt3CsMpFvQ">>,
        <<"gAkIAEiANgAIQJgXPzz66/2O1B2bOZmY3iU6JMuwoyY5Lbliut9BScsF">>,
        <<"gAkIAEiANwAIAdpNN8Uub+fMsPlPnFD39Rj+E8QQMnEIFCpaf5XlTKQr">>,
        <<"gAkIAEiAOAAIdHhieLTFOYrtZ0UBVx5Wzq475Av8gx8ioal08ozaekc4">>,
        <<"gAkIAEiAOQAI+sHyBm6w6TAX1nUEZaYPmccHZ26bJ7blbVm5+yjpNxsM">>,
        <<"gAkIAEiAOgAIYo92NJnZhJ/f7pl9yxgan3staurHtOWYi99ihpxzq9RI">>,
        <<"gAkIAEiAOwAIZWutyYUHsW/i6+uUT07rfjWWzhHZU6IbtNcR6gnFj/uK">>,
        <<"gAkIAEiAPAAImBOS5JEz3EHJUVXgSv2PdBVxAPyL1KMrLs2FcBA0P8u3">>,
        <<"gAkIAEiAPQAIHkbq7HmHcEqmF2gF0ZGgWijCr8p/r/iAk9CV5vTT3HL5">>,
        <<"gAkIAEiAPgAIEwATO/g6icbmxsL4Z3at6ce3tmB9XE+1oXgMHPyXBWVO">>,
        <<"gAkIAEiAPwAIHmWnbYhLRbMwWqB4CblI4ktcp4dpMeh4dOHYO5Op5BcV">>,
        <<"gAkIAEiAQAAIiuNbFyqhGQpK4DfbOmyTYxtK9CzYVbFsz6+eAW4XJ9/g">>,
        <<"gAkIAEiAQQAIEOUXCNRSfbUd0k8TYNNrEWNsXYNLKJzIeUz4/OTor/0N">>,
        <<"gAkIAEiEQgADBwMHCOD5Ichi7+pLDgnlyMslsmSgUnn699RuPsbWdAnRCfFg0w==">>,
        <<"gAkIAEiEQgADBwMHCOD5Ichi7+pLDgnlyMslsmSgUnn699RuPsbWdAnRCfFg0w==">>,
        <<"gAkIAEiAQwAIbPoXQsAAIsykfAgG1WzGE/H73iaOzI+6INkCaAsnvJ6N">>,
        <<"gAkIAEiERAADBwMHCJqX5Np3dZw4XOBPfjjIfmEzMvV2Ko+0jzScRVITxBM/1g==">>,
        <<"gAkIAEiERQADBwMHCLstYaetyqhb4LNRT94PoWymXyHIQgxILEULIp0UtdwawQ==">>,
        <<"gAkIAEiARgAImLQRyMxwQUBDRJMX38YiPrJZvhkBfpXObooB72DDdIz8">>,
        <<"gAkIAEiARwAIQiE/JfKotYdeq3ZuFN3cAmBRmWneiAu06pqfk9gnR2NB">>,
        <<"gAkIAEiASAAI9bph8awQt51kzzaawYsKNqDcz+EKxwR7UMu4lzOM8WsO">>,
        <<"gAkIAEiASQAIN0mQdKk8umQCQHvy37pnclbz1/mWcZh4gSMkVI3gfoDx">>,
        <<"gAkIAEiASgAInlQ+XCi5fAScjxME4OIZmDJ9dMbS/juyiF1UxRA0axzC">>,
        <<"gAkIAEiASwAIcWLr97S80PR69D98C3T+pe6HGyljfJ6qiOLWGV19Y+5x">>,
        <<"gAkIAEiATAAInRs4werODMjYUowQXNNNskaHGojxdmFeFri1aXl4TIqR">>,
        <<"gAkIAEiATQAIq2L+7jj8DC/1IeOkHs6AuHeFjwz0mKPF0sQWWE/VvsLC">>,
        <<"gAkIAEiATgAIh8Mr0sC9n7Q3JfjX+OjNAjIggOjNGWvDi90jZ5rQl9mK">>,
        <<"gAkIAEiATwAINjD9U/w1Osv0KW4VF8SJqwlmtiQUgX1R/c68LJsQ1q9N">>,
        <<"gAkIAEiAUAAIJSz6WKp2gfaxqx3/lDVo/olZl/tHYGvRvJvpPJ0PR4ug">>,
        <<"gAkIAEiAUQAIJl0ItJGamt9xiOA2yvDpJn5MiplI9tjvSEE/9kM/owuQ">>,
        <<"gAkIAEiAUQAIJl0ItJGamt9xiOA2yvDpJn5MiplI9tjvSEE/9kM/owuQ">>,
        <<"gAkIAEiAUgAIiY38NCFUUW/SY4dkICm+RIgSP8Lx83d6mbHgdHD+/sd6">>,
        <<"gAkIAEiAUwAI3BCZ3EgD1S6qo918NGr/jG0BQHeSOLKVNSUgXJA2NYVZ">>,
        <<"gAkIAEiAVAAIvr5r66xWd9x9Yj/0Ua2fW3QgxeECZ7iqXQyzJ6BFl96e">>,
        <<"gAkIAEiAVQAI1nN5C1lUBVTWcQPOZHhs7dvc2RjiFaLNRQcOgAeliSOH">>,
        <<"gAkIAEiAVgAI9tyFqm4L+1zDxtTzDsbEK7/84MCi/9S68ecYq7Nb86v7">>,
        <<"gAkIAEiAVwAIaOwWAHi8kXI1mU7HMBvEmx6kua0pjOTuBUyDqDfxC7jQ">>,
        <<"gAkIAEiAWAAIdruSxW9Q8kZh9IWi4eV+jFA4kPLq/uNkwrFZJREnIEq8">>,
        <<"gAkIAEiAWQAIcKkKbSrBvoMu5oiYcHIdGcroi0pf3pJCoaiNLtmj9sWM">>,
        <<"gAkIAEiAWgAInSmDJGtfyTnOSMLBMiY4Hd4LdnKQhl21NraeX9Qs4mru">>,
        <<"gAkIAEiAWwAIGk6E6u2VNepv/9CJyx9G1ZJnnp9ykMJkYUaFQMCRqujg">>,
        <<"gAkIAEiAXAAIaxLHCGDj5qKcI7j6Y06GVrOQVqb7De7iMZDJxxyPugiY">>,
        <<"gAkIAEiAXQAIhRfDEgK9+B+pLxAszf5tGa0F8PE+Y43Kbmo/oNZYT8hb">>,
        <<"gAkIAEiAXgAIUslyz0V8ECZsFX9OrGWhBHcaYWeMUvjJOGu/VKL4NnJK">>,
        <<"gAkIAEiAXwAIY+Aabr3I+PWjPF6fbViFWZitcamXrFE9l/tRNL7bPaFE">>,
        <<"gAkIAEiAYAAI4cQnk4iJcg0nhnvra+jv6aUp+Fo6H30/FG3V/zBG4Wvb">>,
        <<"gAkIAEiAYQAI0Jt+hp6+3HEUjutoOtHgsoddzQU5rDwMo+qjOoQsP18W">>,
        <<"gAkIAEiAYgAIDGp3hcxmIzJhEkKo2sr+9B/E9AlvYyct3z4PHS53HB+J">>,
        <<"gAkIAEiAYwAIjgKwk/27qtEnp61l1RzkZIZBlLd6orGDXAHkf3rLH1T1">>,
        <<"gAkIAEiAZAAInBHszjTe4hNTRHJiMsYZv2Jx9RnExMBNPk8sUtEpYTSV">>,
        <<"gAkIAEiAZQAIbV7gQGzxDGCdorgcvsZnikPRYYon2m5R3tZYAVcOjN4y">>,
        <<"gAkIAEiAZgAI2rxpTMaZQWNopItZyVnWaTJ4yLE4OVAfELl5N061+kVK">>,
        <<"gAkIAEiAZwAIiFGY1xDoedKahJOS2FhDGc0BSWn404pjZgJybs8ty4Ox">>,
        <<"gAkIAEiAaAAI8ox0++Dg8RYzY2LBW6cSnSA+wtBpb6SyZdnChy1xH9ct">>,
        <<"gAkIAEiAaQAIB+0PxWI5o23sWwThf6fniQEvnTmeRm+Eik8tjflcNBzO">>,
        <<"gAkIAEiAagAIAdFYUCI7Wpbpx9Rf3HrVFqTfjIlba0TRbWG1dvK/t+hR">>,
        <<"gAkIAEiAawAIqxQqL2jB2opM3KXBbmhJYEXveOGdJ71wGAgjgJl/uBmG">>,
        <<"gAkIAEiAbAAIqhJmo4YTMPqkyAOPeyuLMn5XAUNiQ36F3bBovSBPDr8W">>,
        <<"gAkIAEiAbQAI3+4AJaxtMLUsR6+rXvdn0bQKUnTmkNIbe4J4WtCFeeh8">>,
        <<"gAkIAEiAbgAIrw9Fvl/dKhl/dxk5lGhZ/gHUbHIvzszt79aVULMr8O9H">>,
        <<"gAkIAEiAbwAIFiGH73wbOsBtpcl56ullInYhrRyrfq5XLv4lFEG+XDUz">>,
        <<"gAkIAEiAcAAI0200AFhkmbNqo4rJ4vIbg9VTGC9BOx6jlrxO+OIu6xhz">>,
        <<"gAkIAEiAcQAInJ67XoYmhD4ainjS4zdG6kIgv+DJTmqxZ1cZBEtHZ3D5">>,
        <<"gAkIAEiAcgAISDwzkwZgxjS/+NwgIkIrLYy/DXYzlTI/uGp9haFyCWWS">>,
        <<"gAkIAEiAcwAIbBa9cjvZpf3cHZIaMxtBERddb8NN5B2t/j3Vti6ka5Mc">>,
        <<"gAkIAEiAdAAID3Dfonn+qf9RAzVby2UdMln/EwAIJKlgEVE0diPM0MMK">>,
        <<"gAkIAEiAdQAI6fUzE5E76FcePLg5DCJPUryHfe4i8zM/r7YCTgW9inMH">>,
        <<"gAkIAEiAdgAIOO+Qwglj+w80vkt+eEut1l4udwjXckt+de/7DR26ehXU">>,
        <<"gAkIAEiAdwAIJz4Z7OK8gqsSEJUDkVbGMT+kwneULsh+TZaF7x//11hq">>,
        <<"gAkIAEiAeAAIRdxtFt38f1tL2io3KlknY6eg7SWObpmQo9aXUlQHZrzM">>
    ].

bw_downlink_sequence() ->
    [
        <<"IFLY23jCIJk6R3lh+TXICIw=">>,
        <<"YAkIAEigAACM2elC">>,
        <<"YAkIAEigAQAiDJcx">>,
        <<"YAkIAEigAgBnFuyv">>,
        <<"YAkIAEigAwBatA/F">>,
        <<"YAkIAEigBAA4Fqbt">>,
        <<"YAkIAEigBQAzyWQa">>,
        <<"YAkIAEigBgCwqsjY">>,
        <<"YAkIAEigBwBk+vJk">>,
        <<"YAkIAEigCABTdwH1">>,
        <<"YAkIAEigCQBuaj3y">>,
        <<"YAkIAEigCgDwMYVV">>,
        <<"YAkIAEigCwA8hSBN">>,
        <<"YAkIAEigDAAgDF3A">>,
        <<"YAkIAEigDQAnhdqh">>,
        <<"YAkIAEigDgDHCem0">>,
        <<"YAkIAEigDwCnm2zG">>,
        <<"YAkIAEigEABAx7vH">>,
        <<"YAkIAEigEQCoAXMd">>,
        <<"YAkIAEigEgB4lOUZ">>,
        <<"YAkIAEigEwCYGhtN">>,
        <<"YAkIAEigFAC2nbLP">>,
        <<"YAkIAEigFQBgrEDb">>,
        <<"YAkIAEigFgAnd0xf">>,
        <<"YAkIAEigFwCar5kr">>,
        <<"YAkIAEigGABTpNR2">>,
        <<"YAkIAEigGQAJ3KPH">>,
        <<"YAkIAEigGgDOQHcB">>,
        <<"YAkIAEigGwBkKvVI">>,
        <<"YAkIAEigHABnYly0">>,
        <<"YAkIAEigHQByFeog">>,
        <<"YAkIAEigHgA5wHju">>,
        <<"YAkIAEigHwB0Ku2U">>,
        <<"YAkIAEigIAAagoph">>,
        <<"YAkIAEigIQBhdLyp">>,
        <<"YAkIAEigIgAe29lR">>,
        <<"YAkIAEigIwAnlPMV">>,
        <<"YAkIAEigJAC1TxFT">>,
        <<"YAkIAEiqJQADUwAAcANTAP8A+CGCBw==">>,
        <<"YAkIAEiqJgADVwAAcANXAP8AEuu8fQ==">>,
        <<"YAkIAEiqJwADWgAAcANaAP8Aqn+qfg==">>,
        <<"YAkIAEigKADeI1zf">>,
        <<"YAkIAEigKQDYB8ZJ">>,
        <<"YAkIAEigKgBeiUhT">>,
        <<"YAkIAEigKwAascpz">>,
        <<"YAkIAEigLAAoxbiI">>,
        <<"YAkIAEigLQAmUbba">>,
        <<"YAkIAEigLgAk/Xq5">>,
        <<"YAkIAEigLwDkU9mq">>,
        <<"YAkIAEigMAC0FMvc">>,
        <<"YAkIAEigMQDr+L5E">>,
        <<"YAkIAEigMgAhSyo8">>,
        <<"YAkIAEigMwA/NDo0">>,
        <<"YAkIAEigNADW9u5T">>,
        <<"YAkIAEigNQCFXVHP">>,
        <<"YAkIAEigNgBHS2Q8">>,
        <<"YAkIAEigNwC+PzkX">>,
        <<"YAkIAEigOABAIbia">>,
        <<"YAkIAEigOQDztrr8">>,
        <<"YAkIAEigOgDola0y">>,
        <<"YAkIAEigOwAb3heh">>,
        <<"YAkIAEigPABjO7n/">>,
        <<"YAkIAEigPQAT62bj">>,
        <<"YAkIAEigPgDzom+i">>,
        <<"YAkIAEigPwAL7F5c">>,
        <<"YAkIAEigQABVXkj/">>,
        <<"YAkIAEigQQDwmkof">>,
        <<"YAkIAEigQgC8EXJV">>,
        <<"YAkIAEigQwBCIaBU">>,
        <<"YAkIAEiqRAADUwAAcANTAP8AxYFs4g==">>,
        <<"YAkIAEiqRQADVgAAcANWAP8A6MTbVQ==">>,
        <<"YAkIAEigRgDmSI7t">>,
        <<"YAkIAEiqRwADWQAAcANZAP8ACLXbkw==">>,
        <<"YAkIAEiqSAADWgAAcANaAP8AGb5eWA==">>,
        <<"YAkIAEigSQCqOwjF">>,
        <<"YAkIAEigSgCDJLYN">>,
        <<"YAkIAEigSwC0pHJJ">>,
        <<"YAkIAEigTACumm33">>,
        <<"YAkIAEigTQBrLyhf">>,
        <<"YAkIAEigTgBRB320">>,
        <<"YAkIAEigTwAW7Jte">>,
        <<"YAkIAEigUABiPu+D">>,
        <<"YAkIAEigUQATV45e">>,
        <<"YAkIAEigUgBZtuyt">>,
        <<"YAkIAEigUwCMKPH5">>,
        <<"YAkIAEigVAAsksOb">>,
        <<"YAkIAEigVQCBcen2">>,
        <<"YAkIAEigVgBq+ZhO">>,
        <<"YAkIAEigVwB5VlVZ">>,
        <<"YAkIAEigWAC0QV+B">>,
        <<"YAkIAEigWQAjcgLo">>,
        <<"YAkIAEigWgDf9F2g">>,
        <<"YAkIAEigWwAkx4jt">>,
        <<"YAkIAEigXADWFGtE">>,
        <<"YAkIAEigXQCGXkpS">>,
        <<"YAkIAEigXgBY1VBe">>,
        <<"YAkIAEigXwB7NOjc">>,
        <<"YAkIAEigYAB0H9aA">>,
        <<"YAkIAEigYQBUaF9f">>,
        <<"YAkIAEigYgAkFsra">>,
        <<"YAkIAEigYwDjDvOY">>,
        <<"YAkIAEigZACq10Rg">>,
        <<"YAkIAEigZQA4zi3r">>,
        <<"YAkIAEigZgCIMZ59">>,
        <<"YAkIAEigZwBPFpWh">>,
        <<"YAkIAEigaADW6tQK">>,
        <<"YAkIAEigaQBwyMSv">>,
        <<"YAkIAEigagC0RWLH">>,
        <<"YAkIAEigawDRgh0W">>,
        <<"YAkIAEigbAAEBrU5">>,
        <<"YAkIAEigbQC2Lfg7">>,
        <<"YAkIAEigbgCjHbB3">>,
        <<"YAkIAEigbwAz8477">>,
        <<"YAkIAEigcAA/sxP2">>,
        <<"YAkIAEigcQCUDoaH">>,
        <<"YAkIAEigcgAqtpB6">>,
        <<"YAkIAEigcwDEqpNb">>,
        <<"YAkIAEigdACD0/Fy">>,
        <<"YAkIAEigdQC8CnIS">>,
        <<"YAkIAEigdgDzO3Bh">>,
        <<"YAkIAEigdwBcqMMH">>,
        <<"YAkIAEigeACVyezv">>,
        <<"YAkIAEigeQAwQ1ER">>,
        <<"YAkIAEigegDHFpY0">>,
        <<"YAkIAEigewBXdJtb">>,
        <<"YAkIAEigfAB3VjjO">>,
        <<"YAkIAEigfQC9X2WC">>
    ].

issue_633_sequence() ->
    [
        <<"AEs0zXoT7b41sE3t3QkPPWXSmkFcRBk=">>,
        <<"INsw17kgY3VVlYeG4zlQ/Ns=">>,
        <<"gIcCAEgAAAB0idjz9KrTjHE=">>,
        <<"YIcCAEgqAAADQAAAcANAAP8Ap8PYsQ==">>,
        <<"gIcCAEgEAQADBwMHt6zpeyoBTWXV">>,
        <<"YIcCAEggAQCLUWuM">>,
        <<"gIcCAEgAAgC68JMmTcVByxg=">>,
        <<"YIcCAEggAgDWQuIo">>
    ].

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
    [io_lib:format("~2.16.0B", [X]) || <<X:8>> <= Bin].

decode_message_type(Payload) ->
    io:format("~n( MHDR = Ftype[7:5] | RFU[4:2] | Major[1:0] )~n"),
    FType = lora_core:payload_ftype(Payload),
    io:format("FType = ~w~n", [FType]),
    MType = lora_utils:mtype(FType),
    io:format("Message Type = ~s~n", [MType]),
    Direction = lora_core:payload_dirbit(Payload),
    io:format("Direction = ~s~n", [lora_utils:dir_string(Direction)]),
    Major = lora_core:payload_major(Payload),
    io:format("Major = ~w~n", [Major]),
    fin.

decode_macpayload(Payload) ->
    Bin0 = Payload,
    io:format("~n( PHYPayload = MHDR[1] | MACPayload[..] | MIC[4] )~n"),
    MHDR = lora_core:payload_mhdr(Bin0),
    %% io:format("Binary ~8.16.0B~n", [MHDR]),
    io:format("MHDR = ~s~n", [bin_to_hex(MHDR)]),
    MacPayload = lora_core:payload_macpayload(Bin0),
    io:format("MacPayload = ~w~n", [MacPayload]),
    io:format("MacPayload = ~s~n", [bin_to_hex(MacPayload)]),
    MIC = lora_core:payload_mic(Bin0),
    io:format("MIC = ~s~n", [bin_to_hex(MIC)]),
    fin.

decode_join_request(Payload) ->
    Bin0 = Payload,

    {AppEUI, DevEUI, DevNonce} = lora_core:payload_join_request(Bin0),
    io:format("AppEUI = ~s~n", [bin_to_hex(AppEUI)]),
    io:format("DevEUI = ~s~n", [bin_to_hex(DevEUI)]),
    io:format("DevNonce = ~s~n", [bin_to_hex(DevNonce)]),
    fin.

decode_join_accept(Payload) ->
    Bin0 = Payload,

    io:format(
        "~n( MACPayload = AppNonce[3] | "
        "NetID[3] | DevAddr[4] | DLSettings[1] | "
        "RxDelay[1] | CFList[0|15] )~n"
    ),
    {JoinNonce, NetID, DevAddr, DLSettings, RXDelay, CFList} = lora_core:payload_join_accept(Bin0),
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
    FHDR = lora_core:payload_fhdr(Bin0),
    FPort = lora_core:payload_fport(Bin0),
    FrmPayload = lora_core:payload_data(Bin0),
    io:format("FHDR = ~w~n", [FHDR]),
    io:format("FHDR = ~s~n", [bin_to_hex(FHDR)]),
    io:format("FPort = ~w~n", [FPort]),
    io:format("FRMPayload = ~w~n", [FrmPayload]),
    io:format("Encrypted FRMPayload = ~s~n", [bin_to_hex(FrmPayload)]),

    io:format("~n( FHDR = DevAddr[4] | FCtrl[1] | FCnt[2] | FOpts[0..15] )~n"),
    DevAddr = lora_core:payload_devaddr(Bin0),
    % io:format("DevAddr = ~8.16.0B~n", [DevAddr]),
    io:format("DevAddr = ~w~n", [DevAddr]),
    FCtrl = lora_core:payload_fctrl(Bin0),
    io:format("FCtrl = ~w~n", [FCtrl]),
    FCtrlBits = lora_core:payload_fctrl_bits(Bin0),
    io:format("FCtrlBits = ~w~n", [FCtrlBits]),
    FCnt = lora_core:payload_fcnt(Bin0),
    io:format("FCnt = ~w~n", [FCnt]),
    FOptsLen = lora_core:payload_foptslen(Bin0),
    io:format("FOptsLen = ~w~n", [FOptsLen]),
    FOpts = lora_core:payload_fopts(Bin0),
    io:format("FOpts = ~w~n", [FOpts]),
    io:format("FOpts = ~s~n", [bin_to_hex(FOpts)]),

    CID = lora_core:fopts_mac_cid(FOpts),
    io:format("CID = ~w~n", [CID]),

    Direction = lora_core:payload_dirbit(Payload),
    case Direction of
        0 ->
            ParsedFOpts = lora_core:parse_fopts(FOpts),
            io:format("ParsedFOpts = ~w~n", [ParsedFOpts]);
        1 ->
            ParsedFOptsDown = lora_core:parse_fdownopts(FOpts),
            io:format("ParsedFOptsDown = ~w~n", [ParsedFOptsDown])
    end,

    FType = lora_core:payload_ftype(Bin0),
    io:format("~nMessage Type = ~w~n", [FType]),
    Direction = lora_core:payload_dirbit(Bin0),
    io:format("Direction = ~s~n", [lora_utils:dir_string(Direction)]),
    FCnt2 = lora_core:payload_fcnt(Bin0),
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

    MType = lora_core:payload_ftype(Bin0),
    case MType of
        ?JOIN_REQUEST -> decode_join_request(Bin0);
        ?JOIN_ACCEPT -> decode_join_accept(Bin0);
        _ -> decode_frame(Bin0)
    end,
    fin.

bespoke_fopts(String, BespokeFn) ->
    Bin0 = string_to_binary(String),
    MType = lora_core:payload_ftype(Bin0),
    case MType of
        ?JOIN_REQUEST -> ok;
        ?JOIN_ACCEPT -> ok;
        _ -> bespoke_fopts_frame(Bin0, BespokeFn)
    end,
    ok.

log_fopt(FOpt) ->
    case FOpt of
        {link_adr_req, DataRate, TXPower, ChMask, ChMaskCntl, NbTrans} ->
            io:format("  ChMaskCntl = ~w~n", [ChMaskCntl]),
            io:format("  ChMask = 0x~4.16.0B~n", [ChMask]),
            io:format("  DataRate = ~w~n", [DataRate]),
            io:format("  TXPower = ~w~n", [TXPower]),
            io:format("  NbTrans = ~w~n", [NbTrans]);
        _ ->
            ok
    end.

log_fopts(ParsedFOpts) ->
    [log_fopt(FOpt) || FOpt <- ParsedFOpts].

bespoke(_FCnt, _Dir, _CtrlBits, <<>>, _Payload) ->
    ok;
bespoke(FCnt, 0, CtrlBits, FOpts, _Payload) ->
    ParsedFOpts = lora_core:parse_fopts(FOpts),
    io:format("FCnt=~w CtrlBits=~.2B Uplink FOpts = ~w~n", [FCnt, CtrlBits, ParsedFOpts]),
    ok;
bespoke(FCnt, 1, CtrlBits, FOpts, _Payload) ->
    ParsedFOptsDown = lora_core:parse_fdownopts(FOpts),
    io:format("FCnt=~w CtrlBits=~.2B Downlink FOpts = ~w~n", [FCnt, CtrlBits, ParsedFOptsDown]),
    log_fopts(ParsedFOptsDown),
    ok.

bespoke_fopts_frame(Payload, BespokeFn) ->
    Bin0 = Payload,
    FCtrlBits = lora_core:payload_fctrl_bits(Bin0),
    FCnt = lora_core:payload_fcnt(Bin0),
    FOpts = lora_core:payload_fopts(Bin0),
    Direction = lora_core:payload_dirbit(Payload),
    BespokeFn(FCnt, Direction, FCtrlBits, FOpts, Payload),
    ok.

decode_encode(Sample) ->
    {Pay0, Key0, AppKey0} = Sample(),
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
    Frame0 = lora_core:payload_to_frame(Bin0, NwkSKey0, AppSKey0),
    % io:format("frame = ~w~n", [Frame0]),
    Bin1 = lora_core:frame_to_payload(Frame0, NwkSKey0, AppSKey0),
    case Bin0 =/= Bin1 of
        true ->
            %% lager:warning("Bin0 = ~w~n", [Bin0]),
            %% lager:warning("Bin1 = ~w~n", [Bin1]),
            _Base64 = base64:encode(Bin1),
            %% lager:info("Base64 = ~s~n", [Base64]),
            decode_payload(Pay0);
        false ->
            ok
    end,
    ?assert(Bin0 =:= Bin1),
    fin.

encode_decode(Frame0) ->
    Key0 = <<1:128>>,
    AppKey = <<2:128>>,
    Bin0 = lora_core:frame_to_payload(Frame0, Key0, AppKey),
    Frame0 = lora_core:payload_to_frame(Bin0, Key0, AppKey),
    Bin1 = lora_core:frame_to_payload(Frame0, Key0, AppKey),
    case Bin0 =/= Bin1 of
        true ->
            %% lager:warn("bin0 = ~w~n", [Bin0]),
            %% lager:warn("bin1 = ~w~n", [Bin1]);
            ok;
        false ->
            ok
    end,
    ?assert(Bin0 =:= Bin1),
    fin.

process_payload_sequence(SeqFn) ->
    Sequence = SeqFn(),
    [bespoke_fopts(Payload, fun bespoke/5) || Payload <- Sequence].

-endif.
