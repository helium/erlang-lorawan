%%%-------------------------------------------------------------------
%% @doc
%% == LoRaWAN RxDelay ==
%% See LoRaWAN Link Layer spec v1.0.4
%% sections:
%% - 3.3 Receive Windows
%% - 4.2.1 Frame types (FType bit field), for Join Accept
%% - 5.7 Setting Delay between TX and RX (RXTimingSetupReq, RXTimingSetupAns)
%% - 6.2.6 Join-Accept frame, which includes RXDelay
%% @end
%%%-------------------------------------------------------------------
-module(lora_rxdelay).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
    get/1,
    get/2,
    bootstrap/1,
    maybe_update/2,
    adjust_on_join/1,
    adjust/3
]).

%% TODO For tracking internal state, account for that state being
%% durably persisted by the calling scope using a record/struct, such
%% that it would get re-loaded when an idle device sends an uplink
%% again.  Consider a single common struct for `DeviceState' across
%% multiple features in this library to replace use of map/hash-table
%% here.

-define(RX_DELAY_ESTABLISHED, rx_delay_established).
-define(RX_DELAY_CHANGE, rx_delay_change).
-define(RX_DELAY_REQUESTED, rx_delay_requested).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec get(DeviceState :: map()) -> RxDelay :: non_neg_integer().
get(DeviceState) ->
    ?MODULE:get(DeviceState, 0).

-spec get(DeviceState :: map(), Default :: term()) -> RxDelay :: non_neg_integer() | term().
get(DeviceState, Default) ->
    maps:get(rx_delay_actual, DeviceState, Default).

-spec bootstrap(DeviceState0 :: map()) -> DeviceState :: map().
bootstrap(DeviceState) ->
    %% The key `rx_delay' comes from Console/API for changing to that
    %% value.  `rx_delay_actual' represents the value set in LoRaWAN
    %% header during device Join and must go through request/answer
    %% negotiation to be changed.
    case maps:get(rx_delay, DeviceState, 0) of
        0 ->
            %% Console always sends rx_delay via API, so default arm rarely gets used.
            DeviceState#{rx_delay_state => ?RX_DELAY_ESTABLISHED};
        Del when Del =< 15 ->
            DeviceState#{rx_delay_state => ?RX_DELAY_ESTABLISHED, rx_delay_actual => Del}
    end.

-spec maybe_update(
    ApiSettings :: map(),
    DeviceState :: map()
) -> DeviceState1 :: map().
maybe_update(ApiSettings, DeviceState) ->
    %% Run after the value for `rx_delay` gets changed via Console.
    %% Accommodate net-nil changes via Console as no-op; e.g., A -> B -> A.
    Actual = maps:get(rx_delay_actual, DeviceState, 0),
    Requested = maps:get(rx_delay, ApiSettings, Actual),
    case Requested == Actual of
        true ->
            DeviceState#{rx_delay_state => ?RX_DELAY_ESTABLISHED};
        false when Requested =< 15 ->
            %% Track requested value for new `rx_delay` without actually changing to it.
            DeviceState#{rx_delay_state => ?RX_DELAY_CHANGE}
    end.

-spec adjust_on_join(DeviceState0 :: map()) -> State :: map().
adjust_on_join(DeviceState0) ->
    %% LoRaWAN Join-Accept implies `rx_timing_setup_ans` because
    %% `RXDelay` was in the Join-Accept.  A subsequent uplink implies
    %% that the device acknowledged the Join-Accept and thus, RXDelay.
    {_, State, _} = adjust(DeviceState0, [rx_timing_setup_ans], []),
    State.

-spec adjust(DeviceState0 :: map(), UplinkFOpts :: list(), FOpts0 :: list()) ->
    {RxDelay :: non_neg_integer(), DeviceState1 :: map(), FOpts1 :: list()}.
adjust(DeviceState0, UplinkFOpts, FOpts0) ->
    %% When state is undefined, it's probably a test that omits device_update()
    %% or didn't wait long enough for the async/cast to complete.
    State = maps:get(rx_delay_state, DeviceState0, unknown_state),
    Actual = maps:get(rx_delay_actual, DeviceState0, 0),
    Answered = lists:member(rx_timing_setup_ans, UplinkFOpts),
    %% Handle sequential changes to rx_delay; e.g, a previous change
    %% was ack'd, and now there's potentially another new RxDelay value.
    {RxDelay, DeviceState1, FOpts1} =
        case {State, Answered} of
            %% Entering `RX_DELAY_CHANGE' state occurs in maybe_update().
            {?RX_DELAY_ESTABLISHED, _} ->
                {Actual, DeviceState0, FOpts0};
            {?RX_DELAY_CHANGE, _} ->
                %% Console changed `rx_delay' value.
                Requested = maps:get(rx_delay, DeviceState0),
                FOpts = [{rx_timing_setup_req, Requested} | FOpts0],
                DeviceState = DeviceState0#{rx_delay_state => ?RX_DELAY_REQUESTED},
                {Actual, DeviceState, FOpts};
            {?RX_DELAY_REQUESTED, false} ->
                %% Router sent downlink request, Device has yet to ACK.
                Requested = maps:get(rx_delay, DeviceState0),
                FOpts = [{rx_timing_setup_req, Requested} | FOpts0],
                {Actual, DeviceState0, FOpts};
            {?RX_DELAY_REQUESTED, true} ->
                %% Device responded with ACK, so Router can apply the requested rx_delay.
                Requested = maps:get(rx_delay, DeviceState0),
                Map = #{rx_delay_actual => Requested, rx_delay_state => ?RX_DELAY_ESTABLISHED},
                {Requested, maps:merge(DeviceState0, Map), FOpts0};
            {unknown_state, _} ->
                %% TODO extract a meaningful id from device for logging
                lager:error("rx_delay state=unknown device-id=~p", [DeviceState0]),
                {Actual, DeviceState0, FOpts0}
        end,
    {RxDelay, DeviceState1, FOpts1}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% TODO For example Common Tests using rx_delay, see test/router_SUITE.erl
%% in GitHub.com/helium/router

rx_delay_state_test() ->
    ?assertEqual(default, get(#{foo => bar}, default)),
    ?assertEqual(default, get(#{rx_delay => 15}, default)),
    ?assertEqual(15, get(#{rx_delay_actual => 15}, default)),

    DeviceState = #{},

    %% Ensure only LoRaWAN-approved values used
    ApiDeviceDelTooBig = #{rx_delay => 99},
    ?assertError({case_clause, 99}, bootstrap(ApiDeviceDelTooBig)),

    %% Bootstrapping state
    ?assertEqual(
        #{rx_delay_state => ?RX_DELAY_ESTABLISHED},
        bootstrap(DeviceState)
    ),
    ApiSettings = maps:merge(DeviceState, #{rx_delay => 5}),
    ?assertEqual(
        #{rx_delay_state => ?RX_DELAY_ESTABLISHED, rx_delay_actual => 5, rx_delay => 5},
        bootstrap(ApiSettings)
    ),

    %% No net change yet, as LoRaWAN's RxDelay default is 0
    ApiSettings0 = maps:merge(DeviceState, #{rx_delay => 0}),
    ?assertEqual(
        #{rx_delay_state => ?RX_DELAY_ESTABLISHED, rx_delay => 0},
        bootstrap(ApiSettings0)
    ),
    DeviceState0 = maps:merge(
        #{rx_delay_state => ?RX_DELAY_ESTABLISHED, rx_delay => 0},
        DeviceState
    ),
    ?assertEqual(
        #{rx_delay_state => ?RX_DELAY_ESTABLISHED, rx_delay => 0},
        adjust_on_join(DeviceState0)
    ),
    ?assertEqual(
        #{rx_delay_state => ?RX_DELAY_ESTABLISHED, rx_delay => 0},
        maybe_update(ApiSettings0, DeviceState0)
    ),

    %% No net change when using non-default value for RxDelay.
    DeviceState1 = maps:merge(
        DeviceState,
        #{rx_delay_state => ?RX_DELAY_ESTABLISHED, rx_delay_actual => 1}
    ),
    ApiSettings1 = maps:merge(DeviceState, #{rx_delay => 1}),
    ?assertEqual(
        #{rx_delay_state => ?RX_DELAY_ESTABLISHED, rx_delay_actual => 1},
        maybe_update(ApiSettings1, DeviceState1)
    ),

    %% Exercise default case to recover state
    ?assertEqual({0, #{}, []}, adjust(DeviceState, [], [])),
    ?assertEqual(
        {1, #{rx_delay_state => ?RX_DELAY_ESTABLISHED, rx_delay_actual => 1}, []},
        adjust(DeviceState1, [], [])
    ),
    %% No net change from a redundant ACK from device:
    ?assertEqual(
        {1, #{rx_delay_state => ?RX_DELAY_ESTABLISHED, rx_delay_actual => 1}, []},
        adjust(DeviceState1, [rx_timing_setup_ans], [])
    ),

    %% Change delay value

    ApiSettings2 = maps:merge(DeviceState1, #{rx_delay => 2}),
    ?assertEqual(
        #{rx_delay_state => ?RX_DELAY_CHANGE, rx_delay_actual => 1},
        maybe_update(ApiSettings2, DeviceState1)
    ),

    DeviceState2 = maps:merge(
        DeviceState,
        #{
            rx_delay_state => ?RX_DELAY_CHANGE,
            rx_delay_actual => 2,
            rx_delay => 3
        }
    ),
    Metadata2 = #{
        rx_delay_state => ?RX_DELAY_REQUESTED,
        rx_delay_actual => 2,
        rx_delay => 3
    },
    ?assertEqual(
        {2, Metadata2, [{rx_timing_setup_req, 3}]},
        adjust(DeviceState2, [], [])
    ),
    DeviceState3 = maps:merge(Metadata2, DeviceState),
    Metadata3 = #{
        rx_delay_state => ?RX_DELAY_ESTABLISHED,
        rx_delay_actual => 3,
        rx_delay => 3
    },
    ?assertEqual(
        {3, Metadata3, []},
        adjust(DeviceState3, [rx_timing_setup_ans], [])
    ),
    ok.

-endif.
