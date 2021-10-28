%%%-------------------------------------------------------------------
%% @doc erlang_lorawan public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_lorawan_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlang_lorawan_sup:start_link().

stop(_State) ->
    ok.
