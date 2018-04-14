%%%-------------------------------------------------------------------
%%% @author vavaka
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(session_console_logger).

-include("wallashoot.hrl").

-behaviour(gen_event).

%% Callbacks
-export([
  init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  code_change/3,
  terminate/2
]).

-record(state, {
  player_name :: string()
}).

init(PlayerName) ->
  {ok, #state{player_name = PlayerName}}.

handle_event(win, State) ->
  io:format("You win!!! Game over.~n"),
  {ok, State};

handle_event({kill, _Who, PlayerName}, State = #state{player_name = PlayerName}) ->
  io:format("You killed!!!~n"),
  {ok, State};

handle_event({kill, Who, Whom}, State) ->
  io:format("~s killed by ~s~n", [Whom, Who]),
  {ok, State};

handle_event({log, Message}, State) ->
  io:format("~s~n", [Message]),
  {ok, State}.

handle_call(Message, _State) ->
  erlang:error({unsupported_message, Message}).

handle_info(Message, _State) ->
  erlang:error({unsupported_message, Message}).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.
