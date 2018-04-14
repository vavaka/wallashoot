%%%-------------------------------------------------------------------
%%% @author vavaka
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(game_history).

-include("wallashoot.hrl").

-behaviour(gen_event).

%% Callbacks
-export([
  init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {
  history = [] :: list()
}).

history(EmgrRef) ->
  gen_event:call(EmgrRef, ?MODULE, history).

init(_Args) ->
  {ok, #state{}}.

handle_event({join, PlayerName}, State) ->
  Event = io_lib:format("~s joined the game", [PlayerName]),
  {ok, store_event(Event, State)};

handle_event({leave, PlayerName}, State) ->
  Event = io_lib:format("~s left the game", [PlayerName]),
  {ok, store_event(Event, State)};

handle_event({move, PlayerName, NewPosition}, State) ->
  Event = io_lib:format("~s moved to ~p", [PlayerName, NewPosition]),
  {ok, store_event(Event, State)};

handle_event({shoot, PlayerName, Direction}, State) ->
  Event = io_lib:format("~s shooted ~s", [PlayerName, Direction]),
  {ok, store_event(Event, State)};

handle_event({kill, Who, Whom}, State) ->
  Event = io_lib:format("~s killed by ~s~n", [Whom, Who]),
  {ok, store_event(Event, State)}.

handle_call(history, State) ->
  {ok, {ok, State#state.history}, State}.

handle_info(Message, _State) ->
  erlang:error({unsupported_message, Message}).

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

store_event(Event, State = #state{history = History}) ->
  State#state{history = [Event | History]}.
