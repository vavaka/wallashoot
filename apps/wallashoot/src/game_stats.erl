%%%-------------------------------------------------------------------
%%% @author vavaka
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(game_stats).

-include("wallashoot.hrl").

-behaviour(gen_event).

%% Callbacks
-export([
  init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  terminate/2
]).

-record(state, {
  stats :: map()
}).

-record(stat_entry, {
  frags  = 0 :: non_neg_integer(),
  deaths = 0 :: non_neg_integer()
}).

init(_Args) ->
  {ok, #state{stats = #{}}}.

handle_event({kill, Who, Whom}, State = #state{stats = Stats}) ->
  WhoStats = maps:get(Who, Stats, #stat_entry{}),
  WhomStats = maps:get(Whom, Stats, #stat_entry{}),
  NewStats = Stats#{
    Who := WhoStats#stat_entry{frags = WhoStats#stat_entry.frags + 1},
    Whom := WhomStats#stat_entry{frags = WhomStats#stat_entry.deaths + 1}
  },
  {ok, State#state{stats = NewStats}};

handle_event(_, State) ->
  {ok, State}.

handle_call(stats, State) ->
  {ok, {ok, maps:to_list(State#state.stats)}, State}.

handle_info(Message, _State) ->
  erlang:error({unsupported_message, Message}).

terminate(_Reason, _State) ->
  ok.