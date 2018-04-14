%%%-------------------------------------------------------------------
%%% @author vavaka
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(session).

-include("wallashoot.hrl").

-behaviour(gen_server).

%% API
-export([
  start/1,
  stop/1,
  add_event_handler/2,
  move/2,
  shoot/2,
  position/1,
  map/1,
  stats/1
]).

%% Callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {
  game_pid      :: pid(),
  game_monitor  :: pid(),
  game_ref      :: term(),
  event_mgr_ref :: term(),
  player_name   :: reference()
}).

%% ---------------------------------------------------------------
%% API
%% ---------------------------------------------------------------
-spec start({GameRef :: server_ref(), PlayerName :: string()}) -> ok | error().
start(SessionOptions = {GameRef, _PlayerName}) ->
  {GameName, GameNode} = GameRef,
  SessionName = list_to_atom(atom_to_list(GameNode) ++ "/" ++ atom_to_list(GameName)),
  gen_server:start({local, SessionName}, ?MODULE, SessionOptions, []).

-spec stop(Pid :: pid()) -> ok | error().
stop(Pid) ->
  gen_server:call(Pid, stop).

-spec add_event_handler(Pid :: pid(), EventHandler :: atom()) -> ok | error().
add_event_handler(Pid, EventHandler) ->
  gen_server:call(Pid, {add_event_handler, EventHandler}).

-spec move(Pid :: pid(), Direction :: direction()) -> {ok, position()} | error().
move(Pid, Direction) ->
  gen_server:call(Pid, {move, Direction}).

-spec shoot(Pid :: pid(), Direction :: direction()) -> ok | error().
shoot(Pid, Direction) ->
  gen_server:call(Pid, {shoot, Direction}).

-spec position(Pid :: pid()) -> {ok, position()} | error().
position(Pid) ->
  gen_server:call(Pid, position).

-spec map(Pid :: pid()) -> {ok, list()} | error().
map(Pid) ->
  gen_server:call(Pid, map).

-spec stats(Pid :: pid()) -> {ok, list()} | error().
stats(Pid) ->
  gen_server:call(Pid, stats).

%% ---------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------

init({GameRef = {GameName, _GameNode}, PlayerName}) ->
  {ok, GamePid} = game:join(GameRef, PlayerName),
  EventMgrRef = list_to_atom(atom_to_list(GameName) ++ "/" ++ PlayerName),
  {ok, _} = gen_event:start_link({local, EventMgrRef}),
  {ok, #state{
    game_pid = GamePid,
    game_ref = GameRef,
    game_monitor = erlang:monitor(process, GamePid),
    event_mgr_ref = EventMgrRef,
    player_name = PlayerName
  }}.

handle_call(stop, {_Pid, _}, State) ->
  ok = game:leave(State#state.game_ref),
  erlang:demonitor(State#state.game_monitor, [flush]),
  gen_event:stop(State#state.event_mgr_ref),
  {reply, ok, #state{}};

handle_call({add_event_handler, EventHandler}, {_Pid, _}, State) ->
  gen_event:add_handler(State#state.event_mgr_ref, EventHandler, State#state.player_name),
  {reply, ok, State};

handle_call({move, Direction}, {_Pid, _}, State) ->
  {reply, game:move(State#state.game_ref, Direction), State};

handle_call({shoot, Direction}, {_Pid, _}, State) ->
  {reply, game:shoot(State#state.game_ref, Direction), State};

handle_call(position, {_Pid, _}, State) ->
  {reply, game:position(State#state.game_ref), State};

handle_call(map, {_Pid, _}, State) ->
  {ok, Map} = game:map(State#state.game_ref),
  print_map(Map),
  {reply, ok, State};

handle_call(stats, {_Pid, _}, State) ->
  {ok, Stats} = game:stats(State#state.game_ref),
  print_stats(Stats),
  {reply, ok, State};

handle_call(_Message, {_Pid, _}, State) ->
  {reply, ok, State}.

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info({event, Message}, State) ->
  gen_event:notify(State#state.event_mgr_ref, Message),
  {noreply, State};

handle_info({'DOWN', _Monitor, process, Pid, Reason}, State = #state{game_pid = Pid}) ->
  {GameName, _} = State#state.game_ref,
  io:format("Disconnected from game ~p: ~p~n", [GameName, Reason]),
  {noreply, #state{}}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ---------------------------------------------------------------
%% Private
%% ---------------------------------------------------------------

print_map(Map) ->
  lists:foreach(fun(Row) -> io:format("~s~n", [lists:join(" ", Row)]) end, Map).

print_stats(Stats) ->
  SortedStats = lists:sort(fun({_, {Frags1, _}}, {_, {Frags2, _}}) ->
    Frags1 >= Frags2
  end, Stats),

  io:format("~10s  ~10s  ~10s~n", ["Player", "Frags", "Deaths"]),
  lists:foreach(fun({Player, {Frags, Deaths}}) ->
    io:format("~10s  ~10B  ~10B~n", [Player, Frags, Deaths])
  end, SortedStats).