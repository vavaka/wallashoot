%%%-------------------------------------------------------------------
%%% @author vavaka
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(game).

-include("wallashoot.hrl").

-behaviour(gen_server).

%% API
-export([
  start/1,
  join/2,
  leave/1,
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

-record(player, {
  pid       :: pid(),
  nickname  :: string(),
  monitor   :: reference(),
  position  :: position()
}).

-record(state, {
  name          :: string(),
  max_players   :: non_neg_integer(),
  map           :: term(),
  players       :: term(),
  players_stats :: term()
}).

-record(player_stat, {
  pid       :: pid(),
  nickname  :: string(),
  frags     :: non_neg_integer(),
  deaths    :: non_neg_integer()
}).

%% ---------------------------------------------------------------
%% API functions
%% ---------------------------------------------------------------

-spec start(Options :: {Name :: atom(), Width :: non_neg_integer(), Height :: non_neg_integer(), MaxPlayers :: non_neg_integer()}) -> {ok, Pid :: pid()} | error().
start(Options = {Name, _Width, _Height, _MaxPlayers}) ->
  gen_server:start({local, Name}, ?MODULE, Options, []).

-spec join(GameRef :: server_ref(), Nickname :: string()) -> {ok, Pid :: pid()} | error().
join(GameRef, Nickname) ->
  gen_server:call(GameRef, {join, Nickname}).

-spec leave(GameRef :: server_ref()) -> ok | error().
leave(GameRef) ->
  gen_server:call(GameRef, leave).

-spec move(GameRef :: server_ref(), Direction :: direction()) -> {ok, Position :: position()} | error().
move(GameRef, Direction) ->
  gen_server:call(GameRef, {move, Direction}).

-spec shoot(GameRef :: server_ref(), Direction :: direction()) -> ok | error().
shoot(GameRef, Direction) ->
  gen_server:call(GameRef, {shoot, Direction}).

-spec position(GameRef :: server_ref()) -> {ok, {position, Position :: position()}} | error().
position(GameRef) ->
  gen_server:call(GameRef, position).

-spec map(GameRef :: server_ref()) -> {ok, {map, Map :: any()}} | error().
map(GameRef) ->
  gen_server:call(GameRef, map).

-spec stats(GameRef :: server_ref()) -> {ok, {stats, Stats :: any()}} | error().
stats(GameRef) ->
  gen_server:call(GameRef, stats).

%% ---------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------

init(Options = {Name, Width, Height, MaxPlayers}) ->
  random:seed(erlang:timestamp()),
  State = #state{
    name = Name,
    max_players = MaxPlayers,
    players = new_players(),
    map = map:new(Width, Height),
    players_stats = new_players_stats()
  },
  log(system, io_lib:format("Started: ~p", [Options]), State),
  {ok, State}.

handle_call({join, Nickname}, {Pid, _}, State) ->
  handle_join(Pid, Nickname, State);

handle_call(leave, {Pid, _}, State) ->
  handle_leave(Pid, State);

handle_call({move, Direction}, {Pid, _}, State) ->
  handle_move(Pid, Direction, State);

handle_call({shoot, Direction}, {Pid, _}, State) ->
  handle_shoot(Pid, Direction, State);

handle_call(position, {Pid, _}, State) ->
  handle_position(Pid, State);

handle_call(map, {Pid, _}, State) ->
  handle_map(Pid, State);

handle_call(stats, {Pid, _}, State) ->
  handle_stats(Pid, State);

handle_call(_Message, {_Pid, _}, State) ->
  {reply, ok, State}.

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info({'DOWN', _Monitor, process, Pid, _Reason}, State) ->
  handle_process_down(Pid, State);

handle_info(_Message, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ---------------------------------------------------------------
%% Handlers Implementation
%% ---------------------------------------------------------------

handle_join(Pid, Nickname, State) ->
  case is_players_limit_reached(State) of
    true ->
      {reply, {error, players_limit_reached}, State};
    false ->
      handle_join(#player{pid = Pid, nickname = Nickname}, State)
  end.

handle_join(Player = #player{pid = Pid, nickname = Nickname}, State) ->
  NewState = link_player(Player, State),
  log(game, [Pid], io_lib:format("~s joined game", [Nickname]), NewState),
  {reply, {ok, self()}, NewState}.

handle_leave(Pid, State) when is_pid(Pid) ->
  case find_player(Pid, State) of
    false ->
      {reply, {error, player_not_found}, State};
    Player = #player{} ->
      handle_leave(Player, State)
  end;

handle_leave(Player = #player{pid = Pid, nickname = Nickname}, State) ->
  log(game, [Pid], io_lib:format("~s left game", [Nickname]), State),
  NewState = unlink_player(Player, State),
  handle_game_over_or_continue({reply, ok, NewState}, {stop, normal, ok, State}, NewState).

handle_move(Pid, Direction, State) when is_pid(Pid) ->
  case {find_player(Pid, State), map:validate_direction(Direction)} of
    {Player = #player{}, valid} ->
      handle_move(Player, Direction, State);
    {false, _} ->
      {reply, {error, player_not_found}, State};
    {_, {invalid, Reason}} ->
      {reply, {error, Reason}, State}
  end;

handle_move(Player = #player{pid = Pid, nickname = Nickname, position = Position}, Direction, State) ->
  NextPosition = map:next_position(Direction, Position),
  case set_player_position(NextPosition, Player, State) of
    NewState = #state{} ->
      log(game, [Pid], io_lib:format("~s moved to ~p", [Nickname, NextPosition]), State),
      {reply, {ok, NextPosition}, NewState};
    Error ->
      {reply, {error, Error}, State}
  end.

handle_shoot(Pid, Direction, State) when is_pid(Pid) ->
  case {find_player(Pid, State), map:validate_direction(Direction)} of
    {Player = #player{}, valid} ->
      handle_shoot(Player, Direction, State);
    {false, _} ->
      {reply, {error, player_not_found}, State};
    {_, {invalid, Reason}} ->
      {reply, {error, Reason}, State}
  end;

handle_shoot(Player = #player{nickname = Nickname, position = Position}, Direction, State = #state{map = Map}) ->
  log(game, io_lib:format("~s shooted ~s", [Nickname, Direction]), State),
  case get_shoot_victim(Position, Direction, Map) of
    false ->
      handle_miss(Player, State);
    VictimPid ->
      handle_kill(VictimPid, Player, State)
  end.

handle_miss(#player{nickname = Nickname}, State) ->
  log(game, io_lib:format("~s missed", [Nickname]), State),
  {reply, ok, State}.

handle_kill(VictimPid, Killer = #player{nickname = KillerNickname}, State) ->
  Victim = #player{nickname = VictimNickname} = find_player(VictimPid, State),

  log(game, [VictimPid], io_lib:format("~s killed ~s", [KillerNickname, VictimNickname]), State),
  send_message(system, VictimPid, killed),

  NewState1 = log_killing_stats(Victim, Killer, State),
  NewState2 = unlink_player(Victim, NewState1),
  handle_game_over_or_continue({reply, ok, NewState2}, {stop, normal, ok, State}, NewState2).

handle_position(Pid, State) ->
  case find_player(Pid, State) of
    false ->
      {reply, {error, player_not_found}, State};
    #player{position = Position} ->
      {reply, {ok, {position, Position}}, State}
  end.

handle_map(Pid, State) when is_pid(Pid) ->
  case find_player(Pid, State) of
    false ->
      {reply, {error, player_not_found}, State};
    Player ->
      handle_map(Player, State)
  end;

handle_map(#player{pid = Pid}, State = #state{map = Map}) ->
  Encoding = orddict:new(),
  Encoding1 = orddict:store(Pid, "U", Encoding),
  Encoding2 = orddict:store(free, ".", Encoding1),
  {reply, {ok, {map, map:dump(Encoding2, "E", Map)}}, State}.

handle_stats(Pid, State) when is_pid(Pid) ->
  case find_player(Pid, State) of
    false ->
      {reply, {error, player_not_found}, State};
    _Player ->
      handle_stats(State)
  end.

handle_stats(State) ->
  {reply, {ok, {stats, raw_players_stats(State)}}, State}.

handle_process_down(Pid, State) ->
  case find_player(Pid, State) of
    false ->
      log(system, io_lib:format("Unknown process down: ~p", [Pid]), State),
      {noreply, State};
    Player = #player{nickname = Nickname} ->
      log(game, [Pid], io_lib:format("~s disconnected", [Nickname]), State),
      NewState = unlink_player(Player, State),
      handle_game_over_or_continue({noreply, NewState}, {stop, normal, State}, NewState)
  end.

handle_game_over_or_continue(ContinueResult, StopResult, State) ->
  case is_game_over(State) of
    false ->
      ContinueResult;
    true ->
      handle_game_over(StopResult, State)
  end.

handle_game_over(StopResult, State) ->
  case players_list(State) of
    [] ->
      log(system, "Last player left game. Nobody won.", State);
    [#player{pid = WinnerPid, nickname = Nickname} | _] ->
      log(system, io_lib:format("~s won game!!!", [Nickname]), State),
      send_message(system, WinnerPid, win)
  end,
  print_players_stats(State),
  log(system, "Game over.", State),
  StopResult.

%% ---------------------------------------------------------------
%% Private Functions
%% ---------------------------------------------------------------

link_player(Player = #player{pid = Pid}, State) ->
  Monitor = erlang:monitor(process, Pid),
  Position = random_free_position(State),

  NewPlayer = Player#player{position = Position, monitor = Monitor},
  NewState1 = put_player(NewPlayer, State),
  NewState2 = set_player_position(Position, NewPlayer, NewState1),
  initialize_player_stats(Player, NewState2).

unlink_player(Player = #player{position = Position, monitor = Monitor}, State = #state{map = Map}) ->
  erlang:demonitor(Monitor),
  NewState = delete_player(Player, State),
  NewState#state{map = map:set(Position, free, Map)}.

initialize_player_stats(#player{pid = Pid, nickname = Nickname}, State) ->
  case find_player_stat(Pid, State) of
    false ->
      put_player_stat(#player_stat{pid = Pid, nickname = Nickname, frags = 0, deaths = 0}, State);
    _Found ->
      State
  end.

set_player_position(NewPosition, Player = #player{pid = Pid, position = OldPosition}, State = #state{map = Map}) ->
  case {map:validate_position(NewPosition, Map), map:get(NewPosition, Map)} of
    {valid, free} ->
      NewState = put_player(Player#player{position = NewPosition}, State),
      Map1 = map:set(OldPosition, free, Map),
      NewState#state{map = map:set(NewPosition, Pid, Map1)};
    {{invalid, Reason}, _} ->
      Reason;
    {valid, OtherPid} when is_pid(OtherPid) ->
      position_accuired
  end.

get_shoot_victim(Position, Direction, Map) ->
  NextPosition = map:next_position(Direction, Position),
  case {map:validate_position(NextPosition, Map), map:get(NextPosition, Map)} of
    {valid, free} ->
      get_shoot_victim(NextPosition, Direction, Map);
    {{invalid, position_out_of_range}, _} ->
      false;
    {valid, OtherPid} when is_pid(OtherPid) ->
      OtherPid
  end.

log_killing_stats(#player{pid = VictimPid}, #player{pid = KillerPid}, State) ->
  VictimStat = #player_stat{deaths = VictimDeaths} = find_player_stat(VictimPid, State),
  NewState = put_player_stat(VictimStat#player_stat{deaths = VictimDeaths + 1}, State),

  KillerStat = #player_stat{frags = KillerFrags} = find_player_stat(KillerPid, State),
  put_player_stat(KillerStat#player_stat{frags = KillerFrags + 1}, NewState).

is_game_over(State) ->
  players_count(State) =< 1.

random_free_position(State = #state{map = Map}) ->
  Position = map:random_position(Map),
  case map:get(Position, Map) of
    free ->
      Position;
    _ ->
      random_free_position(State)
  end.


new_players() ->
  [].

put_player(Player = #player{pid = Pid}, State = #state{players = Players}) ->
  NewPlayers = lists:keystore(Pid, 2, Players, Player),
  State#state{players = NewPlayers}.

delete_player(#player{pid = Pid}, State = #state{players = Players}) ->
  NewPlayers = lists:keydelete(Pid, 2, Players),
  State#state{players = NewPlayers}.

find_player(Pid, #state{players = Players}) ->
  lists:keyfind(Pid, 2, Players).

players_count(#state{players = Players}) ->
  length(Players).

players_list(#state{players = Players}) ->
  Players.

is_players_limit_reached(State = #state{max_players = MaxPlayers}) ->
  players_count(State) >= MaxPlayers.



new_players_stats() ->
  [].

put_player_stat(Entry = #player_stat{pid = Pid}, State = #state{players_stats = PlayersStats}) ->
  NewStats = lists:keystore(Pid, 2, PlayersStats, Entry),
  State#state{players_stats = NewStats}.

find_player_stat(Pid, #state{players_stats = PlayersStats}) ->
  lists:keyfind(Pid, 2, PlayersStats).



raw_players_stats(#state{players_stats = PlayersStats}) ->
  lists:map(fun(#player_stat{nickname = Nickname, frags = Frags, deaths = Deaths}) ->
    {Nickname, Frags, Deaths}
  end, PlayersStats).

print_players_stats(State) ->
  F = fun(Format, Args) -> log(system, io_lib:format(Format, Args), State) end,
  log(system, "", State),
  log(system, "Statistics", State),
  log(system, "------------------------------------------", State),
  stats:print(F, raw_players_stats(State)),
  log(system, "------------------------------------------", State),
  log(system, "", State).


log(system, Message, #state{name = Name}) ->
  lager:info("GAME ~s: ~s~n", [Name, Message]);

log(game, Message, State) ->
  log(game, [], Message, State).

log(game, Excludes, Message, State) ->
  log(system, Message, State),

  AllPids = lists:map(fun(#player{pid = Pid}) -> Pid end, players_list(State)),
  FilteredPids = lists:filter(fun(Pid) ->
    not lists:member(Pid, Excludes)
  end, AllPids),
  send_message(log, FilteredPids, Message).

send_message(Type, Pids, Message) when is_list(Pids) ->
  lists:foreach(fun(Pid) ->
    send_message(Type, Pid, Message)
  end, Pids);

send_message(Type, Pid, Message) when is_pid(Pid) ->
  catch Pid ! {?APP_NAME, Type, Message}.
