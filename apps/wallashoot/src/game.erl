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
  leave/0,
  move/1,
  shoot/1,
  position/0,
  map/0,
  stats/0
]).

%% Callbacks
-export([
  init/1,
  handle_call/3,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(player, {
  pid       :: pid(),
  monitor   :: reference(),
  nickname  :: string()
}).

-record(state, {
  name          :: string(),
  max_players   :: non_neg_integer(),
  map           :: term(),
  players       :: map(),
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
join(GameRef = {_Name, Node}, PlayerName) ->
  case net_kernel:connect_node(Node) of
    true ->
        case (catch gen_server:call(GameRef, {join, PlayerName})) of
        {ok, _Pid} ->
          set_current_current_game_ref(GameRef),
          ok;
        {'EXIT', {noproc, _}} ->
          {error, invalid_game};
        Error ->
          Error
      end;
    _ ->
      {error, connection_error}
  end.

-spec leave() -> ok | error().
leave() ->
  gen_server:call(current_game_ref(), leave).

-spec move(Direction :: direction()) -> {ok, Position :: position()} | error().
move(Direction) ->
  gen_server:call(current_game_ref(), {move, Direction}).

-spec shoot(Direction :: direction()) -> ok | error().
shoot(Direction) ->
  gen_server:call(current_game_ref(), {shoot, Direction}).

-spec position() -> {ok, {Position :: position()}} | error().
position() ->
  gen_server:call(current_game_ref(), position).

-spec map() -> {ok, {Map :: any()}} | error().
map() ->
  case gen_server:call(current_game_ref(), map) of
    {ok, Map} -> print_map(Map);
    Error -> Error
  end.

-spec stats() -> {ok, {Stats :: any()}} | error().
stats() ->
  gen_server:call(current_game_ref(), stats).

%% Private
set_current_current_game_ref(GameRef) ->
  put(current_game_ref, GameRef).

%% Private
current_game_ref() ->
  get(current_game_ref).

%% ---------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------

init(Options = {Name, Width, Height, MaxPlayers}) ->
  State = #state{
    name = Name,
    max_players = MaxPlayers,
    players = #{},
    map = map:new(Width, Height),
    players_stats = new_players_stats()
  },

  log(system, io_lib:format("Started: ~p", [Options]), State),
  {ok, State}.

handle_call({join, Nickname}, {Pid, _}, State) ->
  try
    check_new_player(Pid, State),
    check_players_limit(State),

    NewState = register_player({Pid, Nickname}, State),
    NewState2 = initialize_player_stats({Pid, Nickname}, NewState),

    log(game, [Pid], io_lib:format("~s joined the game", [Nickname]), NewState2),
    {reply, {ok, self()}, NewState2}
  catch
    error:{precondition_failed, Error} -> {reply, {error, Error}, State}
  end;

handle_call(leave, {Pid, _}, State) ->
  try
    check_player(Pid, State),

    Player = find_player(Pid, State),
    NewState = unregister_player(Player, State),

    log(game, [Pid], io_lib:format("~s left the game", [Player#player.nickname]), NewState),
    {reply, ok, NewState}
  catch
    error:{precondition_failed, Error} -> {reply, {error, Error}, State}
  end;

handle_call({move, Direction}, {Pid, _}, State = #state{map = Map}) ->
  try
    check_player(Pid, State),
    check_direction(Direction),

    CurrentPosition = map:get_object_position(Pid, Map),
    NextPosition = next_position(Direction, CurrentPosition),
    NewState = State#state{map = map:set_object_position(NextPosition, Pid, Map)},

    Player = find_player(Pid, NewState),
    log(game, [Pid], io_lib:format("~s moved to ~p", [Player#player.nickname, NextPosition]), NewState),

    {reply, {ok, NextPosition}, NewState}
  catch
    error:{precondition_failed, Error} -> {reply, {error, Error}, State};
    error:position_out_of_bounds -> {reply, {error, position_out_of_bounds}, State}
  end;

handle_call({shoot, Direction}, {Pid, _}, State = #state{map = Map}) ->
  try
    check_player(Pid, State),
    check_direction(Direction),

    Player = find_player(Pid, State),
    log(game, io_lib:format("~s shooted ~s", [Player#player.nickname, Direction]), State),

    Position = map:get_object_position(Pid),
    case get_object_at_trajectory(Position, Direction, Map) of
      undefined ->
        log(game, io_lib:format("~s missed", [Player#player.nickname]), State),
        {reply, ok, State};
      OtherPid when is_pid(OtherPid) ->
        OtherPlayer = find_player(OtherPid, State),

        NewState1 = unregister_player(OtherPlayer, State),
        send_message(system, OtherPid, killed),

        NewState2 = log_killing_stats(OtherPlayer, Player, NewState1),
        log(game, [OtherPid], io_lib:format("~s killed ~s", [Player#player.nickname, OtherPlayer#player.nickname]), NewState2),

        {reply, ok, NewState2}
    end
  catch
    error:{precondition_failed, Error} -> {reply, {error, Error}, State}
  end;

handle_call(position, {Pid, _}, State = #state{map = Map}) ->
  try
    check_player(Pid, State),

    {reply, {ok, map:get_object_position(Pid, Map)}, State}
  catch
    error:{precondition_failed, Error} -> {reply, {error, Error}, State}
  end;

handle_call(map, {Pid, _}, State = #state{map = Map}) ->
  {reply, {ok, map_to_array(Pid, Map)}, State};

handle_call(stats, {_Pid, _}, State) ->
  {reply, {ok, raw_players_stats(State)}, State}.

handle_info({'DOWN', _Monitor, process, Pid, _Reason}, State) ->
  Player = find_player(Pid, State),
  log(game, [Pid], io_lib:format("~s disconnected", [Player#player.nickname]), State),

  NewState = unregister_player(Player, State),
  {noreply, NewState}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% ---------------------------------------------------------------
%% Private Functions
%% ---------------------------------------------------------------

check_direction(left) -> ok;
check_direction(right) -> ok;
check_direction(up) -> ok;
check_direction(down) -> ok;
check_direction(left_up) -> ok;
check_direction(right_up) -> ok;
check_direction(left_down) -> ok;
check_direction(right_down) -> ok;
check_direction(right_down) -> erlang:error({precondition_failed, invalid_direction}).

next_position(left, {X, Y}) ->
  {X - 1, Y};
next_position(right, {X, Y}) ->
  {X + 1, Y};
next_position(up, {X, Y}) ->
  {X, Y - 1};
next_position(down, {X, Y}) ->
  {X, Y + 1};
next_position(up_left, {X, Y}) ->
  {X - 1, Y - 1};
next_position(down_left, {X, Y}) ->
  {X - 1, Y + 1};
next_position(up_right, {X, Y}) ->
  {X + 1, Y - 1};
next_position(down_right, {X, Y}) ->
  {X + 1, Y + 1};
next_position(_, {_X, _Y}) ->
  invalid_direction.

random_free_position(Map) ->
  Position = random_map_position(Map),
  case map:get_object_at(Position, Map, undefined) of
    undefined ->
      Position;
    _ ->
      random_free_position(Map)
  end.

random_map_position(Map) ->
  Width = map:width(Map),
  Height = map:height(Map),
  {rand:uniform(Width), rand:uniform(Height)}.

get_object_at_trajectory(Position, Direction, Map) ->
  try
    NextPosition = next_position(Direction, Position),
    case map:get_object_at(NextPosition, Map, undefined) of
      undefined ->
        get_object_at_trajectory(NextPosition, Direction, Map);
      Object ->
        Object
    end
  catch
    error:out_of_bounds -> undefined
  end.

map_to_array(PlayerPid, Map) ->
  PositionToSymbol = fun(Position) ->
    case map:get_object_at(Position, Map, undefined) of
      undefined -> ".";
      PlayerPid -> "P";
      OtherPid when is_pid(OtherPid) -> "E"
    end
  end,

  lists:map(fun(Y) ->
    [PositionToSymbol({X, Y}) || X <- lists:seq(1, map:width(Map))]
  end, lists:seq(1, map:height(Map))).

print_map(Map) ->
  lists:foreach(fun(Row) -> io:format("~s~n", [lists:join(" ", Row)]) end, Map).



check_players_limit(State = #state{max_players = MaxPlayers}) ->
  case players_count(State) >= MaxPlayers of
    true -> erlang:error({precondition_failed, players_limit_reached});
    false -> ok
  end.

check_new_player(Pid, State) ->
  case find_player(Pid, State) of
    undefined -> ok;
    _ -> erlang:error({precondition_failed, already_playing})
  end.

check_player(Pid, State) ->
  case find_player(Pid, State) of
    false -> erlang:error({precondition_failed, not_playing});
    _ -> ok
  end.

register_player({Pid, Nickname}, State = #state{map = Map, players = Players}) ->
  Monitor = erlang:monitor(process, Pid),

  Player = #player{pid = Pid, nickname = Nickname, monitor = Monitor},
  NewPlayers = Players#{Pid => Player},

  Position = random_free_position(Map),
  NewMap = map:set_object_position(Position, Pid, Map),

  State#state{map = NewMap, players = NewPlayers}.

unregister_player(#player{pid = Pid, monitor = Monitor}, State = #state{map = Map, players = Players}) ->
  erlang:demonitor(Monitor),

  NewMap = map:delete_object(Pid, Map),
  NewPlayers = maps:remove(Pid, Players),

  State#state{map = NewMap, players = NewPlayers}.

find_player(Pid, #state{players = Players}) ->
  maps:get(Pid, Players, undefined).

players_count(#state{players = Players}) ->
  maps:size(Players).




new_players_stats() ->
  [].

initialize_player_stats({Pid, Nickname}, State) ->
  case find_player_stat(Pid, State) of
    false ->
      put_player_stat(#player_stat{pid = Pid, nickname = Nickname, frags = 0, deaths = 0}, State);
    _Found ->
      State
  end.

put_player_stat(Entry = #player_stat{pid = Pid}, State = #state{players_stats = PlayersStats}) ->
  NewStats = lists:keystore(Pid, 2, PlayersStats, Entry),
  State#state{players_stats = NewStats}.

find_player_stat(Pid, #state{players_stats = PlayersStats}) ->
  lists:keyfind(Pid, 2, PlayersStats).

log_killing_stats(#player{pid = VictimPid}, #player{pid = KillerPid}, State) ->
  VictimStat = #player_stat{deaths = VictimDeaths} = find_player_stat(VictimPid, State),
  NewState = put_player_stat(VictimStat#player_stat{deaths = VictimDeaths + 1}, State),

  KillerStat = #player_stat{frags = KillerFrags} = find_player_stat(KillerPid, State),
  put_player_stat(KillerStat#player_stat{frags = KillerFrags + 1}, NewState).

raw_players_stats(#state{players_stats = PlayersStats}) ->
  lists:map(fun(#player_stat{nickname = Nickname, frags = Frags, deaths = Deaths}) ->
    {Nickname, Frags, Deaths}
  end, PlayersStats).

print_stats(Stats) ->
  stats:print(fun io:format/2, Stats).





log(system, Message, #state{name = Name}) ->
  lager:info("GAME ~s: ~s~n", [Name, Message]);

log(game, Message, State) ->
  log(game, [], Message, State).

log(game, Excludes, Message, State = #state{players = Players}) ->
  log(system, Message, State),

  AllPlayers = maps:values(Players),
  AllPids = lists:map(fun(#player{pid = Pid}) -> Pid end, AllPlayers),
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
