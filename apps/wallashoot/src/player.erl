%%%-------------------------------------------------------------------
%%% @author vavaka
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(player).

-include("wallashoot.hrl").

-behaviour(gen_server).

%% API
-export([
  create/1,
  destroy/1,
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

-record(session, {
  node    :: atom(),
  name    :: atom(),
  pid     :: pid(),
  monitor :: reference()
}).

-type session() :: #session{}.

-record(state, {
  nickname  :: string(),
  session   :: session()
}).

%% ---------------------------------------------------------------
%% API functions
%% ---------------------------------------------------------------

-spec create(Nickname :: string()) -> {ok, Pid :: pid()} | error().
create(Nickname) ->
  gen_server:start(?MODULE, Nickname, []).

-spec destroy(PlayerRef :: server_ref()) -> ok | error().
destroy(PlayerRef) ->
  gen_server:call(PlayerRef, destroy).

-spec join(PlayerRef :: server_ref(), Game :: {Node :: atom(), Name :: atom()}) -> ok | error().
join(PlayerRef, Options = {_Node, _Name}) ->
  gen_server:call(PlayerRef, {join, Options}).

-spec leave(PlayerRef :: server_ref()) -> ok | error().
leave(PlayerRef) ->
  gen_server:call(PlayerRef, leave).

-spec move(PlayerRef :: server_ref(), Direction :: direction()) -> {ok, Position :: position} | error().
move(PlayerRef, Direction) ->
  gen_server:call(PlayerRef, {move, Direction}).

-spec shoot(PlayerRef :: server_ref(), Direction :: direction()) -> ok | error().
shoot(PlayerRef, Direction) ->
  gen_server:call(PlayerRef, {shoot, Direction}).

-spec position(PlayerRef :: server_ref()) -> {ok, {position, position()}} | error().
position(PlayerRef) ->
  gen_server:call(PlayerRef, position).

-spec map(PlayerRef :: server_ref()) -> {ok, {map, list()}} | error().
map(PlayerRef) ->
  gen_server:call(PlayerRef, map).

-spec stats(PlayerRef :: server_ref()) -> {ok, {stats, list()}} | error().
stats(PlayerRef) ->
  gen_server:call(PlayerRef, stats).

%% ---------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------

init(Nickname) ->
  {ok, #state{nickname = Nickname}}.

handle_call({join, Options}, {_Pid, _}, State) ->
  handle_join(Options, State);

handle_call(destroy, {_Pid, _}, State) ->
  handle_destroy(State);

handle_call(leave, {_Pid, _}, State) ->
  handle_leave(State);

handle_call({move, Direction}, {_Pid, _}, State) ->
  handle_move(Direction, State);

handle_call({shoot, Direction}, {_Pid, _}, State) ->
  handle_shoot(Direction, State);

handle_call(position, {_Pid, _}, State) ->
  handle_position(State);

handle_call(map, {_Pid, _}, State) ->
  handle_map(State);

handle_call(stats, {_Pid, _}, State) ->
  handle_stats(State);

handle_call(_Message, {_Pid, _}, State) ->
  {reply, ok, State}.

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info({?APP_NAME, system, win}, State) ->
  handle_win(State);

handle_info({?APP_NAME, system, killed}, State) ->
  handle_killed(State);

handle_info({?APP_NAME, log, Message}, State) ->
  handle_log(Message, State);

handle_info(Message = {'DOWN', _Monitor, process, _Pid, _Reason}, State) ->
  handle_process_down(Message, State);

handle_info(_Message, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ---------------------------------------------------------------
%% Handlers Implementation
%% ---------------------------------------------------------------

handle_join(_, State = #state{session = #session{}}) ->
  {reply, {error, already_playing}, State};

handle_join({Node, Name}, State = #state{nickname = Nickname, session = undefined}) ->
  case join_game(Node, Name, Nickname, State) of
    NewState = #state{} ->
      {reply, ok, NewState};
    Error ->
      {reply, Error, State}
  end.

handle_leave(State = #state{session = undefined}) ->
  {reply, {error, not_playing}, State};

handle_leave(State) ->
  case leave_game(State) of
    NewState = #state{} ->
      {reply, ok, NewState};
    Error ->
      {reply, Error, State}
  end.

handle_move(_Direction, State = #state{session = undefined}) ->
  {reply, {error, not_playing}, State};

handle_move(Direction, State = #state{session = Session}) ->
  Result = game:move(game_alias(Session), Direction),
  {reply, Result, State}.

handle_shoot(_Direction, State = #state{session = undefined}) ->
  {reply, {error, not_playing}, State};

handle_shoot(Direction, State = #state{session = Session}) ->
  Result = game:shoot(game_alias(Session), Direction),
  {reply, Result, State}.

handle_position(State = #state{session = undefined}) ->
  {reply, {error, not_playing}, State};

handle_position(State = #state{session = Session}) ->
  Result = game:position(game_alias(Session)),
  {reply, Result, State}.

handle_map(State = #state{session = undefined}) ->
  {reply, {error, not_playing}, State};

handle_map(State = #state{session = Session}) ->
  {ok, {map, Map}} = game:map(game_alias(Session)),
  print_map(Map),
  {reply, ok, State}.

handle_stats(State = #state{session = undefined}) ->
  {reply, {error, not_playing}, State};

handle_stats(State = #state{session = Session}) ->
  {ok, {stats, Stats}} = game:stats(game_alias(Session)),
  print_stats(Stats),
  {reply, ok, State}.

handle_win(State) ->
  print_win(),
  {noreply, stop_session(State)}.

handle_killed(State) ->
  print_killed(),
  {noreply, stop_session(State)}.

handle_log(Message, State = #state{session = #session{name = Name}}) ->
  print_log(Name, Message),
  {noreply, State}.

handle_process_down({_, _, _, Pid, Reason}, State = #state{session = #session{name = Name, pid = Pid}}) ->
  io:format("Disconnected from game ~p: ~p~n", [Name, Reason]),
  {noreply, stop_session(State)};

handle_process_down(Message, State) ->
  io:format("Unknown process down: ~p~n", [Message]),
  {noreply, State}.

handle_destroy(State = #state{session = undefined}) ->
  {stop, normal, ok, State};

handle_destroy(State) ->
  case leave_game(State) of
    NewState = #state{} ->
      {stop, normal, ok, NewState};
    Error ->
      {stop, Error, State}
  end.

%% ---------------------------------------------------------------
%% Private Functions
%% ---------------------------------------------------------------

game_alias(#session{node = Node, name = Name}) ->
  {Name, Node}.

start_session(Pid, Node, Name, State) ->
  Monitor = erlang:monitor(process, Pid),
  State#state{session = #session{node = Node, name = Name, pid = Pid, monitor = Monitor}}.

stop_session(State = #state{session = #session{monitor = Monitor}}) ->
  erlang:demonitor(Monitor, [flush]),
  State#state{session = undefined}.

join_game(Node, Name, Nickname, State) ->
  case net_kernel:connect_node(Node) of
    true ->
      case (catch game:join({Name, Node}, Nickname)) of
        {ok, Pid} ->
          start_session(Pid, Node, Name, State);
        {'EXIT', {noproc, _}} ->
          {error, invalid_game};
        Error ->
          Error
      end;
    _ ->
      {error, connection_error}
  end.

leave_game(State = #state{session = Session}) ->
  NewState = stop_session(State),
  case game:leave(game_alias(Session)) of
    ok -> NewState;
    Error -> Error
  end.

print_win() ->
  io:format("You win!!! Game over.~n").

print_killed() ->
  io:format("You have been killed!!! Game over.~n").

print_log(Name, Message) ->
  io:format("~s: ~s~n", [Name, Message]).

print_map(Map) ->
  lists:foreach(fun(Row) -> io:format("~s~n", [Row]) end, Map).

print_stats(Stats) ->
  stats:print(fun io:format/2, Stats).
