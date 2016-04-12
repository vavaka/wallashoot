%%%-------------------------------------------------------------------
%%% @author vavaka
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(map).

-include("wallashoot.hrl").

%% API
-export([
  new/2,
  get/2,
  set/3,
  validate_position/2,
  validate_direction/1,
  next_position/2,
  random_position/1,
  dump/3
]).

-record(map, {
  width   :: non_neg_integer(),
  height  :: non_neg_integer(),
  surface :: array:array()
}).

-type game_map() :: #map{}.

%% ---------------------------------------------------------------
%% API functions
%% ---------------------------------------------------------------

-spec new(Width :: non_neg_integer(), Height :: non_neg_integer()) -> game_map().
new(Width, Height) ->
  #map{
    width = Width,
    height = Height,
    surface = array:new(Height, [{default, array:new(Width, [{default, free}])}])
  }.

-spec get(Position :: position(), Map :: game_map()) -> any().
get(Position = {X, Y}, Map = #map{surface = Surface}) ->
  case validate_position(Position, Map) of
    {invalid, Reason} ->
      {error, {Reason, Position}};
    valid ->
      Row = array:get(Y, Surface),
      array:get(X, Row)
  end.

-spec set(Position :: position(), Value :: any(), Map :: game_map()) -> game_map().
set(Position = {X, Y}, Value, Map = #map{surface = Surface}) ->
  case validate_position(Position, Map) of
    {invalid, Reason} ->
      {error, {Reason, Position}};
    valid ->
      Row = array:get(Y, Surface),
      NewRow = array:set(X, Value, Row),
      array:set(X, NewRow, Surface),
      Map#map{surface = array:set(Y, NewRow, Surface)}
  end.

-spec validate_position(Position :: position(), Map :: game_map()) -> validation_result().
validate_position({X, Y}, #map{width = W, height = H}) when X < 0 orelse X >= W orelse Y < 0 orelse Y >= H ->
  {invalid, position_out_of_range};
validate_position(_Position, _Map) ->
  valid.

-spec validate_direction(Direction :: direction()) -> validation_result().
validate_direction(Direction) ->
  case next_position(Direction, {0, 0}) of
    {_X, _Y} ->
      valid;
    Error ->
      {invalid, Error}
  end.

-spec next_position(Direction :: direction(), Position :: position()) -> position() | invalid_direction | invalid_position.
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
  invalid_direction;
next_position(_Direction, _) ->
  invalid_position.

-spec random_position(Map :: game_map()) -> position().
random_position(#map{width = Width, height = Height}) ->
  {random:uniform(Width) - 1, random:uniform(Height) - 1}.

-spec dump(Encoding :: orddict:orddict(), Default :: string(), Map :: game_map()) -> list().
dump(Encoding, Default, #map{surface = Surface}) ->
  MapResult = array:foldl(fun(_Index1, Row, Acc) ->
    RowResult = array:foldl(fun(_Index2, Value, Acc2) ->
      Symbol = case orddict:find(Value, Encoding) of
        error -> Default;
        {ok, S} -> S
      end,
      [Symbol | Acc2]
    end, [], Row),
    [string:join(lists:reverse(RowResult), " ") | Acc]
  end, [], Surface),
  lists:reverse(MapResult).
