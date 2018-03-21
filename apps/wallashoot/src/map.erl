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
  width/1,
  height/1,
  is_position_in_bounds/2,
  get_object_position/2,
  set_object_position/3,
  get_object_at/3,
  delete_object/2
]).

-record(map, {
  bounds  :: {non_neg_integer(), non_neg_integer()},
  obj_pos :: map(),
  pos_obj :: map()
}).

-type game_map() :: #map{}.

-define(IS_POSITION_IN_BOUNDS(X, Y, Width, Height), X > 0 andalso X =< Width andalso Y > 0 andalso Y =< Height).

%% ---------------------------------------------------------------
%% API functions
%% ---------------------------------------------------------------

-spec new(Width :: non_neg_integer(), Height :: non_neg_integer()) -> game_map().
new(Width, Height) ->
  #map{
    bounds = {Width, Height},
    obj_pos = #{},
    pos_obj = #{}
  }.

-spec width(Map :: game_map()) -> non_neg_integer().
width(#map{bounds = {Width, _}}) -> Width.

-spec height(Map :: game_map()) -> non_neg_integer().
height(#map{bounds = {_, Height}}) -> Height.

is_position_in_bounds({X, Y}, #map{bounds = {Width, Height}}) ->
  X > 0 andalso X =< Width andalso Y > 0 andalso Y =< Height.

-spec get_object_position(Object :: any(), Map :: game_map()) -> position().
get_object_position(Object, #map{obj_pos = ObjPos}) ->
  maps:get(Object, ObjPos).

-spec set_object_position(Position :: position(), Object :: any(), Map :: game_map()) -> game_map().
set_object_position(NewPosition, Object, Map = #map{obj_pos = ObjPos, pos_obj = PosObj}) ->
  check_position(NewPosition, Map),

  PosObjWithoutCurrentPosition = case maps:get(Object, ObjPos, undefined) of
    undefined -> PosObj;
    CurrentPosition -> maps:remove(CurrentPosition, PosObj)
  end,

  NewPosObj = maps:put(NewPosition, Object, PosObjWithoutCurrentPosition),
  NewObjPos = maps:put(Object, NewPosition, ObjPos),
  Map#map{obj_pos = NewObjPos, pos_obj = NewPosObj}.

-spec get_object_at(Position :: position(), Map :: game_map(), Default :: term()) -> any().
get_object_at(Position, Map = #map{pos_obj = PosObj}, Default) ->
  check_position(Position, Map),
  maps:get(Position, PosObj, Default).

-spec delete_object(Object :: any(), Map :: game_map()) -> position().
delete_object(Object, Map = #map{obj_pos = ObjPos, pos_obj = PosObj}) ->
  Position = map:get_object_position(Object, Map),
  Map#map{obj_pos = maps:remove(Object, ObjPos), pos_obj = maps:remove(Position, PosObj)}.

check_position(Position, Map) ->
  case is_position_in_bounds(Position, Map) of
    true -> ok;
    _ -> erlang:error(position_out_of_bounds)
  end.