%%%-------------------------------------------------------------------
%%% @author vavaka
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-define(APP_NAME, wallashoot).

-type game_options() :: {Name :: atom(), Width :: non_neg_integer(), Height :: non_neg_integer(), MaxPlayers :: non_neg_integer()}.

-type direction() :: left | right | up | down | up_left | up_right | down_left | down_right.

-type position() :: {X :: non_neg_integer(), Y :: non_neg_integer()}.

-type server_ref() :: atom() | {Name :: atom(), Node :: atom()} | pid().

-type error() :: {error, Reason :: any()}.

-type validation_result() :: valid | {invalid, Reason :: any()}.