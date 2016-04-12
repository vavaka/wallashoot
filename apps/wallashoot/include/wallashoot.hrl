%%%-------------------------------------------------------------------
%%% @author vavaka
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-define(APP_NAME, wallashoot).

-type direction() :: left | right | up | down | up_left | up_right | down_left | down_right.

-type coordinate() :: non_neg_integer().
-type position() :: {coordinate(), coordinate()}.

-type server_ref() :: atom() | {atom(), atom()} | pid().

-type error() :: {error, Reason :: any()}.

-type validation_result() :: valid | {invalid, Reason :: any()}.