%%%-------------------------------------------------------------------
%%% @author vavaka
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(stats).

%% API
-export([
  print/2
]).

-spec print(PrintFunction :: function(), Stats :: list({Nickname :: string(), Frags :: non_neg_integer(), Deaths :: non_neg_integer()})) -> ok.
print(PrintFunction, Stats) ->
  SortedStats = lists:sort(fun({_, Frags1, _}, {_, Frags2, _}) ->
    Frags1 >= Frags2
  end, Stats),

  PrintFunction("~10s  ~10s  ~10s~n", ["Nickname", "Frags", "Deaths"]),
  lists:foreach(fun({Nickname, Frags, Deaths}) ->
    PrintFunction("~10s  ~10B  ~10B~n", [Nickname, Frags, Deaths])
  end, SortedStats).
