-module(katt_compare_as_set).

-include("katt.hrl").

%% API
-export([compare/5]).

-spec compare(string(), list(), list(), any(), list()) -> pass | list().
compare(Key, Data, Actual0, _Unexpected, Callbacks) ->
  {struct, Expected0} = proplists:get_value("value", Data),
  Unexpected = proplists:get_value(?MATCH_ANY, Expected0, ?MATCH_ANY),
  Expected1 = proplists:delete(?MATCH_ANY, Expected0),
  Expected = lists:keysort(2, Expected1),
  Actual = lists:keysort(2, Actual0),
  check_intersection(Key, Expected, Actual, Unexpected, [], Callbacks).

-spec check_intersection(string(), list(), list(), any(), list(), list()) -> pass | list().
check_intersection(_Key, [] = _Expected,  [] = _Actual, _Unexpected, ErrorsSoFar, _Callbacks) ->
  %%   No more items in any of the expected / actual arrays
  case ErrorsSoFar of
    [] ->
      pass;
    _ ->
      ErrorsSoFar
  end;
check_intersection(Key, [], _Actual, ?MATCH_ANY, ErrorsSoFar, Callbacks) ->
  %%   All expected elements have been consumed, and we allow extra actual elements
  check_intersection(Key, [], [], ?MATCH_ANY, ErrorsSoFar, Callbacks);
check_intersection(Key, [], Actual, ?UNEXPECTED, ErrorsSoFar, _Callbacks) ->
  %%   All expected elements have been consumed, and we do not allow extra actual elements
  [{unexpected,  Key++"/"++NoA} || {NoA, _Element} <- Actual] ++ ErrorsSoFar;
check_intersection(Key, Expected, [], _Unexpected, ErrorsSoFar, _Callbacks) ->
  %%   All actual elements have been consumed, and there are still expected elements
  [{not_contains,  Key++"/"++NoE} || {NoE, _Element} <- Expected] ++ ErrorsSoFar;
check_intersection(Key, [{ENo, E1} | ERest], [{ANo, A1}| ARest], Unexpected, ErrorsSoFar, Callbacks) ->
  %%   There is at least one expected and one actual element
  case {estimate(E1, A1), Unexpected} of
    {equal, _} ->
      check_intersection(Key, ERest, ARest, Unexpected, ErrorsSoFar, Callbacks);
    {smaller, _} ->
      check_intersection(Key, ERest, [{ANo, A1}| ARest], Unexpected, [{not_contains, Key++"/"++ENo}] ++ ErrorsSoFar, Callbacks);
    {bigger, ?MATCH_ANY} ->
      check_intersection(Key, [{ENo, E1} | ERest], ARest, Unexpected, ErrorsSoFar, Callbacks);
    {bigger, ?UNEXPECTED} ->
      check_intersection(Key, [{ENo, E1} | ERest], ARest, Unexpected, [{unexpected, Key++"/"++ANo}] ++ ErrorsSoFar, Callbacks)
  end.

estimate(E1, A1) ->
  if
    E1 == A1 ->
      equal;
    E1 < A1 ->
      smaller;
    true ->
      bigger
  end.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
