%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright 2014- AUTHORS
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @copyright 2014- AUTHORS
%%%
%%% @doc Klarna API Testing Tool
%%%
%%% CLI.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(katt_cli).

%%%_* Exports ==================================================================
%% API
-export([main/1]).

%%%_* API ======================================================================

-ifdef(BARE_MODE).
main(_) ->
  throw(bare_mode).
-else.

main([]) ->
  main(["--help"]);
main(["-h"]) ->
  main(["--help"]);
main(["--help"]) ->
  io:fwrite( "Usage: ~s [--json] [--all] [param=string] [param:=non_string] -- "
             "file.katt [file.katt] ~n"
           , [escript:script_name()]
           ),
  katt_har_cli:help();
main(Options) ->
  main(Options, [], [], []).

main(["from-har"|Rest], [], [], []) ->
  katt_har_cli:main(Rest);

%%%_* Internal =================================================================

main(["--json"|Rest], Options, [], []) ->
  main(Rest, [{json, true}|Options], [], []);

main(["--all"|Rest], Options, [], []) ->
  main(Rest, [{all, true}|Options], [], []);

main(["--latency"|Rest], Options, [], []) ->
  main(Rest, [{latency, true}, {all, true}|Options], [], []);

main(["--"|ScenarioFilenames], Options0, Params0, []) ->
  Params = parse_params(Params0),
  Options =
    case proplists:get_value("stress_tasks", Params, 1) > 1 of
      true ->
        [{all, true}|Options0];
      _ ->
        Options0
    end,
  ensure_all_started(),
  run(Options, Params, ScenarioFilenames);
main([Param|Rest], Options, Params, []) ->
  main(Rest, Options, [Param|Params], []).

run(_Options, _Params, []) ->
  ok;
run(Options, Params0, [ScenarioFilename|ScenarioFilenames]) ->
  {Passed, NewParams, KattResults} = katt_run(ScenarioFilename, Params0),
  flush_results(KattResults, Options),
  case Passed of
    true ->
      run(Options, NewParams, ScenarioFilenames);
    false ->
      %% init:stop not setting status code correctly
      %% init:stop(1)
      halt(1)
  end.

katt_run(ScenarioFilename, Params) ->
  KattResults0 =
    katt:run( ScenarioFilename
            , Params
            , [{ progress
               , fun(Step, Detail) ->
                     io:fwrite( standard_error
                              , "\n== PROGRESS REPORT ~p ==\n~p\n\n~p\n\n"
                              , [Step, erlang:localtime(), Detail])
                 end
               }]),
  KattResults =
    case is_list(KattResults0) of
      true ->
        KattResults0;
      false ->
        [KattResults0]
    end,
  case KattResults of
    [{pass, _, _, NewParams , _}] ->
      {true, NewParams, KattResults};
    _ ->
      {false, [], KattResults}
  end.

flush_results(KattResults0, Options) ->
  All = proplists:get_value(all, Options, false),
  Json = proplists:get_value(json, Options, false),
  Latency = proplists:get_value(latency, Options, false),

  KattResults = maybe_only_last_transaction(KattResults0, not All),
  case {Latency, Json} of
    {false, false} ->
      io:fwrite("~p\n\n", [KattResults]);
    {false, true} ->
      JsonResults = lists:map( fun katt_util:run_result_to_jsx/1
                             , KattResults
                             ),
      Results = jsx:encode(JsonResults),
      io:fwrite("~s\n\n", [Results]);
    {true, _} ->
      LatencyResults =
        lists:map( fun(KattResult) ->
                       IntResult = katt_util:run_result_to_latency(KattResult),
                       Result = lists:map(fun katt_util:to_list/1, IntResult),
                       string:join(Result, ", ")
                   end
                 , KattResults
                 ),
      io:fwrite("~s\n\n", [string:join(LatencyResults, "\n")])
  end.

maybe_only_last_transaction(KattResults, false) ->
  KattResults;
maybe_only_last_transaction(KattResults, true) ->
  lists:map( fun({ PassOrFail
                 , ScenarioFilename
                 , Params
                 , FinalParams
                 , TransactionResults
                 }) ->
                 { PassOrFail
                 , ScenarioFilename
                 , Params
                 , FinalParams
                 , [lists:last(TransactionResults)]
                 }
             end
           , KattResults
           ).

parse_params(Params) ->
  parse_params(Params, []).

parse_params([], Acc) ->
  lists:reverse(Acc);
parse_params([Param|Params], Acc) ->
  {Key, Value} = parse_param(Param),
  parse_params(Params, [{Key, Value}|Acc]).

parse_param(Param) ->
  parse_param(Param, []).

parse_param(":=" ++ _Value, []) ->
  throw({error, invalid_key});
parse_param("=" ++ _Value, []) ->
  throw({error, invalid_key});
parse_param(":=" ++ Value, Key) ->
  {lists:reverse(Key), convert(Value)};
parse_param("=" ++ Value, Key) ->
  {lists:reverse(Key), Value};
parse_param([Char|Rest], Key) ->
  parse_param(Rest, [Char|Key]).

convert(Value) ->
  try_to_convert_to([null, integer, float, boolean], Value).

try_to_convert_to([], _Value) ->
  throw({error, unknown_param_type});
try_to_convert_to([null|Rest], Value0) ->
  Value = string:to_lower(Value0),
  case Value of
    "null" ->
      null;
    _ ->
      try_to_convert_to(Rest, Value)
  end;
try_to_convert_to([integer|Rest], Value) ->
  case string:to_integer(Value) of
    {error, _} ->
      try_to_convert_to(Rest, Value);
    {IntValue, []} ->
      IntValue;
    _ ->
      try_to_convert_to(Rest, Value)
  end;
try_to_convert_to([float|Rest], Value) ->
  case string:to_float(Value) of
    {error, _} ->
      try_to_convert_to(Rest, Value);
    {FloatValue, []} ->
      FloatValue;
    _ ->
      try_to_convert_to(Rest, Value)
  end;
try_to_convert_to([boolean|Rest], Value0) ->
  Value = string:to_lower(Value0),
  case Value of
    "true" ->
      true;
    "false" ->
      false;
    _ ->
      try_to_convert_to(Rest, Value0)
  end.

ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.

ensure_all_started() ->
  %% Don't use application:ensure_all_started(katt)
  %% nor application:ensure_started(_)
  %% in order to maintain compatibility with R16B01 and lower
  ok = ensure_started(crypto),
  ok = ensure_started(asn1),
  ok = ensure_started(public_key),
  ok = ensure_started(ssl),

  ok = ensure_started(jsx),

  ok = ensure_started(idna),
  ok = ensure_started(mimerl),
  ok = ensure_started(certifi),
  ok = ensure_started(metrics),
  ok = ensure_started(ssl_verify_fun),
  ok = ensure_started(hackney),

  ok = ensure_started(tdiff),

  ok = ensure_started(katt).

-endif.
