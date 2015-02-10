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

main([]) ->
    main(["--help"]);
main(["-h"]) ->
    main(["--help"]);
main(["--help"]) ->
    io:fwrite( "Usage: ~s [--json] file.katt param=string param:=non_string ~n"
             , [escript:script_name()]
             );
main(["--json", ScenarioFilename|Params]) ->
    KattResult = katt_run(ScenarioFilename, Params),
    JsonResult = katt_util:run_result_to_mochijson3(KattResult),
    Result = iolist_to_binary(mochijson3:encode(JsonResult)),
    io:fwrite("~s\n\n", [Result]),
    case KattResult of
        {pass, _, _, _ , _} ->
            ok;
        _ ->
            %% init:stop not setting status code correctly
            %% init:stop(1)
            halt(1)
    end;
main([ScenarioFilename|Params]) ->
    KattResult = katt_run(ScenarioFilename, Params),
    io:fwrite("~p\n\n", [KattResult]),
    case KattResult of
        {pass, _, _, _ , _} ->
            ok;
        _ ->
            %% init:stop not setting status code correctly
            %% init:stop(1)
            halt(1)
    end.

%%%_* Internal =================================================================

katt_run(ScenarioFilename, Params0) ->
    Params = parse_params(Params0),
    %% Don't use application:ensure_all_started(katt)
    %% in order to maintain compatibility with R16B01 and lower
    ok = application:ensure_started(mochijson3),
    ok = application:ensure_started(crypto),
    ok = application:ensure_started(asn1),
    ok = application:ensure_started(public_key),
    ok = application:ensure_started(ssl),
    ok = application:ensure_started(lhttpc),
    ok = application:ensure_started(katt),
    katt:run( ScenarioFilename
            , Params
            , [{ progress
               , fun(Step, Detail) ->
                         io:fwrite( standard_error
                                  , "\n== PROGRESS REPORT ~p ==\n~p\n\n~p\n\n"
                                  , [Step, erlang:localtime(), Detail])
                 end
               }]).

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
