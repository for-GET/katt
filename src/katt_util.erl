%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright 2013 Klarna AB
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
%%% @copyright 2013 Klarna AB
%%%
%%% @doc Klarna API Testing Tool Utils
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
%% @private
-module(katt_util).

%%%_* Exports ==========================================================
%% API
-export([ merge_proplists/2
        , to_list/1
        , from_utf8/1
        , to_utf8/1
        , to_lower/1
        , escape_regex/1
        , maybe_json_string/1
        , run_result_to_mochijson3/1
        ]).

%%%_* API ==============================================================

%% Merge two proplists. If a property exists in both List1 and List2, then the
%% value from List2 is used.
merge_proplists(List1, List2) ->
  orddict:merge( fun(_K, _V1, V2) -> V2 end
               , orddict:from_list(List1)
               , orddict:from_list(List2)
               ).

to_list(X) when is_atom(X)    -> atom_to_list(X);
to_list(X) when is_integer(X) -> integer_to_list(X);
to_list(X) when is_float(X)   -> my_float_to_list(X);
to_list(X) when is_list(X)    -> X;
to_list(X) when is_binary(X)  -> binary_to_list(X).

%% Transform (possibly utf8 encoded) binary to list, ignore everything else.
from_utf8(X) when is_binary(X) ->
  case unicode:characters_to_list(X, utf8) of
    R when is_list(R)  -> R;
    {error, _, _}      -> binary_to_list(X);
    {incomplete, _, _} -> binary_to_list(X)
  end.

%% Transform list to utf8 encoded binary, ignore everything else
to_utf8(X) when is_list(X) -> unicode:characters_to_binary(X, utf8).

to_lower(X) when is_list(X) -> string:to_lower(X).

escape_regex(Other) when not is_list(Other) andalso not is_binary(Other) ->
  to_list(Other);
escape_regex(Bin0) ->
  Bin = to_list(Bin0),
  to_list(re:replace(
    Bin,
    "[\\-\\[\\]\\/\\{\\}\\(\\)\\*\\+\\?\\.\\,\\\\\^\\$\\|\\#\\s\\&]",
    "\\\\&",
    [global])).

maybe_json_string(Str) when is_binary(Str) orelse is_list(Str) ->
  to_utf8(insert_escape_quotes(Str));
maybe_json_string(X) ->
  X.

insert_escape_quotes(Str) when is_binary(Str) ->
  insert_escape_quotes(to_list(Str));
insert_escape_quotes(Str) when is_list(Str) ->
  "\"" ++ Str ++ "\"".

run_result_to_mochijson3({error, Reason, Details}) ->
  {struct, [ {error, true}
           , {reason, Reason}
           , {details, list_to_binary(io_lib:format("~p", [Details]))}
           ]};
run_result_to_mochijson3({ PassOrFail
                   , ScenarioFilename
                   , Params
                   , FinalParams
                   , TransactionResults0
                   }) ->
  TransactionResults = lists:map( fun transaction_result_to_mochijson3/1
                                , TransactionResults0
                                ),
  {struct, [ {status, PassOrFail}
           , {scenario, list_to_binary(ScenarioFilename)}
           , {params, {struct, proplist_to_mochijson3(Params)}}
           , {final_params, {struct, proplist_to_mochijson3(FinalParams)}}
           , {transaction_results, TransactionResults}
           ]}.

%%%_* Internal =========================================================

my_float_to_list(X) when is_float(X) ->
  my_float_to_list(X, 0).
my_float_to_list(X, Decimals) when is_float(X) ->
  Multiplier = trunc(math:pow(10, Decimals)),
  X1 = X * Multiplier,
  X2 = trunc(X1) + 0.0,
  case X1 =:= X2 of
    true ->
      String = integer_to_list(trunc(X1)),
      Number = [ string:sub_string(String, 1, Decimals)
               , string:sub_string(String, min( Decimals + 1
                                              , string:len(String) + 1
                                              ))
               ],
      string:join(Number, ".");
    false ->
      Decimals1 = Decimals + 1,
      my_float_to_list(X, Decimals1)
  end.

proplist_to_mochijson3(Proplist) ->
  [{K, maybe_list_to_binary(V)} || {K, V} <- Proplist].

maybe_list_to_binary(Str) when is_list(Str) ->
  list_to_binary(Str);
maybe_list_to_binary(NonStr) ->
  NonStr.

transaction_result_to_mochijson3({Description, Params, Request, Response, Result}) ->
  {katt_request, Method, Url, ReqHeaders, ReqBody} = Request,
  {katt_response, Status, ResHeaders, ResBody, _ResParsedBody} = Response,
  Errors = case Result of
             pass ->
               [];
             {fail, Failures0} ->
               lists:map( fun transaction_failure_to_mochijson3/1
                        , Failures0
                        )
           end,
  {struct, [ {description, Description}
           , {params, {struct, proplist_to_mochijson3(Params)}}
           , {request, {struct, [ {method, list_to_binary(Method)}
                                , {url, list_to_binary(Url)}
                                , {headers, {struct, proplist_to_mochijson3(ReqHeaders)}}
                                , {body, ReqBody}
                                ]}}
           , {response, {struct, [ {status, Status}
                                 , {headers, {struct, proplist_to_mochijson3(ResHeaders)}}
                                 , {body, ResBody}
                                 ]}}
           , {errors, Errors}
           ]}.

transaction_failure_to_mochijson3({Reason, {Key0, Expected0, Actual0}}) ->
  Key = list_to_binary(Key0),
  Expected = list_to_binary(io_lib:format("~p", [Expected0])),
  Actual = list_to_binary(io_lib:format("~p", [Actual0])),
  {struct, [ {reason, Reason}
           , {key, Key}
           , {expected, Expected}
           , {actual, Actual}
           ]}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
