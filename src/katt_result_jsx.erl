%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright 2012- Klarna AB
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
%%% @copyright 2012- Klarna AB, AUTHORS
%%%
%%% @doc Klarna API Testing Tool Result to JSX conversion
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
%% @private
-module(katt_result_jsx).

%%%_* Exports ==================================================================
%% API
-export([ run_result_to_jsx/1
        , value_to_jsx/1
        , maybe_json_string/1
        ]).

%%%_* Internals ================================================================

run_result_to_jsx({error, Reason, Details}) ->
  [ {error, true}
  , {reason, Reason}
  , {details, list_to_binary(katt_util:erl_to_list(Details))}
  ];
run_result_to_jsx({ PassOrFail
                  , ScenarioFilename
                  , Params
                  , FinalParams
                  , TransactionResults0
                  }) ->
  TransactionResults = lists:map( fun transaction_result_to_jsx/1
                                , TransactionResults0
                                ),
  [ {status, PassOrFail}
  , {scenario, list_to_binary(ScenarioFilename)}
  , {params, proplist_to_jsx(Params)}
  , {final_params, proplist_to_jsx(FinalParams)}
  , {transaction_results, TransactionResults}
  ].

%%%_* Internal =================================================================

maybe_json_string(Str) when is_binary(Str) orelse is_list(Str) ->
  insert_escape_quotes(Str);
maybe_json_string(X) ->
  X.

insert_escape_quotes(Str) when is_binary(Str) ->
  katt_util:to_utf8(insert_escape_quotes(katt_util:from_utf8(Str)));
insert_escape_quotes(Str) when is_list(Str) ->
  "\"" ++ Str ++ "\"".

proplist_to_jsx([]) ->
  [{}];
proplist_to_jsx(Proplist) ->
  [{maybe_list_to_binary(K), maybe_list_to_binary(V)} || {K, V} <- Proplist].

maybe_list_to_binary(Str) when is_list(Str) ->
  list_to_binary(Str);
maybe_list_to_binary({struct, _} = Structured) ->
  value_to_jsx(Structured);
maybe_list_to_binary({array, _} = Structured) ->
  value_to_jsx(Structured);
maybe_list_to_binary(NonStr) ->
  NonStr.

transaction_result_to_jsx({ Description
                          , Params
                          , Request
                          , Response
                          , Result
                          }) ->
  {katt_request, Method, Url, ReqHeaders, ReqBody, ReqParsedBody} = Request,
  {Status, ResHeaders, ResBody, ResParsedBody} =
    case Response of
      {error, ResBody0} ->
        {500, [], atom_to_binary(ResBody0, utf8), null};
      {katt_response, Status0, ResHeaders0, ResBody0, ResParsedBody0} ->
        {Status0, ResHeaders0, ResBody0, ResParsedBody0}
  end,
  Errors = case Result of
             pass ->
               [];
             {error, Reason} ->
               [[{reason, Reason}]];
             {error, Reason, Details} ->
               [ {reason, Reason}
               , {details, Details}
               ];
             {fail, {error, Reason}} ->
               [[{reason, Reason}]];
             {fail, Failures0} ->
               lists:map( fun transaction_failure_to_jsx/1
                        , Failures0
                        )
           end,
  MaybeErrors = case Errors of
                  [] ->
                    [];
                  _ ->
                    [{errors, Errors}]
                end,
  [ {description, Description}
  , {params, proplist_to_jsx(Params)}
  , {request, [ {method, list_to_binary(Method)}
              , {url, list_to_binary(Url)}
              , { headers
                , proplist_to_jsx(ReqHeaders)
                }
              , {body, value_to_jsx(ReqBody)}
              , {parsed_body, value_to_jsx(ReqParsedBody)}
              ]}
  , {response, [ {status, Status}
               , { headers
                 , proplist_to_jsx(ResHeaders)
                 }
               , {body, value_to_jsx(ResBody)}
               , {parsed_body, value_to_jsx(ResParsedBody)}
               ]}
  ] ++ MaybeErrors.

transaction_failure_to_jsx({Reason, {Key0, Expected0, Actual0}}) ->
  Key = list_to_binary(Key0),
  Expected = value_to_jsx(Expected0),
  Actual = value_to_jsx(Actual0),
  [ {reason, Reason}
  , {key, Key}
  , {expected, Expected}
  , {actual, Actual}
  ];
transaction_failure_to_jsx({ Reason
                           , {Key0, Expected0, Actual0, Details}
                           }) ->
  Key = list_to_binary(Key0),
  Expected = value_to_jsx(Expected0),
  Actual = value_to_jsx(Actual0),
  Props = [ {reason, Reason}
          , {key, Key}
          , {expected, Expected}
          , {actual, Actual}
          ],
  TextDiff = proplists:get_value( text_diff
                                , Details
                                ),
  Props ++ text_diff_to_props(TextDiff).

text_diff_to_props(undefined) ->
  [];
text_diff_to_props(TextDiff) ->
  [{ text_diff
   , lists:map( fun({K, V}) ->
                    [{K, list_to_binary(V)}]
                end
              , TextDiff
              )
   }].

value_to_jsx({struct, []}) ->
  [{}];
value_to_jsx({struct, PropList}) ->
  lists:map( fun ({K, V}) ->
                 {value_to_jsx(K), value_to_jsx(V)}
             end
           , PropList
           );
value_to_jsx([{_, _}|_] = PropList) ->
  lists:map( fun ({K, V}) ->
                 {value_to_jsx(K), value_to_jsx(V)}
             end
           , PropList
           );
value_to_jsx({array, List}) ->
  lists:map( fun ({_K, V}) ->
                 value_to_jsx(V)
             end
           , List
           );
value_to_jsx(List) when is_list(List) ->
  try
    list_to_binary(List)
  catch
    _:_ ->
      lists:map( fun ({_K, V}) ->
                     value_to_jsx(V);
                     (V) ->
                     value_to_jsx(V)
                 end
               , List
               )
  end;
value_to_jsx(Value) when is_integer(Value) ->
  Value;
value_to_jsx(Value) when is_float(Value) ->
  Value;
value_to_jsx(Value) when not is_binary(Value) ->
  list_to_binary(katt_util:erl_to_list(Value));
value_to_jsx(Value) ->
  Value.
