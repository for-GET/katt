%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright 2012- Klarna AB
%%% Copyright 2014- See AUTHORS
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
%%% @copyright 2012- Klarna AB
%%% @copyright 2014- See AUTHORS
%%%
%%% @doc Klarna API Testing Tool Utils
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
%% @private
-module(katt_util).

%%%_* Exports ==================================================================
%% API
-export([ merge_proplists/2
        , to_list/1
        , from_utf8/1
        , to_utf8/1
        , to_lower/1
        , escape_regex/1
        , maybe_json_string/1
        , run_result_to_mochijson3/1
        , is_valid/3
        , validate/3
        , is_valid/5
        , validate/5
        , enumerate/1
        ]).

%%%_* Includes =================================================================
-include("katt.hrl").

%%%_* API ======================================================================

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
to_list(X) when is_binary(X)  -> binary_to_list(X);
to_list(X) when is_list(X)    -> X.

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

%%%_* Internal =================================================================

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

transaction_result_to_mochijson3({ Description
                                 , Params
                                 , Request
                                 , Response
                                 , Result
                                 }) ->
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
                                , { headers
                                  , { struct
                                    , proplist_to_mochijson3(ReqHeaders)
                                    }
                                  }
                                , {body, ReqBody}
                                ]}}
           , {response, {struct, [ {status, Status}
                                 , { headers
                                   , { struct
                                     , proplist_to_mochijson3(ResHeaders)
                                     }
                                   }
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

is_valid(ParentKey, E, A) ->
  case validate(ParentKey, E, A) of
    {pass, _} ->
      true;
    _ ->
      false
  end.

is_valid(ParentKey, E, A, Unexpected, Callbacks) ->
  case validate(ParentKey, E, A, Unexpected, Callbacks) of
    {pass, _} ->
      true;
    _ ->
      false
  end.

validate(ParentKey, E, A) ->
  validate_primitive(ParentKey, E, A).

%% Expected actual
validate(_ParentKey, _E, _E, _Unexpected, _Callbacks) ->
  {pass, []};
%% Expected anything
validate(_ParentKey, ?MATCH_ANY = _E, _A, _Unexpected, _Callbacks) ->
  {pass, []};
%% Expected struct, got struct
validate( ParentKey
        , {struct, EItems} = _E
        , {struct, AItems} = _A
        , Unexpected
        , Callbacks
        ) ->
  case proplists:get_value("{{type}}", EItems) of
    undefined ->
      validate( ParentKey
              , {json_struct, EItems}
              , {json_struct, AItems}
              , Unexpected
              , Callbacks
              );
    Type ->
      EItems1 = proplists:delete("{{type}}", EItems),
      katt_callbacks:validate_type( Type
                                  , ParentKey ++ "/{{" ++ Type ++ "}}"
                                  , EItems1
                                  , AItems
                                  , Unexpected
                                  , Callbacks
                                  )
  end;
%% Expected JSON array/object, got JSON array/object
validate( ParentKey
        , {json_struct, EItems0} = _E
        , {json_struct, AItems} = _A
        , _Unexpected
        , Callbacks
        ) ->
  Unexpected = proplists:get_value(?MATCH_ANY, EItems0, ?MATCH_ANY),
  EItems = proplists:delete(?MATCH_ANY, EItems0),
  Keys = lists:usort([ Key
                       || {Key, _} <- lists:merge(EItems, AItems)
                     ]),
  [ validate( ParentKey ++ "/" ++ Key
            , proplists:get_value(Key, EItems)
            , proplists:get_value(Key, AItems)
            , Unexpected
            , Callbacks
            )
    || Key <- Keys
  ];
validate(ParentKey, E, A, Unexpected, _Callbacks) ->
  validate_simple(ParentKey, E, A, Unexpected).

%% Validate when unexpected values show up
%% Expected anything
validate_simple(_Key, undefined = _E, _A, ?MATCH_ANY) ->
  {pass, []};
%% validate_simple(_Key, [] = _E, _A, ?MATCH_ANY) ->
%%   {pass, []};
%% Not expected and undefined
validate_simple(_Key, ?UNEXPECTED = _E, undefined = _A, _Unexpected) ->
  {pass, []};
%% Not expected
validate_simple(Key, undefined = E, A, ?UNEXPECTED) ->
  {unexpected, {Key, E, A}};
%% Expected undefined
validate_simple(Key, undefined = _E, A, Unexpected) ->
  validate_primitive(Key, Unexpected, A);
%% Expected but undefined
validate_simple(Key, E, undefined = A, _Unexpected) ->
  {not_equal, {Key, E, A}};
%% Otherwise
validate_simple(Key, E, A, _Unexpected) ->
  validate_primitive(Key, E, A).

%% Validate JSON primitive types or empty structured types
validate_primitive(_Key, E, E) ->
  {pass, []};
validate_primitive(Key, E, A) when is_binary(A) ->
  validate_primitive(Key, E, from_utf8(A));
validate_primitive(Key, E, A) when is_binary(E) ->
  validate_primitive(Key, from_utf8(E), A);
validate_primitive(_Key, ?MATCH_ANY, _A) ->
  {pass, []};
validate_primitive(Key, E, A) when is_list(E) ->
  case re:run( E
             , "(" ++ ?STORE_BEGIN_TAG ++ "[^}]+" ++ ?STORE_END_TAG ++ ")"
             , [ global
               , {capture, all_but_first, list}
               ]
             ) of
    nomatch ->
      {not_equal, {Key, E, A}};
    {match, [[E]]} ->
        {pass, [{store_tag2param(E), A}]};
    {match, Params0} ->
      Type = if
               is_list(A) ->
                 list;
               is_binary(A) ->
                 binary
             end,
      Params = lists:map( fun([Match]) ->
                              store_tag2param(Match)
                          end
                        , Params0),
      RE0 = re:replace( E
                      , ?STORE_BEGIN_TAG ++ "[^}]+" ++ ?STORE_END_TAG
                      , "___store___"
                      , [global]
                      ),
      RE1 = re:replace( RE0
                      , "[\\-\\[\\]\\/\\{\\}\\(\\)\\*\\+"
                        ++ "\\?\\.\\,\\\\\^\\$\\|\\#\\s\\&]"
                      , "\\\\&"
                      , [global]
                      ),
      RE2 = re:replace( RE1
                      , "___store___"
                      , "(.+)"
                      , [global]
                      ),
      RE = ["^", RE2, "$"],
      case re:run(A, RE, [global, {capture, all_but_first, Type}]) of
        nomatch ->
          {not_equal, {Key, E, A}};
        {match, [Values]} ->
          {pass, lists:zip(Params, Values)}
      end
  end;
validate_primitive(Key, E, A) ->
  {not_equal, {Key, E, A}}.

store_tag2param(?STORE_BEGIN_TAG ++ Rest) ->
  Param = string:sub_string(Rest, 1, string:str(Rest, ?STORE_END_TAG) - 1),
  Param.

%% Transform simple list to proplist with keys named 0, 1 etc.
enumerate(L) ->
  lists:zip([ integer_to_list(N)
              || N <- lists:seq(0, length(L) - 1)
            ], L).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
