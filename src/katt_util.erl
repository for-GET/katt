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
        , run_result_to_jsx/1
        , is_valid/3
        , validate/3
        , is_valid/5
        , validate/5
        , enumerate/1
        , external_http_request/6
        , erl_to_list/1
        , os_cmd/2
        , blueprint_to_apib/1
        , is_json_content_type/1
        , get_params_and_failures/1
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

to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_integer(X) -> integer_to_list(X);
to_list(X) when is_float(X) -> io_lib:format("~p", [X]);
to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_list(X) -> X.

%% Transform (possibly utf8 encoded) binary to list, ignore everything else.
from_utf8(X) when is_binary(X) ->
  case unicode:characters_to_list(X, utf8) of
    R when is_list(R) -> R;
    {error, _, _} -> binary_to_list(X);
    {incomplete, _, _} -> binary_to_list(X)
  end.

%% Transform list to utf8 encoded binary, ignore everything else
to_utf8(X) when is_list(X) ->
  unicode:characters_to_binary(X, utf8).

to_lower(X) when is_list(X) ->
  string:to_lower(X).

escape_regex(Other) when not is_list(Other) andalso not is_binary(Other) ->
  to_list(Other);
escape_regex([H|_]=List) when is_tuple(H) ->
  List;
escape_regex(Str0) ->
  Str = to_list(Str0),
  re:replace( Str
            , "[\\-\\[\\]\\/\\{\\}\\(\\)\\*\\+\\?\\.\\,\\\\\^\\$\\|\\#\\s\\&]"
            , "\\\\&"
            , [{return, list}, global]
            ).

maybe_json_string(Str) when is_binary(Str) orelse is_list(Str) ->
  insert_escape_quotes(Str);
maybe_json_string(X) ->
  X.

insert_escape_quotes(Str) when is_binary(Str) ->
  to_utf8(insert_escape_quotes(from_utf8(Str)));
insert_escape_quotes(Str) when is_list(Str) ->
  "\"" ++ Str ++ "\"".

run_result_to_jsx({error, Reason, Details}) ->
  [ {error, true}
  , {reason, Reason}
  , {details, list_to_binary(erl_to_list(Details))}
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

-ifdef(BARE_MODE).
external_http_request(_Url, _Method, _Hdrs, _Body, _Timeout, _Options) ->
  {error, katt_bare_mode}.
-else.
external_http_request(Url, Method, Hdrs, Body, Timeout, Options0) ->
  BUrl = list_to_binary(Url),
  BHdrs = lists:map( fun({Name, Value})->
                         {list_to_binary(Name), list_to_binary(Value)}
                     end
                   , Hdrs
                   ),
  DefaultOpts = [ {recv_timeout, Timeout}
                , {insecure, true}
                ],
  Options = merge_proplists(DefaultOpts, Options0),
  case hackney:request(Method, BUrl, BHdrs, Body, Options) of
    OK when element(1, OK) =:= ok ->
      %% lhttpc was the predecessor of hackney
      %% and we're maintaining a backwards compatible return value
      {Status, BResHdrs, ResBody} = case OK of
                                      {ok, Status0, BResHdrs0, Client} ->
                                        {ok, ResBody0} = hackney:body(Client),
                                        {Status0, BResHdrs0, ResBody0};
                                      {ok, Status0, BResHdrs0} ->
                                        {Status0, BResHdrs0, <<>>}
                                    end,
      ResHdrs0 = lists:map( fun({Name, Value})->
                                {binary_to_list(Name), binary_to_list(Value)}
                            end
                          , BResHdrs
                          ),
      ResHdrs = lists:reverse(ResHdrs0),
      {ok, {{Status, ""}, ResHdrs, ResBody}};
    Error ->
      Error
  end.
-endif.

erl_to_list(Term) ->
  io_lib:format("~p", [Term]).

os_cmd(Cmd, Env) ->
  Opt = [ stream
        , exit_status
        , use_stdio
        , stderr_to_stdout
        , in
        , eof
        , hide
        , {env, Env}
        ],
  Port = open_port({spawn, Cmd}, Opt),
  os_cmd_result(Port, []).

blueprint_to_apib(Blueprint) ->
  io:fwrite( "--- ~s ---~n"
             "~n"
             "---~n"
             "---~n"
             "~n"
           , [Blueprint#katt_blueprint.name]
           ),
  lists:foreach( fun(#katt_transaction{ description = Description
                                      , request = Req
                                      , response = Res
                                      }) ->
                     io:fwrite("# ~s~n~n", [Description]),
                     io:fwrite( "~s ~s~n"
                                "~s"
                                "~s"
                              , [ Req#katt_request.method
                                , Req#katt_request.url
                                , headers_to_apib( Req#katt_request.headers
                                                 , "> "
                                                 )
                                , body_to_apib(Req#katt_request.body)
                                ]),
                     io:fwrite( "< ~B~n"
                                "~s"
                                "~s"
                              , [ Res#katt_response.status
                                , headers_to_apib( Res#katt_response.headers
                                                 , "< "
                                                 )
                                , body_to_apib(Res#katt_response.body)
                                ]),
                     io:fwrite("~n", [])
                 end
               , Blueprint#katt_blueprint.transactions).

is_json_content_type(Hdrs0) ->
  Hdrs = [{to_lower(K), V} || {K, V} <- Hdrs0],
  ContentType = proplists:get_value("content-type", Hdrs, ""),
  case re:run(ContentType, "^application/(.+\\+)?json") of
    nomatch -> false;
    {match, _} -> true
  end.

get_params_and_failures(Result) when not is_list(Result) ->
  get_params_and_failures([Result]);
get_params_and_failures(Result) ->
  lists:foldl(
    fun({pass, AddParams1}, {AddParams0, Failures0}) ->
        AddParams = merge_proplists(AddParams0, AddParams1),
        {AddParams, Failures0};
       (Failure, {AddParams0, Failures0}) ->
        {AddParams0, [Failure | Failures0]}
    end,
    {[], []},
    lists:flatten(Result)
   ).

%%%_* Internal =================================================================

proplist_to_jsx([]) ->
  [{}];
proplist_to_jsx(Proplist) ->
  [{maybe_list_to_binary(K), maybe_list_to_binary(V)} || {K, V} <- Proplist].

maybe_list_to_binary(Str) when is_list(Str) ->
  list_to_binary(Str);
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
value_to_jsx(Value) when not is_binary(Value) ->
  list_to_binary(erl_to_list(Value));
value_to_jsx(Value) ->
  Value.

is_valid(ParentKey, E, A) ->
  case validate(ParentKey, E, A) of
    {pass, _} ->
      true;
    _ ->
      false
  end.

is_valid(ParentKey, E, A, ItemsMode, Callbacks) ->
  Result = validate(ParentKey, E, A, ItemsMode, Callbacks),
  case get_params_and_failures(Result) of
    {_, []} ->
      true;
    _ ->
      false
  end.

validate(ParentKey, E, A) ->
  validate_primitive(ParentKey, E, A, []).

%% Expected anything
validate(_ParentKey, ?MATCH_ANY = _E, _A, _ItemsMode, _Callbacks) ->
  {pass, []};

%% Expected actual
validate(_ParentKey, E, E, _ItemsMode, _Callbacks) ->
  {pass, []};

%% Expected struct/array, got struct/array
validate( ParentKey
        , {Type, EItems} = _E
        , {Type, AItems} = _A
        , ItemsMode
        , Callbacks
        ) when Type =:= struct orelse
               Type =:= array ->
  validate_proplist( ParentKey
                   , EItems
                   , AItems
                   , ItemsMode
                   , Callbacks
                   );

%% Expected some Type
validate( ParentKey
        , {Type, Options} = _E
        , A
        , ItemsMode
        , Callbacks
        ) when Type =/= struct andalso
               Type =/= array ->
  katt_callbacks:validate_type( Type
                              , ParentKey ++ "/{{" ++ Type ++ "}}"
                              , Options
                              , A
                              , ItemsMode
                              , Callbacks
                              );

%% Expected something else
validate(ParentKey, E, A, ItemsMode, Callbacks) ->
  validate_simple(ParentKey, E, A, ItemsMode, Callbacks).

validate_proplist( ParentKey
                 , EItems0
                 , AItems
                 , _ItemsMode
                 , Callbacks
                 ) ->
  ItemsMode = proplists:get_value(?MATCH_ANY, EItems0, ?MATCH_ANY),
  EItems = proplists:delete(?MATCH_ANY, EItems0),
  Keys = lists:usort([ Key
                       || {Key, _} <- lists:merge(EItems, AItems)
                     ]),
  lists:map( fun(Key) ->
                 EValue = proplists:get_value(Key, EItems, ItemsMode),
                 case is_items_mode(EValue) of
                   true ->
                     validate( ParentKey ++ "/" ++ Key
                             , undefined
                             , proplists:get_value(Key, AItems)
                             , EValue
                             , Callbacks
                             );
                   false ->
                     validate( ParentKey ++ "/" ++ Key
                             , EValue
                             , proplists:get_value(Key, AItems)
                             , ItemsMode
                             , Callbacks
                             )
                 end
             end
           , Keys
           ).

%% Validate when unexpected values show up
%% Default to ItemsMode
validate_simple(Key, undefined = _E, A, ItemsMode, Callbacks) ->
  validate_primitive(Key, ItemsMode, A, Callbacks);

%% Otherwise
validate_simple(Key, E, A, _ItemsMode, Callbacks) ->
  validate_primitive(Key, E, A, Callbacks).

%% Validate JSON primitive types or empty structured types
%% Expected and undefined
validate_primitive(Key, ?EXPECTED = E, undefined = A, _Callbacks) ->
  {expected, {Key, E, A}};

%% Expected
validate_primitive(_Key, ?EXPECTED = _E, _A, _Callbacks) ->
  {pass, []};

%% Not expected and undefined
validate_primitive(_Key, ?UNEXPECTED = _E, undefined = _A, _Callbacks) ->
  {pass, []};

%% Not expected
validate_primitive(Key, ?UNEXPECTED = E, A, _Callbacks) ->
  {unexpected, {Key, E, A}};

%% Expected anything
validate_primitive(_Key, ?MATCH_ANY, _A, _Callbacks) ->
  {pass, []};

%% Expected actual
validate_primitive(_Key, E, E, _Callbacks) ->
  {pass, []};

%% Expected text or store param
validate_primitive(Key, E, A, Callbacks) when is_binary(E) ->
  validate_primitive(Key, from_utf8(E), A, Callbacks);
validate_primitive(Key, E, A, Callbacks) when is_binary(A) ->
  validate_primitive(Key, E, from_utf8(A), Callbacks);
validate_primitive(Key, E, A, Callbacks) when is_list(E) ->
  TextDiffFun = proplists:get_value( text_diff
                                   , Callbacks
                                   ),
  RE_HAS_PARAMS = "("
    ++ ?STORE_BEGIN_TAG
    ++ "[^}]+"
    ++ ?STORE_END_TAG
    ++ "|"
    ++ ?MATCH_ANY
    ++ ")",
  case re:run(E, RE_HAS_PARAMS, [global, {capture, all_but_first, list}]) of
    nomatch when is_list(A) ->
      {not_equal, {Key, E, A, TextDiffFun(E, A)}};
    {match, [[E]]} when E =/= ?MATCH_ANY  ->
      {pass, [{store_tag2param(E), A}]};
    {match, Params0} when is_list(A) ->
      Params = lists:map( fun([?MATCH_ANY]) ->
                              ?MATCH_ANY;
                             ([Match]) ->
                              store_tag2param(Match)
                          end
                        , Params0),
      RE0 = re:replace( E
                      , ?STORE_BEGIN_TAG ++ "[^}]+" ++ ?STORE_END_TAG
                      , "___store___"
                      , [global]
                      ),
      RE1 = re:replace( RE0
                      , ?MATCH_ANY
                      , "___skip___"
                      , [global]
                      ),
      RE2 = re:replace( RE1
                      , "[\\-\\[\\]\\/\\{\\}\\(\\)\\*\\+"
                        ++ "\\?\\.\\,\\\\\^\\$\\|\\#\\s\\&]"
                      , "\\\\&"
                      , [global]
                      ),
      RE3 = re:replace( RE2
                      , "___store___"
                      , "(.+)"
                      , [global]
                      ),
      RE4 = re:replace( RE3
                      , "___skip___"
                      , "(.*)"
                      , [global]
                      ),
      RE = ["^", RE4, "$"],
      case re:run(A, RE, [global, {capture, all_but_first, list}]) of
        nomatch ->
          {not_equal, {Key, E, A, TextDiffFun(E, A)}};
        {match, [Values]} ->
          ParamsValues = lists:filter( fun({?MATCH_ANY, _Value}) ->
                                           false;
                                          (_) ->
                                           true
                                       end
                                     , lists:zip(Params, Values)
                                     ),
          {pass, ParamsValues}
      end;
    _ ->
      {not_equal, {Key, E, A}}
  end;

%% Otherwise
validate_primitive(Key, E, A, _Callbacks) ->
  {not_equal, {Key, E, A}}.

store_tag2param(?STORE_BEGIN_TAG ++ Rest) ->
  Param = string:sub_string(Rest, 1, string:str(Rest, ?STORE_END_TAG) - 1),
  Param.

%% Transform simple list to proplist with keys named 0, 1 etc.
enumerate(L) ->
  lists:zip([ integer_to_list(N)
              || N <- lists:seq(0, length(L) - 1)
            ], L).

os_cmd_result(Port, Output) ->
  receive
    {Port, {data, NewOutput}} ->
      os_cmd_result(Port, [Output|NewOutput]);
    {Port, eof} ->
      port_close(Port),
      receive
        {Port, {exit_status, ExitStatus}} ->
          {ExitStatus, lists:flatten(Output)}
      end
  end.

headers_to_apib(Headers, Prefix) ->
  headers_to_apib(Headers, Prefix, []).

headers_to_apib([], _Prefix, []) ->
  "";
headers_to_apib([], _Prefix, Acc) ->
  string:join(lists:reverse(Acc), "\n") ++ "\n";
headers_to_apib([{Name, Value}|Headers], Prefix, Acc) ->
  headers_to_apib(Headers, Prefix, [Prefix ++ Name ++ ": " ++ Value|Acc]).

body_to_apib(null) ->
  <<"">>;
body_to_apib(Body) ->
  <<"<<<\n", Body/binary, "\n>>>\n">>.

is_items_mode(_ItemsNode)
  when _ItemsNode =:= ?MATCH_ANY orelse
       _ItemsNode =:= ?UNEXPECTED ->
  true;
is_items_mode(_ItemsNode) ->
  false.
