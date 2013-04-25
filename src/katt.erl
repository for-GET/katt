%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Klarna API Testing Tool (v2)
%%%
%%% Use for shooting http requests in a sequential order and verifying the
%%% response. Any difference between expected and actual responses will
%%% cause a failure.
%%%
%%% Tags with special meaning:
%%% <!-- Special chars below are encoded as HTML entities for make docs -->
%%% <pre>
%%%   "{{_}}"     Match anything (i.e. no real validation, only check existence)
%%%   "{{&gt;key}}"  Store value of the whole string
%%%               (key must be unique within testcase)
%%%   "{{&lt;key}}"  Recall stored value.
%%% </pre>
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(katt).

%%%_* Exports ==========================================================
%% API
-export([ run/1
        , run/2
        , run/3
        ]).

%% Internal exports
-export([ run/4
        ]).

%%%_* Imports ==========================================================
-import(katt_util,  [ to_list/1
                    , from_utf8/1
                    ]).

%%%_* Includes ==========================================================
-include("blueprint_types.hrl").

%%%_* Defines ==========================================================
-define(TABLE,             ?MODULE).
-define(VAR_PREFIX,        "katt_").
-define(RECALL_BEGIN_TAG,  "{{<").
-define(RECALL_END_TAG,    "}}").
-define(STORE_BEGIN_TAG,   "{{>").
-define(STORE_END_TAG,     "}}").
-define(MATCH_ANY,         "{{_}}").
-define(SCENARIO_TIMEOUT,  120000).
-define(REQUEST_TIMEOUT,   20000).

%%%_* API ==============================================================
-spec run(string()) ->
             [{string(), pass|{fail, {atom(), any()}}}|{error, any()}].
%% @doc Run test scenario. Argument is the full path to the scenario file.
%% The scenario file should be a KATT Blueprint file.
%% @end
run(Scenario) -> run(Scenario, []).

-spec run(string(), list()) ->
             [{string(), pass|{fail, {atom(), any()}}}|{error, any()}].
%% @doc Run test scenario. Argument is the full path to the scenario file.
%% @end
run(Scenario, Params) -> run(Scenario, Params, []).

-spec run(string(), list(), list()) ->
             [{string(), pass|{fail, {atom(), any()}}}|{error, any()}].
%% @doc Run test scenario. Argument is the full path to the scenario file.
%% Last argument is a key-value list of parameters.
%% @end
run(Scenario, Params, Vars) ->
  ets:new(?TABLE, [named_table, public, ordered_set]),
  ets:insert(?TABLE, Vars),
  spawn_link(?MODULE, run, [self(), Scenario, Params, Vars]),
  Return = receive {done, Result}    -> Result
           after   ?SCENARIO_TIMEOUT -> {error, timeout}
           end,
  ets:delete(?TABLE),
  Return.

%%%_* Internal export --------------------------------------------------
%% @private
run(From, Scenario, Params, Vars) ->
  {ok, Blueprint} = katt_blueprint_parse:file(Scenario),
  From ! {done, run_scenario(Scenario, Blueprint, Params, Vars)}.

%%%_* Internal =========================================================
run_scenario(Scenario, Blueprint, Params, _Vars) ->
  Result = run_operations( Scenario
                         , Blueprint#katt_blueprint.operations
                         , Params
                         , []
                         ),
  Result.

run_operations( Scenario
              , [#katt_operation{ description=Description
                                , request=Req
                                , response=Rsp
                                }|T]
              , Params
              , Acc
              ) ->
  Request          = make_request(Req, Params),
  ExpectedResponse = make_response(Rsp),
  ActualResponse   = request(Request),
  case Result = validate(ExpectedResponse, ActualResponse) of
    pass -> run_operations( Scenario
                          , T
                          , Params
                          , [{Request, Result}|Acc]
                          );
    _    -> dbg( Scenario
               , Description
               , Request
               , ExpectedResponse
               , ActualResponse
               , Result
               )
  end;
run_operations(_, [], _, Acc) ->
  Acc.

make_request_url(_, Url = "http://" ++ _)  -> Url;
make_request_url(_, Url = "https://" ++ _) -> Url;
make_request_url(Params, Path) ->
  {Protocol, DefaultPort} = case proplists:get_value(ssl, Params, false) of
                              true  -> {"https:", 443};
                              false -> {"http:", 80}
                            end,
  string:join([ Protocol
              , "//"
              , proplists:get_value(host, Params, "localhost")
              , ":"
              , integer_to_list(proplists:get_value(port, Params, DefaultPort))
              , unicode:characters_to_list(proplists:get_value(path, Params, Path))
              ], "").

make_request( #katt_request{headers=Hdrs0, url=Url0, body=RawBody0} = Req
            , Params
            ) ->
  Url1 = katt_util:from_utf8(substitute(katt_util:to_utf8(Url0))),
  Url = make_request_url(Params, Url1),
  Hdrs = [{K, katt_util:from_utf8(substitute(katt_util:to_utf8(V)))} || {K, V} <- Hdrs0],
  RawBody = substitute(RawBody0),
  Req#katt_request{ url  = Url
                  , headers = Hdrs
                  , body = RawBody
                  }.

make_response(#katt_response{headers=Hdrs0, body=RawBody0} = Rsp) ->
  Hdrs = [{K, substitute(V)} || {K, V} <- Hdrs0],
  RawBody = substitute(RawBody0),
  Rsp#katt_response{ body = maybe_parse_body(Hdrs, RawBody)
                   }.

maybe_parse_body(_Hdrs, null) ->
  [];
maybe_parse_body(Hdrs, Body) ->
  case is_json_body(Hdrs, Body) of
    true  -> parse_json(Body);
    false -> from_utf8(Body)
  end.

is_json_body(_Hdrs, <<>>) -> false;
is_json_body(Hdrs, _Body) ->
  ContentType = proplists:get_value("Content-Type", Hdrs, ""),
  case string:str(unicode:characters_to_list(ContentType), "json") of
    0 -> false;
    _ -> true
  end.

parse_json(Binary) ->
  to_proplist(mochijson2:decode(Binary)).

to_proplist(L = [{struct, _}|_])         ->
  [to_proplist(S) || S <- L];
to_proplist({struct, L}) when is_list(L) ->
  [{from_utf8(K), to_proplist(V)} || {K, V} <- L];
to_proplist(List) when is_list(List)     ->
  lists:sort([to_proplist(L) || L <- List]);
to_proplist(Str) when is_binary(Str)     ->
  from_utf8(Str);
to_proplist(Value)                       ->
  Value.

request(R = #katt_request{}) ->
  case http_request(R) of
    {ok, {{Code, _}, Hdrs, RawBody}} ->
      #katt_response{ status  = Code
                    , headers = Hdrs
                    , body    = maybe_parse_body(Hdrs, RawBody)
                    };
    {error, timeout}                 ->
      {error, http_timeout};
    Error = {error, _}               ->
      Error
  end.

http_request(R = #katt_request{}) ->
  Body = case R#katt_request.body of
    null -> <<>>;
    Bin  -> Bin
  end,
  lhttpc:request( R#katt_request.url
                , R#katt_request.method
                , R#katt_request.headers
                , Body
                , ?REQUEST_TIMEOUT
                , []
                ).

dbg(Scenario, Description, Request, ExpectedResponse, ActualResponse, Result) ->
  ct:pal("Scenario:~n~p~n~n"
         "Description:~n~p~n~n"
         "Request:~n~p~n~n"
         "Expected response:~n~p~n~n"
         "Actual response:~n~p~n~n"
         "Result:~n~p~n~n",
         [ Scenario
         , Description
         , Request
         , ExpectedResponse
         , ActualResponse
         , Result
         ]).

substitute(null) ->
  null;
substitute(Bin) ->
  FirstKey = ets:first(?TABLE),
  substitute(Bin, FirstKey).

substitute(Bin, '$end_of_table') -> Bin;
substitute(Bin0, K0) ->
  [{K0, V}] = ets:lookup(?TABLE, K0),
  K = ?RECALL_BEGIN_TAG ++ K0 ++ ?RECALL_END_TAG,
  Bin = re:replace(Bin0, K, to_list(V), [{return, binary}, global]),
  substitute(Bin, ets:next(?TABLE, K0)).

%%%_* Validation -------------------------------------------------------
validate(E = #katt_response{}, A = #katt_response{}) ->
  Result = [validate_status(E, A), validate_headers(E, A), validate_body(E, A)],
  case lists:filter(fun(X) -> X =/= pass end, lists:flatten(Result)) of
    []       -> pass;
    Failures -> Failures
  end;
validate(E, #katt_response{})                        -> {fail, E};
validate(#katt_response{}, A)                        -> {fail, A}.

validate_status(#katt_response{status=E}, #katt_response{status=A}) ->
  compare(status, E, A).

%% Actual headers are allowed to be a superset of expected headers, since
%% we don't want tests full of boilerplate like tests for headers such as
%% Content-Length, Server, Date, etc.
%% The header name (not the value) is compared case-insensitive
validate_headers(#katt_response{headers=E0}, #katt_response{headers=A0}) ->
  E = [{katt_util:to_lower(K), V} || {K, V} <- E0],
  A = [{katt_util:to_lower(K), V} || {K, V} <- A0],
  ExpectedHeaders = lists:usort(proplists:get_keys(E)),
  Get = fun proplists:get_value/2,
  [ do_validate(K, Get(K, E), Get(K, A)) || K <- ExpectedHeaders].

%% Bodies must be identical, no subset matching or similar.
validate_body(#katt_response{body=E}, #katt_response{body=A}) ->
  do_validate(body, E, A).

do_validate(_, E = [{_,_}|_], A = [{_,_}|_])           ->
  Keys = lists:usort([K || {K, _} <- lists:merge(A, E)]),
  [ do_validate(K, proplists:get_value(K, E), proplists:get_value(K, A))
    || K <- Keys
  ];
do_validate(_K, E = [[{_,_}|_]|_], A = [[{_,_}|_]|_])
  when length(E) =/= length(A)                         ->
  missing_object;
do_validate(K, E0 = [[{_,_}|_]|_], A0 = [[{_,_}|_]|_]) ->
  [do_validate(K, E, A) || {E, A} <- lists:zip(E0, A0)];
do_validate(K, E = [[_|_]|_], A = [[_|_]|_])           ->
  do_validate(K, enumerate(E, K), enumerate(A, K));
do_validate(Key, E, undefined)                         ->
  {missing_value, {Key, E}};
do_validate(Key, undefined, A)                         ->
  {unexpected_value, {Key, A}};
do_validate(Key, Str = ?STORE_BEGIN_TAG ++ _, [])      ->
  {empty_value, {Key, Str}};
do_validate(_Key, ?MATCH_ANY ++ _, _)                  ->
  pass;
do_validate(_Key, ?STORE_BEGIN_TAG ++ Rest, A)         ->
  Key = string:sub_string(Rest, 1, string:str(Rest, ?STORE_END_TAG) - 1),
  ets:insert(?TABLE, {Key, A}),
  pass;
do_validate(Key, E, A)                                 ->
  compare(Key, E, A).

%% put_var(Key, Value) -> put(?VAR_PREFIX ++ Key, Value).
%% get_var(Key) -> get(?VAR_PREFIX ++ Key).

compare(_Key, E, E) -> pass;
compare(Key, E, A)  -> {not_equal, {Key, E, A}}.

%% Transform simple list to proplist with keys named Name1, Name2 etc.
enumerate(L, Name) ->
  lists:zip([ to_list(Name) ++ integer_to_list(N)
              || N <- lists:seq(1, length(L))
            ], L).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
