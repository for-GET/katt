%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Klarna API Testing Tool (v2)
%%%
%%% Use for shooting http requests in a sequential order and verifying the
%%% response. Any difference between expected and actual responses will
%%% cause a failure.
%%%
%%% Tags with special meaning:
%%%   "{{_}}"     Match anything (i.e. no real validation, only check existence)
%%%   "{{>key}}"  Store value of the whole string
%%%               (key must be unique within testcase)
%%%   "{{<key}}"  Recall stored value.
%%%   "{{>key<}}" Substitute with value from SubVars list in run/3
%%%
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
-include_lib("include/blueprint_types.hrl").

%%%_* Defines ==========================================================
-define(KEY_PREFIX,        "katt_").
-define(EXTRACT_BEGIN_TAG, "{{<").
-define(EXTRACT_END_TAG,   "}}").
-define(STORE_BEGIN_TAG,   "{{>").
-define(STORE_END_TAG,     "}}").
-define(MATCH_ANY,         "{{_}}").
-define(SUB_BEGIN_TAG,     "{{>").
-define(SUB_END_TAG,       "<}}").
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
%% Last argument is a key-value list of substitute parameters.
%% @end
run(Scenario, Params, SubVars) ->
  spawn_link(?MODULE, run, [self(), Scenario, Params, SubVars]),
  receive {done, Result}    -> Result
  after   ?SCENARIO_TIMEOUT -> {error, timeout}
  end.

%%%_* Internal export --------------------------------------------------
%% @private
run(From, Scenario, Params, SubVars) ->
  {ok, Blueprint} = katt_blueprint_parse:file(Scenario),
  From ! {done, run_scenario(Blueprint, Params, SubVars)}.

%%%_* Internal =========================================================
run_scenario(Blueprint, Params, SubVars) ->
  run_scenario(Blueprint#api_blueprint.operations, Params, SubVars, []).

run_scenario([#operation{request=Req, response=Rsp}|T], Params, SubVars, Acc) ->
  Request          = make_request(Req, Params, SubVars),
  ExpectedResponse = make_response(Rsp, SubVars),
  ActualResponse   = request(Request),
  case Result = validate(ExpectedResponse, ActualResponse) of
    pass -> run_scenario(T, Params, SubVars, [{Request, Result}|Acc]);
    _    -> dbg(Request, ExpectedResponse, ActualResponse, Result)
  end;
run_scenario([], _, _, Acc) ->
  Acc.

make_request_url(_, Url="http://"++_)  -> Url;
make_request_url(_, Url="https://"++_) -> Url;
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
              , proplists:get_value(path, Params, Path)
              ], "").

make_request(#request{headers=Hdrs, url=Url0, body=RawBody0} = Req,
        Params,
        SubVars) ->
  RawBody = substitute(RawBody0, SubVars),
  Url = make_request_url(Params, substitute(extract(Url0), SubVars)),
  Req#request{ url  = Url
             , headers = Hdrs
             , body = RawBody
             }.

make_response(#response{headers=Hdrs, body=RawBody0} = Rsp, SubVars) ->
  RawBody = substitute(RawBody0, SubVars),
  Rsp#response{ body = maybe_parse_body(Hdrs, RawBody)
              }.

maybe_parse_body(_Hdrs, undefined) ->
  undefined;
maybe_parse_body(Hdrs, Body) ->
  case is_json_body(Hdrs, Body) of
    true  -> parse_json(Body);
    false -> from_utf8(Body)
  end.

is_json_body(_Hdrs, <<>>) -> false;
is_json_body(Hdrs, _Body) ->
  ContentType = proplists:get_value("Content-Type", Hdrs, ""),
  case string:str(ContentType, "json") of
    0 -> false;
    _ -> true
  end.

parse_json(Binary) -> to_proplist(mochijson2:decode(Binary)).

to_proplist(L = [{struct, _}|_])         ->
  [to_proplist(S) || S <- L];
to_proplist({struct, L}) when is_list(L) ->
  [{from_utf8(K), to_proplist(V)} || {K, V} <- L];
to_proplist(List) when is_list(List)     ->
  lists:sort([to_proplist(L) || L <- List]);
to_proplist(Str) when is_binary(Str)     ->
  extract(from_utf8(Str));
to_proplist(Value)                       ->
  Value.

request(R = #request{}) ->
  case http_request(R) of
    {ok, {{Code, _}, Hdrs, RawBody}} ->
      #response{ status      = Code
               , headers     = Hdrs
               , body        = maybe_parse_body(Hdrs, RawBody)
               };
    {error, timeout}                 ->
      {error, http_timeout};
    Error = {error, _}               ->
      Error
  end.

http_request(R = #request{}) ->
  Body = case R#request.body of
    undefined -> <<>>;
    Bin       -> Bin
  end,
  lhttpc:request( R#request.url
                , R#request.method
                , R#request.headers
                , Body
                , ?REQUEST_TIMEOUT
                , []
                ).


dbg(Request, ExpectedResponse, ActualResponse, Result) ->
  ct:pal("Request:~n~p~n~n"
         "Expected response:~n~p~n~n"
         "Actual response:~n~p~n~n",
         "Result:~n~p~n~n",
         [Request, ExpectedResponse, ActualResponse, Result]).

substitute(Bin, [])         -> Bin;
substitute(Bin, [{K, V}|T]) -> substitute(substitute(Bin, K, V), T).

substitute(Bin, K, V) ->
  re:replace(Bin, ?SUB_BEGIN_TAG ++ to_list(K) ++ ?SUB_END_TAG,
             to_list(V), [{return, binary}, global]).

%%%_* Validation -------------------------------------------------------
validate(E = #response{}, A = #response{})           ->
  Result = [validate_status(E, A), validate_headers(E, A), validate_body(E, A)],
  case lists:filter(fun(X) -> X =/= pass end, lists:flatten(Result)) of
    []       -> pass;
    Failures -> Failures
  end;
validate(E, #response{})                             -> {fail, E};
validate(#response{}, A)                             -> {fail, A}.

validate_status(#response{status=E}, #response{status=A}) ->
  compare(status, E, A).

%% Actual headers are allowed to be a superset of expected headers, since
%% we don't want tests full of boilerplate like tests for headers such as
%% Content-Length, Server, Date, etc.
validate_headers(#response{headers=E}, #response{headers=A}) ->
  ExpectedHeaders = lists:usort(proplists:get_keys(E)),
  Get = fun proplists:get_value/2,
  [ do_validate(K, Get(K, E), Get(K, A)) || K <- ExpectedHeaders ].

%% Bodies must be identical, no subset matching or similar.
validate_body(#response{body=E}, #response{body=A}) ->
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
  put(?KEY_PREFIX ++ Key, A),
  pass;
do_validate(Key, E, A)                                 ->
  compare(Key, extract(E), extract(A)).

compare(_Key, E, E) -> pass;
compare(Key, E, A)  -> {not_equal, {Key, E, A}}.

%% Replace extract tags with actual values
extract(Str) when is_list(Str) -> do_extract(Str, []);
extract(Val)                   -> Val.

do_extract(?EXTRACT_BEGIN_TAG ++ T0, Acc) ->
  {Key, ?EXTRACT_END_TAG ++ T} =
    lists:split(string:str(T0, ?EXTRACT_END_TAG) - 1, T0),
  Str = case get(?KEY_PREFIX ++ Key) of
          undefined -> "";
          Value     -> Value
        end,
  do_extract(T, lists:reverse(Str) ++ Acc);
do_extract([H|T], Acc)                    -> do_extract(T, [H|Acc]);
do_extract([], Acc)                       -> lists:reverse(Acc).

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
