%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Klarna API Testing Tool v2
%%%
%%% Use for shooting http requests in a sequential order and verifying the
%%% response. Any difference between expected and actual responses will
%%% cause a failure.
%%%
%%% Tags with special meaning:
%%%   "{{_}}"     Match anything (i.e. no real validation, only check existence)
%%%   "{{>key}}"  Store value of the whole string
%%%               (key must be unique within testcase)
%%%   "ERROR"     as a status code means we are expecting to get an error back
%%%               instead of a proper http code.
%%%   "{{<key}}"  Recall stored value.
%%%   "{{>key<}}" Substitute with value from SubVars list in run/3
%%%
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(katt2).

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
-import(katt2_util, [ to_list/1
                    , to_lower/1
                    , from_utf8/1
                    , strip/1
                    ]).

%%%_* Inclues ==========================================================
-include("katt2.hrl").

%%%_* Defines ==========================================================
-define(KEY_PREFIX,        "katt2_").
-define(EXTRACT_BEGIN_TAG, "{{<").
-define(EXTRACT_END_TAG,   "}}").
-define(STORE_BEGIN_TAG,   "{{>").
-define(STORE_END_TAG,     "}}").
-define(MATCH_ANY,         "{{_}}").
-define(SUB_BEGIN_TAG,     "{{>").
-define(SUB_END_TAG,       "<}}").
-define(ERROR,             "ERROR").
-define(SCENARIO_TIMEOUT,  120000).
-define(REQUEST_TIMEOUT,   20000).

%%%_* API ==============================================================
-spec run(string()) ->
             [{string(), pass|{fail, {atom(), any()}}}|{error, any()}].
%% @doc Run test scenario. Argument is the full path to the scenario file.
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
  {ok, Sections} = katt2_parse:file(Scenario),
  From ! {done, run_scenario(Sections, Params, SubVars)}.

%%%_* Internal =========================================================
run_scenario(Sections, Params, SubVars) ->
  run_scenario(Sections, Params, SubVars, []).

run_scenario([{Req, Rsp}|T], Params, SubVars, Acc) ->
  Request          = request(Req, Params, SubVars),
  ExpectedResponse = response(Rsp, SubVars),
  ActualResponse   = make_request(Request),
  case Result = validate(ExpectedResponse, ActualResponse) of
    pass -> ok;
    _    -> dbg(Request, ExpectedResponse, ActualResponse)
  end,
  run_scenario(T, Params, SubVars, [{Request, Result}|Acc]).

request(#request{headers=Hdrs, raw_body=RawBody0} = Req, Params, SubVars) ->
  RawBody = substitute(RawBody0, SubVars),
  Req#request{ host = proplists:get_value(host, Params, "localhost")
             , port = proplists:get_value(port, Params, default_port(Params))
             , ssl  = proplists:get_value(ssl, Params, false)
             , body = maybe_parse_body(Hdrs, RawBody)
             }.

response(#response{headers=Hdrs, raw_body=RawBody0} = Rsp, SubVars) ->
  RawBody = substitute(RawBody0, SubVars),
  Rsp#response{ body = maybe_parse_body(Hdrs, RawBody)
              }.

default_port(Params) ->
  case proplists:get_value(ssl, Params, false) of
    true  -> 443;
    false -> 80
  end.

maybe_parse_body(Hdrs, Body) ->
  case is_json_body(Hdrs, Body) of
    true  -> parse_json(Body);
    false -> strip(from_utf8(Body))
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

make_request(R = #request{}) ->
  case http_request(R) of
    {ok, {{Code, _}, Hdrs, RawBody}} ->
      #response{ status_code = Code
               , headers     = Hdrs
               , body        = maybe_parse_body(Hdrs, RawBody)
               , raw_body    = RawBody
               };
    {error, timeout}                 ->
      {error, http_timeout};
    Error = {error, _}               ->
      Error
  end.

http_request(R = #request{}) ->
  lhttpc:request( R#request.host
                , R#request.port
                , R#request.ssl
                , R#request.path
                , R#request.method
                , R#request.headers
                , strip(R#request.raw_body)
                , ?REQUEST_TIMEOUT
                , []
                ).

dbg(Request, ExpectedResponse, ActualResponse) ->
  ct:pal("Request:~n~p~n~n"
         "Expected response:~n~p~n~n"
         "Actual response:~n~p~n~n",
         [Request, ExpectedResponse, ActualResponse]).

substitute(Bin, [])         -> Bin;
substitute(Bin, [{K, V}|T]) -> substitute(substitute(Bin, K, V), T).

substitute(Bin, K, V) ->
  re:replace(Bin, ?SUB_BEGIN_TAG ++ to_list(K) ++ ?SUB_END_TAG,
             to_list(V), [{return, binary}, global]).

%%%_* Validation -------------------------------------------------------
validate(#response{status_code=?ERROR}, {error, _})  -> pass;
validate(#response{status_code=?ERROR}, #response{}) -> {fail, expected_error};
validate(E = #response{}, A = #response{})           ->
  Result = do_validate(response_to_proplist(E), response_to_proplist(A)),
  case lists:filter(fun(X) -> X =/= pass end, lists:flatten(Result)) of
    []       -> pass;
    Failures -> Failures
  end;
validate(E, #response{})                             -> {fail, E};
validate(#response{}, A)                             -> {fail, A}.

do_validate(E, A) -> do_validate("", E, A).

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
  compare(Key, to_lower(extract(E)), to_lower(extract(A))).

compare(_Key, E, E) -> pass;
compare(Key, E, A)  -> {not_equal, {Key, E, A}}.

%% Replace extract tags with actual values
extract(Str) when is_list(Str) -> do_extract(Str, []);
extract(Val)                   -> Val.

do_extract(?EXTRACT_BEGIN_TAG ++ T0, Acc) ->
  {Key, ?EXTRACT_END_TAG ++ T} =
    lists:split(string:str(T0, ?EXTRACT_END_TAG), T0),
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

%% Transform response record to proplist for validation.
response_to_proplist(Resp) ->
  Keys   = record_info(fields, response),
  Values = tl(tuple_to_list(Resp)),
  lists:zip(Keys, Values).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
