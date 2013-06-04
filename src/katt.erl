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
%%% @doc Klarna API Testing Tool
%%%
%%% Use for shooting http requests in a sequential order and verifying the
%%% response.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(katt).

%%%_* Exports ==========================================================
%% API
-export([ run/1
        , run/2
        ]).

%% Internal exports
-export([ run/3
        ]).

%%%_* Imports ==========================================================
-import(katt_util, [ to_list/1
                   , from_utf8/1
                   ]).

%%%_* Includes ==========================================================
-include_lib("katt/include/blueprint_types.hrl").

%%%_* Defines ==========================================================
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
%% @doc Run test scenario. First argument is the full path to the scenario file.
%% Second argument is a key-value list of parameters, such as hostname, port.
%% You can also pass custom variable names (atoms) and values (strings).
%% @end
run(Scenario, Params) ->
  spawn_link(?MODULE, run, [self(), Scenario, Params]),
  receive {done, Result}    -> Result
  after   ?SCENARIO_TIMEOUT -> {error, timeout}
  end.

%%%_* Internal export --------------------------------------------------
%% @private
run(From, Scenario, ScenarioParams) ->
  {ok, Blueprint} = katt_blueprint_parse:file(Scenario),
  Params0 = make_params(ScenarioParams),
  Params = ordsets:from_list(Params0),
  Result = {Scenario, run_scenario(Scenario, Blueprint, Params)},
  From ! {done, Result}.

%%%_* Internal =========================================================

%% Take default params, and also merge in optional params from Params, to return
%% a proplist of params.
make_params(ScenarioParams) ->
  Protocol = proplists:get_value(protocol, ScenarioParams, "http:"),
  Port = case Protocol of
           "http:"  -> 80;
           "https:" -> 443
         end,
  DefaultParams = [ {hostname, "localhost"}
                  , {protocol, Protocol}
                  , {port, Port}
                  ],
  katt_util:merge_proplists(DefaultParams, ScenarioParams).

run_scenario(Scenario, Blueprint, Params) ->
  Result = run_operations( Scenario
                         , Blueprint#katt_blueprint.operations
                         , Params
                         , []
                         ),
  lists:reverse(Result).

run_operations( Scenario
              , [#katt_operation{ description=Description
                                , request=Req
                                , response=Res
                                }|T]
              , Params
              , Acc
              ) ->
  Request          = make_katt_request(Req, Params),
  ExpectedResponse = make_katt_response(Res, Params),
  ActualResponse   = request(Request),
  ValidationResult = validate(ExpectedResponse, ActualResponse),
  {Pass, AddParams} = ValidationResult,
  case Pass of
    pass -> NewParams = ordsets:union(Params, ordsets:from_list(AddParams)),
            run_operations( Scenario
                          , T
                          , NewParams
                          , [{Description, Request, Pass}|Acc]
                          );
    _    -> [{Description, Request, ValidationResult}|Acc]
  end;
run_operations(_Scenario, [], _Params, Acc) ->
  Acc.

make_request_url(Url = "http://" ++ _, _Params)  -> Url;
make_request_url(Url = "https://" ++ _, _Params) -> Url;
make_request_url(Path0, Params) ->
  Protocol = proplists:get_value(protocol, Params, "http:"),
  DefaultPort = case Protocol of
                  "https:" -> 443;
                  "http:"  -> 80
                end,
  Path = unicode:characters_to_list(proplists:get_value(path, Params, Path0)),
  string:join([ Protocol
              , "//"
              , proplists:get_value(hostname, Params, "localhost")
              , ":"
              , integer_to_list(proplists:get_value(port, Params, DefaultPort))
              , Path
              ], "").

make_katt_request( #katt_request{headers=Hdrs0, url=Url0, body=RawBody0} = Req
            , Params
            ) ->
  Url1 = katt_util:from_utf8(recall(katt_util:to_utf8(Url0), Params)),
  Url = make_request_url(Url1, Params),
  Hdrs = [{K, katt_util:from_utf8(
                recall(katt_util:to_utf8(V), Params)
              )} || {K, V} <- Hdrs0],
  RawBody = recall(RawBody0, Params),
  Req#katt_request{ url  = Url
                  , headers = Hdrs
                  , body = RawBody
                  }.

make_katt_response( #katt_response{headers=Hdrs0, body=RawBody0} = Res
             , Params) ->
  Hdrs = [{K, recall(V, Params)} || {K, V} <- Hdrs0],
  RawBody = recall(RawBody0, Params),
  Res#katt_response{ body = maybe_parse_body(Hdrs, RawBody)
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
  to_proplist(mochijson3:decode(Binary)).

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

recall(null, _Params)          -> null;
recall(Bin, [])                -> Bin;
recall(Bin0, [{K0, V} | Next]) ->
  K = ?RECALL_BEGIN_TAG ++ to_list(K0) ++ ?RECALL_END_TAG,
  EscapedK = katt_util:escape_regex(K),
  EscapedV = katt_util:escape_regex(to_list(V)),
  Bin = re:replace( Bin0
                  , EscapedK
                  , to_list(EscapedV)
                  , [{return, binary}, global]),
  recall(Bin, Next).

%%%_* Validation -------------------------------------------------------
validate(E = #katt_response{}, A = #katt_response{}) ->
  Result = [ validate_status(E, A)
           , validate_headers(E, A)
           , validate_body(E, A)],
  {AddParams, Failures} =
    lists:foldl(
      fun(pass, Acc) -> Acc;
         ({pass, AddParam}, {AddParams0, Failures0}) ->
          {[AddParam | AddParams0], Failures0};
         (Failure, {AddParams0, Failures0}) ->
          {AddParams0, [Failure|Failures0]}
      end,
    {[],[]},
    lists:flatten(Result)
    ),
  case Failures of
    [] -> {pass, AddParams};
    _  -> {fail, Failures}
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
do_validate(Key, E = [[{_,_}|_]|_], A = [[{_,_}|_]|_])
  when length(E) =/= length(A)                         ->
  {missing_object, {Key, {E, A}}};
do_validate(K, E0 = [[{_,_}|_]|_], A0 = [[{_,_}|_]|_]) ->
  [do_validate(K, E, A) || {E, A} <- lists:zip(E0, A0)];
do_validate(K, E = [[_|_]|_], A = [[_|_]|_])           ->
  do_validate(K, enumerate(E, K), enumerate(A, K));
do_validate(Key, undefined, A)                         ->
  {unexpected_value, {Key, A}};
do_validate(Key, E, undefined)                         ->
  {missing_value, {Key, E}};
do_validate(Key, ?STORE_BEGIN_TAG ++ Rest, [])         ->
  Param = string:sub_string(Rest, 1, string:str(Rest, ?STORE_END_TAG) - 1),
  {empty_value, {Key, Param}};
do_validate(_Key, ?MATCH_ANY ++ _, _)                  ->
  pass;
do_validate(_Key, ?STORE_BEGIN_TAG ++ Rest, A)         ->
  Param = string:sub_string(Rest, 1, string:str(Rest, ?STORE_END_TAG) - 1),
  {pass, {Param, A}};
do_validate(Key, E, A)                                 ->
  compare(Key, E, A).

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
