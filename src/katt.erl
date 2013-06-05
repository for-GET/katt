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
        , run/3
        ]).

%% Internal exports
-export([ run/4
        ]).

%%%_* Includes =========================================================
-include("katt.hrl").

%%%_* API ==============================================================

%% @doc Run test scenario. Argument is the full path to the scenario file.
%% The scenario filename should be a KATT Blueprint file.
%% @end
-spec run(scenario_filename()) -> run_result().
run(ScenarioFilename) -> run(ScenarioFilename, []).

%% @doc Run test scenario. Argument is the full path to the scenario file.
%% The scenario file should be a KATT Blueprint file.
%% @end
-spec run(scenario_filename(), proplist()) -> run_result().
run(ScenarioFilename, Params) -> run(ScenarioFilename, Params, []).


%% @doc Run test scenario. First argument is the full path to the scenario file.
%% Second argument is a key-value list of parameters, such as hostname, port.
%% You can also pass custom variable names (atoms) and values (strings).
%% Third argument is a key-value list of special options such as custom
%% parser to use instead of the built-in default parser (maybe_parse_body).
%% @end
-spec run(scenario_filename(), proplist(), callbacks()) -> run_result().
run(Scenario, Params, Callbacks) ->
  ScenarioTimeout = proplists:get_value( scenario_timeout
                                       , Params
                                       , ?DEFAULT_SCENARIO_TIMEOUT
                                       ),
  spawn_link(?MODULE, run, [self(), Scenario, Params, Callbacks]),
  receive {done, Result}    -> Result
  after   ScenarioTimeout -> {error, timeout, ScenarioTimeout}
  end.

%%%_* Internal export --------------------------------------------------
%% @private
run(From, Scenario, ScenarioParams, ScenarioCallbacks) ->
  {ok, Blueprint} = katt_blueprint_parse:file(Scenario),
  Params = ordsets:from_list(make_params(ScenarioParams)),
  Callbacks = make_callbacks(ScenarioCallbacks),
  Result = {Scenario, run_scenario(Scenario, Blueprint, Params, Callbacks)},
  From ! {done, Result}.

%%%_* Internal =========================================================

%% Take default params, and also merge in optional params from Params, to return
%% a proplist of params.
make_params(ScenarioParams) ->
  Protocol = proplists:get_value(protocol, ScenarioParams, ?DEFAULT_PROTOCOL),
  DefaultPort = case Protocol of
                  ?PROTOCOL_HTTP  -> ?DEFAULT_PORT_HTTP;
                  ?PROTOCOL_HTTPS -> ?DEFAULT_PORT_HTTPS
                end,
  DefaultParams = [ {hostname, ?DEFAULT_HOSTNAME}
                  , {protocol, Protocol}
                  , {port, DefaultPort}
                  , {scenario_timeout, ?DEFAULT_SCENARIO_TIMEOUT}
                  , {request_timeout, ?DEFAULT_REQUEST_TIMEOUT}
                  ],
  katt_util:merge_proplists(DefaultParams, ScenarioParams).

make_callbacks(Callbacks) ->
  katt_util:merge_proplists([ {parse, ?DEFAULT_PARSE_FUNCTION}
                            , {request, ?DEFAULT_REQUEST_FUNCTION}
                            , {validate, ?DEFAULT_VALIDATE_FUNCTION}
                            ], Callbacks).

run_scenario(Scenario, Blueprint, Params, Callbacks) ->
  Result = run_operations( Scenario
                         , Blueprint#katt_blueprint.operations
                         , Params
                         , Callbacks
                         , []
                         ),
  lists:reverse(Result).

run_operations( Scenario
              , [#katt_operation{ description=Description
                                , request=Req
                                , response=Res
                                }|T]
              , Params
              , Callbacks
              , Acc
              ) ->
  Request = make_katt_request(Req, Params),
  ExpectedResponse = make_katt_response(Res, Params, Callbacks),
  RequestFun = proplists:get_value(request, Callbacks),
  ValidateFun = proplists:get_value(validate, Callbacks),
  ActualResponse = RequestFun(Request, Params, Callbacks),
  ValidationResult = ValidateFun(ExpectedResponse, ActualResponse, Params),
  {Pass, AddParams} = ValidationResult,
  case Pass of
    pass -> NewParams = ordsets:union(Params, ordsets:from_list(AddParams)),
            run_operations( Scenario
                          , T
                          , NewParams
                          , Callbacks
                          , [{Description, Request, Pass}|Acc]
                          );
    _    -> [{Description, Request, ValidationResult}|Acc]
  end;
run_operations(_Scenario, [], _Params, _Callbacks, Acc) ->
  Acc.

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
                  , Params
                  , Callbacks
                  ) ->
  Hdrs = [{K, recall(V, Params)} || {K, V} <- Hdrs0],
  RawBody = recall(RawBody0, Params),
  ParseFun = proplists:get_value(parse, Callbacks),
  Res#katt_response{body = ParseFun(Hdrs, RawBody, Params)}.

recall(null, _Params)          -> null;
recall(Bin, [])                -> Bin;
recall(Bin0, [{K0, V} | Next]) ->
  K = ?RECALL_BEGIN_TAG ++ katt_util:to_list(K0) ++ ?RECALL_END_TAG,
  EscapedK = katt_util:escape_regex(K),
  EscapedV = katt_util:escape_regex(katt_util:to_list(V)),
  Bin = re:replace( Bin0
                  , EscapedK
                  , katt_util:to_list(EscapedV)
                  , [{return, binary}, global]),
  recall(Bin, Next).

-spec make_request_url( string()
                      , proplist()
                      ) -> nonempty_string().
make_request_url(Url = ?PROTOCOL_HTTP "//" ++ _, _Params)  -> Url;
make_request_url(Url = ?PROTOCOL_HTTPS "//" ++ _, _Params) -> Url;
make_request_url(Path, Params) ->
  Protocol = proplists:get_value(protocol, Params),
  Hostname = proplists:get_value(hostname, Params),
  Port = proplists:get_value(port, Params),
  string:join([ Protocol
              , "//"
              , make_host(Protocol, Hostname, Port)
              , Path
              ], "").


make_host(?PROTOCOL_HTTP,  Hostname, 80)   -> Hostname;
make_host(?PROTOCOL_HTTPS, Hostname, 443)  -> Hostname;
make_host(_Proto,          Hostname, Port) ->
  Hostname ++ ":" ++ katt_util:to_list(Port).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
