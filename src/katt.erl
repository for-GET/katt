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
-spec run(scenario_filename(), params()) -> run_result().
run(ScenarioFilename, Params) -> run(ScenarioFilename, Params, []).


%% @doc Run test scenario. First argument is the full path to the scenario file.
%% Second argument is a key-value list of parameters, such as hostname, port.
%% You can also pass custom variable names (atoms) and values (strings).
%% Third argument is a key-value list of custom callbacks such as a custom
%% parser to use instead of the built-in default parser (maybe_parse_body).
%% @end
-spec run(scenario_filename(), params(), callbacks()) -> run_result().
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
  {FinalParams, TransactionResults} = run_scenario( Scenario
                                                  , Blueprint
                                                  , Params
                                                  , Callbacks),
  FailureFilter = fun({_Transaction, _Request, _Params, ValidationResult}) ->
                    ValidationResult =/= pass
                  end,
  Failures = lists:filter(FailureFilter, TransactionResults),
  Status = case Failures of
             [] -> pass;
             _  -> fail
           end,
  Result = {Status, Scenario, Params, FinalParams, TransactionResults},
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
  DefaultParams = [ {protocol, Protocol}
                  , {hostname, ?DEFAULT_HOSTNAME}
                  , {port, DefaultPort}
                  , {request_timeout, ?DEFAULT_REQUEST_TIMEOUT}
                  , {scenario_timeout, ?DEFAULT_SCENARIO_TIMEOUT}
                  ],
  katt_util:merge_proplists(DefaultParams, ScenarioParams).

make_callbacks(Callbacks) ->
  katt_util:merge_proplists([ {recall, ?DEFAULT_RECALL_FUN}
                            , {parse, ?DEFAULT_PARSE_FUN}
                            , {request, ?DEFAULT_REQUEST_FUN}
                            , {validate, ?DEFAULT_VALIDATE_FUN}
                            ], Callbacks).

run_scenario(Scenario, Blueprint, Params, Callbacks) ->
  Result = run_transactions( Scenario
                           , Blueprint#katt_blueprint.transactions
                           , Params
                           , Callbacks
                           , []
                           ),
  {FinalParams, TransactionResults} = Result,
  {FinalParams, lists:reverse(TransactionResults)}.

run_transactions( Scenario
                , [#katt_transaction{ description=Description
                                    , request=Req
                                    , response=Res
                                    }|T]
                , Params
                , Callbacks
                , Acc
                ) ->
  Request = make_katt_request(Req, Params, Callbacks),
  ExpectedResponse = make_katt_response(Res, Params, Callbacks),
  RequestFun = proplists:get_value(request, Callbacks),
  ValidateFun = proplists:get_value(validate, Callbacks),
  ActualResponse = RequestFun(Request, Params, Callbacks),
  ValidationResult = ValidateFun( ExpectedResponse
                                , ActualResponse
                                , Params
                                , Callbacks
                                ),
  case ValidationResult of
    {pass, AddParams} ->
      NextParams = ordsets:union(Params, ordsets:from_list(AddParams)),
      run_transactions( Scenario
                      , T
                      , NextParams
                      , Callbacks
                      , [{Description, Request, Params, pass}|Acc]
                      );
    _                 ->
      {Params, [{Description, Request, Params, ValidationResult}|Acc]}
  end;
run_transactions(_Scenario, [], FinalParams, _Callbacks, Acc) ->
  {FinalParams, Acc}.

make_katt_request( #katt_request{headers=Hdrs0, url=Url0, body=RawBody0} = Req
                 , Params
                 , Callbacks
                 ) ->
  RecallFun = proplists:get_value(recall, Callbacks),
  Url1 = katt_util:from_utf8(
           RecallFun( url
                    , katt_util:to_utf8(Url0)
                    , Params
                    , Callbacks
                    )),
  Url = make_request_url(Url1, Params),
  Hdrs = RecallFun(headers, Hdrs0, Params, Callbacks),
  [Hdrs, RawBody] = RecallFun(body, [Hdrs, RawBody0], Params, Callbacks),
  Req#katt_request{ url     = Url
                  , headers = Hdrs
                  , body    = RawBody
                  }.

make_katt_response( #katt_response{headers=Hdrs0, body=RawBody0} = Res
                  , Params
                  , Callbacks
                  ) ->
  RecallFun = proplists:get_value(recall, Callbacks),
  ParseFun = proplists:get_value(parse, Callbacks),
  Hdrs = RecallFun(headers, Hdrs0, Params, Callbacks),
  [Hdrs, RawBody] = RecallFun(body, [Hdrs, RawBody0], Params, Callbacks),
  Body = ParseFun(Hdrs, RawBody, Params, Callbacks),
  Res#katt_response{ headers = Hdrs
                   , body    = Body
                   }.

-spec make_request_url( string()
                      , params()
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
