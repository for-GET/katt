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
%%% @doc Klarna API Testing Tool
%%%
%%% Use for shooting http requests in a sequential order and verifying the
%%% response.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(katt).

%%%_* Exports ==================================================================
%% API
-export([ main/1
        , run/1
        , run/2
        , run/3
        , run/4
        ]).

%% Internal exports
-export([ run/5
        , make_callbacks/1
        ]).

%%%_* Includes =================================================================
-include("katt.hrl").

%%%_* API ======================================================================

%% @doc Run from CLI with arguments.
%% @end
-spec main([string()]) -> ok.
main(Args) ->
  katt_cli:main(Args).

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
run(Scenario, ScenarioParams, ScenarioCallbacks) ->
  run(Scenario, ScenarioParams, ScenarioCallbacks, []).


run(Scenario, ScenarioParams, ScenarioCallbacks, ScenarioOptions) ->
  Options = make_options(ScenarioOptions),
  Params = ordsets:from_list(make_params(ScenarioParams)),
  Callbacks = make_callbacks(ScenarioCallbacks),
  ScenarioTimeout = proplists:get_value( "scenario_timeout"
                                       , Params
                                       ),
  ProgressFun = proplists:get_value( progress
                                   , Callbacks
                                   ),
  HttpClient = proplists:get_value(http_client, Options),
  ok = HttpClient:init([]),
  spawn_link(?MODULE, run, [self(), Scenario, Params, Callbacks, Options]),
  run_loop(ScenarioTimeout, ProgressFun).

%%%_* Internal exports =========================================================

%% @private
run(From, Scenario, Params, Callbacks, Options) ->
  From ! {progress, parsing, Scenario},
  {ok, Blueprint} = katt_blueprint_parse:file(Scenario),
  From ! {progress, parsed, Scenario},
  {FinalParams, TransactionResults} = run_scenario( From
                                                  , Scenario
                                                  , Blueprint
                                                  , Params
                                                  , Callbacks
                                                  , Options),
  FailureFilter = fun({ _Transaction
                      , _Params
                      , _Request
                      , _Response
                      , ValidationResult }) ->
                    ValidationResult =/= pass
                  end,
  Failures = lists:filter(FailureFilter, TransactionResults),
  Status = case Failures of
             [] -> pass;
             _  -> fail
           end,
  Result = {Status, Scenario, Params, FinalParams, TransactionResults},
  From ! {progress, status, Status},
  From ! {done, Result}.

%% @private
make_callbacks(Callbacks) ->
  katt_util:merge_proplists([ {ext, ?DEFAULT_EXT_FUN}
                            , {recall, ?DEFAULT_RECALL_FUN}
                            , {parse, ?DEFAULT_PARSE_FUN}
                            , {request, ?DEFAULT_REQUEST_FUN}
                            , {validate, ?DEFAULT_VALIDATE_FUN}
                            , {progress, ?DEFAULT_PROGRESS_FUN}
                            , {text_diff, ?DEFAULT_TEXT_DIFF_FUN}
                            ], Callbacks).

%%%_* Internal =================================================================

make_options(ScenarioOptions) ->
  katt_util:merge_proplists([{http_client, ?DEFAULT_HTTP_CLIENT_MOD}],
                            ScenarioOptions).

run_loop(ScenarioTimeout, ProgressFun) ->
  receive
    {progress, Step, Detail} ->
      ProgressFun(Step, Detail),
      run_loop(ScenarioTimeout, ProgressFun);
    {done, Result} ->
      Result
  after ScenarioTimeout ->
      ProgressFun(status, timeout),
      {error, timeout, ScenarioTimeout}
  end.

%% Take default params, and also merge in optional params from Params, to return
%% a proplist of params.
make_params(ScenarioParams0) ->
  ScenarioParams = [ {katt_util:to_list(K), V} || {K, V} <- ScenarioParams0],
  Protocol = proplists:get_value("protocol", ScenarioParams, ?DEFAULT_PROTOCOL),
  DefaultPort = case Protocol of
                  ?PROTOCOL_HTTP  -> ?DEFAULT_PORT_HTTP;
                  ?PROTOCOL_HTTPS -> ?DEFAULT_PORT_HTTPS
                end,
  DefaultParams = [ {"protocol", Protocol}
                  , {"hostname", ?DEFAULT_HOSTNAME}
                  , {"port", DefaultPort}
                  , {"request_timeout", ?DEFAULT_REQUEST_TIMEOUT}
                  , {"scenario_timeout", ?DEFAULT_SCENARIO_TIMEOUT}
                  ],
  katt_util:merge_proplists(DefaultParams, ScenarioParams).

run_scenario(From, Scenario, Blueprint, Params, Callbacks, Options) ->
  Result = run_transactions( From
                           , Scenario
                           , Blueprint#katt_blueprint.transactions
                           , Params
                           , Callbacks
                           , Options
                           , {0, []}
                           ),
  {FinalParams, {_Count, TransactionResults}} = Result,
  {FinalParams, lists:reverse(TransactionResults)}.

run_transactions( _From
                , _Scenario
                , []
                , FinalParams
                , _Callbacks
                , _Options
                , {Count, Results}
                ) ->
  {FinalParams, {Count, Results}};
run_transactions( From
                , Scenario
                , [#katt_transaction{ description = Description0
                                    , request = Req0
                                    , response = Res
                                    }|T]
                , Params
                , Callbacks
                , Options
                , {Count, Results}
                ) ->
  Hdrs0 = Req0#katt_request.headers,
  Description = case proplists:get_value("x-katt-description", Hdrs0) of
                  undefined ->
                    case Description0 of
                      <<>> ->
                        "Transaction " ++ integer_to_list(Count);
                      _ ->
                        Description0
                    end;
                  Description1 ->
                    Description1
                end,
  Hdrs = proplists:delete("x-katt-description", Hdrs0),
  Req = Req0#katt_request{headers = Hdrs},
  From ! {progress, run_transaction, Description},
  Request = make_katt_request(Req, Params, Callbacks),
  ExpectedResponse = make_katt_response(Res, Params, Callbacks),
  RequestFun = proplists:get_value(request, Callbacks),
  ValidateFun = proplists:get_value(validate, Callbacks),
  ActualResponse = case is_function(RequestFun, 4) of
                     true  -> RequestFun(Request, Params, Callbacks, Options);
                     false -> RequestFun(Request, Params, Callbacks)
                   end,
  ValidationResult = ValidateFun( ExpectedResponse
                                , ActualResponse
                                , Params
                                , Callbacks
                                ),
  case ValidationResult of
    {pass, AddParams} ->
      Result = { Description
               , Params
               , Request
               , ActualResponse
               , pass
               },
      From ! {progress, transaction_result, Result},
      NextParams = katt_util:merge_proplists(Params, AddParams),
      run_transactions( From
                      , Scenario
                      , T
                      , NextParams
                      , Callbacks
                      , Options
                      , {Count + 1, [Result|Results]}
                      );
    _                 ->
      Result = { Description
               , Params
               , Request
               , ActualResponse
               , ValidationResult
               },
      From ! {progress, transaction_result, Result},
      {Params, {Count + 1, [Result|Results]}}
  end.

make_katt_request( #katt_request{headers=Hdrs0, url=Url0, body=Body0} = Req
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
  [Hdrs, Body] = RecallFun(body, [Hdrs, Body0], Params, Callbacks),
  Req#katt_request{ url     = Url
                  , headers = Hdrs
                  , body    = Body
                  }.

make_katt_response( #katt_response{headers=Hdrs0, body=Body0} = Res
                  , Params
                  , Callbacks
                  ) ->
  RecallFun = proplists:get_value(recall, Callbacks),
  ParseFun = proplists:get_value(parse, Callbacks),
  Hdrs = RecallFun(headers, Hdrs0, Params, Callbacks),
  [Hdrs, Body] = RecallFun(body, [Hdrs, Body0], Params, Callbacks),
  ParsedBody = ParseFun(Hdrs, Body, Params, Callbacks),
  Res#katt_response{ headers     = Hdrs
                   , body        = Body
                   , parsed_body = ParsedBody
                   }.

-spec make_request_url( string()
                      , params()
                      ) -> nonempty_string().
make_request_url(Url = ?PROTOCOL_HTTP "//" ++ _, _Params)  -> Url;
make_request_url(Url = ?PROTOCOL_HTTPS "//" ++ _, _Params) -> Url;
make_request_url(Path, Params) ->
  Protocol = proplists:get_value("protocol", Params),
  Hostname = proplists:get_value("hostname", Params),
  Port = proplists:get_value("port", Params),
  string:join([ Protocol
              , "//"
              , make_host(Protocol, Hostname, Port)
              , Path
              ], "").


make_host(?PROTOCOL_HTTP,  Hostname, 80)   -> Hostname;
make_host(?PROTOCOL_HTTPS, Hostname, 443)  -> Hostname;
make_host(_Proto,          Hostname, Port) ->
  Hostname ++ ":" ++ katt_util:to_list(Port).
