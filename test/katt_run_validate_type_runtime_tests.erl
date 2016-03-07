%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%% @copyright 2014- AUTHORS
%%%
%%% KATT Run - Validate Type Runtime Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(katt_run_validate_type_runtime_tests).

-include_lib("eunit/include/eunit.hrl").

-define( FUNCTION
       , element(2, element(2, process_info(self(), current_function)))
       ).

-export([ katt_run_with_runtime_value_blueprint/0
        , katt_run_with_runtime_value_http/6
        , katt_run_with_runtime_value_array_blueprint/0
        , katt_run_with_runtime_value_array_http/6
        , katt_run_with_runtime_value_shell_blueprint/0
        , katt_run_with_runtime_value_shell_http/6
        , katt_run_with_runtime_validation_pass_blueprint/0
        , katt_run_with_runtime_validation_pass_http/6
        , katt_run_with_runtime_validation_fail_blueprint/0
        , katt_run_with_runtime_validation_fail_http/6
        ]).

%%% Suite

katt_test_() ->
  { setup
  , spawn
  , fun() ->
      meck:new(katt_blueprint_parse, [passthrough]),
      meck:expect( katt_blueprint_parse
                 , file
                 , fun mock_katt_blueprint_parse_file/1
                 ),
      meck:new(katt_callbacks, [passthrough]),
      meck:expect( katt_util
                 , external_http_request
                 , fun mock_lhttpc_request/6
                 )
    end
  , fun(_) ->
      meck:unload(katt_blueprint_parse),
      meck:unload(katt_callbacks)
    end
  , [ katt_run_with_runtime_value()
    , katt_run_with_runtime_value_array()
    , katt_run_with_runtime_value_shell()
    , katt_run_with_runtime_validation_pass()
    , katt_run_with_runtime_validation_fail()
    ]
  }.

%%% Tests

%%% Test with runtime_value

katt_run_with_runtime_value() ->
  Scenario = ?FUNCTION,
  ?_assertMatch( { pass
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, _, pass}
                   ]
                 }
               , katt:run(Scenario)
               ).

katt_run_with_runtime_value_blueprint() ->
  katt_blueprint_parse:string(
    <<"--- Comparison as runtime_value ---

GET /katt_run_with_runtime_value
< 200
< Content-Type: application/json
{
    \"{{type}}\": \"runtime_value\",
    \"erlang\": \"{array, [ParentKey, 1]}\"
}
"/utf8>>).

katt_run_with_runtime_value_http( _
                                , "GET"
                                , _
                                , _
                                , _Timeout
                                , _Options
                                ) ->
  {ok, {{200, []}, [
                    {"content-type", "application/json"}
                   ], <<"[
    \"/\",
    1
]
"/utf8>>}}.

%%% Test with runtime_value array

katt_run_with_runtime_value_array() ->
  Scenario = ?FUNCTION,
  ?_assertMatch( { pass
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, _, pass}
                   ]
                 }
               , katt:run(Scenario)
               ).

katt_run_with_runtime_value_array_blueprint() ->
  katt_blueprint_parse:string(
    <<"--- Comparison as runtime_value (array) ---

GET /katt_run_with_runtime_value_array
< 200
< Content-Type: application/json
{
    \"{{type}}\": \"runtime_value\",
    \"erlang\": [\"{ array\",
                 \", [ParentKey, 1]}\"
                ]
}
"/utf8>>).

%% Mock response for runtime_value_array test:
katt_run_with_runtime_value_array_http( _
                                      , "GET"
                                      , _
                                      , _
                                      , _Timeout
                                      , _Options
                                      ) ->
  {ok, {{200, []}, [
                    {"content-type", "application/json"}
                   ], <<"[
    \"/\",
    1
]
"/utf8>>}}.

%%% Test with runtime_value shell

katt_run_with_runtime_value_shell() ->
  Scenario = ?FUNCTION,
  ?_assertMatch( { pass
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, _, pass}
                   ]
                 }
               , katt:run(Scenario)
               ).

katt_run_with_runtime_value_shell_blueprint() ->
  katt_blueprint_parse:string(
    <<"--- Comparison as runtime_value (shell) ---

GET /katt_run_with_runtime_value_shell
< 200
< Content-Type: application/json
{
    \"{{type}}\": \"runtime_value\",
    \"shell\": \"sh -c \\\"echo '{array, [ParentKey, 1]}'\\\"\"
}
"/utf8>>).

%% Mock response for runtime_value_shell test:
katt_run_with_runtime_value_shell_http( _
                                      , "GET"
                                      , _
                                      , _
                                      , _Timeout
                                      , _Options
                                      ) ->
  {ok, {{200, []}, [
                    {"content-type", "application/json"}
                   ], <<"[
    \"/\",
    1
]
"/utf8>>}}.

%%% Test with runtime_validation

katt_run_with_runtime_validation_pass() ->
  Scenario = ?FUNCTION,
  ?_assertMatch( { pass
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, _, pass}
                   ]
                 }
               , katt:run(Scenario)
               ).

katt_run_with_runtime_validation_pass_blueprint() ->
  katt_blueprint_parse:string(
    <<"--- Comparison as runtime_validation_pass ---

GET /katt_run_with_runtime_validation_pass
< 200
< Content-Type: application/json
{
    \"{{type}}\": \"runtime_validation\",
    \"erlang\": \"{pass, [{\\\"Param\\\", \\\"Value\\\"}]}\"
}
"/utf8>>).

katt_run_with_runtime_validation_pass_http( _
                                          , "GET"
                                          , _
                                          , _
                                          , _Timeout
                                          , _Options
                                          ) ->
  {ok, {{200, []}, [
                    {"content-type", "application/json"}
                   ], <<"{
    \"any\": \"value\"
}
"/utf8>>}}.

%%% Test failure with runtime_validation

katt_run_with_runtime_validation_fail() ->
  Scenario = ?FUNCTION,
  ?_assertMatch( { fail
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, _, {fail, [ { not_equal
                                           , { "/body/{{runtime_validation}}"
                                             , false
                                             , true
                                             }}
                                         ]}}
                   ]
                 }
               , katt:run(Scenario)
               ).

katt_run_with_runtime_validation_fail_blueprint() ->
  katt_blueprint_parse:string(
    <<"--- Comparison as runtime_validation_fail ---

GET /katt_run_with_runtime_validation_fail
< 200
< Content-Type: application/json
{
    \"{{type}}\": \"runtime_validation\",
    \"erlang\": \"{not_equal,
                   {\\\"/body/{{runtime_validation}}\\\", false, true}
                  }\"
}
"/utf8>>).

katt_run_with_runtime_validation_fail_http( _
                                          , "GET"
                                          , _
                                          , _
                                          , _Timeout
                                          , _Options
                                          ) ->
  {ok, {{200, []}, [
                    {"content-type", "application/json"}
                   ], <<"
true
"/utf8>>}}.

%%% Helpers

mock_lhttpc_request(Url, Method, Hdrs, Body, Timeout, Options) ->
  Fun = list_to_atom(lists:nth(3, string:tokens(Url, "/")) ++ "_http"),
  Args = [Url, Method, Hdrs, Body, Timeout, Options],
  erlang:apply(?MODULE, Fun, Args).

mock_katt_blueprint_parse_file(Test) ->
  erlang:apply(?MODULE, list_to_atom(atom_to_list(Test) ++ "_blueprint"), []).
