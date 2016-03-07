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

katt_run_with_runtime_value() ->
  Scenario = "/mock/runtime_value.apib",
  ?_assertMatch( { pass
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, _, pass}
                   ]
                 }
               , katt:run(Scenario)
               ).

katt_run_with_runtime_value_array() ->
  Scenario = "/mock/runtime_value_array.apib",
  ?_assertMatch( { pass
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, _, pass}
                   ]
                 }
               , katt:run(Scenario)
               ).

katt_run_with_runtime_value_shell() ->
  Scenario = "/mock/runtime_value_shell.apib",
  ?_assertMatch( { pass
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, _, pass}
                   ]
                 }
               , katt:run(Scenario)
               ).

katt_run_with_runtime_validation_pass() ->
  Scenario = "/mock/runtime_validation_pass.apib",
  ?_assertMatch( { pass
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, _, pass}
                   ]
                 }
               , katt:run(Scenario)
               ).

katt_run_with_runtime_validation_fail() ->
  Scenario = "/mock/runtime_validation_fail.apib",
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

%%% Helpers

%% Mock response for runtime_value test:
mock_lhttpc_request( "http://127.0.0.1/runtime_value"
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
"/utf8>>}};

%% Mock response for runtime_value_array test:
mock_lhttpc_request( "http://127.0.0.1/runtime_value_array"
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
"/utf8>>}};

%% Mock response for runtime_value_shell test:
mock_lhttpc_request( "http://127.0.0.1/runtime_value_shell"
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
"/utf8>>}};

%% Mock response for runtime_validation_pass test:
mock_lhttpc_request( "http://127.0.0.1/runtime_validation_pass"
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
"/utf8>>}};

%% Mock response for runtime_validation_fail test:
mock_lhttpc_request( "http://127.0.0.1/runtime_validation_fail"
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

mock_katt_blueprint_parse_file("/mock/runtime_value.apib") ->
  katt_blueprint_parse:string(
    <<"--- Comparison as runtime_value ---

GET /runtime_value
< 200
< Content-Type: application/json
{
    \"{{type}}\": \"runtime_value\",
    \"erlang\": \"{array, [ParentKey, 1]}\"
}
"/utf8>>);

mock_katt_blueprint_parse_file("/mock/runtime_value_array.apib") ->
  katt_blueprint_parse:string(
    <<"--- Comparison as runtime_value (array) ---

GET /runtime_value_array
< 200
< Content-Type: application/json
{
    \"{{type}}\": \"runtime_value\",
    \"erlang\": [\"{ array\",
                 \", [ParentKey, 1]}\"
                ]
}
"/utf8>>);

mock_katt_blueprint_parse_file("/mock/runtime_value_shell.apib") ->
  katt_blueprint_parse:string(
    <<"--- Comparison as runtime_value (shell) ---

GET /runtime_value_shell
< 200
< Content-Type: application/json
{
    \"{{type}}\": \"runtime_value\",
    \"shell\": \"sh -c \\\"echo '{array, [ParentKey, 1]}'\\\"\"
}
"/utf8>>);

mock_katt_blueprint_parse_file("/mock/runtime_validation_pass.apib") ->
  katt_blueprint_parse:string(
    <<"--- Comparison as runtime_validation_pass ---

GET /runtime_validation_pass
< 200
< Content-Type: application/json
{
    \"{{type}}\": \"runtime_validation\",
    \"erlang\": \"{pass, [{\\\"Param\\\", \\\"Value\\\"}]}\"
}
"/utf8>>);

mock_katt_blueprint_parse_file("/mock/runtime_validation_fail.apib") ->
  katt_blueprint_parse:string(
    <<"--- Comparison as runtime_validation_fail ---

GET /runtime_validation_fail
< 200
< Content-Type: application/json
{
    \"{{type}}\": \"runtime_validation\",
    \"erlang\": \"{not_equal,
                   {\\\"/body/{{runtime_validation}}\\\", false, true}
                  }\"
}
"/utf8>>).
