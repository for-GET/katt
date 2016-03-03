%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%% KATT Run - Unexpected Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(katt_run_unexpected_tests).

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
      meck:expect( katt_http_client_hackney
                 , request
                 , fun mock_hackney_request/5
                 )
    end
  , fun(_) ->
      meck:unload(katt_blueprint_parse),
      meck:unload(katt_callbacks)
    end
  , [ katt_run_with_unexpected_disallow()
    , katt_run_with_expected_but_undefined()
    , katt_run_with_unexpected_and_undefined()
    ]
  }.


%%% Tests

katt_run_with_unexpected_disallow() ->
  Scenario = "/mock/unexpected-disallow.apib",
  ?_assertMatch( { fail
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, _, {fail, [ { unexpected
                                           , {"/body/extra_object/key", _, _}
                                           },
                                           { unexpected
                                           , {"/body/extra_array/0", _, _}
                                           }
                                         ]}}
                   ]
                 }
               , katt:run(Scenario)
               ).

katt_run_with_expected_but_undefined() ->
  Scenario = "/mock/expected-but-undefined.apib",
  ?_assertMatch( { fail
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, _, {fail, [ { not_equal
                                           , {"/body/expected", _, _}
                                           }
                                         ]}}
                   ]
                 }
               , katt:run(Scenario)
               ).

katt_run_with_unexpected_and_undefined() ->
  Scenario = "/mock/unexpected-and-undefined.apib",
  ?_assertMatch( { pass
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, _, pass}
                   ]
                 }
               , katt:run(Scenario)
               ).

%%% Helpers

%% Mock response for unexpected disallow test:
mock_hackney_request( "GET"
                   , "http://127.0.0.1/unexpected-disallow"
                   , _
                   , _
                   , _Timeout
                   ) ->
  {ok, {{200, []}, [{"Content-Type", "application/json"}], <<"{
    \"ok\": true,
    \"extra_object\": {
        \"key\": \"test\"
    },
    \"extra_array\": [\"test\"],
    \"extra_array_2\": []
}
"/utf8>>}};

%% Mock response for expected but undefined test:
mock_hackney_request( "GET"
                   , "http://127.0.0.1/expected-but-undefined"
                   , _
                   , _
                   , _Timeout
                   ) ->
  {ok, {{200, []}, [{"Content-Type", "application/json"}], <<"{
}
"/utf8>>}};

%% Mock response for unexpected and undefined test:
mock_hackney_request( "GET"
                   , "http://127.0.0.1/unexpected-and-undefined"
                   , _
                   , _
                   , _Timeout
                   ) ->
  {ok, {{200, []}, [{"Content-Type", "application/json"}], <<"{
}
"/utf8>>}}.

mock_katt_blueprint_parse_file("/mock/unexpected-disallow.apib") ->
  katt_blueprint_parse:string(
    <<"--- Test 4 ---

GET /unexpected-disallow
< 200
< Content-Type: application/json
{
    \"ok\": true,
    \"extra_object\": {
        \"{{_}}\": \"{{unexpected}}\"
    },
    \"extra_array\": [\"{{unexpected}}\"],
    \"extra_array_2\": [\"{{unexpected}}\"]
}
"/utf8>>);

mock_katt_blueprint_parse_file("/mock/expected-but-undefined.apib") ->
  katt_blueprint_parse:string(
    <<"--- Test 5 ---

GET /expected-but-undefined
< 200
< Content-Type: application/json
{
    \"expected\": \"{{>defined_value}}\"
}
"/utf8>>);

mock_katt_blueprint_parse_file("/mock/unexpected-and-undefined.apib") ->
  katt_blueprint_parse:string(
    <<"--- Test 6 ---

GET /unexpected-and-undefined
< 200
< Content-Type: application/json
{
    \"expected\": \"{{unexpected}}\"
}
"/utf8>>).
