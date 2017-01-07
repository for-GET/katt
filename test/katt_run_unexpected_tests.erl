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

-define( FUNCTION
       , element(2, element(2, process_info(self(), current_function)))
       ).

-export([ katt_run_with_unexpected_disallow_blueprint/0
        , katt_run_with_unexpected_disallow_http/6
        , katt_run_with_expected_but_undefined_blueprint/0
        , katt_run_with_expected_but_undefined_http/6
        , katt_run_with_unexpected_and_undefined_blueprint/0
        , katt_run_with_unexpected_and_undefined_http/6
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
  , [ katt_run_with_unexpected_disallow()
    , katt_run_with_expected_but_undefined()
    , katt_run_with_unexpected_and_undefined()
    ]
  }.


%%% Tests

%%% Test with unexpected disallow

katt_run_with_unexpected_disallow() ->
  Scenario = ?FUNCTION,
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

katt_run_with_unexpected_disallow_blueprint() ->
  katt_blueprint_parse:string(
    <<"--- Test 4 ---

GET /katt_run_with_unexpected_disallow
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
"/utf8>>).

katt_run_with_unexpected_disallow_http( _
                                      , "GET"
                                      , _
                                      , _
                                      , _Timeout
                                      , _Options
                                      ) ->
  {ok, {{200, []}, [{"Content-Type", "application/json"}], <<"{
    \"ok\": true,
    \"extra_object\": {
        \"key\": \"test\"
    },
    \"extra_array\": [\"test\"],
    \"extra_array_2\": []
}
"/utf8>>}}.

%%% Test with expected but undefined

katt_run_with_expected_but_undefined() ->
  Scenario = ?FUNCTION,
  ?_assertMatch( { fail
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, _, {fail, [ { expected
                                           , {"/body/expected", _, _}
                                           }
                                         ]}}
                   ]
                 }
               , katt:run(Scenario)
               ).

katt_run_with_expected_but_undefined_blueprint() ->
  katt_blueprint_parse:string(
    <<"--- Test 5 ---

GET /katt_run_with_expected_but_undefined
< 200
< Content-Type: application/json
{
    \"expected\": \"{{expected}}\"
}
"/utf8>>).

katt_run_with_expected_but_undefined_http( _
                                         , "GET"
                                         , _
                                         , _
                                         , _Timeout
                                         , _Options
                                         ) ->
  {ok, {{200, []}, [{"Content-Type", "application/json"}], <<"{
}
"/utf8>>}}.

%%% Test with unexpected and undefined

katt_run_with_unexpected_and_undefined() ->
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


katt_run_with_unexpected_and_undefined_blueprint() ->
  katt_blueprint_parse:string(
    <<"--- Test 6 ---

GET /katt_run_with_unexpected_and_undefined
< 200
< Content-Type: application/json
{
    \"expected\": \"{{unexpected}}\"
}
"/utf8>>).

katt_run_with_unexpected_and_undefined_http( _
                                           , "GET"
                                           , _
                                           , _
                                           , _Timeout
                                           , _Options
                                           ) ->
  {ok, {{200, []}, [{"Content-Type", "application/json"}], <<"{
}
"/utf8>>}}.

%%% Helpers

mock_lhttpc_request(Url, Method, Hdrs, Body, Timeout, Options) ->
  Fun = list_to_atom(lists:nth(3, string:tokens(Url, "/")) ++ "_http"),
  Args = [Url, Method, Hdrs, Body, Timeout, Options],
  erlang:apply(?MODULE, Fun, Args).

mock_katt_blueprint_parse_file(Test) ->
  erlang:apply(?MODULE, list_to_atom(atom_to_list(Test) ++ "_blueprint"), []).
