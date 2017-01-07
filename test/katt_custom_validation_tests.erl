%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright 2016 Klarna AB
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
%%% @copyright 2016 Klarna AB
%%%
%%% KATT tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(katt_custom_validation_tests).


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
                 ),
        
      meck:new(katt_test_util, [passthrough, non_strict]),
      meck:expect(katt_test_util, return_arg, fun(_,A) ->
          {pass, [{"foo",  A}] } 
        end),
      meck:expect(katt_test_util, return_one, fun(_) ->
          {pass,[ {"foo", "bar"}] } 
        end),
      meck:expect(katt_test_util, return_two, fun(_) ->
          {pass, [{"foo", "bar"} ,  {"foo2", "bar2"} ]}
        end),
      meck:expect(katt_test_util, return_pass, fun(_) ->
          {pass, []} 
        end),
      meck:expect(katt_test_util, return_error, fun(_) ->
          [{not_equal, {"foo", "bar"} }]
        end)
    end
  , fun(_) ->
      meck:unload(katt_blueprint_parse),
      meck:unload(katt_callbacks),
      meck:unload(katt_test_util)
    end
  , [ katt_run_with_custom_function_return_one()
    , katt_run_with_custom_function_return_two()
    , katt_run_with_custom_function_return_pass()
    , katt_run_with_custom_function_return_error()
    , katt_run_with_custom_function_return_arg()
    , katt_run_with_custom_function_on_object()
    , katt_run_with_custom_function_on_array()
    , katt_run_with_custom_function_on_array_empty()
    , katt_run_with_custom_function_missing()
    , katt_run_with_custom_function_special_chars()
    ]
  }.

%%% Tests

katt_run_with_custom_function_return_one() ->
   Scenario = "/mock/custom-function-1.apib",
   ?_assertMatch( { pass
                  , Scenario
                  , _
                  , [ {"foo", "bar"} | _]
                  , [ {_, _, _, _, pass}
                    ]
                  }
                  , katt:run(Scenario)
                ).
katt_run_with_custom_function_return_two() ->
   Scenario = "/mock/custom-function-2.apib",
   ?_assertMatch( { pass
                  , Scenario
                  , _
                  , [  {"foo", "bar"}, {"foo2", "bar2"}, _ ,_ ,_ ,_, _ ]
                  , [ {_, _, _, _, pass}
                    ]  
                  }
                  , katt:run(Scenario)
                ).

katt_run_with_custom_function_return_pass() ->
   Scenario = "/mock/custom-function-pass.apib",
   ?_assertMatch( { pass
                  , Scenario
                  , _
                  , _
                  , _
                  }
                  , katt:run(Scenario)
                ).

katt_run_with_custom_function_return_error() ->
   Scenario = "/mock/custom-function-error.apib",
  ?_assertMatch( { fail
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, _, {fail, [ { not_equal
                                           , {"foo", "bar"}
                                           }
                                         ]}}
                   ]
                 }
               , katt:run(Scenario)
               ).

katt_run_with_custom_function_return_arg() ->
   Scenario = "/mock/custom-function-arg.apib",
   ?_assertMatch( { pass
                  , Scenario
                  , _
                  , [  {"foo", arg} | _]
                  , [ {_, _, _, _, pass}
                    ]
                  }
                  , katt:run(Scenario)
                ).

katt_run_with_custom_function_on_array() ->
   Scenario = "/mock/custom-function-array.apib",
   ?_assertMatch( { pass
                  , Scenario
                  , _
                  , [ {"foo", "bar"} | _] 
                  , [ {_, _, _, _, pass}
                    ]
                  }
                  , katt:run(Scenario)
                ).

katt_run_with_custom_function_on_array_empty () ->
   Scenario = "/mock/custom-function-array-empty.apib",
   ?_assertMatch( { pass
                  , Scenario
                  , _
                  , [ {"foo", "bar"} | _ ]
                  , [ {_, _, _, _, pass}
                    ]
                  }
                  , katt:run(Scenario)
                ).

katt_run_with_custom_function_on_object() ->
   Scenario = "/mock/custom-function-object.apib",
   ?_assertMatch( { pass
                  , Scenario
                  , _
                  , [ {"foo", "bar"} | _ ]
                  , [ {_, _, _, _, pass}
                    ]
                  }
                  , katt:run(Scenario)
                ).

katt_run_with_custom_function_missing() ->
   Scenario = "/mock/custom-function-missing.apib",
   ?_assertMatch( { pass
                  , Scenario 
                  , _
                  , _
                  , _
                  }
                  , katt:run(Scenario)
                ).

katt_run_with_custom_function_special_chars() ->
   Scenario = "/mock/custom-function-special-chars.apib",
   ?_assertMatch( { pass
                  , Scenario
                  , _
                  , [ {"foo", "(){{}}"}|_]
                  , [ {_, _, _, _, pass}
                    ]
                  }
                  , katt:run(Scenario)
                ).
%%% Helpers

%% Mock response for custom function output:
mock_lhttpc_request( "http://127.0.0.1/custom-function"
                   , "GET"
                   , _
                   , _
                   , _Timeout
                   , _Options
                   ) ->
  {ok, {{200, []}, [{"Content-Type", "application/json"}], <<"{
    \"snippet\":\" Some Text  \"
  }
"/utf8>>}};

%% Mock response for custom output function where node is array
mock_lhttpc_request( "http://127.0.0.1/custom-function-array"
                   , "GET"
                   , _
                   , _
                   , _Timeout
                   , _Options
                   ) ->
  {ok, {{200, []}, [{"Content-Type", "application/json"}], <<"{
    \"snippet\":[\" Some Text  \"]
  }
"/utf8>>}};

%% Mock response for custom output function where node is empty array
mock_lhttpc_request( "http://127.0.0.1/custom-function-array-empty"
                   , "GET"
                   , _
                   , _
                   , _Timeout
                   , _Options
                   ) ->
  {ok, {{200, []}, [{"Content-Type", "application/json"}], <<"{
    \"snippet\":[]
  }
"/utf8>>}};

%% Mock response for custom output function where node is object
mock_lhttpc_request( "http://127.0.0.1/custom-function-object"
                   , "GET"
                   , _
                   , _
                   , _Timeout
                   , _Options
                   ) ->
  {ok, {{200, []}, [{"Content-Type", "application/json"}], <<"{
    \"snippet\":{ \"Field\": \" Some Text  \"}
  }
"/utf8>>}}.


mock_katt_blueprint_parse_file("/mock/custom-function-1.apib") ->
   katt_blueprint_parse:string(
    <<"--- Test 1 ---

GET /custom-function
< 200
< Content-Type: application/json
{
    \"snippet\": \"{{{katt_test_util:return_one()}}}\"
}
"/utf8>>);

mock_katt_blueprint_parse_file("/mock/custom-function-2.apib") ->
   katt_blueprint_parse:string(
    <<"--- Test 2 ---

GET /custom-function
< 200
< Content-Type: application/json
{
    \"snippet\": \"{{{katt_test_util:return_two()}}}\"
}
"/utf8>>);

mock_katt_blueprint_parse_file("/mock/custom-function-pass.apib") ->
   katt_blueprint_parse:string(
    <<"--- Test 3 ---

GET /custom-function
< 200
< Content-Type: application/json
{
    \"snippet\": \"{{{katt_test_util:return_pass()}}}\"
}
"/utf8>>);

mock_katt_blueprint_parse_file("/mock/custom-function-error.apib") ->
   katt_blueprint_parse:string(
    <<"--- Test 4 ---

GET /custom-function
< 200
< Content-Type: application/json
{
    \"snippet\": \"{{{katt_test_util:return_error()}}}\"
}
"/utf8>>);


mock_katt_blueprint_parse_file("/mock/custom-function-arg.apib") ->
   katt_blueprint_parse:string(
    <<"--- Test 5 ---

GET /custom-function
< 200
<  Content-Type: application/json
{
    \"snippet\": \"{{{katt_test_util:return_arg(arg)}}}\"
}
"/utf8>>);

mock_katt_blueprint_parse_file("/mock/custom-function-array.apib") ->
   katt_blueprint_parse:string(
    <<"--- Test 6 ---

GET /custom-function-array
< 200
< Content-Type: application/json
{
    \"snippet\": \"{{{katt_test_util:return_one()}}}\"
}
"/utf8>>);
mock_katt_blueprint_parse_file("/mock/custom-function-array-empty.apib") ->
   katt_blueprint_parse:string(
    <<"--- Test 7 ---

GET /custom-function-array-empty
< 200
< Content-Type: application/json
{
    \"snippet\": \"{{{katt_test_util:return_one()}}}\"
}
"/utf8>>);
mock_katt_blueprint_parse_file("/mock/custom-function-object.apib") ->
   katt_blueprint_parse:string(
    <<"--- Test 8 ---

GET /custom-function-object
< 200
< Content-Type: application/json
{
    \"snippet\": \"{{{katt_test_util:return_one()}}}\"
}
"/utf8>>);
mock_katt_blueprint_parse_file("/mock/custom-function-missing.apib") ->
   katt_blueprint_parse:string(
    <<"--- Test 9 ---

GET /custom-function
< 200
< Content-Type: application/json
{
    \"snippeti2\": \"{{{katt_test_util:return_one()}}}\"
}
"/utf8>>);

mock_katt_blueprint_parse_file("/mock/custom-function-special-chars.apib") ->
   katt_blueprint_parse:string(
    <<"--- Test 10 ---

GET /custom-function
< 200
< Content-Type: application/json
{
    \"snippet\": \"{{{katt_test_util:return_arg(\\\"(){{}}\\\")}}}\"
}
"/utf8>>).
%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
