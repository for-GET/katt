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
%%% KATT tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(katt_tests).

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
      meck:new(lhttpc, [passthrough]),
      meck:expect( lhttpc
                 , request
                 , fun mock_lhttpc_request/6
                 )
    end
  , fun(_) ->
      meck:unload(katt_blueprint_parse),
      meck:unload(lhttpc)
    end
  , [ katt_run_basic()
    , katt_run_with_params()
    , katt_run_with_api_mismatch()
    , katt_run_with_unexpected_disallow()
    , katt_run_with_expected_but_undefined()
    , katt_run_with_unexpected_and_undefined()
    , katt_run_with_store()
    , katt_run_with_set_comparison_strict()
    , katt_run_with_set_comparison_strict_fails()
    , katt_run_with_set_comparison_unlimited()
    , katt_run_with_set_comparison_unlimited_fails()
    ]
  }.


%%% Tests

katt_run_basic() ->
  Scenario = "/mock/basic.apib",
  ?_assertMatch( { pass
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, _, pass}
                   , {_, _, _, _, pass}
                   , {_, _, _, _, pass}
                   , {_, _, _, _, pass}
                   , {_, _, _, _, pass}
                   ]
                 }
               , katt:run(Scenario)
               ).

katt_run_with_params() ->
  Scenario = "/mock/test-params.apib",
  ?_assertMatch( { pass
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, _, pass}
                   ]
                 }
               , katt:run( Scenario
                         , [ {hostname, "example.com"}
                           , {some_var, "hi"}
                           , {version, "1"}
                           , {syntax, json}
                           , {test_null, null}
                           , {test_boolean, true}
                           , {test_integer, 1}
                           , {test_float, 1.1}
                           , {test_string, "string"}
                           , {test_binary, <<"binary"/utf8>>}
                           ]
                         )
               ).

katt_run_with_api_mismatch() ->
  Scenario = "/mock/api-mismatch.apib",
  ?_assertMatch( { fail
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, _, {fail, [ {not_equal, {"/status", _, _}}
                                         , {not_equal, {"/body/ok", _, _}}
                                         ]}}
                   ]
                 }
               , katt:run(Scenario)
               ).

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

katt_run_with_store() ->
  Scenario = "/mock/store.apib",
  ?_assertMatch( { pass
                 , Scenario
                 , _
                 , [ _
                   , _
                   , _
                   , _
                   , _
                   , {"param1", "param1"}
                   , {"param2", "param2"}
                   , {"param3", "param3"}
                   , {"param4", "param4"}
                   , {"param5", "param5"}
                   ]
                 , [ {_, _, _, _, pass}
                   ]
                 }
               , katt:run(Scenario)
               ).

katt_run_with_set_comparison_strict() ->
    Scenario = "/mock/set_comparison_strict.apib",
    ?_assertMatch( { pass
                     , Scenario
                     , _
                     , _
                     , [ {_, _, _, _, pass}
                       ]
                     }
                    , katt:run(Scenario)
                    ).
katt_run_with_set_comparison_strict_fails() ->
  Scenario = "/mock/set_comparison_strict_fails.apib",
  ?_assertMatch( { fail
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, _, {fail, [ {unexpected,"/body/set_of_objects/{{set}}/0"}
                                         ]}}
                   ]
                 }
               , katt:run(Scenario)
               ).
katt_run_with_set_comparison_unlimited() ->
  Scenario = "/mock/set_comparison_unlimited.apib",
  ?_assertMatch( { pass
    , Scenario
    , _
    , _
    , [ {_, _, _, _, pass}
    ]
  }
    , katt:run(Scenario)
  ).
katt_run_with_set_comparison_unlimited_fails() ->
  Scenario = "/mock/set_comparison_unlimited_fails.apib",
  ?_assertMatch( { fail
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, _, {fail, [ {not_contains,"/body/set_of_objects/{{set}}/2"}
                                         ]}}
                   ]
                 }
               , katt:run(Scenario)
               ).


%%% Helpers

%% Mock response for Step 2:
%% (default hostname is 127.0.0.1, default port is 80, default protocol is http)
mock_lhttpc_request( "http://127.0.0.1/step1" = _Url
                   , "POST" = _Method
                   , _Headers
                   , _Body
                   , _Timeout
                   , _Options
                   ) ->
  {ok, {{201, []}, [{"Location", "http://127.0.0.1/step2"}], <<>>}};
%% Mock response for Step 2:
mock_lhttpc_request( "http://127.0.0.1/step2"
                   , "GET"
                   , [{"Accept", "application/json"}]
                   , <<>>
                   , _Timeout
                   , _Options
                   ) ->
  {ok, {{200, []}, [{"Content-Type", "application/json"}], <<"{
    \"required_fields\": [
        \"email\"
    ],
    \"cart\": \"{{_}}\",
    \"extra_object\": {
      \"key\": \"test\"
    },
    \"extra_array\": [\"test\"]
}

"/utf8>>}};
%% Mock response for Step 3:
mock_lhttpc_request( "http://127.0.0.1/step2/step3"
                   , "POST"
                   , _
                   , _
                   , _Timeout
                   , _Options
                   ) ->
  {ok, {{200, []}, [{"Content-Type", "application/json"}], <<"{
    \"required_fields\": [
        \"password\"
    ],
    \"cart\": {\"item1\": true}
}
"/utf8>>}};
%% Mock response for Step 4:
mock_lhttpc_request( "http://127.0.0.1/step2/step4"
                   , "POST"
                   , _
                   , _
                   , _Timeout
                   , _Options
                   ) ->
  {ok, {{402, []}, [{"Content-Type", "application/json"}], <<"{
    \"error\": \"payment required\"
}
"/utf8>>}};
%% Mock response for Step 5:
mock_lhttpc_request( "http://127.0.0.1/step5"
                   , "HEAD"
                   , _
                   , _
                   , _Timeout
                   , _Options
                   ) ->
  {ok, {{404, "Not found"}, [{"Content-Type", "text/html"}], <<>>}};

%% Mock response for test-params:
mock_lhttpc_request( "http://example.com/test-params"
                   , "POST"
                   , _
                   , _
                   , _Timeout
                   , _Options
                   ) ->
  {ok, {{200, []}, [{"Content-Type", "application/vnd.katt.test-v1+json"}], <<"{
    \"protocol\": \"http:\",
    \"hostname\": \"example.com\",
    \"port\": 80,
    \"some_var\": \"hi\",
    \"some_var3\": \"hihihi\",
    \"null\": null,
    \"boolean\": true,
    \"integer\": 1,
    \"float\": 1.1,
    \"string\": \"string\",
    \"binary\": \"binary\"
}
"/utf8>>}};

%% Mock response for api mismatch test:
mock_lhttpc_request( "http://127.0.0.1/api-mismatch"
                   , "POST"
                   , [ {"Accept", "application/json"}
                     , {"Content-Type","application/json"}
                     ]
                   , _
                   , _Timeout
                   , _Options
                   ) ->
  {ok, {{401, []}, [{"Content-Type", "application/json"}], <<"{
    \"error\": \"unauthorized\"
}
"/utf8>>}};

%% Mock response for unexpected disallow test:
mock_lhttpc_request( "http://127.0.0.1/unexpected-disallow"
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
"/utf8>>}};

%% Mock response for expected but undefined test:
mock_lhttpc_request( "http://127.0.0.1/expected-but-undefined"
                   , "GET"
                   , _
                   , _
                   , _Timeout
                   , _Options
                   ) ->
  {ok, {{200, []}, [{"Content-Type", "application/json"}], <<"{
}
"/utf8>>}};

%% Mock response for unexpected and undefined test:
mock_lhttpc_request( "http://127.0.0.1/unexpected-and-undefined"
                   , "GET"
                   , _
                   , _
                   , _Timeout
                   , _Options
                   ) ->
  {ok, {{200, []}, [{"Content-Type", "application/json"}], <<"{
}
"/utf8>>}};

%% Mock response for store (and case-insensitive http headers) test:
mock_lhttpc_request( "http://127.0.0.1/store"
                   , "GET"
                   , _
                   , _
                   , _Timeout
                   , _Options
                   ) ->
  {ok, {{200, []}, [{"content-type", "application/json"},
                    {"set-cookie", "mycookie=param1; path=param2;"},
                    {"x-foo", "param3"},
                    {"x-bar", "bazparam4"}], <<"{
    \"param5\": \"param5\"
}
"/utf8>>}};

%% Mock response for set_comparison (match a finite set of values) test:
mock_lhttpc_request( "http://127.0.0.1/set_comparison_strict"
    , "GET"
    , _
    , _
    , _Timeout
    , _Options
) ->
  {ok, {{200, []}, [
    {"content-type", "application/json"}
  ], <<"{
    \"set_of_objects\": [{\"number\":2},{\"number\":1}]
}
"/utf8>>}};
%% Mock response for set_comparison (match a finite set of values) test:
mock_lhttpc_request( "http://127.0.0.1/set_comparison_unlimited"
    , "GET"
    , _
    , _
    , _Timeout
    , _Options
) ->
    {ok, {{200, []}, [
        {"content-type", "application/json"}
    ], <<"{
    \"set_of_objects\": [{\"number\":2},{\"number\":1}, {\"another_number\":3}]
}
"/utf8>>}};

%% Mock response for set_comparison_unexpected (match any set of value, except the unexpected) test:
mock_lhttpc_request( "http://127.0.0.1/set_comparison_unexpected"
    , "GET"
    , _
    , _
    , _Timeout
    , _Options
) ->
    {ok, {{200, []}, [
        {"content-type", "application/json"}
    ], <<"{
    \"set_of_objects\": [{\"number\":2},{\"number\":1},{\"number\":0}]
}
"/utf8>>}}.


mock_katt_blueprint_parse_file("/mock/basic.apib") ->
  katt_blueprint_parse:string(
    <<"--- Test 1 ---

---
Some description
---

# Step 1

The merchant creates a new example object on our server, and we respond with
the location of the created example.

POST /step1
> Accept: application/json
> Content-Type: application/json
{
    \"cart\": {
        \"items\": [
            {
                \"name\": \"Horse\",
                \"quantity\": 1,
                \"unit_price\": 4495000
            },
            {
                \"name\": \"Battery\",
                \"quantity\": 4,
                \"unit_price\": 1000
            },
            {
                \"name\": \"Staple\",
                \"quantity\": 1,
                \"unit_price\": 12000
            }
        ]
    }
}
< 201
< Location: {{>example_uri}}


# Step 2

The client (customer) fetches the created resource data.

GET {{<example_uri}}
> Accept: application/json
< 200
< Content-Type: application/json
{
    \"required_fields\": [
        \"email\"
    ],
    \"cart\": \"{{_}}\"
}


# Step 3

The customer submits an e-mail address in the form.

POST {{<example_uri}}/step3
> Accept: application/json
> Content-Type: application/json
{
    \"email\": \"test-customer@foo.klarna.com\"
}
< 200
< Content-Type: application/json
{
    \"required_fields\": [
        \"password\"
    ],
    \"cart\": \"{{_}}\"
}


# Step 4

The customer submits the form again, this time also with his password.
We inform him that payment is required.

POST {{<example_uri}}/step4
> Accept: application/json
> Content-Type: application/json
{
    \"email\": \"test-customer@foo.klarna.com\",
    \"password\": \"correct horse battery staple\"
}
< 402
< Content-Type: application/json
{
    \"error\": \"payment required\"
}


# Step 5

HEAD /step5
< 404
< Content-Type: text/html
<<<
>>>
"/utf8>>);

mock_katt_blueprint_parse_file("/mock/test-params.apib") ->
  katt_blueprint_parse:string(
    <<"--- Test 2 ---

POST /test-params
< 200
< Content-Type: application/vnd.katt.test-v{{<version}}+{{<syntax}}
{
    \"protocol\": \"{{<protocol}}\",
    \"hostname\": \"{{<hostname}}\",
    \"port\": \"{{<port}}\",
    \"some_var\": \"{{<some_var}}\",
    \"some_var3\": \"hi{{<some_var}}hi\",
    \"boolean\": \"{{<test_boolean}}\",
    \"null\": \"{{<test_null}}\",
    \"integer\": \"{{<test_integer}}\",
    \"float\": \"{{<test_float}}\",
    \"string\": \"{{<test_string}}\",
    \"binary\": \"{{<test_binary}}\"
}
"/utf8>>);

mock_katt_blueprint_parse_file("/mock/api-mismatch.apib") ->
  katt_blueprint_parse:string(
    <<"--- Test 3 ---

POST /api-mismatch
> Accept: application/json
> Content-Type: application/json
{}
< 200
< Content-Type: application/json
{ \"ok\": true }
"/utf8>>);

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
"/utf8>>);

mock_katt_blueprint_parse_file("/mock/store.apib") ->
  katt_blueprint_parse:string(
    <<"--- Test 7 ---

GET /store
< 200
< Content-Type: application/json
< Set-Cookie: mycookie={{>param1}}; path={{>param2}};
< X-Foo: {{>param3}}
< X-Bar: baz{{>param4}}
{
    \"param5\": \"{{>param5}}\"
}
"/utf8>>);

mock_katt_blueprint_parse_file("/mock/set_comparison_strict.apib") ->
    katt_blueprint_parse:string(
        <<"--- Comparison as set ---

GET /set_comparison_strict
< 200
< Content-Type: application/json
{
    \"set_of_objects\": {
        \"{{type}}\" : \"set\",
        \"value\"    : [{\"number\":1},{\"number\":2},\"{{unexpected}}\"]
    }
}
"/utf8>>);
mock_katt_blueprint_parse_file("/mock/set_comparison_strict_fails.apib") ->
  katt_blueprint_parse:string(
    <<"--- Comparison as set ---

GET /set_comparison_strict
< 200
< Content-Type: application/json
{
    \"set_of_objects\": {
        \"{{type}}\" : \"set\",
        \"value\"    : [{\"number\":1},\"{{unexpected}}\"]
    }
}
"/utf8>>);

mock_katt_blueprint_parse_file("/mock/set_comparison_unlimited.apib") ->
  katt_blueprint_parse:string(
    <<"--- Comparison as set ---

GET /set_comparison_unlimited
< 200
< Content-Type: application/json
{
    \"set_of_objects\": {
        \"{{type}}\" : \"set\",
        \"value\"    : [{\"number\":1},{\"number\":2}]
    }
}
"/utf8>>);
mock_katt_blueprint_parse_file("/mock/set_comparison_unlimited_fails.apib") ->
  katt_blueprint_parse:string(
    <<"--- Comparison as set ---

GET /set_comparison_unlimited
< 200
< Content-Type: application/json
{
    \"set_of_objects\": {
        \"{{type}}\" : \"set\",
        \"value\"    : [{\"number\":1},{\"number\":2},{\"number\":3}]
    }
}
"/utf8>>).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
