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
%%% KATT Run Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(katt_run_tests).

-include_lib("eunit/include/eunit.hrl").

-define( FUNCTION
       , element(2, element(2, process_info(self(), current_function)))
       ).

-export([ katt_run_basic_blueprint/0
        , katt_run_basic_http/6
        , katt_run_with_params_blueprint/0
        , katt_run_with_params_http/6
        , katt_run_with_api_mismatch_blueprint/0
        , katt_run_with_api_mismatch_http/6
        , katt_run_with_store_blueprint/0
        , katt_run_with_store_http/6
        , katt_run_with_struct_blueprint/0
        , katt_run_with_struct_http/6
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
  , [ katt_run_basic()
    , katt_run_with_params()
    , katt_run_with_api_mismatch()
    , katt_run_with_store()
    , katt_run_with_struct()
    ]
  }.


%%% Tests

%%% Basic test

katt_run_basic() ->
  Scenario = ?FUNCTION,
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

katt_run_basic_blueprint() ->
  katt_blueprint_parse:string(
    <<"--- Test 1 ---

---
Some description
---

# Step 1

The merchant creates a new example object on our server, and we respond with
the location of the created example.

POST /katt_run_basic/step1
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
< Cache-Control: no-cache
< Cache-Control: {{_}}
< Cache-Control: must-revalidate
< X-Another-Duplicate-Header: foo, bar


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

HEAD /katt_run_basic/step5
< 404
< Content-Type: text/html
<<<
>>>
"/utf8>>).

%% (default hostname is 127.0.0.1, default port is 80, default protocol is http)
katt_run_basic_http( "http://127.0.0.1/katt_run_basic/step1"
                   , "POST" = _Method
                   , _Headers
                   , _Body
                   , _Timeout
                   , _Options
                   ) ->
  {ok, { {201, []}
       , [ {"Location", "http://127.0.0.1/katt_run_basic/step2"}
         , {"Cache-Control", "no-cache"}
         , {"Cache-Control", "no-store"}
         , {"Cache-Control", "must-revalidate"}
         , {"X-Another-Duplicate-Header", "foo"}
         , {"X-Another-Duplicate-Header", "bar"}
         ]
       , <<>>}};
katt_run_basic_http( "http://127.0.0.1/katt_run_basic/step2"
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
katt_run_basic_http( "http://127.0.0.1/katt_run_basic/step2/step3"
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
katt_run_basic_http( "http://127.0.0.1/katt_run_basic/step2/step4"
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
katt_run_basic_http( "http://127.0.0.1/katt_run_basic/step5"
                   , "HEAD"
                   , _
                   , _
                   , _Timeout
                   , _Options
                   ) ->
  {ok, {{404, []}, [{"Content-Type", "text/html"}], <<>>}}.

%%% Test with params

katt_run_with_params() ->
  Scenario = ?FUNCTION,
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

katt_run_with_params_blueprint() ->
  katt_blueprint_parse:string(
    <<"--- Test 2 ---

POST /katt_run_with_params
< 200
< Content-Type: application/vnd.katt.test-v{{<version}}+{{<syntax}}
{
    \"base_url\": \"{{<base_url}}\",
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
"/utf8>>).

katt_run_with_params_http( _
                         , "POST"
                         , _
                         , _
                         , _Timeout
                         , _Options
                         ) ->
  {ok, {{200, []}, [{"Content-Type", "application/vnd.katt.test-v1+json"}], <<"{
    \"base_url\": \"http://example.com\",
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
"/utf8>>}}.

%%% Test with API mismatch

katt_run_with_api_mismatch() ->
  Scenario = ?FUNCTION,
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

katt_run_with_api_mismatch_blueprint() ->
  katt_blueprint_parse:string(
    <<"--- Test 3 ---

POST /katt_run_with_api_mismatch
> Accept: application/json
> Content-Type: application/json
{}
< 200
< Content-Type: application/json
{ \"ok\": true }
"/utf8>>).

katt_run_with_api_mismatch_http( _
                               , "POST"
                               , [ {"Accept", "application/json"}
                                 , {"Content-Type", "application/json"}
                                 ]
                               , _
                               , _Timeout
                               , _Options
                               ) ->
  {ok, {{401, []}, [{"Content-Type", "application/json"}], <<"{
    \"error\": \"unauthorized\"
}
"/utf8>>}}.

%%% Test with store (and case-insensitive http headers)

katt_run_with_store() ->
  Scenario = ?FUNCTION,
  ?_assertMatch( { pass
                 , Scenario
                 , _
                 , [ _
                   , _
                   , {"param1", "param1"}
                   , {"param2", "param2"}
                   , {"param3", "param3"}
                   , {"param4", "param4"}
                   , {"param5", "param5"}
                   , _
                   , _
                   , _
                   , _
                   ]
                 , [ {_, _, _, _, pass}
                   ]
                 }
               , katt:run(Scenario)
               ).

katt_run_with_store_blueprint() ->
  katt_blueprint_parse:string(
    <<"--- Test 7 ---

GET /katt_run_with_store
< 200
< Content-Type: application/json
< Set-Cookie: mycookie={{>param1}}; path={{>param2}};
< X-Foo: {{>param3}}
< X-Bar: baz{{>param4}}
< X-Qux: qux{{_}}
{
    \"param5\": \"{{>param5}}\"
}
"/utf8>>).

katt_run_with_store_http( _
                        , "GET"
                        , _
                        , _
                        , _Timeout
                        , _Options
                        ) ->
  {ok, {{200, []}, [{"content-type", "application/json"},
                    {"set-cookie", "mycookie=param1; path=param2;"},
                    {"x-foo", "param3"},
                    {"x-bar", "bazparam4"},
                    {"x-qux", "quxnorf"}], <<"{
    \"param5\": \"param5\"
}
"/utf8>>}}.

%%% Test with struct

katt_run_with_struct() ->
  Scenario = ?FUNCTION,
  ?_assertMatch( { fail
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, _, { fail
                                  , [ {not_equal, {"/body/not_object", _, _}}
                                    , {not_equal, {"/body/not_array", _, _}}
                                    ]
                                  }}
                   ]
                 }
               , katt:run(Scenario)
               ).

katt_run_with_struct_blueprint() ->
  katt_blueprint_parse:string(
    <<"--- Test 8 ---

GET /katt_run_with_struct
> x-katt-request-sleep: 500
> x-katt-request-timeout: 50000
< 200
< Content-Type: application/json
{
    \"array\": [],
    \"object\": {},
    \"not_array\": {},
    \"not_object\": []
}
"/utf8>>).

katt_run_with_struct_http( _
                         , "GET"
                         , _
                         , _
                         , _Timeout
                         , _Options
                         ) ->
  {ok, {{200, []}, [{"content-type", "application/json"}], <<"{
    \"array\": [],
    \"object\": {},
    \"not_array\": [],
    \"not_object\": {}
}
"/utf8>>}}.

%%% Helpers

mock_lhttpc_request(Url, Method, Hdrs, Body, Timeout, Options) ->
  Fun = list_to_atom(lists:nth(3, string:tokens(Url, "/")) ++ "_http"),
  Args = [Url, Method, Hdrs, Body, Timeout, Options],
  erlang:apply(?MODULE, Fun, Args).

mock_katt_blueprint_parse_file(Test) ->
  erlang:apply(?MODULE, list_to_atom(atom_to_list(Test) ++ "_blueprint"), []).
