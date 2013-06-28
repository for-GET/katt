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
    ]
  }.


%%% Tests

katt_run_basic() ->
  Scenario = "/mock/test1.apib",
  ?_assertMatch( { pass
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, pass}
                   , {_, _, _, pass}
                   , {_, _, _, pass}
                   , {_, _, _, pass}
                   , {_, _, _, pass}
                   ]
                 }
               , katt:run(Scenario)
               ).

katt_run_with_params() ->
  Scenario = "/mock/test2.apib",
  ?_assertMatch( { pass
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, pass}
                   ]
                 }
               , katt:run( Scenario
                         , [ {hostname, "test-params"}
                           , {some_var, "hi"}
                           , {version, "1"}
                           ]
                         )
               ).

katt_run_with_api_mismatch() ->
  Scenario = "/mock/test3.apib",
  ?_assertMatch( { fail
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, {fail, [ {not_equal, {body, _,_}}
                                      , {not_equal, {status, _, _}}
                                      ]}}
                   ]
                 }
               , katt:run(Scenario)
               ).


%%% Helpers

%% Mock request for Step 1:
%% (default hostname is 127.0.0.1, default port is 80, default protocl is http)
mock_lhttpc_request( "http://127.0.0.1/foo/examples" = _Url
                   , "POST" = _Method
                   , _Headers
                   , _Body
                   , _Timeout
                   , _Options
                   ) ->
  {ok, {{201, []}, [{"Location", "http://some-location.com/test"}], <<>>}};
%% Mock request for Step 2:
mock_lhttpc_request( "http://some-location.com/test"
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
    \"cart\": \"{{_}}\"
}

"/utf8>>}};
%% Mock request for Step 3:
mock_lhttpc_request( "http://some-location.com/test/step3"
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
%% Mock request for Step 4:
mock_lhttpc_request( "http://some-location.com/test/step4"
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
%% Mock request for Step 5:
mock_lhttpc_request( "http://127.0.0.1/nothing"
                   , "HEAD"
                   , _
                   , _
                   , _Timeout
                   , _Options
                   ) ->
  {ok, {{404, "Not found"}, [{"Content-Type", "text/html"}], <<>>}};

%% Mock request for test-params:
mock_lhttpc_request( "http://test-params/test2"
                   , "POST"
                   , [ {"Accept", "text/html"}
                     , {"Content-Type","application/vnd.katt.test-v1+json"}
                     ]
                   , _
                   , _Timeout
                   , _Options
                   ) ->
  {ok, {{404, []}, [{"Content-Type", "text/html"}], <<"Not found">>}};

%% Mock request for api mismatch test:
mock_lhttpc_request( "http://127.0.0.1/test3"
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
"/utf8>>}}.


mock_katt_blueprint_parse_file("/mock/test1.apib") ->
  katt_blueprint_parse:string(
    <<"--- Test 1 ---

---
Some description
---

Step 1
here it is:

POST /foo/examples
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


Step 2

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


Step 3

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


Step 4

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

HEAD /nothing
< 404
< Content-Type: text/html
<<<
>>>
"/utf8>>);
mock_katt_blueprint_parse_file("/mock/test2.apib") ->
  katt_blueprint_parse:string(
    <<"--- Test 2 ---

POST /test2
> Accept: text/html
> Content-Type: application/vnd.katt.test-v{{<version}}+json
{
    \"ok\": {{<some_var}}
}
< 404
Not found

"/utf8>>);

mock_katt_blueprint_parse_file("/mock/test3.apib") ->
  katt_blueprint_parse:string(
    <<"--- Test 3 ---

POST /test3
> Accept: application/json
> Content-Type: application/json
{}
< 200
{ \"ok\": true }
"/utf8>>).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
