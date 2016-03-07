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
%%% KATT Run - Validate Type Set Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(katt_run_validate_type_set_tests).

-include_lib("eunit/include/eunit.hrl").

-define( FUNCTION
       , element(2, element(2, process_info(self(), current_function)))
       ).

-export([ katt_run_set_blueprint/0
        , katt_run_set_http/6
        , katt_run_set_fails_blueprint/0
        , katt_run_set_fails_http/6
        , katt_run_set_with_unexpected_blueprint/0
        , katt_run_set_with_unexpected_http/6
        , katt_run_set_with_unexpected_fails_blueprint/0
        , katt_run_set_with_unexpected_fails_http/6
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
  , [ katt_run_set_with_unexpected()
    , katt_run_set_with_unexpected_fails()
    , katt_run_set()
    , katt_run_set_fails()
    ]
  }.

%%% Tests

%%% Test set with strict (match a finite set of values)

katt_run_set_with_unexpected() ->
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

katt_run_set_with_unexpected_blueprint() ->
  katt_blueprint_parse:string(
    <<"--- Comparison as set ---

GET /katt_run_set_with_unexpected
< 200
< Content-Type: application/json
{
    \"set_of_objects\": {
        \"{{type}}\": \"set\",
        \"value\": [{\"number\":1}, {\"number\":2}, \"{{unexpected}}\"]
    }
}
"/utf8>>).

katt_run_set_with_unexpected_http( _
                                 , "GET"
                                 , _
                                 , _
                                 , _Timeout
                                 , _Options
                                 ) ->
  { ok, {{200, []}, [
                     {"content-type", "application/json"}
                    ], <<"{
    \"set_of_objects\": [{\"number\":2}, {\"number\":1}]
}
"/utf8>>}}.

%%% Test set failure with strict (match any set of value, except the unexpected)

katt_run_set_with_unexpected_fails() ->
  Scenario = ?FUNCTION,
  ?_assertMatch( { fail
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, _, {fail, [ { unexpected
                                           , { "/body/set_of_objects/{{set}}/0"
                                             , undefined
                                             , {struct, [{"number", 2}]}
                                             }}
                                         ]}}
                   ]
                 }
               , katt:run(Scenario)
               ).

katt_run_set_with_unexpected_fails_blueprint() ->
  katt_blueprint_parse:string(
    <<"--- Comparison as set ---

GET /katt_run_set_with_unexpected_fails
< 200
< Content-Type: application/json
{
    \"set_of_objects\": {
        \"{{type}}\": \"set\",
        \"value\": [{\"number\":1}, \"{{unexpected}}\"]
    }
}
"/utf8>>).

katt_run_set_with_unexpected_fails_http( _
                                       , "GET"
                                       , _
                                       , _
                                       , _Timeout
                                       , _Options
                                       ) ->
  {ok, {{200, []}, [
                    {"content-type", "application/json"}
                   ], <<"{
    \"set_of_objects\": [{\"number\":2}, {\"number\":1}]
}
"/utf8>>}}.

%%% Test set with unlimited

katt_run_set() ->
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

katt_run_set_blueprint() ->
  katt_blueprint_parse:string(
    <<"--- Comparison as set ---

GET /katt_run_set
< 200
< Content-Type: application/json
{
    \"set_of_objects\": {
        \"{{type}}\": \"set\",
        \"value\": [{\"number\":1}, {\"number\":2}]
    }
}
"/utf8>>).

katt_run_set_http( _
                 , "GET"
                 , _
                 , _
                 , _Timeout
                 , _Options
                 ) ->
  {ok, {{200, []}, [
                    {"content-type", "application/json"}
                   ], <<"{
    \"set_of_objects\": [{\"number\":2}, {\"number\":1}, {\"another_number\":3}]
}
"/utf8>>}}.

%%% Test set failure with unlimited

katt_run_set_fails() ->
  Scenario = ?FUNCTION,
  ?_assertMatch( { fail
                 , Scenario
                 , _
                 , _
                 , [ {_, _, _, _, {fail, [ { not_contains
                                           , { "/body/set_of_objects/{{set}}/2"
                                             , {struct, [{"number", 3}]}
                                             , undefined
                                             }}
                                         ]}}
                   ]
                 }
               , katt:run(Scenario)
               ).

katt_run_set_fails_blueprint() ->
  katt_blueprint_parse:string(
    <<"--- Comparison as set ---

GET /katt_run_set_fails
< 200
< Content-Type: application/json
{
    \"set_of_objects\": {
        \"{{type}}\": \"set\",
        \"value\": [{\"number\":1}, {\"number\":2}, {\"number\":3}]
    }
}
"/utf8>>).

katt_run_set_fails_http( _
                       , "GET"
                       , _
                       , _
                       , _Timeout
                       , _Options
                       ) ->
  {ok, {{200, []}, [
                    {"content-type", "application/json"}
                   ], <<"{
    \"set_of_objects\": [{\"number\":2}, {\"number\":1}, {\"another_number\":3}]
}
"/utf8>>}}.

%%% Helpers

mock_lhttpc_request(Url, Method, Hdrs, Body, Timeout, Options) ->
  Fun = list_to_atom(lists:nth(3, string:tokens(Url, "/")) ++ "_http"),
  Args = [Url, Method, Hdrs, Body, Timeout, Options],
  erlang:apply(?MODULE, Fun, Args).

mock_katt_blueprint_parse_file(Test) ->
  erlang:apply(?MODULE, list_to_atom(atom_to_list(Test) ++ "_blueprint"), []).
