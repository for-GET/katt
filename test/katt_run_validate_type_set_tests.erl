%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright 2014- See AUTHORS
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
%%% @copyright 2014- See AUTHORS
%%%
%%% KATT Run - Validate Type Set Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(katt_run_validate_type_set_tests).

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
  , [ katt_run_with_set_comparison_strict()
    , katt_run_with_set_comparison_strict_fails()
    , katt_run_with_set_comparison_unlimited()
    , katt_run_with_set_comparison_unlimited_fails()
    ]
  }.

%%% Tests

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
                 , [ {_, _, _, _, {fail, [ { unexpected
                                           , "/body/set_of_objects/{{set}}/0"
                                           }
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
                 , [ {_, _, _, _, {fail, [ { not_contains
                                           , "/body/set_of_objects/{{set}}/2"
                                           }
                                         ]}}
                   ]
                 }
               , katt:run(Scenario)
               ).

%%% Helpers

%% Mock response for set_comparison test:
%% (match a finite set of values)
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
    \"set_of_objects\": [{\"number\":2}, {\"number\":1}]
}
"/utf8>>}};
%% Mock response for set_comparison test:
%% (match a finite set of values):
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
    \"set_of_objects\": [{\"number\":2}, {\"number\":1}, {\"another_number\":3}]
}
"/utf8>>}};
%% Mock response for set_comparison_unexpected test:
%% (match any set of value, except the unexpected)
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
    \"set_of_objects\": [{\"number\":2}, {\"number\":1}, {\"number\":0}]
}
"/utf8>>}}.

mock_katt_blueprint_parse_file("/mock/set_comparison_strict.apib") ->
    katt_blueprint_parse:string(
        <<"--- Comparison as set ---

GET /set_comparison_strict
< 200
< Content-Type: application/json
{
    \"set_of_objects\": {
        \"{{type}}\": \"set\",
        \"value\": [{\"number\":1}, {\"number\":2}, \"{{unexpected}}\"]
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
        \"{{type}}\": \"set\",
        \"value\": [{\"number\":1}, \"{{unexpected}}\"]
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
        \"{{type}}\": \"set\",
        \"value\": [{\"number\":1}, {\"number\":2}]
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
        \"{{type}}\": \"set\",
        \"value\": [{\"number\":1}, {\"number\":2}, {\"number\":3}]
    }
}
"/utf8>>).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
