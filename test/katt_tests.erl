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
      ok = ssl:start(),
      ok = lhttpc:start(),
      ok
      % TODO: Figure out how meck works and mock file read and network access.
      % meck:new(katt_blueprint_parse),
      % meck:expect( katt_blueprint_parse, file
      %            , fun mock_katt_blueprint_parse_file/1
      %            )
    end
  , fun(_) ->
      % lhttpc:stop(),
      % ssl:stop(),
      ok
      % catch meck:unload(katt_blueprint_parse),
      % ok
    end
  , [ ?_test(katt_run_positive())
    , ?_test(katt_run_negative())
    ]
  }.


%%% Tests

katt_run_positive() ->
  ?assertMatch([ {_, _, pass}
               , {_, _, pass}
               , {_, _, pass}
               , {_, _, pass}
               , {_, _, pass}
               , {_, _, pass}
               ], katt:run("../doc/example-httpbin.apib"
                          , [ {hostname, "httpbin.org"}
                            ]
                          )).


katt_run_negative() ->
  ?assertMatch([ {_, _, [{not_equal, _} | _]}
               | _
               ], katt:run("../doc/example.apib"
                          , [ {hostname, "httpbin.org"}
                            ]
                          )).


%%% Helpers

% mock_katt_blueprint_parse_file(_File) ->
%   katt_blueprint_parse:string(
%     <<"--- Example blueprint for some scenario (this is the title) ---

% ---
% Here, between the \"---\" lines you can write a general description of the
% operations that shoudl take place in this scenario. Markdown is allowed too.

% This is just an example of how a *KATT Blueprint* file can look and work.
% In this example we are describing a contrived checkout service.
% ---


% # Step 1

% The merchant creates a new example object on our server, and we respond with
% the location of the created example.

% POST /foo/examples
% > Accept: application/json
% > Content-Type: application/json
% {
%     \"cart\": {
%         \"items\": [
%             {
%                 \"name\": \"Horse\",
%                 \"quantity\": 1,
%                 \"unit_price\": 4495000
%             },
%             {
%                 \"name\": \"Battery\",
%                 \"quantity\": 4,
%                 \"unit_price\": 1000
%             },
%             {
%                 \"name\": \"Staple\",
%                 \"quantity\": 1,
%                 \"unit_price\": 12000
%             }
%         ]
%     }
% }
% < 201
% < Location: {{>example_uri}}


% # Step 2

% The client (customer) fetches the created resource data.

% GET {{<example_uri}}
% > Accept: application/json
% < 200
% < Content-Type: application/json
% {
%     \"required_fields\": [
%         \"email\"
%     ],
%     \"cart\": \"{{_}}\"
% }


% # Step 3

% The customer submits an e-mail address in the form.

% POST {{<example_uri}}
% > Accept: application/json
% > Content-Type: application/json
% {
%     \"email\": \"test-customer@foo.klarna.com\"
% }
% < 200
% < Content-Type: application/json
% {
%     \"required_fields\": [
%         \"password\"
%     ],
%     \"cart\": \"{{_}}\"
% }


% # Step 4

% The customer submits the form again, this time also with his password.
% We inform him that payment is required.

% POST {{<example_uri}}
% > Accept: application/json
% > Content-Type: application/json
% {
%     \"email\": \"test-customer@foo.klarna.com\",
%     \"password\": \"correct horse battery staple\"
% }
% < 402
% < Content-Type: application/json
% {
%     \"error\": \"payment required\"
% }
% "/utf8>>).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
