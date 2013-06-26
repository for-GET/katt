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
%%% @doc Common definitions and types for KATT.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("blueprint_types.hrl").

-define(VAR_PREFIX,                 "katt_").
-define(RECALL_BEGIN_TAG,           "{{<").
-define(RECALL_END_TAG,             "}}").
-define(STORE_BEGIN_TAG,            "{{>").
-define(STORE_END_TAG,              "}}").
-define(MATCH_ANY,                  "{{_}}").
-define(PROTOCOL_HTTP,              "http:").
-define(PROTOCOL_HTTPS,             "https:").
-define(DEFAULT_SCENARIO_TIMEOUT,   120000).
-define(DEFAULT_REQUEST_TIMEOUT,    20000).
-define(DEFAULT_PROTOCOL,           ?PROTOCOL_HTTP).
-define(DEFAULT_HOSTNAME,           "127.0.0.1").
-define(DEFAULT_PORT_HTTP,          80).
-define(DEFAULT_PORT_HTTPS,         443).
-define(DEFAULT_PARSE_FUNCTION,     fun katt_callback:parse/3).
-define(DEFAULT_REQUEST_FUNCTION,   fun katt_callback:request/3).
-define(DEFAULT_VALIDATE_FUNCTION,  fun katt_callback:validate/3).

-type run_result()          :: scenario_result() | run_error().
-type run_error()           :: {error, reason(), details()}.
-type scenario_result()     :: {scenario_filename(), [operation_result()]}.
-type validation_result()   :: pass | {fail, reason(), details()}.
-type operation_result()    :: {string(), #katt_request{}, validation_result()}.
-type scenario_filename()   :: nonempty_string().
-type description()         :: string().
-type reason()              :: atom().
-type details()             :: any().
-type param()               :: string() | atom() | binary() | integer().
-type params()              :: [{param(), param()}].
-type callbacks()           :: [{atom(), function()}].
-type response()            :: #katt_response{} | {error, any()}.
-type body()                :: binary().
-type headers()             :: [{string(), string()}].

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
