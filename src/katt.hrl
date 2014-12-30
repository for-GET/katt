%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright 2012- Klarna AB
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
%%% @copyright 2012- Klarna AB
%%% @copyright 2014- See AUTHORS
%%%
%%% @doc Common definitions and types for KATT.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("blueprint_types.hrl").

-define(VAR_PREFIX,                 "katt_").
-define(RECALL_BEGIN_TAG,           "{{<").
-define(RECALL_END_TAG,             "}}").
-define(STORE_BEGIN_TAG,            "{{>").
-define(STORE_END_TAG,              "}}").
-define(MATCH_ANY,                  "{{_}}").
-define(UNEXPECTED,                 "{{unexpected}}").
-define(PROTOCOL_HTTP,              "http:").
-define(PROTOCOL_HTTPS,             "https:").
-define(DEFAULT_SCENARIO_TIMEOUT,   120000).
-define(DEFAULT_REQUEST_TIMEOUT,    20000).
-define(DEFAULT_PROTOCOL,           ?PROTOCOL_HTTP).
-define(DEFAULT_HOSTNAME,           "127.0.0.1").
-define(DEFAULT_PORT_HTTP,          80).
-define(DEFAULT_PORT_HTTPS,         443).
-define(DEFAULT_EXT_FUN,            fun katt_callbacks:ext/1).
-define(DEFAULT_RECALL_FUN,         fun katt_callbacks:recall/4).
-define(DEFAULT_PARSE_FUN,          fun katt_callbacks:parse/4).
-define(DEFAULT_REQUEST_FUN,        fun katt_callbacks:request/3).
-define(DEFAULT_VALIDATE_FUN,       fun katt_callbacks:validate/4).
-define(DEFAULT_PROGRESS_FUN,       fun katt_callbacks:progress/2).

-type scenario_filename()   :: nonempty_string().
-type params()              :: [{param_name(), param()}].
-type param_name()          :: atom()
                             | string().
-type param()               :: atom()
                             | integer()
                             | float()
                             | string()
                             | binary().
-type callbacks()           :: [{atom(), function()}].

-type run_result()          :: run_error()
                             | scenario_result().
-type run_error()           :: {error, reason(), details()}.
-type reason()              :: atom().
-type details()             :: any().
-type scenario_result()     :: { pass | fail
                               , scenario_filename()
                               , params()
                               , params()
                               , [transaction_result()]
                               }.
-type transaction_result()  :: { string()
                               , params()
                               , #katt_request{}
                               , #katt_response{}
                               , validation_result()
                               }.
-type validation_result()   :: pass
                             | {fail, [validation_failure()]}.
-type validation_failure()  :: {reason(), details()}.

-type description()         :: string().
-type request()             :: #katt_request{}.
-type response()            :: #katt_response{}
                             | {error, any()}.
-type body()                :: null
                             | binary().
-type headers()             :: [{string(), string()}].

-type recall_scope()        :: url
                             | status
                             | headers
                             | body
                             | text.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
