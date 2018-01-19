%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%% @doc KATT Blueprint Parser types
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type utf8_string() :: unicode:unicode_binary().
-type http_header() :: {string(), string()}.

-type katt_transaction_params()      :: [{ katt_transaction_param_name()
                                         , katt_transaction_param_value()
                                         }].
-type katt_transaction_param_name()  :: string().
-type katt_transaction_param_value() :: atom()
                                      | integer()
                                      | float()
                                      | string()
                                      | binary().

-record(katt_request,     { method = "GET"              :: string()
                          , url = "/"                   :: string()
                          , headers = []                :: [http_header()]
                          , body = null                 :: binary() | null
                          }).

-record(katt_response,    { status = 200                :: integer()
                          , headers = []                :: [http_header()]
                          , body = null                 :: binary() | null
                          , parsed_body = null          :: any()
                          }).

-record(katt_transaction, { description = null          :: utf8_string() | null
                          , params = []                 :: katt_transaction_params()
                          , request = #katt_request{}   :: #katt_request{}
                          , response = #katt_response{} :: #katt_response{}
                          }).

-record(katt_blueprint,   { filename = null             :: file:filename() | null
                          , name = null                 :: utf8_string() | null
                          , description = null          :: utf8_string() | null
                          , transactions = []           :: [#katt_transaction{}]
                          , footer = null               :: utf8_string() | null
                          }).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
