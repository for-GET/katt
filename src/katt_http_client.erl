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
%%% @doc Klarna API Testing Tool Utils
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
%% @private
-module(katt_http_client).

%%%_* Exports ==================================================================

-export([init/2,
         request/6]).

%%%_* Types ====================================================================

-type method()  :: string()
                 | atom().
-type url()     :: string().
-type headers() :: [[{string() | atom(), string()}]].
-type body()    :: iolist().
-type result()  :: {ok, {{pos_integer(), string()}, headers(), binary()}}
                 | {error, atom()}.


%%%_* Callbacks ================================================================

-callback init(Args       :: [term()]) -> ok.
-callback request(Method  :: method(),
                  URL     :: url(),
                  Headers :: headers(),
                  Body    :: body(),
                  Timeout :: timeout()) -> result().

%%%_* API ======================================================================

-spec init(CBModule :: module(), Args :: [term()]) -> ok.
init(CBModule, Args) ->
  CBModule:init(Args).

-spec request(CBModule :: module(),
              Method   :: method(),
              URL      :: url(),
              Headers  :: headers(),
              Body     :: body(),
              Timeout  :: timeout()) -> result().
request(CBModule, Method, URL, Headers, Body, Timeout) ->
  CBModule:request(Method, URL, Headers, Body, Timeout).
