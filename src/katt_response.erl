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
%%% @doc Accessors for katt_response
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(katt_response).

%%%_* Exports ==================================================================
%% API
-export([ get_body/1
        , set_body/2
        , get_headers/1
        , set_headers/2
        , get_status/1
        , set_status/2
        ]).

%%%_* Includes =================================================================
-include("katt.hrl").

%%%_* API ======================================================================

-spec get_body(response()) -> binary() | null.
get_body(#katt_response{body = Body}) -> Body.

-spec set_body(binary()|null, response()) -> response().
set_body(Body, #katt_response{} = Res) -> Res#katt_response{body = Body}.

-spec get_headers(response()) -> [http_header()].
get_headers(#katt_response{headers = Hdrs}) -> Hdrs.

-spec set_headers([http_header()], response()) -> response().
set_headers(Hdrs, #katt_response{} = Res) -> Res#katt_response{headers = Hdrs}.

-spec get_status(response()) -> integer().
get_status(#katt_response{status = Status}) -> Status.

-spec set_status(integer(), response()) -> response().
set_status(Status, #katt_response{} = Res) ->
  Res#katt_response{status = Status}.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
