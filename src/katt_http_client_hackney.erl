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
-module(katt_http_client_hackney).

%%%_* Exports ==================================================================
%% API
-export([ init/1
        , request/5
        ]).

%%%_* Includes =================================================================

%%%_* API ======================================================================

init(_) ->
    katt_util:ensure_applications_started([ kernel
                                          , stdlib
                                          , crypto
                                          , asn1
                                          , public_key
                                          , ssl
                                          , idna
                                          , mimerl
                                          , certifi]).

request(Method, Url, Hdrs, Body, Timeout) ->
  BUrl = list_to_binary(Url),
  BHdrs = lists:map( fun({Name, Value})->
                         {list_to_binary(Name), list_to_binary(Value)}
                     end
                   , Hdrs
                   ),
  Options = [{recv_timeout, Timeout}],
  case hackney:request(Method, BUrl, BHdrs, Body, Options) of
    {ok, Status, BResHdrs, Client} ->
      %% lhttpc was the predecesor of hackney
      %% and we're maintaining a backwards compatible return value
      {ok, ResBody} = hackney:body(Client),
      ResHdrs0 = lists:map( fun({Name, Value})->
                                {binary_to_list(Name), binary_to_list(Value)}
                            end
                          , BResHdrs
                          ),
      ResHdrs = lists:reverse(ResHdrs0),
      {ok, {{Status, ""}, ResHdrs, ResBody}};
    Error ->
      Error
  end.

%%%_* Internal =================================================================

