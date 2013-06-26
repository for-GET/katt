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
%%% @doc Built-in default callback functions.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(katt_callback).

%%%_* Exports ==========================================================
%% API
-export([ parse/3
        , request/3
        , validate/3
        ]).

%%%_* Includes =========================================================
-include("katt.hrl").

%%%_* API ==============================================================

%% @doc Parse the body of e.g. an HTTP response.
%% @end
-spec parse( headers()
           , body()
           , params()
           ) -> any().
parse(_Hdrs, null, _Params) ->
  [];
parse(Hdrs, Body, _Params) ->
  case is_json_body(Hdrs, Body) of
    true  -> parse_json(Body);
    false -> katt_util:from_utf8(Body)
  end.

%% @doc Make a request, e.g. an HTTP request.
%% @end
-spec request( #katt_request{}
             , params()
             , callbacks()
             ) -> response().
request(R = #katt_request{}, Params, Callbacks) ->
  ParseFun = proplists:get_value(parse, Callbacks),
  case http_request(R, Params) of
    {ok, {{Code, _}, Hdrs, RawBody}} ->
      #katt_response{ status  = Code
                    , headers = Hdrs
                    , body    = ParseFun(Hdrs, RawBody, Params)
                    };
    Error = {error, _}               ->
      Error
  end.

%% @doc Validate a response.
%% @end
-spec validate( #katt_response{}
              , #katt_response{}
              , params() | _
              ) -> {pass, details()} | {fail, details()}.
validate( Expected = #katt_response{}
        , Actual = #katt_response{}
        , _Params)                                     ->
  Result = [ validate_status(Expected, Actual)
           , validate_headers(Expected, Actual)
           , validate_body(Expected, Actual)],
  {AddParams, Failures} =
    lists:foldl(
      fun(pass, Acc) -> Acc;
         ({pass, AddParam}, {AddParams0, Failures0}) ->
          {[AddParam | AddParams0], Failures0};
         (Failure, {AddParams0, Failures0})          ->
          {AddParams0, [Failure|Failures0]}
      end,
    {[],[]},
    lists:flatten(Result)
    ),
  case Failures of
    [] -> {pass, AddParams};
    _  -> {fail, Failures}
  end;
validate(Expected, #katt_response{}, _Params) -> {fail, Expected};
validate(#katt_response{}, Actual, _Params)   -> {fail, Actual}.


%%%_* Internal =========================================================

parse_json(Binary) -> to_proplist(mochijson3:decode(Binary)).

is_json_body(_Hdrs, <<>>) -> false;
is_json_body(Hdrs, _Body) ->
  ContentType = proplists:get_value("Content-Type", Hdrs, ""),
  case string:str(ContentType, "json") of
    0 -> false;
    _ -> true
  end.

to_proplist(L = [{struct, _}|_])         ->
  [to_proplist(S) || S <- L];
to_proplist({struct, L}) when is_list(L) ->
  [{katt_util:from_utf8(K), to_proplist(V)} || {K, V} <- L];
to_proplist(List) when is_list(List)     ->
  lists:sort([to_proplist(L) || L <- List]);
to_proplist(Str) when is_binary(Str)     ->
  katt_util:from_utf8(Str);
to_proplist(Value)                       ->
  Value.

http_request(R = #katt_request{}, Params) ->
  Body = case R#katt_request.body of
    null -> <<>>;
    Bin  -> Bin
  end,
  lhttpc:request( R#katt_request.url
                , R#katt_request.method
                , R#katt_request.headers
                , Body
                , proplists:get_value(request_timeout, Params)
                , []
                ).

validate_status(#katt_response{status=E}, #katt_response{status=A}) ->
  compare(status, E, A).

%% Actual headers are allowed to be a superset of expected headers, since
%% we don't want tests full of boilerplate like tests for headers such as
%% Content-Length, Server, Date, etc.
%% The header name (not the value) is compared case-insensitive
validate_headers(#katt_response{headers=E0}, #katt_response{headers=A0}) ->
  E = [{katt_util:to_lower(K), V} || {K, V} <- E0],
  A = [{katt_util:to_lower(K), V} || {K, V} <- A0],
  ExpectedHeaders = lists:usort(proplists:get_keys(E)),
  Get = fun proplists:get_value/2,
  [ do_validate(K, Get(K, E), Get(K, A)) || K <- ExpectedHeaders].

%% Bodies must be identical, no subset matching or similar.
validate_body(#katt_response{body=E}, #katt_response{body=A}) ->
  do_validate(body, E, A).

do_validate(_, E = [{_,_}|_], A = [{_,_}|_])           ->
  Keys = lists:usort([K || {K, _} <- lists:merge(A, E)]),
  [ do_validate(K, proplists:get_value(K, E), proplists:get_value(K, A))
    || K <- Keys
  ];
do_validate(Key, E = [[{_,_}|_]|_], A = [[{_,_}|_]|_])
  when length(E) =/= length(A)                         ->
  {missing_object, {Key, {E, A}}};
do_validate(K, E0 = [[{_,_}|_]|_], A0 = [[{_,_}|_]|_]) ->
  [do_validate(K, E, A) || {E, A} <- lists:zip(E0, A0)];
do_validate(K, E = [[_|_]|_], A = [[_|_]|_])           ->
  do_validate(K, enumerate(E, K), enumerate(A, K));
do_validate(Key, undefined, A)                         ->
  {unexpected_value, {Key, A}};
do_validate(Key, E, undefined)                         ->
  {missing_value, {Key, E}};
do_validate(Key, ?STORE_BEGIN_TAG ++ Rest, [])         ->
  Param = string:sub_string(Rest, 1, string:str(Rest, ?STORE_END_TAG) - 1),
  {empty_value, {Key, Param}};
do_validate(_Key, ?MATCH_ANY ++ _, _)                  ->
  pass;
do_validate(_Key, ?STORE_BEGIN_TAG ++ Rest, A)         ->
  Param = string:sub_string(Rest, 1, string:str(Rest, ?STORE_END_TAG) - 1),
  {pass, {Param, A}};
do_validate(Key, E, A)                                 ->
  compare(Key, E, A).

compare(_Key, E, E) -> pass;
compare(Key, E, A)  -> {not_equal, {Key, E, A}}.

%% Transform simple list to proplist with keys named Name1, Name2 etc.
enumerate(L, Name) ->
  lists:zip([ katt_util:to_list(Name) ++ integer_to_list(N)
              || N <- lists:seq(1, length(L))
            ], L).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
