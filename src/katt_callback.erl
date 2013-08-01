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
-export([ recall/4
        , parse/4
        , request/3
        , validate/4
        ]).

%%%_* Includes =========================================================
-include("katt.hrl").

%%%_* API ==============================================================

%% @doc Recall all params inside url/status/headers/body/text/json content.
%% @end
-spec recall( recall_scope()
            , any()
            , params()
            , callbacks()
            ) -> any().
recall(_Scope, null, _Params, _Callbacks) ->
  null;
recall(_Scope, Input, [], _Callbacks) ->
  Input;
recall(text, Bin0, [{K0, V} | Next], Callbacks) ->
  K = ?RECALL_BEGIN_TAG ++ katt_util:to_list(K0) ++ ?RECALL_END_TAG,
  REK = katt_util:escape_regex(K),
  REV = katt_util:escape_regex(V),
  Bin = re:replace( Bin0
                  , REK
                  , REV
                  , [{return, binary}, global]),
  recall(text, Bin, Next, Callbacks);
recall(json, Bin0, [{K0, V0} | Next], Callbacks) ->
  K = ?RECALL_BEGIN_TAG ++ katt_util:to_list(K0) ++ ?RECALL_END_TAG,
  REK = "\"" ++ katt_util:escape_regex(K) ++ "\"",
  V = katt_util:maybe_json_string(V0),
  REV = katt_util:escape_regex(V),
  Bin = re:replace( Bin0
                  , REK
                  , REV
                  , [{return, binary}, global]),
  recall(json, Bin, Next, Callbacks);
recall(url, Bin, Params, Callbacks) ->
  recall(text, Bin, Params, Callbacks);
recall(headers, Hdrs0, Params, Callbacks) ->
  [{K, katt_util:from_utf8(
         recall(text, katt_util:to_utf8(V), Params, Callbacks)
       )} || {K, V} <- Hdrs0];
recall(body, [Hdrs, Bin], Params, Callbacks) ->
  ContentType = proplists:get_value("Content-Type", Hdrs, ""),
  Syntax = case is_json_content_type(ContentType) of
             true  -> json;
             false -> text
           end,
  [Hdrs, recall(Syntax, Bin, Params, Callbacks)].

%% @doc Parse the body of e.g. an HTTP response.
%% @end
-spec parse( headers()
           , body()
           , params()
           , callbacks()
           ) -> any().
parse(_Hdrs, null, _Params, _Callbacks) ->
  [];
parse(Hdrs, Body, _Params, _Callbacks) ->
  ContentType = proplists:get_value("Content-Type", Hdrs, ""),
  case is_json_content_type(ContentType) of
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
                    , body    = ParseFun(Hdrs, RawBody, Params, Callbacks)
                    };
    Error = {error, _}               ->
      Error
  end.

%% @doc Validate a response.
%% @end
-spec validate( #katt_response{}
              , #katt_response{}
              , params()
              , callbacks()
              ) -> {pass, details()} | {fail, details()}.
validate( Expected = #katt_response{}
        , Actual = #katt_response{}
        , _Params
        , _Callbacks)                                     ->
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
    _  -> {fail, lists:reverse(Failures)}
  end;
validate(Expected, #katt_response{}, _Params, _Callbacks) -> {fail, Expected};
validate(#katt_response{}, Actual, _Params, _Callbacks)   -> {fail, Actual}.


%%%_* Internal =========================================================

parse_json(Bin) ->
  to_proplist(mochijson3:decode(Bin)).

is_json_content_type(ContentType) ->
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
  compare("/status", E, A).

%% Actual headers are allowed to be a superset of expected headers, since
%% we don't want tests full of boilerplate like tests for headers such as
%% Content-Length, Server, Date, etc.
%% The header name (not the value) is compared case-insensitive
validate_headers(#katt_response{headers=E0}, #katt_response{headers=A0}) ->
  E = [{katt_util:to_lower(K), V} || {K, V} <- E0],
  A = [{katt_util:to_lower(K), V} || {K, V} <- A0],
  compare_struct("/headers", E, A, ?MATCH_ANY).

%% Bodies are also allowed to be a superset of expected body, if the parseFun
%% returns a structure.
validate_body(#katt_response{body=E}, #katt_response{body=A}) ->
  compare_struct("/body", E, A, ?MATCH_ANY).

%% Compare non-empty JSON structured types; defer to simple comparison otherwise
compare_struct(ParentKey, E0, A = [{_,_}|_], _Unexpected) when is_list(E0) ->
  Unexpected = proplists:get_value(?MATCH_ANY, E0, ?MATCH_ANY),
  E = proplists:delete(?MATCH_ANY, E0),
  Keys = lists:usort([K || {K, _} <- lists:merge(A, E)]),
  [ compare_struct( ParentKey ++ "/" ++ K
                  , proplists:get_value(K, E)
                  , proplists:get_value(K, A)
                  , Unexpected)
    || K <- Keys
  ];
compare_struct(K, E0, A0 = [[{_,_}|_]|_], Unexpected) when is_list(E0)     ->
  [ compare_struct(K, E, A, Unexpected)
    || {E, A} <- lists:zip(E0, A0)
  ];
compare_struct(K, E, A = [[_|_]|_], _Unexpected) when is_list(E)           ->
  compare_struct(K, enumerate(E), enumerate(A), ?UNEXPECTED);
compare_struct(K, E, A, Unexpected) ->
  compare(K, E, A, Unexpected).

%% Compare when unexpected values show up
compare(_Key, undefined, _A, ?MATCH_ANY) ->
  pass;
compare(_Key, [], _A, ?MATCH_ANY)        ->
  pass;
compare(Key, undefined, A, ?UNEXPECTED)  ->
  {unexpected, {Key, undefined, A}};
compare(Key, undefined, A, Unexpected)   ->
  compare(Key, Unexpected, A);
compare(Key, E, A, _Unexpected)          ->
  compare(Key, E, A).

%% Compare JSON primitive types or empty structured types
compare(_Key, ?MATCH_ANY, _)               ->
  pass;
compare(_Key, ?STORE_BEGIN_TAG ++ Rest, A) ->
  Param = string:sub_string(Rest, 1, string:str(Rest, ?STORE_END_TAG) - 1),
  {pass, {Param, A}};
compare(_Key, E, E)                        ->
  pass;
compare(Key, E, A)                         ->
  {not_equal, {Key, E, A}}.

%% Transform simple list to proplist with keys named 0, 1 etc.
enumerate(L) ->
  lists:zip([ integer_to_list(N)
              || N <- lists:seq(0, length(L) - 1)
            ], L).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
