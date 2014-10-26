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
-module(katt_callbacks).

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
  Bin1 = re:replace( Bin0
                   , REK
                   , REV
                   , [{return, binary}, global]),
  Bin = recall(text, Bin1, [{K0, V0}], Callbacks),
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
    {ok, {{Code, _}, Hdrs, Body}} ->
      #katt_response{ status      = Code
                    , headers     = Hdrs
                    , body        = Body
                    , parsed_body = ParseFun(Hdrs, Body, Params, Callbacks)
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
 {AddParams0, Failures0} = get_params_and_failures(
                             validate_status(Expected, Actual)),
 {AddParams1, Failures1} = get_params_and_failures(
                             validate_headers(Expected, Actual)),
 {AddParams2, Failures2} = get_params_and_failures(
                             validate_body(Expected, Actual)),
 AddParams = lists:flatten([ AddParams0
                           , AddParams1
                           , AddParams2]),
 Failures = lists:flatten([ Failures0
                          , Failures1
                          , Failures2]),
  case Failures of
    [] -> {pass, AddParams};
    _  -> {fail, Failures}
  end;
validate(Expected, #katt_response{}, _Params, _Callbacks) -> {fail, Expected};
validate(#katt_response{}, Actual, _Params, _Callbacks)   -> {fail, Actual}.


%%%_* Internal =========================================================

get_params_and_failures(Result) ->
  lists:foldl(
    fun(pass, Acc) -> Acc;
       ({pass, AddParam}, {AddParams0, Failures0}) ->
        {[AddParam | AddParams0], Failures0};
       (Failure, {AddParams0, Failures0})          ->
        {AddParams0, [Failure | Failures0]}
    end,
    {[],[]},
    lists:flatten([Result])
   ).

parse_json(Bin) when is_binary(Bin), size(Bin) =:= 0 ->
  [];
parse_json(Bin) ->
  to_proplist(mochijson3:decode(Bin)).

is_json_content_type(ContentType) ->
  case string:str(ContentType, "json") of
    0 -> false;
    _ -> true
  end.

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

validate_status(#katt_response{status=E}, #katt_response{status=A}, _Callbacks) ->
  katt_util:compare("/status", E, A).

%% Actual headers are allowed to be a superset of expected headers, since
%% we don't want tests full of boilerplate like tests for headers such as
%% Content-Length, Server, Date, etc.
%% The header name (not the value) is compared case-insensitive
validate_headers(#katt_response{headers=E0}, #katt_response{headers=A0}) ->
  E = [{katt_util:to_lower(K), V} || {K, V} <- E0],
  A = [{katt_util:to_lower(K), V} || {K, V} <- A0],
  katt_util:compare_struct("/headers", E, A, ?MATCH_ANY).

%% Bodies are also allowed to be a superset of expected body, if the parseFun
%% returns a structure.
validate_body(#katt_response{parsed_body=E}, #katt_response{parsed_body=A}) ->
      katt_util:compare_struct("/body", E, A, ?MATCH_ANY);


%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
