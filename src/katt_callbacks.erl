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
%%% @doc Built-in default callback functions.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(katt_callbacks).

%%%_* Exports ==================================================================
%% API
-export([ ext/1
        , recall/4
        , parse/4
        , request/3
        , validate/4
        , validate_type/6
        , progress/2
        ]).

%%%_* Includes =================================================================
-include("katt.hrl").

%%%_* API ======================================================================

%% @doc Get a list of available extensions
%% @end
-spec ext(any()) -> list().
ext(recall_body) ->
  [ fun katt_callbacks_json:recall_body/4
  ];
ext(parse) ->
  [ fun katt_callbacks_json:parse/5
  ];
ext(validate_body) ->
  [ fun katt_callbacks_json:validate_body/4
  ];
ext(validate_type) ->
  [ fun katt_callbacks_json:validate_type/7
  ].


%% @doc Recall all params inside url/status/headers/body/text content.
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
recall(url, Bin, Params, Callbacks) ->
  recall(text, Bin, Params, Callbacks);
recall(headers, Hdrs0, Params, Callbacks) ->
  [ { K
    , katt_util:from_utf8(
        recall(text, katt_util:to_utf8(V), Params, Callbacks)
       )
    }
    || {K, V} <- Hdrs0
  ];
recall(body, [Hdrs, Bin], Params, Callbacks) ->
  ExtFun = proplists:get_value(ext, Callbacks),
  Ext = ExtFun(recall_body),
  MatchingExt = lists:dropwhile( fun(Fun) ->
                                     not Fun( _JustCheck = true
                                            , [Hdrs, Bin]
                                            , Params
                                            , Callbacks
                                            )
                                 end
                               , Ext
                               ),
  case MatchingExt of
    [] ->
      [ Hdrs
      , recall(text, Bin, Params, Callbacks)
      ];
    [Fun|_] ->
      [ Hdrs
      , Fun( _JustCheck = false
           , [Hdrs, Bin]
           , Params
           , Callbacks
           )
      ]
  end.

%% @doc Parse the body of e.g. an HTTP response.
%% @end
-spec parse( headers()
           , body()
           , params()
           , callbacks()
           ) -> any().
parse(_Hdrs, null, _Params, _Callbacks) ->
  [];
parse(Hdrs, Body, Params, Callbacks) ->
  ExtFun = proplists:get_value(ext, Callbacks),
  Ext = ExtFun(parse),
  MatchingExt = lists:dropwhile( fun(Fun) ->
                                     not Fun( _JustCheck = true
                                            , Hdrs
                                            , Body
                                            , Params
                                            , Callbacks
                                            )
                                 end
                               , Ext),
  case MatchingExt of
    [] ->
      Body;
    [Fun|_] ->
      Fun( _JustCheck = false
         , Hdrs
         , Body
         , Params
         , Callbacks
         )
  end.

%% @doc Make a request, e.g. an HTTP request.
%% @end
-spec request( request()
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
-spec validate( response()
              , response()
              , params()
              , callbacks()
              ) -> {pass, details()} | {fail, details()}.
validate( Expected = #katt_response{}
        , Actual = #katt_response{}
        , _Params
        , Callbacks)                                     ->
 {AddParams0, Failures0} = get_params_and_failures(
                             validate_status(Expected, Actual, Callbacks)),
 {AddParams1, Failures1} = get_params_and_failures(
                             validate_headers(Expected, Actual, Callbacks)),
 {AddParams2, Failures2} = get_params_and_failures(
                             validate_body(Expected, Actual, Callbacks)),
 AddParams = AddParams0 ++ AddParams1 ++ AddParams2,
 Failures = Failures0 ++ Failures1 ++ Failures2,
  case Failures of
    [] -> {pass, AddParams};
    _  -> {fail, Failures}
  end;
validate(Expected, #katt_response{}, _Params, _Callbacks) -> {fail, Expected};
validate(#katt_response{}, Actual, _Params, _Callbacks)   -> {fail, Actual}.

%% @doc Notify of scenario progress
%% @end
-spec progress( term()
              , any()
              ) -> ok.
progress(_Step, _Detail) ->
  ok.

%%%_* Internal =================================================================

get_params_and_failures(Result) when not is_list(Result) ->
  get_params_and_failures([Result]);
get_params_and_failures(Result) ->
  lists:foldl(
    fun({pass, AddParams}, {AddParams0, Failures0}) ->
        {AddParams ++ AddParams0, Failures0};
       (Failure, {AddParams0, Failures0}) ->
        {AddParams0, [Failure | Failures0]}
    end,
    {[], []},
    lists:flatten(Result)
   ).

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

validate_status( #katt_response{status=E}
               , #katt_response{status=A}
               , _Callbacks
               ) ->
  katt_util:validate("/status", E, A).

%% Actual headers are allowed to be a superset of expected headers, since
%% we don't want tests full of boilerplate like tests for headers such as
%% Content-Length, Server, Date, etc.
%% The header name (not the value) is compared case-insensitive
validate_headers( #katt_response{headers=E0}
                , #katt_response{headers=A0}
                , _Callbacks
                ) ->
  E = {struct, [{katt_util:to_lower(K), V} || {K, V} <- E0]},
  A = {struct, [{katt_util:to_lower(K), V} || {K, V} <- A0]},
  katt_util:validate("/headers", E, A, ?MATCH_ANY, []).

%% Bodies are also allowed to be a superset of expected body, if the parseFun
%% returns a structure.
validate_body( #katt_response{parsed_body=E} = ER
             , #katt_response{parsed_body=A} = AR
             , Callbacks
             ) ->
  ExtFun = proplists:get_value(ext, Callbacks),
  Ext = ExtFun(validate_body),
  MatchingExt = lists:dropwhile( fun(Fun) ->
                                     not Fun( _JustCheck = true
                                            , ER
                                            , AR
                                            , Callbacks
                                            )
                                 end
                               , Ext
                               ),
  case MatchingExt of
    [] ->
      katt_util:validate("/body", E, A, ?MATCH_ANY, Callbacks);
    [Fun|_] ->
      Fun( _JustCheck = false
         , ER
         , AR
         , Callbacks
         )
  end.


validate_type( Type
             , ParentKey
             , Expected
             , Actual
             , Unexpected
             , Callbacks
             ) ->
  ExtFun = proplists:get_value(ext, Callbacks),
  Ext = ExtFun(validate_type),
  MatchingExt = lists:dropwhile( fun(Fun) ->
                                     not Fun( _JustCheck = true
                                            , Type
                                            , ParentKey
                                            , Expected
                                            , Actual
                                            , Unexpected
                                            , Callbacks
                                            )
                                 end
                               , Ext
                               ),
  case MatchingExt of
    [] ->
      fail;
    [Fun|_] ->
      Fun( _JustCheck = false
         , Type
         , ParentKey
         , Expected
         , Actual
         , Unexpected
         , Callbacks
         )
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
