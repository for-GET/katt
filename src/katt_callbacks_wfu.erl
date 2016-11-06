%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright 2016- AUTHORS
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
%%% @copyright 2016- AUTHORS
%%%
%%% @doc application/x-www-form-urlencoded callback functions.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(katt_callbacks_wfu).

%%%_* Exports ==================================================================
%% API
-export([ recall_body/4
        , parse/5
        , validate_body/4
        , validate_type/7
        , parse_wfu/1
        ]).

%%%_* Includes =================================================================
-include("katt.hrl").

%%%_* API ======================================================================

%% @doc Recall all params inside application/x-www-form-urlencoded content.
%% @end
-spec recall_body( boolean()
                 , any()
                 , params()
                 , callbacks()
                 ) -> any().
recall_body(true = _JustCheck, [Hdrs, _Bin], _Params, _Callbacks) ->
  is_wfu_content_type(Hdrs);
recall_body(false = _JustCheck, [_Hdrs, <<"{{", _/binary>> = Bin], [], _Callbacks) ->
  Bin;
recall_body(false = _JustCheck, [_Hdrs, <<"{", _/binary>> = Bin], [], _Callbacks) ->
  hackney_url:qs(jsx:decode(Bin));
recall_body(false = _JustCheck, [_Hdrs, Bin], [], _Callbacks) ->
  Bin;
recall_body(false = _JustCheck, [Hdrs, Bin0], [{K0, V0} | Next], Callbacks) ->
  K = ?RECALL_BEGIN_TAG ++ K0 ++ ?RECALL_END_TAG,
  REK = katt_util:escape_regex(K),
  V = katt_util:from_utf8(hackney_url:urlencode(katt_util:to_list(V0))),
  REV = katt_util:escape_regex(V),
  Bin1 =
    lists:foldl( fun({Prefix, Suffix}, LoopBin) ->
                     re:replace( LoopBin
                               , "(" ++ Prefix ++ ")" ++ REK ++ "(" ++ Suffix ++ ")"
                               , "\\1" ++ REV ++ "\\2"
                               , [{return, binary}, global])
                 end
               , Bin0
               , [ {"^", "="}
                 , {"&", "="}
                 , {"=", "$"}
                 , {"=", "&"}
                 ]
     ),
  Bin = katt_callbacks:recall(text, Bin1, [{K0, V0}], Callbacks),
  recall_body(false, [Hdrs, Bin], Next, Callbacks).


%% @doc Parse the body of e.g. an HTTP response.
%% @end
-spec parse( boolean()
           , headers()
           , body()
           , params()
           , callbacks()
           ) -> any().
parse(true = _JustCheck, Hdrs, _Body, _Params, _Callbacks) ->
  is_wfu_content_type(Hdrs);
parse(false = _JustCheck, _Hdrs, null, _Params, _Callbacks) ->
  [];
parse(false = _JustCheck, Hdrs, Body, _Params, _Callbacks) ->
  case is_wfu_content_type(Hdrs) of
    true ->
      parse_wfu(Body);
    false ->
      katt_util:from_utf8(Body)
  end.


validate_body( true = _Justcheck
             , #katt_response{headers=EHdrs}
             , #katt_response{headers=AHdrs}
             , _Callbacks
             ) ->
  is_wfu_content_type(EHdrs) andalso
    is_wfu_content_type(AHdrs);
validate_body( false = _Justcheck
             , #katt_response{parsed_body=E}
             , #katt_response{parsed_body=A}
             , Callbacks
             ) ->
  katt_util:validate("/body", E, A, ?MATCH_ANY, Callbacks).


validate_type( true = _JustCheck
             , Type
             , _ParentKey
             , _Options
             , _Actual
             , _Unexpected
             , _Callbacks
             ) when Type =:= "set" orelse
                    Type =:= "runtime_value" orelse
                    Type =:= "runtime_validation" ->
  true;
validate_type( true = _JustCheck
             , _Type
             , _ParentKey
             , _Options
             , _Actual
             , _Unexpected
             , _Callbacks
             ) ->
  false;
validate_type( false = _JustCheck
             , "set"
             , ParentKey
             , Options
             , Actual
             , Unexpected
             , Callbacks
             ) ->
  katt_validate_type:validate_type_set( ParentKey
                                      , Options
                                      , Actual
                                      , Unexpected
                                      , Callbacks
                                      );
validate_type( false = _JustCheck
             , "runtime_value"
             , ParentKey
             , Options
             , Actual
             , Unexpected
             , Callbacks
             ) ->
  katt_validate_type:validate_type_runtime_value( ParentKey
                                                , Options
                                                , Actual
                                                , Unexpected
                                                , Callbacks
                                                );
validate_type( false = _JustCheck
             , "runtime_validation"
             , ParentKey
             , Options
             , Actual
             , Unexpected
             , Callbacks
             ) ->
  katt_validate_type:validate_type_runtime_validation( ParentKey
                                                     , Options
                                                     , Actual
                                                     , Unexpected
                                                     , Callbacks
                                                     );
validate_type( false = _JustCheck
             , _Type
             , _ParentKey
             , _Options
             , _Actual
             , _Unexpected
             , _Callbacks
             ) ->
  fail.

%%%_* Internal =================================================================

-ifdef(BARE_MODE).
parse_wfu(_Bin) ->
  throw(bare_mode).
-else.

parse_wfu(Bin) when is_binary(Bin) andalso size(Bin) =:= 0 ->
  [];
parse_wfu(Bin) ->
  {struct, hackney_url:parse_qs(Bin)}.

-endif.

is_wfu_content_type(Hdrs0) ->
  Hdrs = [{katt_util:to_lower(K), V} || {K, V} <- Hdrs0],
  ContentType = proplists:get_value("content-type", Hdrs, ""),
  case re:run(ContentType, "^application/x-www-form-urlencoded(;.+)?$") of
    nomatch -> false;
    {match, _} -> true
  end.
