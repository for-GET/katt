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
%%% @doc Klarna API Testing Tool Validation
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
%% @private
-module(katt_validate).

%%%_* Exports ==================================================================
%% API
-export([ is_valid/3
        , is_valid/5
        , validate/3
        , validate/5
        , get_params_and_failures/1
        ]).

%%%_* Includes =================================================================
-include("katt.hrl").

%%%_* API ======================================================================

is_valid(ParentKey, E, A) ->
  case validate(ParentKey, E, A) of
    {pass, _} ->
      true;
    _ ->
      false
  end.

is_valid(ParentKey, E, A, ItemsMode, Callbacks) ->
  Result = validate(ParentKey, E, A, ItemsMode, Callbacks),
  case get_params_and_failures(Result) of
    {_, []} ->
      true;
    _ ->
      false
  end.

validate(ParentKey, E, A) ->
  validate_primitive(ParentKey, E, A, []).

%% Expected anything
validate(_ParentKey, ?MATCH_ANY = _E, _A, _ItemsMode, _Callbacks) ->
  {pass, []};

%% Expected actual
validate(_ParentKey, E, E, _ItemsMode, _Callbacks) ->
  {pass, []};

%% Expected struct/array, got struct/array
validate( ParentKey
        , {Type, EItems} = _E
        , {Type, AItems} = _A
        , ItemsMode
        , Callbacks
        ) when Type =:= struct orelse
               Type =:= array ->
  validate_proplist( ParentKey
                   , EItems
                   , AItems
                   , ItemsMode
                   , Callbacks
                   );

%% Expected some Type
validate( ParentKey
        , {Type, Options} = _E
        , A
        , ItemsMode
        , Callbacks
        ) when Type =/= struct andalso
               Type =/= array ->
  katt_callbacks:validate_type( Type
                              , ParentKey ++ "/{{" ++ Type ++ "}}"
                              , Options
                              , A
                              , ItemsMode
                              , Callbacks
                              );

%% Expected something else
validate(ParentKey, E, A, ItemsMode, Callbacks) ->
  validate_simple(ParentKey, E, A, ItemsMode, Callbacks).

%% Fold a list of {pass, Params} / Failures into a pair of params and failures
get_params_and_failures(Result) when not is_list(Result) ->
  get_params_and_failures([Result]);
get_params_and_failures(Result) ->
  lists:foldl(
    fun({pass, AddParams1}, {AddParams0, Failures0}) ->
        AddParams = katt_util:merge_proplists(AddParams0, AddParams1),
        {AddParams, Failures0};
       (Failure, {AddParams0, Failures0}) ->
        {AddParams0, [Failure | Failures0]}
    end,
    {[], []},
    lists:flatten(Result)
   ).

%%%_* Internal =================================================================

validate_proplist( ParentKey
                 , EItems0
                 , AItems
                 , _ItemsMode
                 , Callbacks
                 ) ->
  ItemsMode = proplists:get_value(?MATCH_ANY, EItems0, ?MATCH_ANY),
  EItems = proplists:delete(?MATCH_ANY, EItems0),
  Keys = lists:usort([ Key
                       || {Key, _} <- lists:merge(EItems, AItems)
                     ]),
  lists:map( fun(Key) ->
                 EValue = proplists:get_value(Key, EItems, ItemsMode),
                 case is_items_mode(EValue) of
                   true ->
                     validate( ParentKey ++ "/" ++ Key
                             , undefined
                             , proplists:get_value(Key, AItems)
                             , EValue
                             , Callbacks
                             );
                   false ->
                     validate( ParentKey ++ "/" ++ Key
                             , EValue
                             , proplists:get_value(Key, AItems)
                             , ItemsMode
                             , Callbacks
                             )
                 end
             end
           , Keys
           ).

%% Validate when unexpected values show up
%% Default to ItemsMode
validate_simple(Key, undefined = _E, A, ItemsMode, Callbacks) ->
  validate_primitive(Key, ItemsMode, A, Callbacks);

%% Otherwise
validate_simple(Key, E, A, _ItemsMode, Callbacks) ->
  validate_primitive(Key, E, A, Callbacks).

%% Validate JSON primitive types or empty structured types
%% Expected and undefined
validate_primitive(Key, ?EXPECTED = E, undefined = A, _Callbacks) ->
  {expected, {Key, E, A}};

%% Expected
validate_primitive(_Key, ?EXPECTED = _E, _A, _Callbacks) ->
  {pass, []};

%% Not expected and undefined
validate_primitive(_Key, ?UNEXPECTED = _E, undefined = _A, _Callbacks) ->
  {pass, []};

%% Not expected
validate_primitive(Key, ?UNEXPECTED = E, A, _Callbacks) ->
  {unexpected, {Key, E, A}};

%% Expected anything
validate_primitive(_Key, ?MATCH_ANY, _A, _Callbacks) ->
  {pass, []};

%% Expected actual
validate_primitive(_Key, E, E, _Callbacks) ->
  {pass, []};

%% Expected text or store param
validate_primitive(Key, E, A, Callbacks) when is_binary(E) ->
  validate_primitive(Key, katt_util:from_utf8(E), A, Callbacks);
validate_primitive(Key, E, A, Callbacks) when is_binary(A) ->
  validate_primitive(Key, E, katt_util:from_utf8(A), Callbacks);
validate_primitive(Key, E, A, Callbacks) when is_list(E) ->
  TextDiffFun = proplists:get_value( text_diff
                                   , Callbacks
                                   ),
  RE_HAS_PARAMS = "("
    ++ ?STORE_BEGIN_TAG
    ++ "[^}]+"
    ++ ?STORE_END_TAG
    ++ "|"
    ++ ?MATCH_ANY
    ++ ")",
  case re:run(E, RE_HAS_PARAMS, [global, {capture, all_but_first, list}]) of
    nomatch when is_list(A) ->
      {not_equal, {Key, E, A, TextDiffFun(E, A)}};
    {match, [[E]]} when E =/= ?MATCH_ANY  ->
      {pass, [{store_tag2param(E), A}]};
    {match, Params0} when is_list(A) ->
      Params = lists:map( fun([?MATCH_ANY]) ->
                              ?MATCH_ANY;
                             ([Match]) ->
                              store_tag2param(Match)
                          end
                        , Params0),
      RE0 = re:replace( E
                      , ?STORE_BEGIN_TAG ++ "[^}]+" ++ ?STORE_END_TAG
                      , "___store___"
                      , [global]
                      ),
      RE1 = re:replace( RE0
                      , ?MATCH_ANY
                      , "___skip___"
                      , [global]
                      ),
      RE2 = re:replace( RE1
                      , "[\\-\\[\\]\\/\\{\\}\\(\\)\\*\\+"
                        ++ "\\?\\.\\,\\\\\^\\$\\|\\#\\s\\&]"
                      , "\\\\&"
                      , [global]
                      ),
      RE3 = re:replace( RE2
                      , "___store___"
                      , "(.+)"
                      , [global]
                      ),
      RE4 = re:replace( RE3
                      , "___skip___"
                      , "(.*)"
                      , [global]
                      ),
      RE = ["^", RE4, "$"],
      case re:run(A, RE, [global, {capture, all_but_first, list}]) of
        nomatch ->
          {not_equal, {Key, E, A, TextDiffFun(E, A)}};
        {match, [Values]} ->
          ParamsValues = lists:filter( fun({?MATCH_ANY, _Value}) ->
                                           false;
                                          (_) ->
                                           true
                                       end
                                     , lists:zip(Params, Values)
                                     ),
          {pass, ParamsValues}
      end;
    _ ->
      {not_equal, {Key, E, A}}
  end;

%% Otherwise
validate_primitive(Key, E, A, _Callbacks) ->
  {not_equal, {Key, E, A}}.

store_tag2param(?STORE_BEGIN_TAG ++ Rest) ->
  Param = string:sub_string(Rest, 1, string:str(Rest, ?STORE_END_TAG) - 1),
  Param.

is_items_mode(_ItemsNode)
  when _ItemsNode =:= ?MATCH_ANY orelse
       _ItemsNode =:= ?UNEXPECTED ->
  true;
is_items_mode(_ItemsNode) ->
  false.
