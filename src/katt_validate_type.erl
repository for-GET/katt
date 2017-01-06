%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%% @copyright 2014- AUTHORS
%%%
%%% @doc Klarna API Testing Tool Validate Type
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
%% @private
-module(katt_validate_type).

%%%_* Exports ==================================================================
%% API
-export([ validate_type_set/5
        , validate_type_runtime_value/5
        , validate_type_runtime_validation/5
        ]).

%%%_* Includes =================================================================
-include("katt.hrl").

%%%_* API ======================================================================

-spec validate_type_set( string()
                       , proplists:proplist()
                       , proplists:proplist()
                       , term()
                       , callbacks()
                       ) -> pass | [validation_failure()].
validate_type_set( ParentKey
                 , Options
                 , {array, AItems0} = _Actual
                 , _ItemsMode
                 , Callbacks
                 ) ->
  {array, EItems0} = proplists:get_value("value", Options),
  ItemsMode = proplists:get_value(?MATCH_ANY, EItems0, ?MATCH_ANY),
  EItems1 = proplists:delete(?MATCH_ANY, EItems0),
  EItems = lists:keysort(2, EItems1),
  AItems = lists:keysort(2, AItems0),
  validate_set(ParentKey, EItems, AItems, ItemsMode, Callbacks, []);
validate_type_set(ParentKey, Options, Actual, _ItemsMode, _Callbacks) ->
  [{not_equal, {ParentKey, Options, Actual}}].

-spec validate_type_runtime_value( string()
                                 , proplists:proplist()
                                 , proplists:proplist()
                                 , term()
                                 , callbacks()
                                 ) -> pass | [validation_failure()].
validate_type_runtime_value( ParentKey
                           , [{"erlang", Erlang0} | _Options]
                           , Actual
                           , ItemsMode
                           , Callbacks
                           ) ->
  Erlang = case Erlang0 of
             {array, ErlangLines} ->
               string:join(lists:map(fun({_, V}) -> V end, ErlangLines), "\n");
             _ ->
               Erlang0
           end ++ ".",
  {Error, Expected} =
    try
      {ok, Tokens, _} = erl_scan:string(Erlang),
      {ok, Exprs} = erl_parse:parse_exprs(Tokens),
      {value, Expected0, _} = erl_eval:exprs( Exprs
                                            , [ {'ParentKey', ParentKey}
                                              , {'Actual', Actual}
                                              , {'ItemsMode', ItemsMode}
                                              , {'Callbacks', Callbacks}]
                                            ),
      {undefined, Expected0}
    catch
      C:E ->
        { Erlang ++ "~n"
          ++ katt_util:erl_to_list(C) ++ ":" ++ katt_util:erl_to_list(E)
        , undefined
        }
    end,
  case Error of
    undefined ->
      katt_util:validate(ParentKey, Expected, Actual, ItemsMode, Callbacks);
    _ ->
      {not_equal, {ParentKey, Error, Actual}}
  end;
validate_type_runtime_value( ParentKey
                           , [{"shell", Shell0} | _Options]
                           , Actual
                           , ItemsMode
                           , Callbacks
                           ) ->
  Shell = case Shell0 of
            {array, ShellLines} ->
              string:join(lists:map(fun({_, V}) -> V end, ShellLines), "\n");
            _ ->
              Shell0
          end,
  try
    {0, Erlang} = katt_util:os_cmd( Shell
                                  , [ {"KATT_PARENT_KEY", ParentKey}
                                    , { "KATT_ACTUAL"
                                      , io_lib:format("~p", [Actual])
                                      }
                                    , { "KATT_ITEMS_MODE"
                                      , io_lib:format("~p", [ItemsMode])
                                      }
                                    ]),
    validate_type_runtime_value( ParentKey
                               , [{"erlang", Erlang}]
                               , Actual
                               , ItemsMode
                               , Callbacks
                               )
  catch
    C:E ->
      Error = Shell ++ "~n"
        ++ katt_util:erl_to_list(C) ++ ":" ++ katt_util:erl_to_list(E),
      {not_equal, {ParentKey, Error, Actual}}
  end.


-spec validate_type_runtime_validation( string()
                                      , proplists:proplist()
                                      , proplists:proplist()
                                      , term()
                                      , callbacks()
                                      ) -> pass | [validation_failure()].
validate_type_runtime_validation( ParentKey
                                , [{"erlang", Erlang0} | _Options]
                                , Actual
                                , ItemsMode
                                , Callbacks
                                ) ->
  Erlang = case Erlang0 of
             {array, ErlangLines} ->
               string:join(lists:map(fun({_, V}) -> V end, ErlangLines), "\n");
             _ ->
               Erlang0
           end ++ ".",
  try
    {ok, Tokens, _} = erl_scan:string(Erlang),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    {value, Result, _} = erl_eval:exprs( Exprs
                                       , [ {'ParentKey', ParentKey}
                                         , {'Actual', Actual}
                                         , {'ItemsMode', ItemsMode}
                                         , {'Callbacks', Callbacks}]
                                       ),
    Result
  catch
    C:E ->
      Error = { Erlang ++ "~n"
                ++ katt_util:erl_to_list(C) ++ ":" ++ katt_util:erl_to_list(E)
              , undefined
              },
      {not_equal, {ParentKey, Error, Actual}}
  end;
validate_type_runtime_validation( ParentKey
                                , [{"shell", Shell0} | _Options]
                                , Actual
                                , ItemsMode
                                , Callbacks
                                ) ->
  Shell = case Shell0 of
            {array, ShellLines} ->
              string:join(lists:map(fun({_, V}) -> V end, ShellLines), "\n");
            _ ->
              Shell0
          end,
  try
    {0, Erlang} = katt_util:os_cmd( Shell
                                  , [ {"KATT_PARENT_KEY", ParentKey}
                                    , { "KATT_ACTUAL"
                                      , io_lib:format("~p", [Actual])
                                      }
                                    , { "KATT_ITEMS_MODE"
                                      , io_lib:format("~p", [ItemsMode])
                                      }
                                    ]),
    validate_type_runtime_validation( ParentKey
                                    , [{"erlang", Erlang}]
                                    , Actual
                                    , ItemsMode
                                    , Callbacks
                                    )
  catch
    C:E ->
      Error = { Shell ++ "~n"
                ++ katt_util:erl_to_list(C) ++ ":" ++ katt_util:erl_to_list(E)
              , undefined
              },
      {not_equal, {ParentKey, Error, Actual}}
  end.


%%%_* Internal =================================================================

-spec validate_set( string()
                  , proplists:proplist()
                  , proplists:proplist()
                  , term()
                  , callbacks()
                  , list()
                  ) -> pass | [validation_failure()].
%% No more items in any of the expected / actual arrays
validate_set( _ParentKey
            , [] = _Expected
            , [] = _Actual
            , _ItemsMode
            , _Callbacks
            , Errors
            ) ->
  lists:flatten(Errors);
%% All expected elements have been consumed,
%% and we allow extra actual elements
validate_set( _ParentKey
            , [] = _Expected
            , _Actual
            , ?MATCH_ANY
            , _Callbacks
            , Errors
            ) ->
  lists:flatten(Errors);
%% All expected elements have been consumed,
%% and we do not allow extra actual elements
validate_set( ParentKey
            , [] = _Expected
            , Actual
            , ?UNEXPECTED
            , _Callbacks
            , Errors
            ) ->
  [ {unexpected, {ParentKey ++ "/" ++ AKey, undefined, AValue}}
    || {AKey, AValue} <- Actual
  ] ++ Errors;
%% All actual elements have been consumed,
%% and there are still expected elements
validate_set( ParentKey
            , Expected
            , []
            , _ItemsMode
            , _Callbacks
            , Errors
            ) ->
  [ {not_contains, {ParentKey ++ "/" ++ EKey, EValue, undefined}}
    || {EKey, EValue} <- Expected
  ] ++ Errors;
%% There is at least one expected and one actual element
validate_set( ParentKey
            , [{EKey, EValue} | ERest] = _Expected
            , Actual
            , ItemsMode
            , Callbacks
            , Errors
            ) ->
  Split =
    lists:splitwith( fun({_Key, Value}) ->
                         not katt_util:is_valid( _ParentKey = ""
                                               , EValue
                                               , Value
                                               , ItemsMode
                                               , Callbacks
                                               )
                     end
                   , Actual),
  case Split of
    {Actual, []} ->
      Errors1 =
        [{not_contains, {ParentKey ++ "/" ++ EKey, EValue, undefined}}] ++
        Errors,
      validate_set(ParentKey, ERest, Actual, ItemsMode, Callbacks, Errors1);
    {ARest1, [{_AKey, AValue}|ARest2]} ->
      ARest = ARest1 ++ ARest2,
      Errors1 = [katt_util:validate( _ParentKey = ""
                                   , EValue
                                   , AValue
                                   , ItemsMode
                                   , Callbacks
                                   )] ++ Errors,
      validate_set(ParentKey, ERest, ARest, ItemsMode, Callbacks, Errors1)
  end.
