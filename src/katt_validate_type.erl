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
validate_type_set(ParentKey, Options, Actual0, _Unexpected, Callbacks) ->
  {struct, Expected0} = proplists:get_value("value", Options),
  Unexpected = proplists:get_value(?MATCH_ANY, Expected0, ?MATCH_ANY),
  Expected1 = proplists:delete(?MATCH_ANY, Expected0),
  Expected = lists:keysort(2, Expected1),
  Actual = lists:keysort(2, Actual0),
  validate_set(ParentKey, Expected, Actual, Unexpected, Callbacks, []).

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
            , _Unexpected
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
  [ {unexpected, ParentKey ++ "/" ++ Key}
    || {Key, _Value} <- Actual
  ] ++ Errors;
%% All actual elements have been consumed,
%% and there are still expected elements
validate_set( ParentKey
            , Expected
            , []
            , _Unexpected
            , _Callbacks
            , Errors
            ) ->
  [ {not_contains, ParentKey ++ "/" ++ Key}
    || {Key, _Value} <- Expected
  ] ++ Errors;
%% There is at least one expected and one actual element
validate_set( ParentKey
            , [{EKey, EValue} | ERest] = _Expected
            , Actual
            , Unexpected
            , Callbacks
            , Errors
            ) ->
  Split =
    lists:splitwith( fun({_Key, Value}) ->
                         not katt_util:is_valid( _ParentKey = ""
                                               , EValue
                                               , Value
                                               , Unexpected
                                               , Callbacks
                                               )
                     end
                   , Actual),
  case Split of
    {Actual, []} ->
      Errors1 = [{not_contains, ParentKey ++ "/" ++ EKey}] ++ Errors,
      validate_set(ParentKey, ERest, Actual, Unexpected, Callbacks, Errors1);
    {ARest1, [{_AKey, AValue}|ARest2]} ->
      ARest = ARest1 ++ ARest2,
      Errors1 = [katt_util:validate( _ParentKey = ""
                                   , EValue
                                   , AValue
                                   , Unexpected
                                   , Callbacks
                                   )] ++ Errors,
      validate_set(ParentKey, ERest, ARest, Unexpected, Callbacks, Errors1)
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
