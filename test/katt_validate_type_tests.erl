%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%% @copyright 2014- See AUTHORS
%%%
%%% KATT Validate Type Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(katt_validate_type_tests).

-include("../src/katt.hrl").
-include_lib("eunit/include/eunit.hrl").

-define( OBJECT_1
       , { struct
         , [ { "0"
             , { struct
               , [{ "inner_object"
                  , { struct
                    , [ {"0", {struct, [{"inner_object1", 1}]}}
                      , {"1", {struct, [{"inner_object2", 2}]}}
                      , {"2", {struct, [{"inner_object3", 3}]}}
                      ]
                    }
                  }]
               }
             }
           , {"1", {struct, [{"object1", 1}]}}
           , {"2", {struct, [{"object2", 3}]}}
           ]
         }
       ).
-define( OBJECT_2
       , { struct
         , [ { "0"
             , { struct
               , [{ "inner_object"
                  , { struct
                    , [ {"0", {struct, [{"inner_object1", 2}]}}
                      , {"1", {struct, [{"inner_object2", 4}]}}
                      , {"2", {struct, [{"inner_object3", 6}]}}
                      ]
                    }
                  }]
               }
             }
           , {"1", {struct, [{"object1", 1}]}}
           , {"2", {struct, [{"object2", 3}]}}
           ]
         }
       ).
-define( OBJECT_3
       , { struct
         , [ { "0"
             , { struct
               , [{ "inner_object"
                  , { struct
                    , [ {"0", {struct, [{"inner_object1", 3}]}}
                      , {"1", {struct, [{"inner_object2", 6}]}}
                      , {"2", {struct, [{"inner_object3", 9}]}}
                      ]
                    }
                  }]
               }
             }
           , {"1", {struct, [{"object1", 1}]}}
           , {"2", {struct, [{"object2", 3}]}}
           ]
         }
       ).
-define( SIMPLE_OBJECT1
       , { struct
         , [ {"0", {struct, [{"type", "unknown"}]}}
           , {"1", {struct, [{"origin", "space"}]}}
           ]
         }
       ).
-define( SIMPLE_OBJECT2
       , { struct
         , [ {"0", {struct, [{"type", "known"}]}}
           , {"1", {struct, [{"origin", "earth"}]}}
           ]
         }
       ).
-define( UNEXPECTED_OBJECT
       , {?MATCH_ANY, ?UNEXPECTED}
       ).

pass_when_there_is_only_one_element_test() ->
  ?assertMatch( []
              , filter_errors(compare_as_set( [{"0", ?SIMPLE_OBJECT1}]
                                            , [{"0", ?SIMPLE_OBJECT1}]
                                            ))
              ).
pass_when_lists_reversed_test() ->
  ?assertMatch( []
              , filter_errors(compare_as_set( [ {"0", ?SIMPLE_OBJECT1}
                                              , {"1", ?SIMPLE_OBJECT2}
                                              ]
                                            , [ {"0", ?SIMPLE_OBJECT2}
                                              , {"1", ?SIMPLE_OBJECT1}
                                              ]
                                            ))
              ).
pass_when_lists_reversed_and_unexpected_test() ->
  ?assertMatch( []
              , filter_errors(compare_as_set( [ {"0", ?OBJECT_1}
                                              , {"1", ?OBJECT_2}
                                              , ?UNEXPECTED_OBJECT
                                              ]
                                            , [ {"0", ?OBJECT_2}
                                              , {"1", ?OBJECT_1}
                                              ]
                                            ))
              ).
pass_when_expected_contained_by_actual_test() ->
  ?assertMatch( []
              , filter_errors(compare_as_set( [ {"0", ?OBJECT_1}
                                              , {"1", ?OBJECT_2}
                                              ]
                                            , [ {"0", ?OBJECT_2}
                                              , {"1", ?OBJECT_1}
                                              , {"2", ?OBJECT_3}
                                              ]
                                            ))
              ).
fail_when_expected_contained_by_actual_and_unexpected_test() ->
  ?assertMatch( [{unexpected, "/2"}]
              , filter_errors(compare_as_set( [ {"0", ?OBJECT_1}
                                              , {"1", ?OBJECT_2}
                                              , ?UNEXPECTED_OBJECT
                                              ]
                                            , [ {"0", ?OBJECT_2}
                                              , {"1", ?OBJECT_1}
                                              , {"2", ?OBJECT_3}
                                              ]
                                            ))
              ).

fail_with_3_reasons_test() ->
  ?assertMatch( [ {unexpected, "/0"}
                , {unexpected, "/1"}
                , {not_contains, "/0"}
                ]
              , filter_errors(compare_as_set( [ {"0", ?OBJECT_1}
                                              , ?UNEXPECTED_OBJECT
                                              ]
                                            , [ {"0", ?OBJECT_2}
                                              , {"1", ?OBJECT_3}
                                              ]
                                            ))
              ).

%% Filters out all the {pass , _ } tuples in the result, so that only errors
%% remain.
filter_errors(Errors) ->
  lists:filter( fun({pass, _}) -> false;
                   (_) -> true
                end
              , Errors
              ).

%% Do some extra preparation for calling katt_validate_type:validate_type_set/5
compare_as_set(Expected, Actual) ->
  Options = [{"value", {struct, Expected}}],
  Callbacks = katt:make_callbacks([]),
  katt_validate_type:validate_type_set( ""
                                      , Options
                                      , Actual
                                      , ?MATCH_ANY
                                      , Callbacks
                                      ).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
