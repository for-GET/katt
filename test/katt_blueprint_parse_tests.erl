%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Basic interface module tests. More detailed and corner-case tests are done
%%% in the api_blueprint module's tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(katt_blueprint_parse_tests).

-include_lib("eunit/include/eunit.hrl").
-include("blueprint_types.hrl").

-define(EXAMPLE_FILE, "../test/examples/example1.apib").
-define(INVALID_FILE, "../test/examples/example_invalid1.apib").


parse_string_test()->
  ?assertEqual(
    {ok, #api_blueprint{ name=(<<"¿Title · hello"/utf8>>) }},
    katt_blueprint_parse:string("--- ¿Title · hello ---\n---\n---\n")).


parse_invalid_string_test_()->
  [ ?_assertError(
      {badmatch, _},
      {ok, _} = katt_blueprint_parse:string(""))
  , ?_assertError(
      {badmatch, _},
      {ok, _} = katt_blueprint_parse:string("--- API ---\n\n---\n---\n---\n"))
  ].


parse_file_test()->
  Name = <<"Sample API v2">>,
  Op1Desc = <<"List products added into your shopping-cart. (comment block "
                 "again in Markdown)">>,
  ?assertMatch(
    {ok, #api_blueprint{ name=Name
                       , description=_
                       , operations=[ #katt_operation{description=Op1Desc}
                                    , #katt_operation{}
                                    , #katt_operation{}
                                    ]
                       }},
    katt_blueprint_parse:file(?EXAMPLE_FILE)).


parse_invalid_file_test_()->
  [ ?_assertError(
      {badmatch, _},
      {ok, _} = katt_blueprint_parse:file("..."))
  , ?_assertError(
      {badmatch, _},
      {ok, _} = katt_blueprint_parse:file(?INVALID_FILE))
  , ?_assertError(
      {badmatch, _},
      {ok, _} = katt_blueprint_parse:file("/dev/null"))
  ].


%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
