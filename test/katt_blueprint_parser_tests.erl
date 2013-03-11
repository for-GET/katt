%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Basic interface module tests. More detailed and corner-case tests are done
%%% in the api_blueprint module's tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(katt_blueprint_parser_tests).

-include_lib("eunit/include/eunit.hrl").
-include("blueprint_types.hrl").

-ifdef(EUNIT).

-define(EXAMPLE_FILE, "../test/examples/example1.apib").


parse_string_test()->
  ?assertEqual(
    {ok, #api_blueprint{ name=(<<"¿Title · hello"/utf8>>) }},
    katt_blueprint_parser:parse("--- ¿Title · hello ---\n---\n---\n")).


parse_invalid_string_test()->
  ?assertMatch(
    {error, _},
    katt_blueprint_parser:parse("")),
  ?assertMatch(
    {error, _},
    katt_blueprint_parser:parse("--- API ---\n\n---\n---\n---\n")).


parse_file_test()->
  Name = <<"Sample API v2">>,
  Op1Desc = <<"List products added into your shopping-cart. (comment block "
                 "again in Markdown)">>,
  ?assertMatch(
    {ok, #api_blueprint{ name=Name
                       , description=_
                       , operations=[ #operation{description=Op1Desc}
                                    , #operation{}
                                    , #operation{}
                                    ]
                       }},
    katt_blueprint_parser:parse_file(?EXAMPLE_FILE)).


parse_invalid_file_test()->
  ?assertMatch(
    {error, _},
    katt_blueprint_parser:parse_file("...")).

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
