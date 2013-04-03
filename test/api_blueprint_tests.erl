%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% KATT Blueprint Parser tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(api_blueprint_tests).

-include_lib("eunit/include/eunit.hrl").
-include("blueprint_types.hrl").

-define(EXAMPLE_FILE, "../test/examples/example1.apib").

parse_api_test_()->
  Expected = #api_blueprint{ name=(<<"API tæst"/utf8>>)
                           , description=(<<"¿Test ÄPI?"/utf8>>)
                           , operations=[ op_url("/one")
                                        , op_url("/two")
                                        , op_url("/three")
                                        ]
                           },
  [ ?_assertEqual(
      Expected,
      parse_unindented("--- API tæst ---
        ---
        ¿Test ÄPI?
        ---
        GET /one
        < 200

        GET /two
        < 200

        GET /three
        < 200
        "))
  , ?_assertEqual(
      Expected,
      parse_unindented("

        --- API tæst ---

        ---
        ¿Test ÄPI?
        ---

        GET /one
        < 200

        GET /two
        < 200

        GET /three
        < 200

        "))
  , ?_assertEqual(
      Expected,
      parse_unindented("



        --- API tæst ---



        ---
        ¿Test ÄPI?
        ---



        GET /one
        < 200

        GET /two
        < 200

        GET /three
        < 200



        "))
  ].


parse_api_name_test_() ->
  Expected = #api_blueprint{ name=utf8("abcd") },
  [ ?_assertEqual(Expected, parse_unindented("--- abcd"))
  , ?_assertEqual(Expected, parse_unindented("---   abcd"))
  , ?_assertEqual(Expected, parse_unindented("--- abcd ---"))
  , ?_assertEqual(Expected, parse_unindented("--- abcd   ---"))
  ].


parse_api_description_test_() ->
  [ ?_assertEqual(
      #api_blueprint{ name=utf8("API") },
      parse_unindented("
          --- API ---

          ---
          ---
          ")
      )
  , ?_assertEqual(
      #api_blueprint{ name=utf8("API")
                    , description=utf8("asdf")
                    },
      parse_unindented("
        --- API ---

        ---
        asdf
        ---
        "))
  , ?_assertEqual(
      #api_blueprint{ name=utf8("API")
                    , description=utf8("abcd\nefgh\nijkl")
                    },
      parse_unindented("
        --- API ---

        ---
        abcd\nefgh\nijkl
        ---
        "))
  ].


fail_parse_invalid_api_description_test() ->
  ?assertMatch(
       {fail, _},
       parse("
        --- API ---

        ---
        ---
        ---
        ")).


parse_operations_test_() ->
  Expected0 = #api_blueprint{ name=utf8("API")
                            },
  Expected1 = #api_blueprint{ name=utf8("API")
                            , operations=[ op_url("/one")
                                         ]
                            },
  Expected2 = #api_blueprint{ name=utf8("API")
                            , operations=[ op_url("/one")
                                         , op_url("/two")
                                         , op_url("/three")
                                         ]
                            },
  [ ?_assertEqual(
      Expected0,
      parse_unindented("
        --- API ---

        "))
  , ?_assertEqual(
      Expected1,
      parse_unindented("
        --- API ---

        GET /one
        < 200
        "))
  , ?_assertEqual(
      Expected2,
      parse_unindented("
        --- API ---

        GET /one
        < 200

        GET /two
        < 200

        GET /three
        < 200
        "))
  , ?_assertEqual(
      Expected2,
      parse_unindented("
        --- API ---

        GET /one
        < 200



        GET /two
        < 200



        GET /three
        < 200
        "))
  ].


parse_operation_test_() ->
  Request = #request{ headers = [ {"Content-Type", "application/json"}
                                ]
                    , body = <<"{ \"status\": \"ok\" }">>
                    },
  Response = #response{ headers = [ {"Content-Type", "application/json"}
                                  ]
                      , body = <<"{ \"id\": 1 }">>
                      },
  Op = #operation{ request=Request, response=Response },
  [ ?_assertEqual(
      #api_blueprint{ name=utf8("API")
                    , operations=[Op]
                    },
      parse_unindented("
        --- API ---

        GET /
        > Content-Type: application/json
        { \"status\": \"ok\" }
        < 200
        < Content-Type: application/json
        { \"id\": 1 }
      "))
  , ?_assertEqual(
      #api_blueprint{ name=utf8("API")
                    , operations=[ Op#operation{description=utf8("Root resource")}
                                 ]
                    },
      parse_unindented("
        --- API ---

        Root resource
        GET /
        > Content-Type: application/json
        { \"status\": \"ok\" }
        < 200
        < Content-Type: application/json
        { \"id\": 1 }
      "))
  , ?_assertEqual(
      #api_blueprint{ name=utf8("API")
                    , operations=[ op_url("url")
                                 , op_url("/url")
                                 , op_url("/")
                                 , op_url("http://host:80/")
                                 , op_url("{{<var}}")
                                 , op_url("/url/{{<var}}")
                                 ]
                    },
      parse_unindented("
        --- API ---

        GET url
        < 200

        GET /url
        < 200

        GET /
        < 200

        GET http://host:80/
        < 200

        GET {{<var}}
        < 200

        GET /url/{{<var}}
        < 200

      "))
  ].


parse_operation_description_test_() ->
  [ ?_assertEqual(
      #api_blueprint{ name=utf8("API")
                    , operations=[ #operation{description=utf8("abcd")}
                                 ]
                    },
      parse_unindented("
        --- API ---

        abcd
        GET /
        < 200
      "))
  , ?_assertEqual(
      #api_blueprint{ name=utf8("API")
                    , operations=[ #operation{description=utf8("# abcd\nhello")}
                                 ]
                    },
      parse_unindented("
        --- API ---

        # abcd
        hello
        GET /
        < 200
      "))
  ].


fail_parse_invalid_operation_description_test() ->
  ?assertMatch(
     {fail, _},
     parse("
      --- API ---

      GET
      GET /
      < 200
    ")).


parse_http_method_test_() ->
  [ ?_assertEqual(
      #api_blueprint{ name=utf8("API")
                    , operations=[ op_method(Method)
                                 ]
                    },
      parse_unindented("
        --- API ---

        " ++ Method ++ " /
        < 200
        ")
    ) || Method <- [ "GET"
                   , "POST"
                   , "PUT"
                   , "MKCOL"
                   , "OPTIONS"
                   , "PATCH"
                   , "PROPPATCH"
                   , "LOCK"
                   , "UNLOCK"
                   , "COPY"
                   , "MOVE"
                   , "DELETE"
                   , "HEAD"
                   ]].


fail_parse_invalid_http_method_test_() ->
  [ ?_assertMatch(
      {fail, _},
      parse_unindented("
        --- API ---

        " ++ Method ++ " /
        < 200
      ")
    ) || Method <- [ "GOT", "PAST", "PUN", "DEPLETE", "HEAT"]].


parse_request_test_() ->
  [ ?_assertEqual(
      #api_blueprint{
        name=utf8("API"),
        operations=[
          #operation{
            request=#request{
              headers=[{"Content-Type", "application/json"}],
              body=null
            }
          }
        ]
      },
      parse_unindented("
        --- API ---

        GET /
        > Content-Type: application/json
        < 200
      "))
  , ?_assertEqual(
      #api_blueprint{
        name=utf8("API"),
        operations=[
          #operation{
            request=#request{
              headers=[{"Content-Type", "application/json"}],
              body=utf8("{ \"status\": \"ok\" }")
            }
          }
        ]
      },
      parse_unindented("
        --- API ---

        GET /
        > Content-Type: application/json
        { \"status\": \"ok\" }
        < 200
      "))
  ].


parse_request_headers_test_() ->
  [ ?_assertEqual(
      #api_blueprint{
        name=utf8("API"),
        operations=[#operation{}]
      },
      parse_unindented("
        --- API ---

        GET /
        < 200
      "))
  , ?_assertEqual(
      #api_blueprint{
        name=utf8("API"),
        operations=[
          #operation{
            request=#request{
              headers=[{"Content-Type", "application/json"}]
            }
          }
        ]
      },
      parse_unindented("
        --- API ---

        GET /
        > Content-Type: application/json
        < 200
      "))
  , ?_assertEqual(
      #api_blueprint{
        name=utf8("API"),
        operations=[
          #operation{
            request=#request{
              headers=[
                {"Content-Type", "application/json"},
                {"Content-Length", "153"},
                {"Cache-Control", "no-cache"}
              ]
            }
          }
        ]
      },
      parse_unindented("
        --- API ---

        GET /
        > Content-Type: application/json
        > Content-Length: 153
        > Cache-Control: no-cache
        < 200
      "))
  ].


parse_response_test_() ->
  [ ?_assertEqual(
      #api_blueprint{
        name=utf8("API"),
        operations=[
          #operation{
            response=#response{
              headers=[
                {"Content-Type", "application/json"}
              ],
              body=utf8("{ \"id\": 1 }")
            }
          }
        ]
      },
      parse_unindented("
        --- API ---

        GET /
        < 200
        < Content-Type: application/json
        { \"id\": 1 }
      "))
  , ?_assertEqual(
      #api_blueprint{
        name=utf8("API"),
        operations=[
          #operation{
            response=#response{
              headers=[
                {"Content-Type", "application/json"}
              ],
              body=null
            }
          }
        ]
      },
      parse_unindented("
        --- API ---

        GET /
        < 200
        < Content-Type: application/json
      "))
  ].


parse_response_status_test_() ->
  [ ?_assertEqual(
      #api_blueprint{
        name=utf8("API"),
        operations=[#operation{}]
      },
      parse_unindented("
        --- API ---

        GET /
        < 200
      "))
  , ?_assertEqual(
      #api_blueprint{
        name=utf8("API"),
        operations=[
          #operation{
            response=#response{status=400}
          }
        ]
      },
      parse_unindented("
        --- API ---

        GET /
        < 400
      "))
  ].


parse_response_headers_test_() ->
  [ ?_assertEqual(
      #api_blueprint{
        name=utf8("API"),
        operations=[#operation{}]
      },
      parse_unindented("
        --- API ---

        GET /
        < 200
      "))
  , ?_assertEqual(
      #api_blueprint{
        name=utf8("API"),
        operations=[
          #operation{
            response=#response{
              headers=[{"Content-Type", "application/json"}]
            }
          }
        ]
      },
      parse_unindented("
        --- API ---

        GET /
        < 200
        < Content-Type: application/json
      "))
  , ?_assertEqual(
      #api_blueprint{
        name=utf8("API"),
        operations=[
          #operation{
            response=#response{
              headers=[
                {"Content-Type", "application/json"},
                {"Content-Length", "153"},
                {"Cache-Control", "no-cache"}
              ]
            }
          }
        ]
      },
      parse_unindented("
        --- API ---

        GET /
        < 200
        < Content-Type: application/json
        < Content-Length: 153
        < Cache-Control: no-cache
      "))
  ].


parse_response_header_test() ->
  ?assertEqual(
    #api_blueprint{
      name=utf8("API"),
      operations=[
        #operation{
          response=#response{
            headers=[
              {"Content-Type", "application/json"}
            ]
          }
        }
      ]
    },
    parse_unindented("
      --- API ---

      GET /
      < 200
      < Content-Type: application/json
    ")).

parse_http_status_test_() ->
  %% Test a small sample of valid HTTP codes.
  [ ?_assertEqual(
      #api_blueprint{ name=utf8("API")
                    , operations=[ #operation{ response=#response{ status=N }}
                                 ]
                    },
      parse_unindented("
        --- API ---

        GET /
        < " ++ integer_to_list(N) ++ "
        ")
    ) || N <- [100, 101, 200, 302, 400, 401, 404, 500, 599]].


fail_parse_invalid_http_status_test_() ->
  %% Test a small sample of invalid HTTP codes.
  [ ?_assertMatch(
      {fail, _},
      parse_unindented("
        --- API ---

        GET /
        < " ++ integer_to_list(N) ++ "
        ")
    ) || N <- [0, 9, 666, -300, -1000, 999999]].


fail_parse_non_numeric_http_status_test_() ->
  [ ?_assertMatch(
      {fail, _},
      parse_unindented("
        --- API ---

        GET /
        <
      "))
  , ?_assertMatch(
      {fail, _},
      parse_unindented("
        --- API ---

        GET /
        < FAIL
        "))
  ].


parse_http_header_test_() ->
  [ ?_assertEqual(
      #api_blueprint{
        name=utf8("API"),
        operations=[
          #operation{
            response=#response{
              headers=[
                {"Content-Type", "application/json"}
              ]
            }
          }
        ]
      },
      parse_unindented("
        --- API ---

        GET /
        < 200
        < " ++ Header ++ "
        ")
    ) || Header <- [ "Content-Type:application/json"
                   , "Content-Type: application/json"
                   , "Content-Type:   application/json"
                   ]].


parse_http_header_name_test_() ->
  [ ?_assertEqual(
      #api_blueprint{
        name=utf8("API"),
        operations=[
          #operation{
            response=#response{
              headers=[
                {HeaderName, "application/json"}
              ]
            }
          }
        ]
      },
      parse_unindented("
        --- API ---

        GET /
        < 200
        < " ++ HeaderName ++ ": application/json
        ")
    ) || HeaderName <- ["!", "9", ";", "~~", "abc"]].


parse_http_header_value_test_() ->
  [ ?_assertEqual(
      #api_blueprint{
        name=utf8("API"),
        operations=[
          #operation{
            response=#response{
              headers=[
                {"X-Some-Header", HeaderValue}
              ]
            }
          }
        ]
      },
      parse_unindented("
        --- API ---

        GET /
        < 200
        < X-Some-Header: " ++ HeaderValue ++ "
        ")
    ) || HeaderValue <- [ "foo"
                        , "<<<"
                        , "asdf\\nasdfasdf"
                        , ""
                        , "123"
                        , "{{>var}}"
                        , "{{<var}}"
                        , "{{_}}"
                        ]].


fail_parse_invalid_http_header_name_test_() ->
  [ ?_assertMatch(
      {fail, _},
      parse_unindented("
        --- API ---

        GET /
        < 200
        < " ++ BadHeaderName ++ ": application/json
        ")
    ) || BadHeaderName <- [":", "", "Œ"]].


parse_body_test_() ->
  [ ?_assertEqual(
      #api_blueprint{
        name=utf8("API"),
        operations=[
          #operation{
            response=#response{
              body=utf8(Body)
            }
          }
        ]
      },
      parse_unindented("
        --- API ---

        GET /
        < 200
        " ++ Body ++ "
        ")
    ) || Body <- [ "foo"
                 , "asdf\\nasdfasdf"
                 , "abcd\nefgh\nijkl"
                 , "123"
                 , "{{>var}}"
                 , "{{<var}}"
                 , "{{_}}"
                 ]].


%% We don't support the delimited body
fail_parse_invalid_body_test_() ->
  [ ?_assertMatch(
      {fail, _},
      parse_unindented("
        --- API ---

        GET /
        < 200
        " ++ Body ++ "
        ")
    ) || Body <- [ "<<<"
                 , "<<<EOT\nEOT"
                 , "<<<>>>"
                 , "<<<\nabcd\n>>>"
                 ]].


parse_file_test() ->
  Name = utf8("Sample API v2"),
  Op1Desc = utf8("List products added into your shopping-cart. (comment block "
                 "again in Markdown)"),
  ?assertMatch(
    #api_blueprint{ name=Name
                  , description=_
                  , operations=[ #operation{description=Op1Desc}
                               , #operation{}
                               , #operation{}
                               ]
                  },
    api_blueprint:file(?EXAMPLE_FILE)).


%%% Consider adding tests for simple_body, simple_body_line, out, in, text0,
%%% text1, empty_line, eolf, eol, eof, s.
%%% For now these are at least mostly covered through higer-level tests.


%%% Helpers

op_url(Url) ->
  #operation{ request=#request{ url = Url } }.

op_method(Method) ->
  #operation{ request=#request{ method = Method } }.

%% Unindent the blueprint before parsing it.
parse_unindented(BlueprintString) ->
  api_blueprint:parse(unindent(BlueprintString)).

parse(BlueprintString) ->
  api_blueprint:parse(BlueprintString).

%% Naive conversion to UTF-8.
utf8(Chars) ->
  unicode:characters_to_binary(Chars, utf8).

%% Remove leading spaces from all lines in Str.
unindent(Str) ->
  Lines = re:split(Str, "\n", [{return, list}]),
  F = fun(Line) -> string:strip(Line, left, hd(" ")) end,
  string:join(lists:map(F, Lines), "\n").


%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
