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
%%% @doc Klarna API Testing Tool
%%%
%%% KATT2HAR CLI.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(katt_har_cli).

%%%_* Exports ==================================================================
%% API
-export([ main/1
        , help/0
        ]).

%%%_* Includes =================================================================
-include("katt.hrl").

%%%_* API ======================================================================

main([]) ->
  main(["--help"]);
main(["-h"]) ->
  main(["--help"]);
main(["--help"]) ->
  help();
main(Options) ->
  main(Options, [], []).

help() ->
  io:fwrite( "Usage: ~s from-har [--apib] [--header=string] -- "
             "file.har [file.har] ~n"
           , [escript:script_name()]
           ).

%%%_* Internal =================================================================

main(["--apib"|Rest], Options, []) ->
  main(Rest, [{apib, true}|Options], []);

main(["--pretty-print"|Rest], Options, []) ->
  main(Rest, [{pretty_print, true}|Options], []);

main(["--header=" ++ Name|Rest], Options, []) ->
  main(Rest, [{header, string:to_lower(Name)}|Options], []);

main(["--"|HARs], Options, []) ->
  run(Options, HARs).

run(_Options, []) ->
  ok;
run(Options, HARs) ->
  %% Don't use application:ensure_all_started(katt)
  %% nor application:ensure_started(_)
  %% in order to maintain compatibility with R16B01 and lower
  ok = ensure_started(jsx),
  Transactions = hars_to_transactions(HARs, Options),
  Blueprint = #katt_blueprint{ name = <<"APIB generated from HAR">>
                             , transactions = Transactions
                             },
  case proplists:get_value(apib, Options) of
    undefined ->
      io:fwrite("~p", [Blueprint]);
    true ->
      katt_util:blueprint_to_apib(Blueprint)
  end.

hars_to_transactions(HARs, Options) ->
  hars_to_transactions(HARs, Options, [], 1).

hars_to_transactions([], _Options, Transactions, _Step) ->
  Transactions;
hars_to_transactions([HAR|HARs], Options, Transactions0, Step0) ->
  {ok, Bin} = file:read_file(HAR),
  Json = jsx:decode(Bin),
  Log = proplists:get_value(<<"log">>, Json),
  {Transactions, Step} = lists:foldl( fun(Entry, Acc) ->
                                          convert_entry(Entry, Acc, Options)
                                      end
                                    , {[], Step0}
                                    , proplists:get_value(<<"entries">>, Log)
                                    ),
  hars_to_transactions(HARs, Options, Transactions ++ Transactions0, Step).

convert_entry(Entry, {Transactions, Step}, Options) ->
  BinStep = list_to_binary(integer_to_list(Step)),
  Description = <<"Step ",BinStep/binary>>,
  Request = convert_request( proplists:get_value(<<"request">>, Entry)
                           , Options
                           ),
  Response = convert_response( proplists:get_value(<<"response">>, Entry)
                             , Options
                             ),
  Transaction = #katt_transaction{ description = Description
                                 , request = Request
                                 , response = Response
                                 },
  {[Transaction|Transactions], Step + 1}.

convert_request(Request, Options) ->
  Method = binary_to_list(proplists:get_value(<<"method">>, Request)),
  Url = binary_to_list(proplists:get_value(<<"url">>, Request)),
  Headers = convert_headers( proplists:get_value(<<"headers">>, Request)
                           , Options
                           ),
  Body = convert_body( proplists:get_value(<<"content">>, Request)
                     , Headers
                     , Options
                     ),
  #katt_request{ method = Method
               , url = Url
               , headers = Headers
               , body = Body
               }.

convert_response(Response, Options) ->
  Status = proplists:get_value(<<"status">>, Response),
  Headers = convert_headers( proplists:get_value(<<"headers">>, Response)
                           , Options
                           ),
  Body = convert_body( proplists:get_value(<<"content">>, Response)
                     , Headers
                     , Options
                     ),
  #katt_response{ status = Status
                , headers = Headers
                , body = Body
                }.

convert_headers(Headers0, Options) ->
  Headers1 = lists:map( fun convert_header/1
                      , Headers0
                      ),
  case proplists:get_all_values(headers, Options) of
    [] ->
      Headers1;
    LimitedHeaderNames ->
      lists:filter( fun({Name, _Value}) ->
                        lists:member( string:to_lower(Name)
                                    , LimitedHeaderNames
                                    )
                    end
                  , Headers1
                  )
  end.

convert_header(Header) ->
  Name = binary_to_list(proplists:get_value(<<"name">>, Header)),
  Value = binary_to_list(proplists:get_value(<<"value">>, Header)),
  {Name, Value}.

convert_body(Content, Headers, Options) ->
  PrettyPrint = proplists:get_value(pretty_print, Options, false),
  case Content of
    undefined ->
      null;
    Content ->
      Text = proplists:get_value(<<"text">>, Content),
      Body = case proplists:get_value(<<"encoding">>, Content) of
                "base64" ->
                  base64:decode_to_string(Text);
                _ ->
                  Text
              end,
      case PrettyPrint of
        false ->
          Body;
        true ->
          case katt_util:is_json_content_type(Headers) of
            true ->
              jsx:encode(jsx:decode(Body), [space, {indent, 2}]);
            false ->
              Body
          end
      end
  end.

ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.
