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
%%% @doc Klarna API Testing Tool Utils
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
%% @private
-module(katt_util).

%%%_* Exports ==================================================================
%% API
-export([ merge_proplists/2
        , to_list/1
        , from_utf8/1
        , to_utf8/1
        , to_lower/1
        , escape_regex/1
        , external_http_request/6
        , erl_to_list/1
        , os_cmd/2
        , blueprint_to_apib/1
        , is_json_content_type/1
        , enumerate/1
        ]).

%%%_* Includes =================================================================
-include("katt.hrl").

%%%_* API ======================================================================

%% Merge two proplists. If a property exists in both List1 and List2, then the
%% value from List2 is used.
merge_proplists(List1, List2) ->
  orddict:merge( fun(_K, _V1, V2) -> V2 end
               , orddict:from_list(List1)
               , orddict:from_list(List2)
               ).

to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_integer(X) -> integer_to_list(X);
to_list(X) when is_float(X) -> io_lib:format("~p", [X]);
to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_list(X) -> X.

%% Transform (possibly utf8 encoded) binary to list, ignore everything else.
from_utf8(X) when is_binary(X) ->
  case unicode:characters_to_list(X, utf8) of
    R when is_list(R) -> R;
    {error, _, _} -> binary_to_list(X);
    {incomplete, _, _} -> binary_to_list(X)
  end.

%% Transform list to utf8 encoded binary, ignore everything else
to_utf8(X) when is_list(X) ->
  unicode:characters_to_binary(X, utf8).

to_lower(X) when is_list(X) ->
  string:to_lower(X).

escape_regex(Other) when not is_list(Other) andalso not is_binary(Other) ->
  to_list(Other);
escape_regex([H|_]=List) when is_tuple(H) ->
  List;
escape_regex(Str0) ->
  Str = to_list(Str0),
  re:replace( Str
            , "[\\-\\[\\]\\/\\{\\}\\(\\)\\*\\+\\?\\.\\,\\\\\^\\$\\|\\#\\s\\&]"
            , "\\\\&"
            , [{return, list}, global]
            ).

-ifdef(BARE_MODE).
external_http_request(_Url, _Method, _Hdrs, _Body, _Timeout, _Options) ->
  {error, katt_bare_mode}.
-else.
external_http_request(Url, Method, Hdrs, Body, Timeout, Options0) ->
  BUrl = list_to_binary(Url),
  BHdrs = lists:map( fun({Name, Value})->
                         {list_to_binary(Name), list_to_binary(Value)}
                     end
                   , Hdrs
                   ),
  DefaultOpts = [ {recv_timeout, Timeout}
                , {insecure, true}
                ],
  Options = merge_proplists(DefaultOpts, Options0),
  case hackney:request(Method, BUrl, BHdrs, Body, Options) of
    OK when element(1, OK) =:= ok ->
      %% lhttpc was the predecessor of hackney
      %% and we're maintaining a backwards compatible return value
      {Status, BResHdrs, ResBody} = case OK of
                                      {ok, Status0, BResHdrs0, Client} ->
                                        {ok, ResBody0} = hackney:body(Client),
                                        {Status0, BResHdrs0, ResBody0};
                                      {ok, Status0, BResHdrs0} ->
                                        {Status0, BResHdrs0, <<>>}
                                    end,
      ResHdrs0 = lists:map( fun({Name, Value})->
                                {binary_to_list(Name), binary_to_list(Value)}
                            end
                          , BResHdrs
                          ),
      ResHdrs = lists:reverse(ResHdrs0),
      {ok, {{Status, ""}, ResHdrs, ResBody}};
    Error ->
      Error
  end.
-endif.

erl_to_list(Term) ->
  io_lib:format("~p", [Term]).

os_cmd(Cmd, Env) ->
  Opt = [ stream
        , exit_status
        , use_stdio
        , stderr_to_stdout
        , in
        , eof
        , hide
        , {env, Env}
        ],
  Port = open_port({spawn, Cmd}, Opt),
  os_cmd_result(Port, []).

blueprint_to_apib(Blueprint) ->
  io:fwrite( "--- ~s ---~n"
             "~n"
             "---~n"
             "---~n"
             "~n"
           , [Blueprint#katt_blueprint.name]
           ),
  lists:foreach( fun(#katt_transaction{ description = Description
                                      , request = Req
                                      , response = Res
                                      }) ->
                     io:fwrite("# ~s~n~n", [Description]),
                     io:fwrite( "~s ~s~n"
                                "~s"
                                "~s"
                              , [ Req#katt_request.method
                                , Req#katt_request.url
                                , headers_to_apib( Req#katt_request.headers
                                                 , "> "
                                                 )
                                , body_to_apib(Req#katt_request.body)
                                ]),
                     io:fwrite( "< ~B~n"
                                "~s"
                                "~s"
                              , [ Res#katt_response.status
                                , headers_to_apib( Res#katt_response.headers
                                                 , "< "
                                                 )
                                , body_to_apib(Res#katt_response.body)
                                ]),
                     io:fwrite("~n", [])
                 end
               , Blueprint#katt_blueprint.transactions).

is_json_content_type(Hdrs0) ->
  Hdrs = [{to_lower(K), V} || {K, V} <- Hdrs0],
  ContentType = proplists:get_value("content-type", Hdrs, ""),
  case re:run(ContentType, "^application/(.+\\+)?json") of
    nomatch -> false;
    {match, _} -> true
  end.

%%%_* Internal =================================================================

%% Transform simple list to proplist with keys named 0, 1 etc.
enumerate(L) ->
  lists:zip([ integer_to_list(N)
              || N <- lists:seq(0, length(L) - 1)
            ], L).

os_cmd_result(Port, Output) ->
  receive
    {Port, {data, NewOutput}} ->
      os_cmd_result(Port, [Output|NewOutput]);
    {Port, eof} ->
      port_close(Port),
      receive
        {Port, {exit_status, ExitStatus}} ->
          {ExitStatus, lists:flatten(Output)}
      end
  end.

headers_to_apib(Headers, Prefix) ->
  headers_to_apib(Headers, Prefix, []).

headers_to_apib([], _Prefix, []) ->
  "";
headers_to_apib([], _Prefix, Acc) ->
  string:join(lists:reverse(Acc), "\n") ++ "\n";
headers_to_apib([{Name, Value}|Headers], Prefix, Acc) ->
  headers_to_apib(Headers, Prefix, [Prefix ++ Name ++ ": " ++ Value|Acc]).

body_to_apib(null) ->
  <<"">>;
body_to_apib(Body) ->
  <<"<<<\n", Body/binary, "\n>>>\n">>.
