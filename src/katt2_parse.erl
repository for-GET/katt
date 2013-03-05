%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Klarna API Testing Tool v2 Parse
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
%% @private
-module(katt2_parse).

%%%_* Exports ==========================================================
%% API
-export([ file/1
        ]).

%%%_* Import ===========================================================
-import(katt2_util, [ to_list/1
                    ]).

%%%_* Inclues ==========================================================
-include("katt2.hrl").

%%%_* API ==============================================================
file(Name) ->
  {ok, Binary} = file:read_file(Name),
  {ok, Lines}  = parse(Binary),
  parse_lines(Lines).

%%%_* Internal =========================================================
parse(Binary) when is_binary(Binary) ->
  parse(Binary, [], []).

parse(<<>>, [], Lines) ->
  {ok, lists:reverse(Lines)};
parse(<<$\r:8, $\n:8, Rest/binary>>, Buffer, Lines) ->
  parse(Rest, [], [compile(Buffer)|Lines]);
parse(<<$\r:8, Rest/binary>>, Buffer, Lines) ->
  parse(Rest, [], [compile(Buffer)|Lines]);
parse(<<$\n:8, Rest/binary>>, Buffer, Lines) ->
  parse(Rest, [], [compile(Buffer)|Lines]);
parse(<<B:1/binary, Rest/binary>>, Buffer, Lines) ->
  parse(Rest, [B|Buffer], Lines).

compile(Buffer) -> list_to_binary(lists:reverse(Buffer)).

parse_lines(Lines) when is_list(Lines) ->
  parse_lines(Lines, []).

parse_lines([], Sections)                              ->
  {ok, lists:reverse(Sections)};
parse_lines([<<"GET ", Path/binary>>|T0], Sections)    ->
  {Section, T} = parse_section(T0, get, Path),
  parse_lines(T, [Section|Sections]);
parse_lines([<<"HEAD ", Path/binary>>|T0], Sections)   ->
  {Section, T} = parse_section(T0, head, Path),
  parse_lines(T, [Section|Sections]);
parse_lines([<<"POST ", Path/binary>>|T0], Sections)   ->
  {Section, T} = parse_section(T0, post, Path),
  parse_lines(T, [Section|Sections]);
parse_lines([<<"PUT ", Path/binary>>|T0], Sections)    ->
  {Section, T} = parse_section(T0, put, Path),
  parse_lines(T, [Section|Sections]);
parse_lines([<<"DELETE ", Path/binary>>|T0], Sections) ->
  {Section, T} = parse_section(T0, delete, Path),
  parse_lines(T, [Section|Sections]);
parse_lines([<<"PATCH ", Path/binary>>|T0], Sections)  ->
  {Section, T} = parse_section(T0, patch, Path),
  parse_lines(T, [Section|Sections]);
parse_lines([_Line|T], Sections)                       ->
  parse_lines(T, Sections).

parse_section(T0, Method, Path) ->
  {Req, T1} = parse_request(T0, #request{path=to_list(Path), method=Method}),
  {Rsp, T}  = parse_response(T1, #response{}),
  {{Req, Rsp}, T}.

parse_request([<<"< ", _/binary>>|_] = T, #request{raw_body=Body} = Req) ->
  {Req#request{raw_body=compile(Body)}, T};
parse_request([<<"> ", Hdr/binary>>|T], #request{headers=Hdrs} = Req)    ->
  parse_request(T, Req#request{headers=[parse_header(Hdr)|Hdrs]});
parse_request([Payload|T], #request{raw_body=Bdy} = Req)                 ->
  parse_request(T, Req#request{raw_body=[Payload|Bdy]}).

parse_response([<<"< ", _/binary>>|_] = T, #response{raw_body=Body} = Rsp) ->
  {Rsp#response{raw_body = compile(Body)}, T};
parse_response([<<"< ", StatusCode0:24>>|T], Rsp)                          ->
  StatusCode = list_to_integer(binary_to_list(StatusCode0)),
  parse_response(T, Rsp#response{status_code=StatusCode});
parse_response([<<"< ", Hdr/binary>>|T], #response{headers=Hdrs} = Rsp)    ->
  parse_response(T, Rsp#response{headers=[parse_header(Hdr)|Hdrs]});
parse_response([Payload|T], #response{raw_body=Body} = Rsp)                ->
  parse_response(T, Rsp#response{raw_body=[Payload|Body]}).

parse_header(Hdr) ->
  to_list(
    binary:split(
      binary:replace(Hdr, <<" ">>, <<>>, [global]), <<":">>, [])).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
