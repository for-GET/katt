%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Klarna Api Testing Tool.
%%%
%%% Use for shooting requests against any kind of http interface
%%% (e.g. json, xml) and verifying the response.
%%%
%%% @copyright 2012 Klarna AB
%%% @end
%%%
%%%
%%% Instructions for the input files expected by this tool:
%%%
%%% * Place the files in a dir named after the test case.
%%%
%%% * Naming: XX_request.json,XX_response.json where XX = 01,02,03...
%%%   These files must be json, even if the body is in another format.
%%%
%%% * The bodies (optional) are kept in separate files named
%%%   XX_request_body.json, XX_response_body.json etc.
%%%
%%% * Body files can be in any format (json, xml, html, text..).
%%%   Note that the body is stripped of spaces and newlines before it is sent,
%%%   except inside quotes "..." or xml/html tags <...>
%%%
%%% * Files must be encoded in utf-8
%%%
%%% * Url can be given either as a "url" parameter or as host/port/ssl/path
%%%   Default host is 127.0.0.1 in run/1 but could be passed in run/2
%%%   as well. Including "host" in the input file will override the default.
%%%
%%% * Tags with special meaning in response files. Try not to overuse these:
%%%    ">>_"   Match anything (i.e. no real validation, only check existence)
%%%    ">>01"  Store value of the whole string. 01 can be any two characters
%%%            (must be unique within testcase)
%%%    "ERROR" as a status code means we are expecting to get an error back
%%%            instead of a proper http code (Probably not used very often)
%%%
%%% * Tags with special meaning in all files:
%%%    "<<01"    Recall stored value. 01 can be any two (alphanumeric)
%%%              characters
%%%    ">>key<<" Substitute with key with parameter passed in parameter list in
%%%              run/3
%%%
%%% * Any difference between expected and actual responses will cause a failure.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%_* Module declaration ===============================================
-module(katt).

%%%_* Exports ==========================================================
%% API
-export([ run/1
        , run/2
        , run/3
        , get_requests_with_bodies/1
        ]).

%% Internal exports
-export([ run_test/4
        ]).

%%%_* Defines ==========================================================
-define(BODY_SUFFIX,      "_body").
-define(KEY_PREFIX,       "api_test_").
-define(EXTRACT_TAG,      "<<").
-define(STORE_TAG,        ">>").
-define(MATCH_ANY,        ">>_").
-define(SUB_BEGIN_TAG,    ">>").
-define(SUB_END_TAG,      "<<").
-define(RESPONSE_ERROR,   "ERROR").
-define(REQUEST_TIMEOUT,  20000).
-define(TESTCASE_TIMEOUT, 120000).

%%%_* Records ==========================================================
-record(request, { url      :: string()
                 , host     :: string()
                 , port     :: integer()
                 , ssl      :: boolean()
                 , path     :: string()
                 , method   :: atom()
                 , headers  :: [{string(), string() | integer()}]
                 , body     :: [{string(), any()}]
                 , raw_body :: binary()
                 }).

-record(response, { status_code :: integer()
                  , headers     :: [{string(), string() | integer()}]
                  , body        :: [{string(), any()}]
                  }).

%%%_* API ==============================================================
-spec ?MODULE:run(string()) ->
                     [{string(), pass | {fail, {atom(), any()}}}
                      | {error, any()}].
%% @doc Run test scenario. Argument is the full path to the testcase dir.
%% @end
run(TestcaseDir) -> run(TestcaseDir, "127.0.0.1").

-spec ?MODULE:run(string(), string()) ->
                     [{string(), pass | {fail, {atom(), any()}}}
                      | {error, any()}].
%% @doc Run test scenario. Argument is the full path to the testcase dir.
%% @end
run(TestCaseDir, DefaultHost) -> run(TestCaseDir, DefaultHost, []).

-spec ?MODULE:run(string(), string(), list()) ->
                     [{string(), pass | {fail, {atom(), any()}}}
                      | {error, any()}].
%% @doc Run test scenario. Argument is the full path to the testcase dir.
%% Last argument is a key-value list of substitute parameters.
%% @end
run(TestcaseDir, DefaultHost, Params) ->
  spawn_link(?MODULE, run_test, [self(), TestcaseDir, DefaultHost, Params]),
  receive {done, Result}    -> Result
  after   ?TESTCASE_TIMEOUT -> {error, testcase_timeout}
  end.

-spec ?MODULE:get_requests_with_bodies(string()) -> [{string(), binary()}
                                                     | {error, any()}].
%% @doc Get a list of all requests in a testcase dir together with their bodies
%% @end
get_requests_with_bodies(TestcaseDir) ->
  RequestFiles = get_files(TestcaseDir, "request"),
  [{F, strip(read_body(F, []))} || F <- RequestFiles].

%%%_* Internal export --------------------------------------------------
run_test(Caller, TestcaseDir, DefaultHost, Params) ->
  Result = run_scenario(get_scenario(TestcaseDir), DefaultHost, Params),
  Caller ! {done, Result}.

%%%_* Internal =========================================================
%% Look through Dir to find all request/response file pairs
%% TODO: handle missing files gracefully
get_scenario(Dir0) ->
  Dir = ensure_trailing_slash(Dir0),
  RequestFiles  = get_files(Dir, "request"),
  ResponseFiles = get_files(Dir, "response"),
  lists:zip(RequestFiles, ResponseFiles).

run_scenario(S, DefaultHost, Params) ->
  run_scenario(S, DefaultHost, Params, []).

run_scenario([], _DefaultHost, _Params, Acc)                    ->
  lists:reverse(Acc);
run_scenario([{ReqFile, RespFile}|T], DefaultHost, Params, Acc) ->
  Request        = read_request(ReqFile, DefaultHost, Params),
  ExpResponse    = read_response(RespFile, Params),
  ActualResponse = make_request(Request),
  case Result = validate_response(ExpResponse, ActualResponse) of
    pass -> ok;
    _    -> print_debug(ReqFile, Request, ExpResponse, ActualResponse)
  end,
  run_scenario(T, DefaultHost, Params, [{ReqFile, Result} | Acc]).

print_debug(ReqFile, Request, ExpResponse, ActualResponse) ->
  ct:pal("~p:~n~p~n~n"
         "Expected response:~n~p~n~n"
         "Actual response:~n~p~n",
         [ReqFile, Request, ExpResponse, ActualResponse]).

read_request(RequestFile, DefaultHost, Params) ->
  Data    = parse_file(RequestFile, Params),
  Headers = lk("headers", Data),
  RawBody = read_body(RequestFile, Params),
  #request{ url      = lk("url", Data)
          , host     = lk("host", Data, DefaultHost)
          , port     = lk("port", Data)
          , ssl      = lk("ssl", Data)
          , path     = lk("path", Data)
          , method   = lk("method", Data)
          , headers  = Headers
          , body     = maybe_parse_body(Headers, RawBody)
          , raw_body = RawBody
          }.

read_response(ResponseFile, Params) ->
  Data    = parse_file(ResponseFile, Params),
  Headers = lk("headers", Data, []),
  #response{ status_code = lk("status_code", Data)
           , headers     = Headers
           , body        =
               maybe_parse_body(Headers, read_body(ResponseFile, Params))
           }.

%% Get body from a file if the file exists
read_body(File, Params)    ->
  case file:read_file(get_body_file(all_body_files(File))) of
    {ok, Data}      -> substitute(Data, Params);
    {error, enoent} -> ""
  end.

all_body_files(File) ->
  Rootname = filename:rootname(File),
  lists:filter(fun is_not_tempfile/1,
               filelib:wildcard(Rootname ++ ?BODY_SUFFIX ++ "*")).

get_body_file([File])  -> File;
get_body_file([])      -> "";
get_body_file([_,_|_]) -> error(too_many_body_files).

%% Only parse body if we find "json" in Content-Type
maybe_parse_body(Headers, Body) ->
  ContentType = lk("Content-Type", Headers, ""),
  case string:str(ContentType, "json") of
    0 -> from_utf8(Body);
    _ -> parse_json(Body)
  end.

parse_file(File, Params) ->
  {ok, Contents} = file:read_file(File),
  parse_json(substitute(Contents, Params)).

parse_json([])  -> [];
parse_json(Bin) -> to_proplist(mochijson2:decode(Bin)).

%% Transform nested json structs to proplists
%% Values can be binary before, in which case they are utf8 decoded
to_proplist(L = [{struct, _}|_])         ->
  [to_proplist(S) || S <- L];
to_proplist({struct, L}) when is_list(L) ->
  [{from_utf8(K), to_proplist(V)} || {K, V} <- L];
to_proplist({struct, _})                 ->
  error(should_not_happen); % Would be an invalid json struct
to_proplist(List) when is_list(List)     ->
  lists:sort([to_proplist(L) || L <- List]);
to_proplist(Str) when is_binary(Str)     ->
  replace_variables(from_utf8(Str));
to_proplist(Value)                       ->
  Value.

substitute(Str, [])         -> Str;
substitute(Str, [{K, V}|T]) ->
  substitute(re:replace(Str, ?SUB_BEGIN_TAG ++ to_list(K) ++ ?SUB_END_TAG,
                        to_list(V), [{return, binary}, global]), T).

%% Replace "extract" tags with actual values
replace_variables(Str) when is_list(Str) -> replace_variables(Str, []);
replace_variables(Val)                   -> Val.

replace_variables(?EXTRACT_TAG ++ [N1,N2|T], Acc) ->
  Key = ?KEY_PREFIX  ++ [N1,N2],
  Str = case get(Key) of
          undefined -> "";
          Value     -> Value
        end,
  replace_variables(T, lists:reverse(Str) ++ Acc);
replace_variables([H|T], Acc)                     ->
  replace_variables(T, [H|Acc]);
replace_variables([], Acc)                        ->
  lists:reverse(Acc).

make_request(R = #request{}) ->
  case http_request(R) of
    {ok, {{StatusCode, _}, Headers, RawBody}} ->
      #response{ status_code = StatusCode
               , headers     = Headers
               , body        = maybe_parse_body(Headers, RawBody)
               };
    {error, timeout}   -> {error, http_timeout};
    Error = {error, _} -> Error
  end.

http_request(R = #request{url=undefined}) ->
  lhttpc:request( R#request.host
                , R#request.port
                , R#request.ssl
                , R#request.path
                , R#request.method
                , R#request.headers
                , strip(R#request.raw_body)
                , ?REQUEST_TIMEOUT
                , []
                );
http_request(R = #request{url=[_|_]})     ->
  lhttpc:request( R#request.url
                , R#request.method
                , R#request.headers
                , strip(R#request.raw_body)
                , ?REQUEST_TIMEOUT
                , []
                );
http_request(#request{url=Url})           ->
  {error, {bad_request_url, Url}};
http_request(R)                           ->
  {error, {bad_request, R}}.

validate_response(#response{status_code=?RESPONSE_ERROR}, {error, _})  ->
  pass;
validate_response(#response{status_code=?RESPONSE_ERROR}, #response{}) ->
  {fail, expected_error};
validate_response(Exp = #response{}, Actual = #response{})             ->
  Result = do_validate_response(response_to_proplist(Exp),
                                response_to_proplist(Actual)),
  case lists:filter(fun(X) -> X =/= pass end, lists:flatten(Result)) of
    []       -> pass;
    Failures -> Failures
  end;
validate_response(Exp, #response{})                                    ->
  {fail, Exp};
validate_response(#response{}, Actual)                                 ->
  {fail, Actual}.

do_validate_response(Exp, Actual) -> do_validate_response("", Exp, Actual).

%% Proplist: we need to go deeper
do_validate_response(_, Exp = [{_,_}|_], Actual = [{_,_}|_])  ->
  Keys = lists:usort([K || {K, _} <- lists:merge(Actual, Exp)]),
  [do_validate_response(K, lk(K, Exp), lk(K, Actual)) || K <- Keys];
%% Unwrap lists of proplists by changing them to nested proplists
do_validate_response(K, E = [[{_,_}|_]|_], A = [[{_,_}|_]|_]) ->
  [do_validate_response(K, Exp, Actual) || {Exp, Actual} <- lists:zip(E, A)];
do_validate_response(K, E = [[_|_]|_], A = [[_|_]|_])         ->
  do_validate_response(K, enumerate(E, K), enumerate(A, K));
%% Here comes the actual validation
do_validate_response(Key, Exp, undefined)                     ->
  {missing_value, {Key, Exp}};
do_validate_response(Key, undefined, Actual)                  ->
  {unexpected_value, {Key, Actual}};
do_validate_response(Key, Str = ?STORE_TAG ++ _, [])          ->
  {empty_value, {Key, Str}};
do_validate_response(_Key, ?MATCH_ANY ++ _, _)                ->
  pass;
do_validate_response(_Key, ?STORE_TAG ++ [N1,N2|_], Actual)   ->
  PKey = ?KEY_PREFIX ++ [N1,N2],
  put(PKey, Actual),
  pass;
do_validate_response(Key, Exp, Actual)                        ->
  E = to_lower(replace_variables(Exp)),
  A = to_lower(replace_variables(Actual)),
  compare(Key, E, A).

compare(_Key, Exp, Exp)   -> pass;
compare(Key, Exp, Actual) -> {not_equal, {Key, Exp, Actual}}.

%% Transform simple list to proplist with keys named Name1, Name2 etc.
enumerate(L, Name) ->
  Keys = [to_list(Name) ++ integer_to_list(N) || N <- lists:seq(1, length(L))],
  lists:zip(Keys, L).

%% Transform response record to proplist for validation.
response_to_proplist(Resp) ->
  Keys   = record_info(fields, response),
  Values = tl(tuple_to_list(Resp)),
  lists:zip(Keys, Values).

lk(K, V) -> proplists:get_value(K, V).

lk(K, V, Default) -> proplists:get_value(K, V, Default).

%%%_* File handling ----------------------------------------------------
get_files(Dir, Type) ->
  {ok, FilesRaw} = file:list_dir(Dir),
  Files = lists:sort(FilesRaw),
  [filename:join([Dir, F]) || F <- select_files(Type, Files)].

select_files(String, FileList) ->
  lists:filter(fun(F) ->
                   string:str(F, String) =/= 0 andalso
                   is_json_file(F) andalso
                   is_not_tempfile(F) andalso
                   is_not_body_file(F)
               end, FileList).

is_json_file(F) -> ".json" =:= filename:extension(F).

is_not_tempfile(F) -> not lists:suffix("~", F).

is_not_body_file(F) -> not lists:suffix(?BODY_SUFFIX ++ ".json", F).

ensure_trailing_slash(Dir) -> do_ensure_trailing_slash(lists:reverse(Dir)).

do_ensure_trailing_slash(Dir = [$/|_]) -> lists:reverse(Dir);
do_ensure_trailing_slash(Dir)          -> lists:reverse([$/|Dir]).

%%%_* String utils -----------------------------------------------------
to_lower(S) when is_list(S) -> string:to_lower(S);
to_lower(V)                 -> V.

%% Strip spaces and newlines, but not inside quotes ("...") or xml tags (<...>)
%% Output is always a binary string
strip(Input) -> to_utf8(strip(from_utf8(Input), [])).

strip([], Acc)        -> lists:reverse(Acc);
strip(" " ++ T, Acc)  -> strip(T, Acc);
strip("\n" ++ T, Acc) -> strip(T, Acc);
strip([H|T], Acc)     ->
  {NewT, NewAcc} = get_new_values(H, T, Acc),
  strip(NewT, NewAcc).

get_new_values($", Str, Acc) -> do_get_new_values({$", $"}, Str, Acc);
get_new_values($<, Str, Acc) -> do_get_new_values({$<, $>}, Str, Acc);
get_new_values(C,  Str, Acc) -> {Str, [C|Acc]}.

do_get_new_values({C1, C2}, Str, Acc) ->
  Position      = string:str(Str, [C2]), % Search for the closing char
  {Quote, Tail} = lists:split(Position, Str),
  {Tail, lists:reverse([C1|Quote]) ++ Acc}.

%% Transform (possibly utf8 encoded) binary to list, ignore everything else
from_utf8(X) when is_binary(X) -> unicode:characters_to_list(X, utf8);
from_utf8(X)                   -> X.

%% Transform list to utf8 encoded binary, ignore everything else
to_utf8(X) when is_list(X) -> unicode:characters_to_binary(X, utf8);
to_utf8(X)                 -> X.

to_list(X) when is_list(X)    -> X;
to_list(X) when is_atom(X)    -> atom_to_list(X);
to_list(X) when is_tuple(X)   -> tuple_to_list(X);
to_list(X) when is_binary(X)  -> binary_to_list(X);
to_list(X) when is_integer(X) -> integer_to_list(X).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
