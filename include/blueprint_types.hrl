%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc KATT Blueprint Parser types
%%% @copyright 2013 Klarna AB
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type utf8_string() :: unicode:unicode_binary().
-type http_header() :: {string(), string()|integer()}.

-record(katt_request,     { method = "GET"              :: string()
                          , url = "/"                   :: string()
                          , headers = []                :: [http_header()]
                          , body = null                 :: binary() | null
                          }).

-record(katt_response,    { status = 200                :: integer()
                          , headers = []                :: [http_header()]
                          , body = null                 :: binary() | null
                          }).

-record(katt_operation,   { description = null          :: utf8_string()
                          , request = #katt_request{}   :: #katt_request{}
                          , response = #katt_response{} :: #katt_response{}
                          }).

-record(katt_blueprint,   { name = null                 :: utf8_string()
                          , description = null          :: utf8_string()
                          , operations = []             :: [#katt_operation{}]
                          }).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
