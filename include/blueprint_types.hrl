%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc KATT Blueprint Parser types
%%% @copyright 2013 Klarna AB
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type utf8_string() :: unicode:unicode_binary().
-type http_header() :: {string(), string()}.

-record(katt_request,     { method = "GET"              :: string()
                          , url = "/"                   :: string()
                          , headers = []                :: [http_header()]
                          , body = null                 :: binary() | null
                          }).

-record(katt_response,    { status = 200                :: integer()
                          , headers = []                :: [http_header()]
                          , body = null                 :: binary() | null
                          , parsed_body = null          :: any()
                          }).

-record(katt_transaction, { description = null          :: utf8_string()
                          , request = #katt_request{}   :: #katt_request{}
                          , response = #katt_response{} :: #katt_response{}
                          }).

-record(katt_blueprint,   { name = null                 :: utf8_string()
                          , description = null          :: utf8_string()
                          , transactions = []           :: [#katt_transaction{}]
                          }).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
