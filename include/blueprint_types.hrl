%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc KATT Blueprint Parser types
%%% @copyright 2013 Klarna AB
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type utf8_string() :: unicode:unicode_binary().
-type http_header() :: {string(), string()|integer()}.

-record(request,          { method = "GET"         :: string()
                          , url = "/"              :: string()
                          , headers = []           :: [http_header()]
                          , body = null            :: binary()
                          }).

-record(response,         { status = 200           :: integer()
                          , headers = []           :: [http_header()]
                          , body = null            :: binary()
                          }).

-record(operation,        { description = null     :: utf8_string()
                          , request = #request{}   :: #request{}
                          , response = #response{} :: #response{}
                          }).

-record(api_blueprint,    { name = null            :: utf8_string()
                          , description = null     :: utf8_string()
                          , operations = []        :: [#operation{}]
                          }).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
