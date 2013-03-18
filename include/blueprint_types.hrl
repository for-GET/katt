%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc KATT Blueprint Parser types
%%% @copyright 2013 Klarna AB
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type utf8_string() :: unicode:unicode_binary().
-type http_header() :: {string(), string()|integer()}.

-record(request,          { method = "GET"         :: string()
                          , url = <<"/">>          :: utf8_string()
                          , headers = []           :: [http_header()]
                          , body                   :: utf8_string()
                          }).

-record(response,         { status = 200           :: integer()
                          , headers = []           :: [http_header()]
                          , body                   :: utf8_string()
                          }).

-record(operation,        { description            :: utf8_string()
                          , request = #request{}   :: #request{}
                          , response = #response{} :: #response{}
                          }).

-record(api_blueprint,    { name                   :: utf8_string()
                          , description            :: utf8_string()
                          , operations = []        :: [#operation{}]
                          }).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
