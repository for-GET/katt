%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Klarna API Testing Tool v2
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(request, { host     :: string()
                 , port     :: integer()
                 , ssl      :: boolean()
                 , path     :: string()
                 , method   :: atom()
                 , headers  :: [{string(), string()|integer()}]
                 , body     :: [{string(), any()}]
                 , raw_body :: binary()
                 }).

-record(response, { status_code = 200 :: integer()
                  , headers           :: [{string(), string()|integer()}]
                  , body              :: [{string(), any()}]
                  , raw_body          :: binary()
                  }).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
