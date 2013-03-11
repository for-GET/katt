%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc KATT Blueprint parser
%%%
%%% This is an interface module which provides access to the main functionality
%%% of katt_blueprint_parser, i.e. parsing a KATT Blueprint string or file.
%%% @copyright 2013 Klarna AB
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(katt_blueprint_parser).

%%%_* Exports ==========================================================
%% API
-export([ parse/1
        , parse_file/1
        ]).
%% Types
-export_type([ katt_blueprint/0
             ]).

%%%_* Includes =========================================================

-include("blueprint_types.hrl").


%%%_* API ==============================================================

-type katt_blueprint() :: #api_blueprint{}.

-spec parse(string()) -> {ok, katt_blueprint()} | {error, any()}.
%% @doc Parse a KATT Blueprint string.
parse(Str) ->
  blueprint_or_error(api_blueprint:parse(Str)).

-spec parse_file(file:name()) -> {ok, katt_blueprint()} | {error, any()}.
%% @doc Parse a KATT Blueprint file.
parse_file(File) ->
  try
    api_blueprint:file(File)
  of
    BP=#api_blueprint{} -> blueprint_or_error(BP)
  catch
    error:Error -> blueprint_or_error(Error)
  end.

%%%_* Internal functions ===============================================

blueprint_or_error(Blueprint=#api_blueprint{})  -> {ok, Blueprint};
blueprint_or_error({_, Reason})                 -> {error, Reason}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
