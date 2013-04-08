%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc KATT Blueprint parser
%%%
%%% This is an interface module which provides access to the main functionality
%%% of katt_blueprint_parser, i.e. parsing a KATT Blueprint string or file.
%%%
%%% @copyright 2013 Klarna AB
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(katt_blueprint_parse).

%%%_* Exports ==========================================================
%% API
-export([ string/1
        , file/1
        ]).
%% Types
-export_type([ katt_blueprint/0
             ]).

%%%_* Includes =========================================================

-include("blueprint_types.hrl").

%%%_* Types ============================================================

-type katt_blueprint() :: #katt_blueprint{}.

%%%_* API ==============================================================

-spec string(string()) -> {ok, katt_blueprint()}.
%% @doc Parse a KATT Blueprint string.
string(Str) ->
  #katt_blueprint{} = BP = katt_blueprint:parse(Str),
  {ok, BP}.

-spec file(file:name()) -> {ok, katt_blueprint()}.
%% @doc Parse a KATT Blueprint file.
file(File) ->
  #katt_blueprint{} = BP = katt_blueprint:file(File),
  {ok, BP}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
