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
%%% @doc KATT Blueprint parser
%%%
%%% This is an interface module which provides access to the main functionality
%%% of katt_blueprint_parser, i.e. parsing a KATT Blueprint string or file.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(katt_blueprint_parse).

%%%_* Exports ==================================================================
%% API
-export([ string/1
        , file/1
        ]).
%% Types
-export_type([ katt_blueprint/0
             ]).

%%%_* Includes =================================================================
-include("blueprint_types.hrl").

%%%_* Types ====================================================================
-type katt_blueprint() :: #katt_blueprint{}.

%%%_* API ======================================================================

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

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
