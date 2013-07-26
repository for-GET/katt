%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright 2013 Klarna AB
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
%%% @copyright 2013 Klarna AB
%%%
%%% @doc Klarna API Testing Tool Utils
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
%% @private
-module(katt_util).

%%%_* Exports ==========================================================
%% API
-export([ merge_proplists/2
        , to_list/1
        , from_utf8/1
        , to_utf8/1
        , to_lower/1
        , escape_regex/1
        ]).

%%%_* API ==============================================================

%% Merge two proplists. If a property exists in both List1 and List2, then the
%% value from List2 is used.
merge_proplists(List1, List2) ->
  orddict:merge( fun(_K, _V1, V2) -> V2 end
               , orddict:from_list(List1)
               , orddict:from_list(List2)
               ).

to_list(X) when is_list(X)    -> X;
to_list(X) when is_atom(X)    -> atom_to_list(X);
to_list(X) when is_binary(X)  -> binary_to_list(X);
to_list(X) when is_integer(X) -> integer_to_list(X).

%% Transform (possibly utf8 encoded) binary to list, ignore everything else.
from_utf8(X) when is_binary(X) ->
  case unicode:characters_to_list(X, utf8) of
    R when is_list(R)  -> R;
    {error, _, _}      -> binary_to_list(X);
    {incomplete, _, _} -> binary_to_list(X)
  end.

%% Transform list to utf8 encoded binary, ignore everything else
to_utf8(X) when is_list(X) -> unicode:characters_to_binary(X, utf8).

to_lower(S) when is_list(S) -> string:to_lower(S).

escape_regex(Bin0) ->
  Bin = to_list(Bin0),
  Result0 = re:replace(
    Bin,
    "[\\-\\[\\]\\/\\{\\}\\(\\)\\*\\+\\?\\.\\,\\\\\^\\$\\|\\#\\s\\&]",
    "\\\\&",
    [global]),
  to_list(Result0).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
