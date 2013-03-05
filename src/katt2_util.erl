%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Klarna API Testing Tool v2 Utils
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
%% @private
-module(katt2_util).

%%%_* Exports ==========================================================
%% API
-export([ to_list/1
        , to_integer/1
        , to_lower/1
        , from_utf8/1
        , to_utf8/1
        , strip/1
        ]).

%%%_* API ==============================================================
to_list(X) when is_list(X)    -> X;
to_list(X) when is_atom(X)    -> atom_to_list(X);
to_list(X) when is_binary(X)  -> binary_to_list(X);
to_list(X) when is_integer(X) -> integer_to_list(X).

to_integer(X) when is_integer(X) -> X;
to_integer(X) when is_list(X)    -> list_to_integer(X);
to_integer(X) when is_binary(X)  -> to_integer(binary_to_list(X)).

to_lower(X) when is_list(X) -> string:to_lower(X);
to_lower(X)                 -> X.

%% Transform (possibly utf8 encoded) binary to list, ignore everything else
from_utf8(X) when is_binary(X) ->
  case unicode:characters_to_list(X, utf8) of
    R when is_list(R)  -> R;
    {error, _, _}      -> binary_to_list(X);
    {incomplete, _, _} -> binary_to_list(X)
  end;
from_utf8(X)                   -> X.

%% Transform list to utf8 encoded binary, ignore everything else
to_utf8(X) when is_list(X) -> unicode:characters_to_binary(X, utf8);
to_utf8(X)                 -> X.

%% Strip spaces and newlines, but not inside quotes ("...") or xml tags (<...>)
%% Output is always a binary string
strip(Input) -> to_utf8(strip(from_utf8(Input), [])).

%%%_* Internal =========================================================
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

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
