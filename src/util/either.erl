-module(either).

-export([
    apply/2,
    chain/2,
    conjunction/2,
    disjunction/2,
    get_either/1,
    get_left/1,
    get_right/1,
    is_left/1,
    is_right/1,
    make_left/1,
    make_right/1,
    unbox/1
]).

-export_type([either/2]).

-opaque either(L, R) :: {left, L} | {right, R}.

-spec apply(fun((R1) -> R2), either(L, R1)) -> either(L, R2).
apply(_, Left={left, _}) ->
    Left;
apply(Fun, {right, Right}) ->
    make_right(Fun(Right)).

-spec chain(fun((R1) -> either(L2, R2)), either(any(), R1)) -> either(L2, R2).
chain(_, Left={left, _}) ->
    Left;
chain(Fun, {right, Right}) ->
    Fun(Right).

-spec conjunction(either(L, R), either(L, R)) -> either(L, R).
conjunction(A, B) ->
    case is_right(A) of
        true ->
            B;
        false ->
            A
    end.

-spec disjunction(either(L, R), either(L, R)) -> either(L, R).
disjunction(A, B) ->
    case is_left(A) of
        true ->
            B;
        false ->
            A
    end.


-spec get_either(either(L, R)) -> {left, L} | {right, R}.
get_either(Either) ->
    Either.

-spec get_left(either(L, any())) -> L | no_return().
get_left({left, Val}) ->
    Val;
get_left({right, Val}) ->
    throw(Val).

-spec get_right(either(any(), R)) -> R | no_return().
get_right({right, Val}) ->
    Val;
get_right({left, Val}) ->
    throw(Val).

-spec is_left(either(any(), any())) -> boolean().
is_left({left, _}) ->
    true;
is_left(_) ->
    false.

-spec is_right(either(any(), any())) -> boolean().
is_right({right, _}) ->
    true;
is_right(_) ->
    false.

-spec make_left(L) -> either(L, any()).
make_left(Val) ->
    {left, Val}.

-spec make_right(R) -> either(any(), R).
make_right(Val) ->
    {right, Val}.

-spec unbox(either(L, R)) -> L | R.
unbox({right, R}) ->
    R;
unbox({left, L}) ->
    L.
