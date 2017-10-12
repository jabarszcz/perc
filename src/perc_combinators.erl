%% Ad-hoc (quick and dirty) Parser combinators
-module(perc_combinators).

%% API exports

-export([
    alt/1,
    alt_/2,
    list_permissive/4,
    list_permissive_/5,
    list_strict/4,
    list_strict_/5,
    many/1,
    many_/2,
    seq/1,
    seq_/2,
    succ/1,
    succ_/2
]).

-export_type([
    parser/3
]).

%%====================================================================
% API Types
%%====================================================================

-type parser(Value, Error, Tokens) ::
        fun((Tokens) -> either:either(Error, {Value, Tokens})).

%%====================================================================
%% API functions
%%====================================================================

-spec alt([parser(Val, Err, Toks)]) ->
                 fun((Toks) -> either:either([Err], {Val, Toks})).
alt(Funs) ->
    fun(Tokens) -> alt_(Funs, Tokens) end.

-spec alt_([parser(Val, Err, Toks)], Toks) -> either:either([Err], {Val, Toks}).
alt_([F|Funs], Tokens) ->
    Ret = F(Tokens),
    case either:is_right(Ret) of
        true ->
            Ret;
        false ->
            case Funs of
                [] -> either:make_left(either:get_left(Ret));
                _-> alt_(Funs, Tokens)
            end
    end.

-spec list_permissive(
        parser(Val, Err, Toks),
        parser(S, Err, Toks),
        parser(any(), Err, Toks),
        parser(E, Err, Toks)
       ) -> fun((Toks) -> either:either(Err, {{list(Val), S, E}, Toks})).
list_permissive(Fun, Start, Sep, End) ->
    fun(Tokens) -> list_permissive_(Fun, Start, Sep, End, Tokens) end.

-spec list_permissive_(
        parser(Val, Err, Toks),
        parser(S, Err, Toks),
        parser(any(), Err, Toks),
        parser(E, Err, Toks),
        Toks
       ) -> either:either(Err, {{list(Val), S, E}, Toks}).
list_permissive_(Fun, Start, Sep, End, Tokens) ->
    list_strict_(
      Fun,
      Start,
      Sep,
      seq([many(Sep), End]),
      Tokens
     ).

-spec list_strict(
        parser(Val, Err, Toks),
        parser(S, Err, Toks),
        parser(any(), Err, Toks),
        parser(E, Err, Toks)
       ) -> fun((Toks) -> either:either(Err, {{list(Val), S, E}, Toks})).
list_strict(Fun, Start, Sep, End) ->
    fun(Tokens) -> list_strict_(Fun, Start, Sep, End, Tokens) end.

-spec list_strict_(
        parser(Val, Err, Toks),
        parser(S, Err, Toks),
        parser(any(), Err, Toks),
        parser(E, Err, Toks),
        Toks
       ) -> either:either(Err, {{list(Val), S, E}, Toks}).
list_strict_(Fun, Start, Sep, End, Tokens) ->
    alt_(
      [fun(Toks) -> list_strict_empty_(Start, End, Toks) end,
       fun(Toks) -> list_strict_nonempty_(Fun, Start, Sep, End, Toks) end],
      Tokens
     ).

-spec many(parser(Val, Err, Toks)) ->
                  fun((Toks) -> either:either(Err, {list(Val), Toks})).
many(Fun) ->
    fun(Tokens) -> many_(Fun, Tokens) end.

-spec many_(parser(Val, Err, Toks), Toks) ->
                  either:either(Err, {list(Val), Toks}).
many_(Fun, Tokens) ->
    many_rec_(Fun, Tokens, []).

-spec seq([parser(Val, Err, Toks)]) ->
                 fun((Toks) -> either:either(Err, {list(Val), Toks})).
seq(Funs) ->
    fun(Tokens) -> seq_(Funs, Tokens) end.

-spec seq_([parser(Val, Err, Toks)], Toks) ->
                 either:either(Err, {list(Val), Toks}).
seq_(Funs, Tokens) ->
    seq_(Funs, Tokens, []).

-spec seq_([parser(Val, Err, Toks)], Toks, list(Val)) ->
                 either:either(Err, {list(Val), Toks}).
seq_([F|Funs], Tokens, Acc) ->
    either:chain(
      fun({Val, Rest}) ->
              seq_(Funs, Rest, [Val | Acc])
      end,
      F(Tokens)
     );
seq_([], Tokens, Acc) ->
    either:make_right({lists:reverse(Acc), Tokens}).

-spec succ(Val) -> fun((Toks) -> either:either(any(), {Val, Toks})).
succ(Val) ->
    fun(Tokens) -> succ_(Val, Tokens) end.

-spec succ_(Val, Toks) -> either:either(any(), {Val, Toks}).
succ_(Val, Tokens) ->
    either:make_right({Val, Tokens}).

%%====================================================================
%% Internal functions
%%====================================================================

-spec list_strict_empty_(
        parser(S, Err, Toks),
        parser(E, Err, Toks),
        Toks
       ) -> either:either(Err, {{list(), S, E}, Toks}).
list_strict_empty_(Start, End, Tokens) ->
    Ret = seq_([Start, End], Tokens),
    either:apply(
      fun({[StartVal, EndVal], Rest}) ->
              {{[], StartVal, EndVal}, Rest}
      end,
      Ret
     ).

-spec list_strict_nonempty_(
        parser(Val, Err, Toks),
        parser(S, Err, Toks),
        parser(any(), Err, Toks),
        parser(E, Err, Toks),
        Toks
       ) -> either:either(Err, {{list(Val), S, E}, Toks}).
list_strict_nonempty_(Fun, Start, Sep, End, Tokens) ->
    Ret = seq_([Start, Fun], Tokens),
    either:chain(
      fun({[StartVal, Val], Rest}) ->
              list_strict_nonempty_rec_(Fun, Sep, End, Rest, [Val], StartVal)
      end,
      Ret
     ).

-spec list_strict_nonempty_rec_(
        parser(Val, Err, Toks),
        parser(any(), Err, Toks),
        parser(E, Err, Toks),
        Toks,
        list(Val),
        S
       ) -> either:either(Err, {{list(Val), S, E}, Toks}).
list_strict_nonempty_rec_(Fun, Sep, End, Tokens, Acc, StartVal) ->
    EitherEnd = End(Tokens),
    case either:get_either(EitherEnd) of
        {right, {EndVal, Rest}} ->
            either:make_right({{lists:reverse(Acc), StartVal, EndVal}, Rest});
        {left, _} ->
            Ret = seq_([Sep, Fun], Tokens),
            either:chain(
              fun({[_, Val], Rest}) ->
                      list_strict_nonempty_rec_(
                        Fun, Sep, End, Rest, [Val | Acc], StartVal
                       )
              end,
              Ret
             )
    end.

-spec many_rec_(parser(Val, Err, Toks), Toks, list(Val)) ->
                      either:either(Err, {list(Val), Toks}).
many_rec_(Fun, Tokens, Acc) ->
    Ret = Fun(Tokens),
    case either:get_either(Ret) of
        {right, {Val, Rest}} ->
            many_rec_(Fun, Rest, [Val | Acc]);
        {left, _} ->
            either:make_right({lists:reverse(Acc), Tokens})
    end.
