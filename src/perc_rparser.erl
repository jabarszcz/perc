-module(perc_rparser).

-export([
    type/1,
    parse_defs/1
%%,record_def/1
]).

-export_type([
    parse_error/0
]).

-record(parse_error, {
          expected :: any(),
          actual :: any(),
          line :: integer()
}).

-opaque parse_error() :: #parse_error{}.

-type parse_success(V) :: {V, integer(), perc_parser:tokens()}.

-type parse_result(V) ::
        either:either(parse_error(), parse_success(V)).

-type parser(V) :: fun((perc_scanner:tokens()) -> parse_result(V)).

-spec parse_defs(perc_scanner:tokens()) -> {ok, perc_types:defs()} | {error, parse_error()}.
parse_defs(Tokens) ->
    case either:get_either(defs(Tokens)) of
        {right, {Val, _Line, []}} ->
            {ok, Val};
        {left, Err} ->
            {error, Err}
    end.

defs(Tokens) ->
    Parser =
        fun(Toks) ->
                choice(
                  [fun record_def/1,
                   fun usertype_def/1],
                  Toks
                 )
        end,
    Ret = kleene(Parser, Tokens),
    case either:get_right(Ret) of
        {List, Line, []} ->
            Defs =
                perc:make_defs(
                  lists:filter(fun perc_types:is_record_def/1, List),
                  lists:filter(fun perc_types:is_usertype_def/1, List)
                 ),                  
            either:make_right({Defs, Line, []});
        {_, _, Toks} ->
            Parser(Toks) %% Does not parse but return the error
    end.

record_def(Tokens) ->
    Ret = seq(
            [fun(Toks) -> match(record, Toks) end,
             fun id/1,
             fun(Toks) -> match('::', Toks) end,
             fun fields/1,
             fun(Toks) -> match(def_sep, Toks) end],
            Tokens
           ),
    either:apply(
      fun({[_, {Id, Line, _}, _, {Fields, _, _}, _], Rest}) ->
              {perc_types:make_record_def(Id, Fields), Line, Rest}
      end,
      Ret
     ).

usertype_def(Tokens) ->
    Ret = seq(
            [fun(Toks) -> match(usertype, Toks) end,
             fun id/1,
             fun(Toks) -> match('::', Toks) end,
             fun type/1,
             fun(Toks) -> match(def_sep, Toks) end],
            Tokens
           ),
    either:apply(
      fun({[_, {Id, Line, _}, _, {Type, _, _}, _], Rest}) ->
              {perc_types:make_record_def(Id, Type), Line, Rest}
      end,
      Ret
     ).

fields(Tokens) ->
    list_permissive(
      fun field/1,
      fun(Toks) -> match('{', Toks) end,
      fun(Toks) -> match(',', Toks) end,
      fun(Toks) -> match('}', Toks) end,
      Tokens
     ).

field(Tokens) ->
    choice(
      [fun filtered_field/1,
       fun field_body/1],
      Tokens
     ).

filtered_field(Tokens) ->
    Ret =
        seq(
          [fun field_body/1,
           fun filters/1],
          Tokens
         ),
    either:apply(
      fun({[{Field, Line, _}, {Filters, _, _}], Rest}) ->
              {perc_types:set_record_field_filters(Field, Filters), Line, Rest}
      end,
      Ret
     ).

field_body(Tokens) ->
    choice(
      [fun wildcard_field_body/1,
       fun named_field_body/1],
      Tokens
     ).

wildcard_field_body(Tokens) ->
    either:apply(
      fun({_, Line, Rest}) ->
              {perc_types:make_record_field(
                 undefined, perc_types:make_ignored()
                ),
               Line, Rest}
      end,
      match(wildcard, Tokens)
     ).

named_field_body(Tokens) ->
    Ret =
        seq(
          [fun id/1,
           fun(Toks) -> match('::', Toks) end,
           fun type/1],
          Tokens
         ),
    either:apply(
      fun({[{Id, Line, _}, _, {Type, _, _}], Rest}) ->
              {perc_types:make_record_field(Id, Type), Line, Rest}
      end,
      Ret
     ).

filters(Tokens) ->
    Ret = list(
            fun id/1,
            fun(Toks) -> match('[', Toks) end,
            fun(Toks) -> match(',', Toks) end,
            fun(Toks) -> match(']', Toks) end,
            Tokens
           ),
    either:apply(
      fun({List, Line, Rest}) ->
              {lists:map(fun perc_filter:make/1, List), Line, Rest}
      end,
      Ret
     ).


-spec type(perc_scanner:tokens()) -> parse_result(perc_types:perc_type()).
type(Tokens) ->
    choice(
      [fun record_ref/1,
       fun type_application/1,
       fun usertype_ref_type/1,
       fun id_type/1],
      Tokens
     ).

-spec id(perc_scanner:tokens()) -> parse_result(string()).
id(Tokens) ->
    match(id, Tokens).

-spec id_type(perc_scanner:tokens()) -> parse_result(perc_types:perc_type()).
id_type(Tokens) ->
    either:chain(
      fun(Succ) -> make_type(Succ) end,
      id(Tokens)
     ).

-spec record_ref(perc_scanner:tokens()) -> parse_result(perc_types:perc_type()).
record_ref(Tokens) ->
    Ret =
        seq(
          [fun(Toks) -> match(record, Toks) end,
           fun(Toks) -> match('<', Toks) end,
           fun id/1,
           fun(Toks) -> match('>', Toks) end],
          Tokens
         ),
    either:chain(
      fun({[_, _, {Name, Line, _}, _], Rest}) ->
            make_type({record, Name}, Line, Rest, [])
      end,
      Ret
     ).

usertype_ref(Tokens) ->
    Ret =
        seq(
          [fun(Toks) -> match(usertype, Toks) end,
           fun(Toks) -> match('<', Toks) end,
           fun id/1,
           fun(Toks) -> match('>', Toks) end],
          Tokens
         ),
    either:apply(
      fun({[_, _, {Name, Line, _}, _], Rest}) ->
              {{usertype, Name}, Line, Rest}
      end,
      Ret
     ).

usertype_ref_type(Tokens) ->
    either:chain(
      fun(Succ) -> make_type(Succ) end,
      usertype_ref(Tokens)
     ).

type_application(Tokens) ->
    Ret =
        seq(
          [fun applicable/1,
           fun args/1],
          Tokens
         ),
    either:chain(
      fun({[{App, Line, _}, {Args, _, _}], Rest}) ->
              make_type(App, Line, Rest, Args)
      end,
      Ret
     ).

applicable(Tokens) ->
    choice(
      [fun id/1,
       fun usertype_ref/1,
       fun function_ref/1
      ],
      Tokens
     ).

args(Tokens) ->
    list(
      fun type/1,
      fun(Toks) -> match('(', Toks) end,
      fun(Toks) -> match(',', Toks) end,
      fun(Toks) -> match(')', Toks) end,
      Tokens
     ).

function_ref(Tokens) ->
    Ret = seq(
            [fun(Toks) -> match(function, Toks) end,
             fun(Toks) -> match('<', Toks) end,
             fun func_name/1,
             fun(Toks) -> match(',', Toks) end,
             fun func_name/1,
             fun(Toks) -> match('>', Toks) end],
            Tokens
           ),
    either:apply(
      fun({[{_, Line, _}, _, {NameA, _, _}, _, {NameB, _, _}, _], Rest}) ->
              {{function, {NameA, NameB}}, Line, Rest}
      end,
      Ret
     ).

func_name(Tokens) ->
    choice([fun id/1, fun func_name_wildcard/1], Tokens).

func_name_wildcard(Tokens) ->
    either:apply(
      fun({_, Line, Rest}) ->
              {undefined, Line, Rest}
      end,
      match(wildcard, Tokens)
     ).

%% Ad-hoc (Slow) Parser combinators

%% caller_name() ->
%%     catch throw(away),
%%     [_, _, Caller | _] = erlang:get_stacktrace(),
%%     element(2, Caller).

-spec seq(
        [parser(any())],
        perc_scanner:tokens()
       ) -> either:either(
              parse_error(),
              {[parse_success(any())], perc_scanner:tokens()}
             ).
seq(Funs, Tokens) ->
    seq(Funs, Tokens, []).

-spec seq(
        [parser(any())],
        perc_parser:tokens(),
        list()
       ) -> either:either(
              parse_error(),
              {[parse_success(any())], perc_parser:tokens()}
             ).
seq([F|Funs], Tokens, Acc) ->
    %% Caller = caller_name(),
    %% io:format("trying ~p.seq ~p~n", [Caller, F]),
    %% Ret = F(Tokens),
    %% io:format("result of ~p.seq ~p is ~p~n", [Caller, F, Ret]),
    either:chain(
      fun(Succ = {_, _, Rest}) ->
              seq(Funs, Rest, [Succ | Acc])
      end,
      F(Tokens)
     );
seq([], Tokens, Acc) ->
    either:make_right({lists:reverse(Acc), Tokens}).


-spec choice([parser(V)], perc_parser:tokens()) -> parse_result(V).
choice([F|Funs], Tokens) ->
    choice([F|Funs], Tokens, []).

choice([F|Funs], Tokens, ErrorsAcc) ->
    %% Caller = caller_name(),
    %% io:format("trying ~p.choice ~p~n", [Caller, F]),
    Ret = F(Tokens),
    %% io:format("result of ~p.choice ~p is ~p~n", [Caller, F, Ret]),
    case either:is_right(Ret) of
        true ->
            Ret;
        false ->
            Errors = [either:get_left(Ret)|ErrorsAcc],
            case Funs of
                [] -> either:make_left(lists:reverse(Errors));
                _-> choice(Funs, Tokens, Errors)
            end
    end.

list(Fun, Start, Sep, End, Tokens) ->
    choice(
      [fun(Toks) -> empty_list(Start, End, Toks) end,
       fun(Toks) -> nonempty_list(Fun, Start, Sep, End, Toks) end],
      Tokens
     ).

empty_list(Start, End, Tokens) ->
    Ret = seq([Start, End], Tokens),
    either:apply(
      fun({[{_, Line, _}, _], Rest}) ->
              {[], Line, Rest}
      end,
      Ret
     ).

-spec nonempty_list(
        parser(A),
        parser(any()),
        parser(any()),
        parser(any()),
        perc_scanner:tokens()
       ) -> parse_result(list(A)).
nonempty_list(Fun, Start, Sep, End, Tokens) ->
    Ret = seq([Start, Fun], Tokens),
    either:chain(
      fun({[{_, Line, _}, {Val, _, _}], Rest}) ->
              list_rec(Fun, Sep, End, Rest, Line, [Val])
      end,
      Ret
     ).

-spec list_rec(
        parser(A),
        parser(any()),
        parser(any()),
        perc_scanner:tokens(),
        integer(),
        [A]
       ) -> parse_result([A]).
list_rec(Fun, Sep, End, Tokens, Line, Acc) ->
    EitherEnd = End(Tokens),
    case either:get_either(EitherEnd) of
        {right, {_, _, Rest}} ->
            either:make_right({lists:reverse(Acc), Line, Rest});
        {left, _} ->
            Ret = seq([Sep, Fun], Tokens),
            either:chain(
              fun({[_, {Val, _, _}], Rest}) ->
                      list_rec(Fun, Sep, End, Rest, Line, [Val | Acc])
              end,
              Ret
             )
    end.

-spec kleene(parser(A), perc_scanner:tokens()) -> parse_result(A).
kleene(Fun, Tokens) ->
    kleene_rec(Fun, Tokens, either:make_left(0), []).

kleene_rec(Fun, Tokens, Line, Acc) ->
    Ret = Fun(Tokens),
    case either:get_either(Ret) of
        {right, {Val, LineParsed, Rest}} ->
            L = either:disjunction(Line, either:make_right(LineParsed)),
            kleene_rec(Fun, Rest, L, [Val | Acc]);
        {left, _} ->
            either:make_right({Acc, either:unbox(Line), Tokens})
    end.

list_permissive(Fun, Start, Sep, End, Tokens) ->
    list(
      Fun,
      Start,
      Sep,
      fun(Toks1) ->
              Ret = seq([fun(Toks2) -> kleene(Sep, Toks2) end, End], Toks1),
              either:apply(
                fun({[{_, Line, _}, _], Rest}) ->
                        {undefined, Line, Rest}
                end,
                Ret
               )
      end,
      Tokens
     ).


%% Utils

match(Category, [{Category, Line, Value} | Rest]) ->
    either:make_right({Value, Line, Rest});
match(Value, [{Value, Line} | Rest]) ->
    either:make_right({Value, Line, Rest});
match(Expected, [Token|_]) ->
    make_error(Token, Expected);
match(Expected, []) ->
    make_error("End of tokens", Expected, -1).


make_error(Actual, Expected, Line) ->
    either:make_left(
      #parse_error{
         expected=Expected,
         actual=Actual,
         line=Line
        }
     ).

make_error(Token, Expected) ->
    make_error(
      perc_scanner:token_val(Token),
      Expected,
      perc_scanner:token_line(Token)
     ).


%% Other functions

make_type(Succ) ->
    make_type(Succ, []).

make_type({Id, Line, Rest}, Args) ->
    make_type(Id, Line, Rest, Args).

-spec make_type(
        any(),
        integer(),
        perc_scanner:tokens(),
        list()
       ) -> parse_result(perc_types:perc_type()). 
make_type(Id, Line, Rest, Args) ->
    Res =
        case {Id, Args} of
            {"ignored", _} ->
                perc_types:make_ignored();
            {"undefined", []} ->
                perc_types:make_undefined_atom();
            {"list", [Arg]} ->
                perc_types:make_list(Arg);
            {"tuple", _} ->
                perc_types:make_tuple(Args);
            {"union", _} ->
                perc_types:make_union(Args);
            {{record, Name}, []} ->
                perc_types:make_record(Name);
            {{usertype, Name}, _} ->
                perc_types:make_usertype(Name); %% TODO Args
            {{function, NamePair}, [Arg]} ->
                perc_types:make_function(NamePair, Arg);
            {Basic, []} ->
                BasicSet = sets:from_list([integer, float, atom,
                                           binary, string, boolean]),
                Atom = list_to_atom(Basic),
                case sets:is_element(Atom, BasicSet) of
                    true ->
                        perc_types:make_basic(Atom);
                    _ ->
                    make_error(
                      Basic,
                      "Basic perc type (integer, float, string, etc.)",
                      Line
                     )
                end;
            _ ->
                make_error({Id, Args}, "Perc type application", Line)
        end,
    case either:is_left(Res) of
        true ->
            Res;
        _ ->
            either:make_right({Res, Line, Rest})
    end.
