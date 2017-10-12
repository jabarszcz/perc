-module(perc_rparser).

%% API exports

-export([
    error_line/1,
    format_error/1,
    parse_annotation/1,
    parse_defs/1,
    parse_type/1
]).

-export_type([
    parse_error/0,
    linespec/0
]).

%%====================================================================
% API Types
%%====================================================================

-type linespec() :: integer() | undefined.

-record(parse_error, {
          expected :: any(),
          actual :: any(),
          line :: linespec()
}).

-opaque parse_error() :: #parse_error{}.

%%====================================================================
%% API functions
%%====================================================================

-spec error_line(parse_error()) -> linespec().
error_line(Err) ->
    Err#parse_error.line.

-spec format_error(parse_error()) -> iolist().
format_error(Error) ->
    io_lib:format(
      "Expected: ~p, Found: ~p.~n",
      [Error#parse_error.expected,
       Error#parse_error.actual]
      ).

-spec parse_annotation(perc_scanner:tokens()) ->
                              {ok, {[string()] | undefined, list()}}
                                  | {error, linespec(), parse_error()}.
parse_annotation(Tokens) ->
    case either:get_either(annotation(Tokens)) of
        {right, {{Val, _Line}, []}} ->
            {ok, Val};
        {left, Err} ->
            {error, error_line(Err), Err}
    end.

-spec parse_defs(perc_scanner:tokens()) ->
                        {ok, perc_types:defs()}
                            | {error, linespec(), parse_error()}.
parse_defs(Tokens) ->
    case either:get_either(defs(Tokens)) of
        {right, {{Val, _Line}, []}} ->
            {ok, Val};
        {left, Err} ->
            {error, error_line(Err), Err}
    end.

-spec parse_type(perc_scanner:tokens()) ->
                        {ok, perc_types:perc_type()}
                            | {error, linespec(), parse_error()}.
parse_type(Tokens) ->
    case either:get_either(type(Tokens)) of
        {right, {{Val, _Line}, []}} ->
            {ok, Val};
        {left, Err} ->
            {error, error_line(Err), Err}
    end.

%%====================================================================
%% Internal Types
%%====================================================================

-type val(V) :: {V, linespec()}.

%%====================================================================
%% Internal functions
%%====================================================================

line_from_tokens([T|_]) ->
    perc_scanner:token_line(T);
line_from_tokens(_) ->
    undefined.

-spec get_val(val(V)) -> V.
get_val({Val, _Line}) ->
    Val.

-spec get_line(val(any())) -> linespec().
get_line({_Val, Line}) ->
    Line.

annotation(Tokens) ->
    Ret = perc_combinators:seq_(
            [perc_combinators:alt(
               [fun codec_names/1,
                perc_combinators:succ({undefined, line_from_tokens(Tokens)})]
              ),
             fun field_props/1,
             fun eof/1],
            Tokens
           ),
    either:apply(
      fun ({[{Names, _}, {Props, Line}, _], Rest}) ->
              {{{Names, Props}, Line}, Rest}
      end,
      Ret
     ).

codec_names(Tokens) ->
    Ret = perc_combinators:list_strict_(
            fun id/1,
            match('('),
            match(','),
            match(')'),
            Tokens
           ),
    either:apply(
      fun({{List, {_, Line}, _}, Rest}) ->
              Names = lists:map(fun get_val/1, List),
              {{Names, Line}, Rest}
      end,
      Ret
     ).

defs(Tokens) ->
    Parser =
        perc_combinators:alt(
          [fun record_def/1,
           fun usertype_def/1]
         ),
    Ret = many(Parser, Tokens),
    case either:get_right(Ret) of
        {{List, Line}, []} ->
            Defs =
                perc:make_defs(
                  lists:filter(fun perc_types:is_record_def/1, List),
                  lists:filter(fun perc_types:is_usertype_def/1, List)
                 ),
            either:make_right({{Defs, Line}, []});
        {{_, _}, Toks} ->
            Parser(Toks) %% Does not parse but return the error
    end.

record_def(Tokens) ->
    Ret = perc_combinators:seq_(
            [match(record),
             fun id/1,
             match('::'),
             fun fields/1,
             match(def_sep)],
            Tokens
           ),
    either:apply(
      fun({[_, {Id, Line}, _, {Fields, _}, _], Rest}) ->
              {{perc_types:make_record_def(Id, Fields), Line}, Rest}
      end,
      Ret
     ).

usertype_def(Tokens) ->
    Ret = perc_combinators:seq_(
            [match(usertype),
             fun id/1,
             match('::'),
             fun type/1,
             match(def_sep)],
            Tokens
           ),
    either:apply(
      fun({[_, {Id, Line}, _, {Type, _}, _], Rest}) ->
              {{perc_types:make_record_def(Id, Type), Line}, Rest}
      end,
      Ret
     ).

fields(Tokens) ->
    Ret =
        perc_combinators:list_permissive_(
          fun field/1,
          match('{'),
          match(','),
          match('}'),
          Tokens
         ),
    either:apply(
      fun({{List, Start, _}, Rest}) ->
              {{lists:map(fun get_val/1, List), get_line(Start)}, Rest}
      end,
      Ret
     ).

field(Tokens) ->
    Ret = perc_combinators:seq_(
            [fun id_maybe/1,
             fun field_props/1],
            Tokens
           ),
    either:apply(
      fun({[{Id, Line}, {List, _}], Rest}) ->
              {{make_field(Id, List), Line}, Rest}
      end,
      Ret
     ).

field_props(Tokens) ->
    many(fun field_prop/1, Tokens).

field_prop(Tokens) ->
    perc_combinators:alt_(
      [fun type_prop/1,
       fun filters_prop/1],
      Tokens
     ).

type_prop(Tokens) ->
    Ret = perc_combinators:seq_(
            [match('::'),
             fun type/1],
            Tokens
           ),
    either:apply(
      fun({[{_, Line}, {Type, _}], Rest}) ->
              {{{type, Type}, Line}, Rest}
      end,
      Ret
     ).

filters_prop(Tokens) ->
    either:apply(
      fun({{Filters, Line}, Rest}) ->
              {{{filters, Filters}, Line}, Rest}
      end,
      filters(Tokens)
     ).

filters(Tokens) ->
    Ret =
        perc_combinators:list_strict_(
          fun id/1,
          match('['),
          match(','),
          match(']'),
          Tokens
         ),
    either:apply(
      fun({{List, Start, _}, Rest}) ->
              Filters = [perc_filter:make(get_val(V)) || V <- List],
              {{Filters, get_line(Start)}, Rest}
      end,
      Ret
     ).

type(Tokens) ->
    perc_combinators:alt_(
      [fun record_ref/1,
       fun type_application/1,
       fun usertype_ref_type/1,
       fun id_type/1],
      Tokens
     ).

id(Tokens) ->
    match_(id, Tokens).

id_type(Tokens) ->
    either:chain(
      fun(Succ) -> make_type(Succ) end,
      id(Tokens)
     ).

record_ref(Tokens) ->
    Ret =
        perc_combinators:seq_(
          [match(record), match('<'), fun id/1, match('>')],
          Tokens
         ),
    either:chain(
      fun({[_, _, {Name, Line}, _], Rest}) ->
            make_type({record, Name}, Line, Rest, [])
      end,
      Ret
     ).

usertype_ref(Tokens) ->
    Ret =
        perc_combinators:seq_(
          [match(usertype), match('<'), fun id/1, match('>')],
          Tokens
         ),
    either:apply(
      fun({[_, _, {Name, Line}, _], Rest}) ->
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
        perc_combinators:seq_(
          [fun applicable/1, fun args/1],
          Tokens
         ),
    either:chain(
      fun({[{App, Line}, {Args, _}], Rest}) ->
              make_type(App, Line, Rest, Args)
      end,
      Ret
     ).

applicable(Tokens) ->
    perc_combinators:alt_(
      [fun id/1,
       fun usertype_ref/1,
       fun function_ref/1],
      Tokens
     ).

args(Tokens) ->
    Ret =
        perc_combinators:list_strict_(
          fun type/1,
          match('('),
          match(','),
          match(')'),
          Tokens
         ),
    either:apply(
      fun({{List, Start, _}, Rest}) ->
              {{lists:map(fun get_val/1, List), get_line(Start)}, Rest}
      end,
      Ret
     ).

function_ref(Tokens) ->
    Ret =
        perc_combinators:seq_(
          [match(function),
           match('<'),
           fun id_maybe/1,
           match(','),
           fun id_maybe/1,
           match('>')],
          Tokens
         ),
    either:apply(
      fun({[{_, Line}, _, {NameA, _}, _, {NameB, _}, _], Rest}) ->
              {{{function, {NameA, NameB}}, Line}, Rest}
      end,
      Ret
     ).

id_maybe(Tokens) ->
    perc_combinators:alt_([fun id/1, fun wildcard/1], Tokens).

wildcard(Tokens) ->
    either:apply(
      fun({{_, Line}, Rest}) ->
              {{undefined, Line}, Rest}
      end,
      match_(wildcard, Tokens)
     ).

%% Utils

match(Val) ->
    fun(Tokens) -> match_(Val, Tokens) end.

match_(Category, [{Category, Line, Value} | Rest]) ->
    either:make_right({{Value, Line}, Rest});
match_(Value, [{Value, Line} | Rest]) ->
    either:make_right({{Value, Line}, Rest});
match_(Expected, [Token|_]) ->
    make_error(Token, Expected);
match_(Expected, []) ->
    make_error("End of tokens", Expected, undefined).

eof([]) ->
    either:make_right({{undefined, undefined}, []});
eof([Tok|_]) ->
    make_error(Tok, "End of tokens").

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

many(Parser, Tokens) ->
    either:apply(
      fun({List, Rest}) ->
              {{lists:map(fun get_val/1, List), line_from_tokens(Tokens)}, Rest}
      end,
      perc_combinators:many_(Parser, Tokens)
     ).

make_type(Succ) ->
    make_type(Succ, []).

make_type({{Id, Line}, Rest}, Args) ->
    make_type(Id, Line, Rest, Args).

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
            either:make_right({{Res, Line}, Rest})
    end.

make_field(Id, Proplist) ->
    Type = proplists:get_value(type, Proplist, perc_types:make_ignored()),
    Filters = proplists:get_all_values(filters, Proplist),
    perc_types:make_record_field(Id, Type, lists:append(Filters)).
