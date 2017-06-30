-module(perc_json).
-behaviour(perc_backend).
-export([name/0,
         gen_record_enc_func/1,
         gen_usertype_enc_func/1
        ]).

-include("perc_types.hrl").

-spec name() -> string().
name() ->
    "json".

-spec gen_record_enc_func(#record_def{}) -> iolist().
gen_record_enc_func(Record) ->
    RecordType = perc_types:make_record_type(Record#record_def.name),
    Dict = [{name_template, template(RecordType)},
            {enc_func, [name(), "_encode"]},
            {fields, [field_dict(Field, Index)
                      || {Field, Index} <-
                             enumerate(Record#record_def.fields)]}],
    {ok, Source} = json_encode_record_dtl:render(Dict),
    Source.

-spec gen_usertype_enc_func(#user_type_def{}) -> iolist().
gen_usertype_enc_func(#user_type_def{name=Name, type=Type}) ->
    UserType = perc_types:make_user_type(Name, undefined),
    Dict = [{name_template, template(UserType)},
            {enc_func, [name(), "_encode"]},
            {type_template, template(Type)}],
    {ok, Source} = json_encode_usertype_dtl:render(Dict),
    Source.

field_dict(Field, Index) ->
    Type = Field#record_field.type,
    case perc_types:get_type(Type) of
        ignored ->
            Reason =
                case perc_types:get_ignored_reason(Type) of
                    none -> "";
                    Else -> io_lib:format("~p", [Else])
                end,
            [{ignored, true},
             {ignored_reason, Reason}];
        maybe ->
            [{maybe, true},
             {type_template, template(perc_types:get_maybe_type(Type))}];
        _ ->
            [{type_template, template(Type)}]
    end ++
        [{name, Field#record_field.name},
         {index, Index}].

template(Type) ->
    case perc_types:get_type(Type) of
        basic ->
            capitalize(atom_to_list(perc_types:get_basic_type(Type)));
        maybe ->
            MaybeType = perc_types:get_maybe_type(Type),
            io_lib:format("Maybe<~s>", [template(MaybeType)]);
        list ->
            ListType = perc_types:get_list_type(Type),
            io_lib:format("List<~s>", [template(ListType)]);
        tuple ->
            TupleTypes = perc_types:get_tuple_types(Type),
            io_lib:format(
              "Tuple<~s>",
              [string:join([template(T) || T <- TupleTypes], ",")]
             );
        union ->
            UnionTypes = perc_types:get_union_types(Type),
            io_lib:format(
              "Union<~s>",
              [string:join([template(T) || T <- UnionTypes], ",")]
             );
        record ->
            Name = perc_types:get_record_type_name(Type),
            io_lib:format("Record<~s>", [perc_backend:id_from_name(Name)]);
        user_type ->
            Name = perc_types:get_user_type_name(Type),
            io_lib:format("UserType<~s>", [perc_backend:id_from_name(Name)])
    end.

enumerate(List) ->
    lists:zip(List, lists:seq(1, length(List))).

capitalize([H|T]) ->
    [string:to_upper(H)|T].
