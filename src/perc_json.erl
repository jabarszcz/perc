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

gen_record_enc_func(Record) ->
    Dict = [{name_template, template({record, Record#record_def.name})},
            {enc_func, [name(), "_encode"]},
            {fields, [field_dict(Field, Index)
                      || {Field, Index} <-
                             enumerate(Record#record_def.fields)]}],
    {ok, Source} = json_encode_record_dtl:render(Dict),
    Source.

gen_usertype_enc_func(#user_type_def{name=Name, type=Type}) ->
    Dict = [{name_template, template({user_type, {Name, undefined}})},
            {enc_func, [name(), "_encode"]},
            {type_template, template(Type)}],
    {ok, Source} = json_encode_usertype_dtl:render(Dict),
    Source.

field_dict(Field, Index) ->
    case Field#record_field.type of
        ignored ->
            [{ignored, true},
             {ignored_reason, ""}];
        {ignored, Reason} ->
            [{ignored, true},
             {ignored_reason, io_lib:format("~p", [Reason])}];
        {maybe, Type} ->
            [{maybe, true},
             {type_template, template(Type)}];
        Type ->
            [{type_template, template(Type)}]
    end ++
        [{name, Field#record_field.name},
         {index, Index}].

template({basic, Basic}) ->
    capitalize(atom_to_list(Basic));
template({maybe, Type}) ->
    io_lib:format("Maybe<~s>", [template(Type)]);
template({list, Type}) ->
    io_lib:format("List<~s>", [template(Type)]);
template({tuple, Types}) ->
    io_lib:format("Tuple<~s>",
                  [string:join([template(Type) || Type <- Types], ",")]);
template({union, Types}) ->
    io_lib:format("Union<~s>",
                  [string:join([template(Type) || Type <- Types], ",")]);
template({record, RecordName}) ->
    io_lib:format("Record<~s>", [perc_backend:id_from_name(RecordName)]);
template({user_type, {UserTypeName, _}}) ->
    io_lib:format("UserType<~s>", [perc_backend:id_from_name(UserTypeName)]).

enumerate(List) ->
    lists:zip(List, lists:seq(1, length(List))).

capitalize([H|T]) ->
    [string:to_upper(H)|T].
