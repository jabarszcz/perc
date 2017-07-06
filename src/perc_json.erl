-module(perc_json).

-behaviour(perc_backend).

%% API exports
-export([
    name/0,
    gen_record_enc_func/1,
    gen_usertype_enc_func/1
  ]).

%%====================================================================
%% API functions
%%====================================================================

-spec name() -> string().
name() ->
    "json".

-spec gen_record_enc_func(perc_types:record_def()) -> iolist().
gen_record_enc_func(RecordDef) ->
    RecordName = perc_types:get_record_def_name(RecordDef),
    Fields = perc_types:get_record_def_fields(RecordDef),
    RecordType = perc_types:make_record(RecordName),
    Dict =
        [{name, RecordName},
         {name_template, perc_backend:template(RecordType)},
         {fields, [field_dict(Field, Index)
                   || {Field, Index} <- enumerate(Fields)]}],
    {ok, Source} = json_encode_record_dtl:render(Dict),
    Source.

-spec gen_usertype_enc_func(perc_types:usertype_def()) -> iolist().
gen_usertype_enc_func(UserTypeDef) ->
    UserTypeName = perc_types:get_usertype_def_name(UserTypeDef),
    UserTypeType = perc_types:get_usertype_def_type(UserTypeDef),
    UserType = perc_types:make_usertype(UserTypeName),
    Dict = [{name_template, perc_backend:template(UserType)},
            {type_template, perc_backend:template(UserTypeType)}],
    {ok, Source} = json_encode_usertype_dtl:render(Dict),
    Source.

%%====================================================================
%% Internal functions
%%====================================================================

field_dict(Field, Index) ->
    Type = perc_types:get_record_field_type(Field),
    case perc_types:get_type(Type) of
        ignored ->
            ReasonLines =
                case perc_types:get_ignored_reason(Type) of
                    none -> "";
                    Else ->
                        List = io_lib:format("~p", [Else]),
                        Str = lists:flatten(List),
                        string:tokens(Str, "\n")
                end,
            [{ignored, true},
             {ignored_reason_lines, ReasonLines}];
        maybe ->
            [{maybe, true},
             {type_template,
              perc_backend:template(perc_types:get_maybe_type(Type))}];
        _ ->
            [{type_template, perc_backend:template(Type)}]
    end ++
        [{name, perc_types:get_record_field_name(Field)},
         {index, Index}].

enumerate(List) ->
    lists:zip(List, lists:seq(1, length(List))).
