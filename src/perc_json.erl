-module(perc_json).

-behaviour(perc_backend).

-define(EMPTY_OBJ_FILTER, "no_empty_obj").

%% API exports
-export([
    name/0,
    gen_record_enc_func/2,
    gen_usertype_enc_func/2
  ]).

%%====================================================================
%% API functions
%%====================================================================

-spec name() -> string().
name() ->
    "json".

-spec gen_record_enc_func(
        perc_defs:record_def(),
        dict:dict(perc_types:perc_type(), integer())
       ) -> iolist().
gen_record_enc_func(RecordDef, IdMap) ->
    RecordName = perc_defs:get_record_def_name(RecordDef),
    Fields = perc_defs:get_record_def_fields(RecordDef),
    RecordType = perc_defs:def_to_type(RecordDef),
    Dict =
        [{name, RecordName},
         {name_template, perc_backend:template(RecordType, IdMap)},
         {fields, [field_dict(Field, Index, IdMap)
                   || {Index, Field} <- perc_utils:enumerate(Fields)]}],
    {ok, Source} = json_encode_record_dtl:render(Dict),
    Source.

-spec gen_usertype_enc_func(
        perc_defs:usertype_def(),
        dict:dict(perc_types:perc_type(), integer())
       ) -> iolist().
gen_usertype_enc_func(UserTypeDef, IdMap) ->
    UserTypeType = perc_defs:get_usertype_def_type(UserTypeDef),
    UserType = perc_defs:def_to_type(UserTypeDef),
    Dict = [{name_template, perc_backend:template(UserType, IdMap)},
            {type_template, perc_backend:template(UserTypeType, IdMap)}],
    {ok, Source} = json_encode_usertype_dtl:render(Dict),
    Source.

%%====================================================================
%% Internal functions
%%====================================================================

field_dict(Field, Index, IdMap) ->
    Type = perc_defs:get_record_field_type(Field),
    TypeVals =
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
            _ ->
                [{type_template, perc_backend:template(Type, IdMap)}]
        end,
    FilterVals =
        case perc_defs:get_record_field_filters(Field) of
            [] -> [];
            Filters ->
                [{filters, [perc_filter:get_name(F)
                            || F <- Filters,
                               perc_filter:get_name(F) /= ?EMPTY_OBJ_FILTER]}]
        end,
    EmptyObjFilter =
        lists:any(fun (Filter) ->
                          perc_filter:get_name(Filter) == ?EMPTY_OBJ_FILTER
                  end, perc_defs:get_record_field_filters(Field)),
    lists:append(
      [TypeVals, FilterVals,
       [{name, perc_defs:get_record_field_name(Field)},
        {index, Index},
        {no_empty_obj, EmptyObjFilter}]
      ]).
