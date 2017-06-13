-module(perc_analysis).

-define(S, erl_syntax).

-include("perc_types.hrl").

-export([analyse_forms/1]).

-spec analyse_forms([erl_parse:abstract_form()]) -> {[#user_type_def{}], [#record_def{}]}.
analyse_forms(Forms) ->
    {get_user_types(Forms), get_records(Forms)}.

get_records(Forms) ->
    RecForms = forms:filter(fun is_record_def/1, Forms),
    [record_from_form(RecForm) || RecForm <- RecForms ].

get_user_types(Forms) ->
    UserTypeForms = forms:filter(fun is_user_type_def/1, Forms),
    [user_type_from_form(Form) || Form <- UserTypeForms].

is_record_def(Form) ->
    try ?S:atom_literal(?S:attribute_name(Form)) of
        "record" -> true;
        _ -> false
    catch
        error:{badarg,_} -> false
    end.

is_user_type_def(Form) ->
    try ?S:atom_literal(?S:attribute_name(Form)) of
        "type" -> true;
        _ -> false
    catch
        error:{badarg,_} -> false
    end.

record_from_form(Form) ->
    #record_def{
       name = ?S:atom_name(hd(?S:attribute_arguments(Form))),
       fields = get_record_fields(Form)
      }.

user_type_from_form(Form) ->
    %% erl_syntax does not recognise type attributes well;
    %% Use the abstract form directly
    {attribute, _, type, {TypeNameAtom, TypeDefTree, Params}} = Form,
    #user_type_def{name = atom_to_list(TypeNameAtom),
                   type = type_from_typetree(TypeDefTree),
                   parameters = Params}.

get_record_fields(Form) ->
    RecDef = lists:nth(2, ?S:attribute_arguments(Form)),
    lists:map(
      fun(Field) ->
              NameTree = ?S:record_field_name(
                            ?S:typed_record_field_body(Field) %% TODO handle untyped
                           ),
              Name = ?S:atom_name(NameTree),
              Type = ?S:typed_record_field_type(Field),
              #record_field{
                 name=Name,
                 type=type_from_typetree(Type)
                }
      end, ?S:tuple_elements(RecDef)).

%% TODO support the full list : http://erlang.org/doc/reference_manual/typespec.html
type_from_typetree(Type) ->
    case ?S:type(Type) of
        type_union ->
            {union, [type_from_typetree(Sub)
                     || Sub <- ?S:type_union_types(Type)]};
        record_type ->
            {record, ?S:atom_name(?S:record_type_name(Type))};
        user_type_application ->
            {user_type, {?S:atom_name(?S:user_type_application_name(Type)),
                         ?S:user_type_application_arguments(Type)}};
        type_application ->
            case ?S:atom_value(?S:type_application_name(Type)) of
                atom -> {basic, atom};
                string -> {basic, string};
                binary -> {basic, binary};
                integer -> {basic, integer};
                non_neg_integer -> {basic, integer};
                pos_integer -> {basic, integer};
                neg_integer -> {basic, integer};
                float -> {basic, float};
                number -> {union, [{basic, integer}, {basic, float}]};
                boolean -> {basic, boolean};
                list ->
                    case ?S:type_application_arguments(Type) of
                        [] -> {ignored, {incomplete_type, Type}};
                        List -> {list, type_from_typetree(hd(List))}
                    end;
                Else ->
                    {ignored, {unknown_type_application, Else}}
            end;
        list ->
            {list, type_from_typetree(?S:list_head(Type))}; %% TODO handle [] (nil)
        tuple_type ->
            case ?S:tuple_type_elements(Type) of
                any_size ->
                    {ignored, {incomplete_type, Type}};
                Types ->
                    {tuple, [type_from_typetree(Sub) || Sub <- Types]}
            end;
        integer ->
            {basic, integer};
        integer_range_type ->
            {basic, integer};
        atom ->
            case ?S:atom_value(Type) of
                true -> {basic, boolean};
                false -> {basic, boolean};
                undefined -> {basic, undefined_atom};
                _Else -> {basic, atom}
            end;
        Else ->
            {ignored, {unknown_type_syntax, Else}}
    end.
