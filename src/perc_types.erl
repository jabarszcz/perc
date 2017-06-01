-module(perc_types).

-define(S, erl_syntax).

-include("perc_types.hrl").

-export([get_records_from_forms/1, get_user_types/1, get_record_from_form/2,
        get_nested_records/1]).

get_records_from_forms(Forms) ->
    UserTypes = get_user_types(Forms),
    Records = forms:filter(fun is_record_def/1, Forms),
    [ get_record_from_form(RecForm, UserTypes) || RecForm <- Records ].

get_user_types(Forms) ->
    undefined. %% TODO

get_record_from_form(Form, UserTypes) ->
    #record_def{
       name = ?S:atom_name(hd(?S:attribute_arguments(Form))),
       fields = get_record_fields(Form)
      }.

get_nested_records(#record_def{fields=Fields}) ->
    lists:foldl(
      fun(#record_field{type=FieldType}, Acc) ->
              sets:union(Acc, get_record_deps(FieldType))
      end,
      sets:new(),
      Fields).

%====================================

get_record_deps({maybe, Type}) ->
    get_record_deps(Type);
get_record_deps({list, Type}) ->
    get_record_deps(Type);
get_record_deps({tuple, Types}) ->
    sets:union([get_record_deps(Type) || Type <- Types]);
get_record_deps({record, Name}) ->
    sets:from_list([Name]);
get_record_deps(_) ->
    sets:new().

is_record_def(Form) ->
    try ?S:atom_literal(?S:attribute_name(Form)) of
        "record" -> true;
        _ -> false
    catch
        error:{badarg,_} -> false
    end.

get_record_fields(Form) ->
    RecDef = lists:nth(2, ?S:attribute_arguments(Form)),
    lists:map(
      fun(Field) ->
              NameTree = ?S:record_field_name(
                            ?S:typed_record_field_body(Field)
                           ),
              Name = ?S:atom_name(NameTree),
              Type = ?S:typed_record_field_type(Field),
              #record_field{
                 name=Name,
                 type=get_perc_type(Name, Type)
                }
      end, ?S:tuple_elements(RecDef)).

get_perc_type(FieldName, FieldTypeTree) ->
    try perc_type_from_type(FieldTypeTree)
    catch
        throw:{ignored, Reason} ->
            warn_ignored(FieldName, Reason),
            ignored
    end.

warn_ignored(Field, Reason) ->
    io:format(standard_error, "Ignored field ~s: ~p~n", [Field, Reason]).

%% TODO support the full list : http://erlang.org/doc/reference_manual/typespec.html
perc_type_from_type(Type) ->
    case ?S:type(Type) of
        type_union ->
            merge_types(?S:type_union_types(Type));
        record_type ->
            {record, ?S:atom_name(?S:record_type_name(Type))};
        user_type_application ->
            throw({ignored, {unimplemented, user_type_application}}); %%TODO
        type_application ->
            case ?S:atom_value(?S:type_application_name(Type)) of
                atom -> atom;
                string -> string;
                binary -> binary;
                integer -> integer;
                non_neg_integer -> integer;
                pos_integer -> integer;
                neg_integer -> integer;
                float -> float;
                boolean -> boolean;
                list ->
                    case ?S:type_application_arguments(Type) of
                        [] -> throw({ignored, {incomplete_type, Type}});
                        List -> make_list(perc_type_from_type(hd(List)))
                    end;
                Else ->
                    throw({ignored, {unknown_type_application, Else}})
            end;
        list ->
            make_list(perc_type_from_type(?S:list_head(Type)));
        tuple_type ->
            throw({ignored, {unimplemented, tuple_type}}); %% TODO
        integer ->
            integer;
        integer_range_type ->
            integer;
        atom ->
            case ?S:atom_value(Type) of
                true -> boolean;
                false -> boolean;
                _Else -> atom
            end;
        Else ->
            throw({ignored, {unknown_type_syntax, Else}})
    end.

merge_types([T]) ->
    perc_type_from_type(T);
merge_types([TA,TB|Ts]) ->
    case {is_undef(TA), is_undef(TB)} of
        {true, true} ->
            make_maybe(merge_types(Ts));
        {true, _} ->
            make_maybe(merge_types([TB|Ts]));
        {_, true} ->
            make_maybe(merge_types([TA|Ts]));
        _Else ->
            case perc_type_from_type(TA) ==
                perc_type_from_type(TB) of
                true -> merge_types([TB|Ts]);
                false -> throw({ignored, {union_mismatch, TA, TB}})
            end
    end.

is_undef(Type) ->
    case ?S:type(Type) of
        atom ->
            case ?S:atom_value(Type) of
                undefined ->
                    true;
                _Else ->
                    false
            end;
        _Else ->
            false
    end.

make_maybe(ignored) ->
    ignored;
make_maybe(Type) ->
    {maybe, Type}.

make_list(ignored) ->
    ignored;
make_list(Type) ->
    {list, Type}.
