-module(perc_parse_erl).

%% API exports
-export([
    read/1,
    read_all/1
  ]).

%%====================================================================
%% API functions
%%====================================================================

-spec analyse_forms([erl_parse:abstract_form()]) -> perc:defs().
analyse_forms(Forms) ->
    perc:make_defs(get_record_defs(Forms), get_usertype_defs(Forms)).

-spec read(string()) -> perc:defs() | no_return().
read(Filename) ->
    Forms =
        case epp:parse_file(Filename, []) of
            {ok, ParsedForms} ->
                ParsedForms;
            {ok, ParsedForms, _} ->
                ParsedForms;
            {error, enoent} ->
                throw({file_not_found, Filename})
        end,
    analyse_forms(Forms).

-spec read_all([string()]) -> perc:defs() | no_return().
read_all(Filenames) ->
    perc:merge_defs([read(Filename) || Filename <- Filenames]).

%%====================================================================
%% Internal functions
%%====================================================================

get_record_defs(Forms) ->
    RecForms = lists:filter(fun is_record_form/1, Forms),
    [analyse_record_form(RecForm) || RecForm <- RecForms ].

get_usertype_defs(Forms) ->
    UserTypeForms = lists:filter(fun is_usertype_form/1, Forms),
    [analyse_usertype_form(Form) || Form <- UserTypeForms].

analyse_record_form(Form) ->
    [NameTree, FieldsTree] = erl_syntax:attribute_arguments(Form),
    Name = erl_syntax:atom_name(NameTree),
    Fields =
        [analyse_record_field(FieldTree)
         || FieldTree <- erl_syntax:tuple_elements(FieldsTree)],
    perc_types:make_record_def(Name, Fields).

analyse_usertype_form(Form) ->
    %% erl_syntax does not recognise type attributes well;
    %% Use the abstract form directly
    {attribute, _, type, {TypeNameAtom, TypeTree, _Params}} = Form,
    perc_types:make_usertype_def(
      atom_to_list(TypeNameAtom),
      analyse_typetree(TypeTree)
     ).

analyse_record_field(Field) ->
    Name =
        erl_syntax:atom_name(
           erl_syntax:record_field_name(
              erl_syntax:typed_record_field_body(Field) %% TODO handle untyped
             )
          ),
    Type =
        analyse_typetree(
          erl_syntax:typed_record_field_type(Field)
         ),
    perc_types:make_record_field(Name, Type).

%% TODO support the full list :
%% http://erlang.org/doc/reference_manual/typespec.html
analyse_typetree(Type) ->
    case erl_syntax:type(Type) of
        type_union ->
            perc_types:make_union(
              [analyse_typetree(Sub)
               || Sub <- erl_syntax:type_union_types(Type)]
             );
        record_type ->
            perc_types:make_record(
              erl_syntax:atom_name(erl_syntax:record_type_name(Type))
             );
        user_type_application ->
            perc_types:make_usertype(
              erl_syntax:atom_name(
                erl_syntax:user_type_application_name(Type)
               )
             );
        type_application ->
            analyse_type_application(Type);
        list ->
            perc_types:make_list(
              analyse_typetree(
                erl_syntax:list_head(Type) %% TODO handle [] (nil)
               )
             );
        tuple_type ->
            analyse_tuple(Type);
        integer ->
            perc_types:make_basic(integer);
        integer_range_type ->
            perc_types:make_basic(integer);
        atom ->
            analyse_atom(Type);
        Else ->
            perc_types:make_ignored({unknown_type_syntax, Else})
    end.

analyse_atom(Type) ->
    case erl_syntax:atom_value(Type) of
        true -> perc_types:make_basic(boolean);
        false -> perc_types:make_basic(boolean);
        undefined -> perc_types:make_basic(undefined_atom);
        _Else -> perc_types:make_basic(atom)
    end.

analyse_type_application(Type) ->
    TypeApplicationName =
        erl_syntax:atom_value(erl_syntax:type_application_name(Type)),
    case TypeApplicationName of
        atom -> perc_types:make_basic(atom);
        string -> perc_types:make_basic(string);
        binary -> perc_types:make_basic(binary);
        integer -> perc_types:make_basic(integer);
        non_neg_integer -> perc_types:make_basic(integer);
        pos_integer -> perc_types:make_basic(integer);
        neg_integer -> perc_types:make_basic(integer);
        float -> perc_types:make_basic(float);
        number -> perc_types:make_union([{basic, integer}, {basic, float}]);
        boolean -> perc_types:make_basic(boolean);
        list -> analyse_list_type_application(Type);
        Else -> perc_types:make_ignored({unknown_type_application, Else})
    end.

analyse_list_type_application(Type) ->
    case erl_syntax:type_application_arguments(Type) of
        [] -> perc_types:make_ignored({incomplete_type, Type});
        List -> perc_types:make_list(analyse_typetree(hd(List)))
    end.

analyse_tuple(Type) ->
    case erl_syntax:tuple_type_elements(Type) of
        any_size ->
            perc_types:make_ignored({incomplete_type, Type});
        Types ->
            perc_types:make_tuple([analyse_typetree(Sub) || Sub <- Types])
    end.

is_record_form(Form) ->
    try erl_syntax:atom_literal(erl_syntax:attribute_name(Form)) of
        "record" -> true;
        _ -> false
    catch
        error:{badarg, _} -> false
    end.

is_usertype_form(Form) ->
    try erl_syntax:atom_literal(erl_syntax:attribute_name(Form)) of
        "type" -> true;
        _ -> false
    catch
        error:{badarg, _} -> false
    end.
