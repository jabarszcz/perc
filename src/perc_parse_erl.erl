-module(perc_parse_erl).

%% API exports
-export([
    read/1,
    read/2,
    read_all/1,
    read_all/2
  ]).


%%====================================================================
%% API functions
%%====================================================================

-spec read(string()) -> perc:defs() | no_return().
read(Filename) ->
    read(Filename, undefined).

-spec read(string(), string() | undefined) -> perc:defs() | no_return().
read(Filename, CodecName) ->
    Forms =
        case epp:parse_file(Filename, []) of
            {ok, ParsedForms} ->
                ParsedForms;
            {ok, ParsedForms, _} ->
                ParsedForms;
            {error, enoent} ->
                throw({file_not_found, Filename})
        end,
    Comments = erl_comment_scan:file(Filename),
    Recommented = erl_recomment:recomment_forms(Forms, Comments),
    analyse_forms(Recommented, CodecName).

-spec read_all([string()]) -> perc:defs() | no_return().
read_all(Filenames) ->
    read_all(Filenames, undefined).

-spec read_all([string()], string() | undefined) -> perc:defs() | no_return().
read_all(Filenames, CodecName) ->
    perc:merge_defs([read(Filename, CodecName) || Filename <- Filenames]).

%%====================================================================
%% Internal functions
%%====================================================================

-spec analyse_forms(
        erl_syntax:syntaxtree(),
        string() | undefined
       ) -> perc:defs().
analyse_forms(FormList, CodecName) ->
    perc:make_defs(
      get_record_defs(FormList, CodecName),
      get_usertype_defs(FormList)
     ).

get_record_defs(FormList, CodecName) ->
    RecForms =
        lists:filter(
          fun is_record_form/1,
          erl_syntax:form_list_elements(FormList)
         ),
    [analyse_record_form(RecForm, CodecName) || RecForm <- RecForms ].

get_usertype_defs(FormList) ->
    UserTypeForms =
        lists:filter(
          fun is_usertype_form/1,
          erl_syntax:form_list_elements(FormList)
         ),
    [analyse_usertype_form(Form) || Form <- UserTypeForms].

analyse_record_form(Form, CodecName) ->
    [NameTree, FieldsTree] = erl_syntax:attribute_arguments(Form),
    Name = erl_syntax:atom_name(NameTree),
    Fields =
        [analyse_record_field(FieldTree, CodecName)
         || FieldTree <- erl_syntax:tuple_elements(FieldsTree)],
    perc_types:make_record_def(Name, Fields).

analyse_usertype_form(Form) ->
    %% erl_syntax does not recognise type attributes well;
    %% Use the abstract form directly
    {attribute, _, _, {TypeNameAtom, TypeTree, _Params}} = Form,
    perc_types:make_usertype_def(
      atom_to_list(TypeNameAtom),
      analyse_typetree(TypeTree)
     ).

analyse_comments(Tree, CodecName) ->
    CommentLines =
        [strip_comment(T) || C <- erl_syntax:get_precomments(Tree)
                                 ++ erl_syntax:get_postcomments(Tree),
                             T <- erl_syntax:comment_text(C)],
    Comment = string:join(CommentLines, "\n"),
    %% return the type, filters, and the codec names to which it applies
    case extract_perc_comment_annotation(Comment) of
        {match, AnnotationsStrings} ->
            analyse_annotations(
              lists:map(fun hd/1, AnnotationsStrings),
              CodecName
             );
        nomatch ->
            [] % empty annotations
    end.

strip_comment(String) ->
    %% Remove extra percent signs (%) at beginning
    string:strip(String, left, $%).

extract_perc_comment_annotation(String) ->
    %% TODO line number?
    re:run(String, "#perc#([^;]*);", [{capture, [1], list}, global]).

analyse_annotations(AnnotationStrings, CodecName) ->
    Annotations =
        lists:append(
          [distribute_over_codecs(
             analyse_annotation(String)
            )
           || String <- AnnotationStrings]
         ),
    choose_annotations(Annotations, CodecName).

choose_annotations(Annotations, CodecName) ->
    General = proplists:get_all_values(undefined, Annotations),
    Specific =
        case CodecName of
            undefined ->
                [];
            Searched ->
                proplists:get_all_values(Searched, Annotations)
        end,
    %% Specific annotations come before for higher priority
    lists:append(Specific ++ General).

distribute_over_codecs({Names, Props}) ->
    case Names of
        undefined ->
            [{undefined, Props}];
        _ ->
            [{Name, Props} || Name <- Names]
    end.

analyse_annotation(String) ->
    case perc_scanner:string(String) of
        {ok, Toks, _EndLine} ->
            case perc_rparser:parse_annotation(Toks) of
                {ok, Parsed} ->
                    Parsed;
                {error, ParseErrorLine, ParseReason} ->
                    io:format(
                      "Parse error: (line ~B) ~s~n",
                      [ParseErrorLine, perc_rparser:format_error(ParseReason)]
                     ),
                    [] % empty annotations
            end;
        {error, {ErrorLine, _Module, Reason}, _} ->
            io:format(
              "Scan error: (line ~B) ~s~n",
              [ErrorLine, perc_scanner:format_error(Reason)]
             ),
            [] % empty annotations
    end.

analyse_record_field(FieldTree, CodecName) ->
    CommentInfo = analyse_comments(FieldTree, CodecName),
    Field1 =
        case erl_syntax:type(FieldTree) of
            typed_record_field ->
                Name =
                    erl_syntax:atom_name(
                      erl_syntax:record_field_name(
                        erl_syntax:typed_record_field_body(FieldTree)
                       )
                     ),
                Type =
                    analyse_typetree(
                      erl_syntax:typed_record_field_type(FieldTree)
                     ),
                perc_types:make_record_field(Name, Type);
            record_field ->
                Name =
                    erl_syntax:atom_name(
                      erl_syntax:record_field_name(FieldTree)
                     ),
                perc_types:make_record_field(
                  Name,
                  perc_types:make_ignored(untyped_field)
                 )
        end,
    Field2 =
        case proplists:get_value(type, CommentInfo) of
            undefined ->
                Field1;
            TypeVal ->
                perc_types:set_record_field_type(Field1, TypeVal)
        end,
    Field3 =
        case proplists:get_all_values(filters, CommentInfo) of
            [] ->
                Field2;
            Filters ->
                perc_types:set_record_field_filters(
                  Field2,
                  lists:append(Filters)
                 )
        end,
    Field3.

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
        undefined -> perc_types:make_undefined_atom();
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
        error:{badarg, _} -> false;
        error:{badrecord, _} -> false
    end.

is_usertype_form(Form) ->
    try erl_syntax:atom_literal(erl_syntax:attribute_name(Form)) of
        "type" -> true;
        "opaque" -> true;
        _ -> false
    catch
        error:{badarg, _} -> false;
        error:{badrecord, _} -> false
    end.
