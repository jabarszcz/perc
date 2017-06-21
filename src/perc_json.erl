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
    HeaderFormat =
        "template <>\n"
        "int json_encoder<Record<~s>>::encode("
        "struct encoder *e, const ERL_NIF_TERM term)",
    Header =
        io_lib:format(HeaderFormat,
                      [perc_backend:id_from_name(Record#record_def.name)]),
    BodyFormat =
        "    int first=1, arity;\n"
        "    const ERL_NIF_TERM *fields;\n"
        "    if (!enif_get_tuple(e->env, term, &arity, &fields)) {\n"
        "        enif_make_badarg(e->env);\n"
        "        return 0;\n"
        "    }\n"
        "    json_enc_begin_obj(e);\n"
        "~s"
        "    json_enc_end_obj(e);\n"
        "    return 1;\n",
    Body = io_lib:format(
             BodyFormat,
             [[field_code(Field, Index) ||
                    {Field, Index} <- enumerate(Record#record_def.fields)]]),
    [Header, "{\n", Body, "}\n"].

gen_usertype_enc_func(#user_type_def{name=Name, type=Type}) ->
    HeaderFormat =
        "template <>\n"
        "inline\n"
        "int json_encoder<UserType<~s>>::encode("
        "struct encoder *e, const ERL_NIF_TERM term)",
    Header = io_lib:format(HeaderFormat, [perc_backend:id_from_name(Name)]),
    Body = ["return ", translation_code(Type, "term")],
    [Header, "{\n", Body, "}\n"].

field_code(#record_field{type=ignored, name=Name}, Index) ->
    io_lib:format("    // Field ~s (~B) ignored\n", [Name, Index]);
field_code(#record_field{type={ignored, Reason}, name=Name}, Index) ->
    io_lib:format("    // Field ~s (~B) ignored : ~w\n", [Name, Index, Reason]);
field_code(#record_field{type={maybe, Type}, name=Name}, Index) ->
    io_lib:format(
      "    if (!is_undefined(e, fields[~B])) {\n    ~s    }\n",
      [Index, field_code(#record_field{type=Type, name=Name}, Index)]);
field_code(#record_field{type=Type, name=Name}, Index) ->
    [io_lib:format("    JSON_ENC_KEY(e, first, \"~s\");\n", [Name]),
     translation_code(Type, io_lib:format("fields[~B]", [Index]))].


translation_code(Type, TermString) ->
    io_lib:format("    json_encode<~s>(e, ~s);\n", [template(Type), TermString]).

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
    io_lib:format("Record<~s>", [string:to_upper(RecordName)]);
template({user_type, {UserTypeName, _}}) ->
    io_lib:format("UserType<~s>", [string:to_upper(UserTypeName)]).

enumerate(List) ->
    lists:zip(List, lists:seq(1, length(List))).

capitalize([H|T]) ->
    [string:to_upper(H)|T].
