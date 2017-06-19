-module(perc_json).
-behaviour(perc_backend).
-export([name/0,
         gen_record_enc_func/1,
         gen_record_enc_func_header/1,
         gen_usertype_enc_func/1,
         gen_usertype_enc_func_header/1
        ]).

-include("perc_types.hrl").

-spec name() -> string().
name() ->
    "json".

gen_record_enc_func_header(Record) ->
    HeaderFormat =
        "int ~s(struct encoder *e, const ERL_NIF_TERM term);",
    io_lib:format(HeaderFormat,
                  [perc_backend:get_enc_func_name(
                     perc_json,
                     "encode",
                     Record#record_def.name)]).

gen_record_enc_func(Record) ->
    HeaderFormat =
        "int ~s(struct encoder *e, const ERL_NIF_TERM term)",
    Header = io_lib:format(HeaderFormat,
                           [perc_backend:get_enc_func_name(
                              perc_json,
                              "encode",
                              Record#record_def.name)]),
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

gen_usertype_enc_func_header(#user_type_def{name=Name}) ->
    HeaderFormat =
        "int ~s(struct encoder *e, const ERL_NIF_TERM term);",
    io_lib:format(
      HeaderFormat,
      [perc_backend:get_utype_enc_func_name(perc_json, "encode", Name)]
     ).

gen_usertype_enc_func(#user_type_def{name=Name, type=Type}) ->
    HeaderFormat =
        "int ~s(struct encoder *e, const ERL_NIF_TERM term)",
    Header = io_lib:format(
      HeaderFormat,
      [perc_backend:get_utype_enc_func_name(perc_json, "encode", Name)]
     ),
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
    io_lib:format("    ~s(e, ~s);\n", [encode_func(Type), TermString]).

encode_func({basic, Basic}) ->
    io_lib:format("json_enc_~s", [atom_to_list(Basic)]);
encode_func({maybe, Type}) ->
    io_lib:format("enc_maybe<~s>", [encode_func(Type)]);
encode_func({list, Type}) ->
    io_lib:format("json_enc_list<~s>", [encode_func(Type)]);
encode_func({tuple, Types}) ->
    io_lib:format("json_enc_tuple<~s>",
                  [string:join([encode_func(Type) || Type <- Types], ",")]);
encode_func({union, Types}) ->
    io_lib:format("enc_union<~s>",
                  [string:join([encode_func(Type) || Type <- Types], ",")]);
encode_func({record, RecordName}) ->
    perc_backend:get_enc_func_name(perc_json, "encode", RecordName);
encode_func({user_type, {UserTypeName, _}}) -> %% TODO args
    perc_backend:get_utype_enc_func_name(perc_json, "encode", UserTypeName).

enumerate(List) ->
    lists:zip(List, lists:seq(1, length(List))).
