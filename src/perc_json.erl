-module(perc_json).
-behaviour(perc_backend).
-export([name/0,
         generate_encode_function/1,
         generate_encode_function_header/1]).

-include("perc_types.hrl").

-spec name() -> string().
name() ->
    "json".

generate_encode_function_header(Record) ->
    HeaderFormat =
        "int ~s(struct encoder *e, const ERL_NIF_TERM term);",
    io_lib:format(HeaderFormat,
                  [perc_backend:get_enc_func_name(
                     perc_json,
                     "encode",
                     Record#record_def.name)]).

generate_encode_function(Record) ->
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

field_code(#record_field{type=ignored, name=Name}, Index) ->
    io_lib:format("    // Field ~s (~B) ignored\n", [Name, Index]);
field_code(#record_field{type={ignored, Reason}, name=Name}, Index) ->
    io_lib:format("    // Field ~s (~B) ignored : ~w\n", [Name, Index, Reason]);
field_code(#record_field{type={maybe, Type}, name=Name}, Index) ->
    io_lib:format(
      "    if (!is_undefined(e, fields[~B])) {\n    ~s    }\n",
      [Index, field_code(#record_field{type=Type, name=Name}, Index)]);
field_code(#record_field{type={tuple, Types}, name=Name}, Index) ->
    io_lib:format("    // Tuple encoding not implemented (types ~p)\n",
                  [Types]); %% TODO
field_code(#record_field{type=Type, name=Name}, Index) ->
    [io_lib:format("    JSON_ENC_KEY(e, first, \"~s\");\n", [Name]),
     translation_code(Type, io_lib:format("fields[~B]", [Index]))].


translation_code({list, Type}, TermString) ->
    case Type of
        ignored -> erlang:throw(badarg);
        {maybe, _} -> erlang:throw(badarg);
        {list, _} -> erlang:throw(nested_list_unimplemented);
        {tuple, _} -> erlang:throw(tuple_list_unimplemented);
        Else -> io_lib:format("    json_enc_list(e, ~s, ~s);\n",
                              [TermString, encode_func(Else)])
    end;
translation_code(Type, TermString) ->
    io_lib:format("    ~s(e, ~s);\n", [encode_func(Type), TermString]).

encode_func({record, RecordName}) ->
    perc_backend:get_enc_func_name(perc_json, "encode", RecordName);
encode_func({basic, Basic}) ->
    io_lib:format("json_enc_~s", [atom_to_list(Basic)]);
encode_func({union, Types}) ->
    io_lib:format("// json_enc_union_<~s>",
                  [string:join([encode_func(Type) || Type <- Types], ",")]).

enumerate(List) ->
    lists:zip(List, lists:seq(1, length(List))).
