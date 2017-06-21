-module(perc_backend).

-export([get_nif_name/3,
         get_enc_func_name/3,
         get_utype_enc_func_name/3,
         generate_nif_source/2,
         id_from_name/1]).

-include("perc.hrl").
-include("perc_types.hrl").

-callback name() -> string().
-callback gen_record_enc_func(#record_def{}) -> iolist().
-callback gen_usertype_enc_func(#user_type_def{}) -> iolist().

generate_nif_source(Module, RecordMap) ->
    Include =
        "#include \"erl_nif.h\"\n"
        "#include \"perc_json.hpp\"\n", %% Todo generalize
    EnumIds = io_lib:format(
                "enum Id : unsigned int {\n~s\n};\n",
                [string:join(
                   [id_from_name(Name) || Name <- Module#nif_module.all_records]
                   ++ [id_from_name(UType#user_type_def.name)
                       || UType <- Module#nif_module.user_types] ,
                   ",\n")]),
    REncodeFuncs =
        [ [Backend:gen_record_enc_func(Record), "\n"] ||
            Backend <- Module#nif_module.backends,
            Record <- [ dict:fetch(RecName, RecordMap) ||
                          RecName <- Module#nif_module.all_records ]],
    UEncodeFuncs =
        [ [Backend:gen_usertype_enc_func(UserType), "\n"] ||
            Backend <- Module#nif_module.backends,
            UserType <- Module#nif_module.user_types ],
    Nifs = [ [generate_encode_nif(Backend, Record), "\n"] ||
               Backend <- Module#nif_module.backends,
               Record <- [ dict:fetch(RecName, RecordMap) ||
                             RecName <- Module#nif_module.exported_records ]],
    string:join([Include,
                 EnumIds,
                 REncodeFuncs,
                 UEncodeFuncs,
                 Nifs,
                 generate_nif_init(Module)],
                "\n").

get_nif_name(Backend, Action, RecordName) ->
    [Backend:name(), "_", Action, "_", RecordName, "_nif"].

get_enc_func_name(Backend, Action, RecordName) ->
    [Backend:name(), "_", Action, "<Record<", id_from_name(RecordName), ">>"].

get_utype_enc_func_name(Backend, Action, UserTypeName) ->
    [Backend:name(), "_", Action, "<UserType<", id_from_name(UserTypeName), ">>"].

generate_encode_nif(Backend, Record) ->
    RecordName = Record#record_def.name,
    NifName = get_nif_name(Backend, "encode", RecordName),
    Header =
        io_lib:format(
          "ERL_NIF_TERM "
          "~s(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])~n",
          [NifName]),
    BodyFormat =
        "    struct encoder e;\n"
        "    if (argc != 1)\n"
        "        return enif_make_badarg(env);\n"
        "    if (!encoder_init(env, &e, 64))\n"
        "        return enif_make_atom(env, \"error\");\n"
        "    if (!json_encode<Record<~s>>(&e, argv[0]))\n"
        "        return enif_make_badarg(env);\n"
        "    return encoder_binary(&e);\n",
    Body = io_lib:format(BodyFormat, [id_from_name(RecordName)]),
    [Header, "{\n", Body, "}\n"].

generate_nif_init(Module) ->
    NifFuncsFormat =
        "static ErlNifFunc nif_funcs[] = {\n"
        "~s~n"
        "};\n",
    NifFuncDefs = [ io_lib:format("    {\"~s\", 1, ~s}", [Func, Func]) ||
                      Func <- [ get_nif_name(Backend, "encode", RecName) ||
                                  RecName <- Module#nif_module.exported_records,
                                  Backend <- Module#nif_module.backends ]],
    NifFuncs = io_lib:format(NifFuncsFormat,
                             [string:join(NifFuncDefs, ",\n")]),
    Init =io_lib:format(
            "ERL_NIF_INIT(~s, nif_funcs, NULL, NULL, NULL, NULL)~n",
            [Module#nif_module.name]),
    [NifFuncs, Init].

id_from_name(Str) ->
    string:to_upper(Str).
