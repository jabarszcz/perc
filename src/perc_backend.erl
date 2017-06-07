-module(perc_backend).

-export([get_nif_name/3,
         get_enc_func_name/3,
         generate_nif_source/2]).

-include("perc.hrl").
-include("perc_types.hrl").

-callback name() -> string().
-callback generate_encode_function(#record_def{}) -> iolist().
-callback generate_encode_function_header(#record_def{}) -> iolist().

generate_nif_source(Module, RecordMap) ->
    Include =
        "#include \"erl_nif.h\"\n"
        "#include \"perc_encode.h\"\n",
    Signatures =
        [ [generate_encode_function_header(Backend, Record), "\n"] ||
            Backend <- Module#nif_module.backends,
            Record <- [ dict:fetch(RecName, RecordMap) ||
                          RecName <- Module#nif_module.all_records ]],
    EncodeFuncs =
        [ [generate_encode_function(Backend, Record), "\n"] ||
            Backend <- Module#nif_module.backends,
            Record <- [ dict:fetch(RecName, RecordMap) ||
                          RecName <- Module#nif_module.all_records ]],
    Nifs = [ [generate_encode_nif(Backend, Record), "\n"] ||
               Backend <- Module#nif_module.backends,
               Record <- [ dict:fetch(RecName, RecordMap) ||
                             RecName <- Module#nif_module.exported_records ]],
    string:join([Include,
                 Signatures,
                 EncodeFuncs,
                 Nifs,
                 generate_nif_init(Module)],
                "\n").

get_nif_name(Backend, Action, RecordName) ->
    [Backend:name(), "_", Action, "_", RecordName, "_nif"].

get_enc_func_name(Backend, Action, RecordName) ->
    [Backend:name(), "_", Action, "_", RecordName].

generate_encode_nif(Backend, Record) ->
    RecordName = Record#record_def.name,
    NifName = get_nif_name(Backend, "encode", RecordName),
    EncFuncName = get_enc_func_name(Backend, "encode", RecordName),
    Header =
        io_lib:format(
          "static ERL_NIF_TERM "
          "~s(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])~n",
          [NifName]),
    BodyFormat =
        "    struct encoder e;\n"
        "    if (argc != 1)\n"
        "        return enif_make_badarg(env);\n"
        "    if (!encoder_init(env, &e, 64))\n"
        "        return enif_make_atom(env, \"errorb\");\n"
        "    if (!~s(&e, argv[0]))\n"
        "        return enif_make_atom(env, \"errorc\");\n"
        "    return encoder_binary(&e);\n",
    Body = io_lib:format(BodyFormat, [EncFuncName]),
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

generate_encode_function(Backend, Record) ->
    Backend:generate_encode_function(Record).

generate_encode_function_header(Backend, Record) ->
    Backend:generate_encode_function_header(Record).
