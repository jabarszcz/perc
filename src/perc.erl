-module(perc).

%% API exports
-export([
    main/1,
    generate_codecs/1,
    get_gen_erl_out/1,
    get_gen_sopath/1,
    get_gen_appname/1,
    get_gen_inputs/1,
    get_gen_exported/1,
    get_gen_record_defs/1,
    get_gen_usertype_defs/1,
    get_gen_backends/1,
    set_gen_defs/2,
    make_defs/2,
    merge_defs/1,
    get_defs_records/1,
    get_defs_usertypes/1,
    get_optspec/0
  ]).

-export_type([
    option/0,
    defs/0,
    generator/0
  ]).

%%====================================================================
% API Types
%%====================================================================

-type option() :: {erl_out, string()}
                | {cpp_out, string()}
                | {appname, string()}
                | {in, string()}
                | {in_dir, string()}
                | {cpp_dir, string()}
                | {erl_dir, string()}
                | {record, string()}
                | {backend, string()}
                | {cpp_flags, string()}
                | compile | {compile, boolean()}
                | beam | {beam, boolean()}
                | load | {load, boolean()}
                | so | {so, boolean()}
                | graph | {graph, boolean()}.

-record(defs, {
          records = [] :: [perc_types:record_def()],
          usertypes = [] :: [perc_types:usertype_def()]
         }).

-opaque defs() :: #defs{}.

-record(generator, {
    inputs :: [string()],
    exported :: [perc_types:perc_type()],
    defs :: defs(),
    backends :: [atom()],
    opts :: [option()]
 }).

-opaque generator() :: #generator{}.

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
-spec main([string()]) -> no_return().
main(Args) ->
    OptSpec = get_optspec(),
    case getopt:parse(OptSpec, Args) of
        {ok, {Options, []}} ->
            Res = generate_codecs(Options),
            io:format("~s~n", [Res]),
            erlang:halt(0);
        {error, Reason} ->
            io:format(standard_error,
                      "Bad arguments : ~p~n",
                      [Reason]),
            getopt:usage(OptSpec, "perc"),
            erlang:halt(1);
        _ ->
            getopt:usage(OptSpec, "perc"),
            erlang:halt(1)
    end.

-spec generate_codecs([option()]) -> ok | no_return().
generate_codecs(Options) ->
    Gen = gen_from_options(Options),
    case Gen#generator.exported of
        [] ->
            ok;
        _ ->
            Reduced = perc_reduce:reduce(Gen),
            generate_nif(Reduced),
            generate_erl(Reduced),
            case proplists:get_bool(graph, Options) of
                true -> save_graph(Reduced, "type_graph.png", "png");
                _ -> ok
            end,
            ok
    end.

-spec get_gen_erl_out(generator()) -> string().
get_gen_erl_out(Gen) ->
    proplists:get_value(erl_out, Gen#generator.opts, "generated").

-spec get_gen_sopath(generator()) -> string().
get_gen_sopath(Gen) ->
    Opts = Gen#generator.opts,
    AppName = perc:get_gen_appname(Gen),
    Dir = proplists:get_value(cpp_dir, Opts, "./priv"),
    SoName = proplists:get_value(cpp_out, Opts, "generated"), % without ext
    case AppName of
        undefined ->
            filename:join(Dir, SoName);
        _ ->
            AppAtom = list_to_atom(AppName),
            Root = code:lib_dir(AppAtom),
            filename:join([Root, Dir, SoName])
    end.

-spec get_gen_appname(generator()) -> string() | undefined.
get_gen_appname(Gen) ->
    proplists:get_value(appname, Gen#generator.opts).

-spec get_gen_inputs(generator()) -> [string()].
get_gen_inputs(Gen) ->
    Gen#generator.inputs.

-spec get_gen_exported(generator()) -> [perc_types:perc_type()].
get_gen_exported(Gen) ->
    Gen#generator.exported.

-spec get_gen_record_defs(generator()) -> [perc_types:record_def()].
get_gen_record_defs(Gen) ->
    Gen#generator.defs#defs.records.

-spec get_gen_usertype_defs(generator()) -> [perc_types:usertype_def()].
get_gen_usertype_defs(Gen) ->
    Gen#generator.defs#defs.usertypes.

-spec get_gen_backends(generator()) -> [atom()].
get_gen_backends(Gen) ->
    Gen#generator.backends.

-spec set_gen_defs(generator(), defs()) -> generator().
set_gen_defs(Gen, Defs) ->
    Gen#generator{defs=Defs}.

-spec make_defs(
        [perc_types:record_defs()],
        [perc_types:usertype_defs()]
       ) -> defs().
make_defs(RecordDefs, UserTypeDefs) ->
    #defs{
       records=RecordDefs,
       usertypes=UserTypeDefs
      }.

-spec merge_defs([defs()]) -> defs().
merge_defs(DefsList) ->
    {RecordDefsLists, UserTypeDefsLists} =
        lists:unzip(
          [{RecDefs, UserDefs}
           || #defs{records=RecDefs,
                    usertypes=UserDefs} <- DefsList]
         ),
    RecordDefs = lists:append(RecordDefsLists),
    UserTypeDefs = lists:append(UserTypeDefsLists),
    #defs{records=RecordDefs,
          usertypes=UserTypeDefs}.

-spec get_defs_records(defs()) -> [perc_types:record_def()].
get_defs_records(Defs) ->
    Defs#defs.records.

-spec get_defs_usertypes(defs()) -> [perc_types:usertype_def()].
get_defs_usertypes(Defs) ->
    Defs#defs.usertypes.

-spec get_optspec() -> [{atom(),
                         integer(),
                         string(),
                         atom() | tuple(),
                         string()}].
get_optspec() ->
    [
     {erl_out, $e, "erl-out", {string, "generated"},
      "The generated erlang module name"},
     {cpp_out, $c, "cpp-out", {string, "generated"},
      "The generated cpp file name (without ext.)"},
     {in, $i , "in", string,
      "An erlang file containing type and record definitions"},
     {in_dir, undefined, "in-dir", {string, "."},
      "The directory where the input files are"},
     {cpp_dir, undefined, "cpp-dir", {string, "."},
      "Where to put the cpp/so generated nif"},
     {erl_dir, undefined, "erl-dir", {string, "."},
      "Where to put the generated erlang module"},
     {record, $r, "record", string,
      "The records for which we want an encoding function"},
     {usertype, $u, "usertype", string,
      "The user types for which we want an encoding function"},
     {backend, $b, "backend", {string, "json"},
      "The codec backends (json, etc.)"},
     {cpp_flags, undefined, "cpp-flags", string,
      "Flags to pass to the c++ compiler if compiling"},
     {compile, $C, "compile", boolean,
      "Compile both the generated module and nif"},
     {beam, undefined, "beam", boolean,
      "Compile the erlang module"},
     {so, undefined, "so", boolean,
      "Compile the generated nif to a shared library"},
     {graph, $g, "graph", boolean,
      "Save the type graph"}
    ].

%%====================================================================
%% Internal functions
%%====================================================================

gen_from_options(Opts) ->
    Inputs = proplists:get_all_values(in, Opts),
    RecordNames = proplists:get_all_values(record, Opts),
    UserTypeNames = proplists:get_all_values(usertype, Opts),
    Backends =
        case lists:usort(proplists:get_all_values(backend, Opts)) of
            [] -> ["json"];
            List -> List
        end,
    Records =
        [perc_types:make_record(Name) || Name <- RecordNames],
    UserTypes =
        [perc_types:make_usertype(Name) || Name <- UserTypeNames],
    Exported = Records ++ UserTypes,
    Defs = perc_parse_erl:read_all(Inputs),
    #generator{
       inputs=Inputs,
       exported=Exported,
       defs=Defs,
       backends=[perc_backend:backend_from_name(B) || B <- Backends],
       opts=Opts
      }.

save_graph(Gen, Filename, Format) ->
    RecordDefs = perc:get_gen_record_defs(Gen),
    UserTypeDefs = perc:get_gen_usertype_defs(Gen),
    Graph = perc_digraph:make(RecordDefs, UserTypeDefs),
    Ret = perc_digraph:save(Graph, Filename, Format),
    perc_digraph:delete(Graph),
    Ret.

-spec generate_nif(generator()) -> ok | no_return().
generate_nif(Gen) ->
    Opts = Gen#generator.opts,
    CppStr = perc_backend:generate_nif_source(Gen),
    SoPath = perc:get_gen_sopath(Gen),
    ok = filelib:ensure_dir(SoPath),
    Load = proplists:get_bool(load, Opts),
    Compile =
        proplists:get_bool(compile, Opts) or
        proplists:get_bool(so, Opts) or
        Load,
    case Compile of
        true ->
            TempFile = lib:nonl(os:cmd("mktemp --suffix '.cpp'")),
            SoFile = io_lib:format("~s.so", [SoPath]),
            ErlNifIncludeDir = filename:join(code:root_dir(), "usr/include"),
            CIncludeDir = code:priv_dir(perc),
            Flags = proplists:get_value(cpp_flags, Opts, ""),
            Cmd = io_lib:format(
                    "g++ -fvisibility=hidden -nodefaultlibs "
                    "-o ~s -fpic -shared ~s -I ~s -I ~s ~s",
                    [SoFile, TempFile, ErlNifIncludeDir, CIncludeDir, Flags]
                   ),
            ok = file:write_file(TempFile, CppStr),
            io:format("~s~n", [Cmd]), %% TODO remove
            case os:cmd(Cmd) of
                [] ->
                    ok;
                Error ->
                    BinError = unicode:characters_to_binary(Error),
                    erlang:error({cpp_compiler_output, BinError})
            end;
        false ->
            File = io_lib:format("~s.cpp", [SoPath]),
            ok = file:write_file(File, CppStr)
    end.

-spec generate_erl(generator()) -> ok | no_return().
generate_erl(Gen) ->
    ErlForms = perc_erl_gen:generate(Gen),
    ErlDir = proplists:get_value(erl_dir, Gen#generator.opts, "."),
    filelib:ensure_dir(ErlDir),
    Load = proplists:get_bool(load, Gen#generator.opts),
    Compile =
        proplists:get_bool(compile, Gen#generator.opts) or
        proplists:get_bool(beam, Gen#generator.opts) or
        Load,
    case Compile of
        true ->
            {Module, Binary} =
                case compile:forms(ErlForms) of
                    {ok, ModuleName, Bin} when is_binary(Bin) ->
                        {ModuleName, Bin};
                    {ok, ModuleName, Bin, _Warn} when is_binary(Bin) ->
                        {ModuleName, Bin};
                    Error ->
                        erlang:error({not_a_binary, Error})
                end,
            File = filename:join(
                     ErlDir,
                     io_lib:format("~s.beam", [Module])
                    ),
            ok = file:write_file(File, Binary),
            case Load of
                true ->
                    {module, _} = code:load_abs(filename:join(ErlDir, Module)),
                    ok;
                _ ->
                    ok
            end;
        false ->
            Output = erl_prettypr:format(erl_syntax:form_list(ErlForms)),
            ModuleName = get_gen_erl_out(Gen),
            File = filename:join(ErlDir, io_lib:format("~s.erl", [ModuleName])),
            ok = file:write_file(File, Output)
    end.
