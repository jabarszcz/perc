-module(perc_opts).

-export([
    get_appname/1,
    get_backends/1,
    get_codec_name/1,
    get_cpp_flags/1,
    get_erl_dir/1,
    get_erl_out/1,
    get_exported/1,
    get_includes/1,
    get_inputs/1,
    get_schema/1,
    get_sopath/1,
    has_compile_cpp/1,
    has_compile_erl/1,
    has_force/1,
    has_graph/1,
    has_load/1,
    optspec/0,
    optspec_nodefaults/0
  ]).

-export_type([
    option/0,
    options/0
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
                | graph | {graph, boolean()}
                | {schema, string()}
                | force | {force, boolean()}
                | absolute | {absolute, boolean()}.

-type options() :: [option()].

%%====================================================================
%% API functions
%%====================================================================

-spec get_appname(options()) -> string() | undefined.
get_appname(Opts) ->
    proplists:get_value(appname, Opts).

-spec get_backends(options()) -> [atom()].
get_backends(Opts) ->
    Backends =
        case lists:usort(proplists:get_all_values(backend, Opts)) of
            [] -> ["json"];
            List -> List
        end,
    [perc_backend:backend_from_name(B) || B <- Backends].

-spec get_codec_name(options()) -> [atom()].
get_codec_name(Opts) ->
    proplists:get_value(name, Opts).

-spec get_cpp_flags(options()) -> string().
get_cpp_flags(Opts) ->
    string:join(proplists:get_all_values(cpp_flags, Opts), " ").

-spec get_erl_dir(options()) -> string().
get_erl_dir(Opts) ->
    proplists:get_value(erl_dir, Opts, ".").

-spec get_erl_out(options()) -> string().
get_erl_out(Opts) ->
    proplists:get_value(erl_out, Opts, "generated").

-spec get_exported(options()) -> [perc_types:perc_type()].
get_exported(Opts) ->
    RecordNames = proplists:get_all_values(record, Opts),
    UserTypeNames = proplists:get_all_values(usertype, Opts),
    Records =
        [perc_types:make_record(Name) || Name <- RecordNames],
    UserTypes =
        [perc_types:make_usertype(Name) || Name <- UserTypeNames],
    Records ++ UserTypes.

-spec get_includes(options()) -> [string()].
get_includes(Opts) ->
    proplists:get_all_values(include, Opts).

-spec get_inputs(options()) -> [string()].
get_inputs(Opts) ->
    proplists:get_all_values(in, Opts).

-spec get_schema(options()) -> string() | undefined.
get_schema(Opts) ->
    proplists:get_value(schema, Opts).

-spec get_sopath(options()) -> string().
get_sopath(Opts) ->
    AppName = get_appname(Opts),
    Dir = proplists:get_value(cpp_dir, Opts, "./priv"),
    SoName = proplists:get_value(cpp_out, Opts, "generated"), % without ext
    Path =
        case AppName of
            undefined ->
                filename:join(Dir, SoName);
            _ ->
                AppAtom = list_to_atom(AppName),
                case code:lib_dir(AppAtom) of
                    {error, bad_name} ->
                        filename:join(Dir, SoName);
                    Root ->
                        filename:join([Root, Dir, SoName])
                end
        end,
    case proplists:get_bool(absolute, Opts) of
        true -> filename:absname(Path);
        _ -> Path
    end.

-spec has_compile_cpp(options()) -> boolean().
has_compile_cpp(Opts) ->
    Load = proplists:get_bool(load, Opts),
    Compile =
        proplists:get_bool(compile, Opts) or
        proplists:get_bool(so, Opts) or
        Load,
    Compile.

-spec has_compile_erl(options()) -> boolean().
has_compile_erl(Opts) ->
    Load = proplists:get_bool(load, Opts),
    Compile =
        proplists:get_bool(compile, Opts) or
        proplists:get_bool(beam, Opts) or
        Load,
    Compile.

-spec has_force(options()) -> boolean().
has_force(Opts) ->
    proplists:get_bool(force, Opts).

-spec has_graph(options()) -> boolean().
has_graph(Opts) ->
    proplists:get_bool(graph, Opts).

-spec has_load(options()) -> boolean().
has_load(Opts) ->
    proplists:get_bool(load, Opts).

-spec optspec() -> [{atom(), integer(), string(), atom() | tuple(), string()}].
optspec() ->
    [
     {name, $n, "name", string,
      "The name of the codec for configuration"},
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
      "Save the type graph"},
     {schema, $s, "schema", string,
      "Output the schema to a file"},
     {include, undefined, "include", string,
      ".h file to include in the generated native code"},
     {force, $F, "force", boolean,
      "Force perc to run even if the generated files are newer than the input"},
     {absolute, undefined, "absolute", boolean,
      "Use an absolute paths in the generated files"}
    ].

-spec optspec_nodefaults() -> [{atom(),
                                integer(),
                                string(),
                                atom() | tuple(),
                                string()}].
optspec_nodefaults() ->
    [{A, B, C, case is_tuple(D) of
                   true ->
                       element(1, D);
                   _ ->
                       D
               end,
      E} || {A, B, C, D, E} <- optspec()].
