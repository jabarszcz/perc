-module(perc_opts).

-export([
    get_appname/1,
    get_backends/1,
    get_codec_name/1,
    get_cpp_flags/1,
    get_cxx/1,
    get_exported/1,
    get_includes/1,
    get_inputs/1,
    get_module_name/1,
    get_outputs/1,
    get_outputs_beam/1,
    get_outputs_cpp/1,
    get_outputs_erl/1,
    get_outputs_graph/1,
    get_outputs_schema/1,
    get_outputs_so/1,
    get_sopath/1,
    get_sopath_indirect/1,
    has_force/1,
    has_load/1,
    normalize/1,
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

-type output_opts() :: {appname, atom()}
                     | {lib_dir, string()}
                     | {module, string()}
                     | {erl, string()}
                     | {beam, string()}
                     | {cpp, string()}
                     | {so, string()}
                     | {graph, string()}
                     | {schema, string()}
                     | {out, string()}.

-type option() :: output_opts()
                | {in, string()}
                | {record, string()}
                | {backend, string()}
                | {cxx, string()}
                | {cpp_flags, string()}
                | load | {load, boolean()}
                | force | {force, boolean()}.


-type options() :: [option()].

%%====================================================================
%% API functions
%%====================================================================

-spec get_appname(options()) -> atom() | undefined.
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

-spec get_cxx(options()) -> string().
get_cxx(Opts) ->
    % Allow setting though environement variable too
    proplists:get_value(cxx, Opts, os:getenv("CXX", "g++")).

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

-spec get_module_name(options()) -> string() | no_return().
get_module_name(Opts) ->
    ErlOuts = proplists:get_all_values(erl, Opts),
    BeamOuts = proplists:get_all_values(beam, Opts),
    case
        {proplists:get_value(module, Opts),
         lists:filter(
           fun(X) -> X =/= "" end,
           lists:usort(
             [filename:rootname(filename:basename(F))
              || F <- ErlOuts ++ BeamOuts,
                 not lists:suffix("/", F) % basename ignores the trailing slash
             ]
            )
          )}
    of
        {undefined, []} ->
            default_filename(module);
        {undefined, [ModName]} ->
            ModName;
        {ModName, [ModName]} ->
            ModName;
        {ModName, []} ->
            ModName;
        {N, Names} ->
            case N of
                undefined ->
                    erlang:throw({module_names_differ, Names});
                _ ->
                    erlang:throw({module_names_differ, [N | Names]})
            end
    end.


-spec get_outputs(options()) -> [string()].
get_outputs(Opts) ->
    get_outputs_cpp(Opts) ++
        get_outputs_so(Opts) ++
        get_outputs_erl(Opts) ++
        get_outputs_beam(Opts) ++
        get_outputs_graph(Opts) ++
        get_outputs_schema(Opts).

-spec get_outputs_beam(options()) -> [string()].
get_outputs_beam(Opts) ->
    Mod = get_module_name(Opts) ++ ".beam",
    [filename:join(D, Mod)
     || D <- get_all_x_output_dirs(Opts, beam)].

-spec get_outputs_cpp(options()) -> [string()].
get_outputs_cpp(Opts) ->
    get_all_x_outputs(Opts, cpp, default_filename(cpp), ".cpp").

-spec get_outputs_erl(options()) -> [string()].
get_outputs_erl(Opts) ->
    Mod = get_module_name(Opts) ++ ".erl",
    [filename:join(D, Mod)
     || D <- get_all_x_output_dirs(Opts, erl)].

-spec get_outputs_graph(options()) -> [string()].
get_outputs_graph(Opts) ->
    get_all_x_outputs(Opts, graph, default_filename(graph), ".png").

-spec get_outputs_schema(options()) -> [string()].
get_outputs_schema(Opts) ->
    get_all_x_outputs(Opts, schema, default_filename(schema), ".perc").

-spec get_outputs_so(options()) -> [string()].
get_outputs_so(Opts) ->
    case proplists:is_defined(so, Opts) of
        true ->
            [get_sopath(Opts)];
        _ ->
            []
    end.

-spec get_sopath(options()) -> string().
get_sopath(Opts) ->
    get_path(Opts, get_sopath_indirect(Opts)).

-spec get_sopath_indirect(options()) -> string() | {atom(), string()}.
get_sopath_indirect(Opts) ->
    SoOpt = proplists:get_value(so, Opts),
    So =
        case SoOpt of
            undefined ->
                CppName = proplists:get_value(cpp, Opts, default_filename(so)),
                filename:rootname(CppName);
            _ ->
                SoOpt
        end,
    maybe_add_default_name_ext(So, default_filename(so), ".so").

-spec has_force(options()) -> boolean().
has_force(Opts) ->
    proplists:get_bool(force, Opts).

-spec has_load(options()) -> boolean().
has_load(Opts) ->
    proplists:get_bool(load, Opts).

-spec normalize(options()) -> options() | no_return().
normalize(Opts) ->
    choose_opts_from_format(Opts).

%% Option specification for command line parsing
%%
%% The 'load' option is ommitted from this list because it is not
%% relevant on the command line.
-spec optspec() -> [{atom(), integer(), string(), atom() | tuple(), string()}].
optspec() ->
    [
     {name, $n, "name", string,
      "The name of the codec for configuration"},
     {backend, $B, "backend", {string, "json"},
      "The codec backends (json, etc.)"},
     {in, $i , "in", string,
      "An erlang/perc file containing type and record definitions"},
     {record, $r, "record", string,
      "The records for which we want an encoding function"},
     {usertype, $u, "usertype", string,
      "The user types for which we want an encoding function"},
     {module, $m, "module", string,
      "The name of the generated erlang module"},
     {cpp, $c, "cpp", string,
      "Save the NIF c++ source at the given path"},
     {so, $s, "so", string,
      "Save the compiled NIF .so file at the given path"
      " (only the first is used)"},
     {erl, $e, "erl", string,
      "Save the erlang module source at the given path"},
     {beam, $b, "beam", string,
      "Save the compiled erlang module at the given path"},
     {graph, $g, "graph", string,
      "Save an image of the type graph at the given path"},
     {schema, $p, "schema", string,
      "Save the .perc schema at the given path"},
     {out, $o, "out", string,
      "Infer the kind of output file from the extension"},
     {cxx, undefined, "cxx", string,
      "The c++ compiler to use"},
     {cpp_flags, undefined, "cpp-flags", string,
      "Flags to pass to the c++ compiler if compiling"},
     {include, undefined, "include", string,
      ".h file to include in the generated native code"},
     {force, $f, "force", boolean,
      "Force perc to run even if the generated files are newer than the input"},
     {appname, undefined, "appname", atom,
      "The erlang application name that is used to find the lib_dir"
      " at runtime"},
     {lib_dir, undefined, "lib_dir", string,
      "The library directory that is used as base path for the NIF .so file"
      " (at build time)"}
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

%%====================================================================
%% Internal functions
%%====================================================================

-spec choose_opts_from_format(options()) -> options() | no_return().
choose_opts_from_format(Opts) ->
    lists:map(
      fun({out, OutStr}) ->
              Dict = dict:from_list([{B, A} || {A, B} <- default_formats()]),
              Ext = filename:extension(OutStr),
              case dict:find(Ext, Dict) of
                  {ok, Opt} ->
                      {Opt, OutStr};
                  error ->
                      erlang:throw({unrecognized_ext, OutStr})
              end;
         (Else) ->
              Else
      end,
      Opts
     ).

-spec default_filename(atom()) -> string().
default_filename(module) ->
    "generated";
default_filename(so) ->
    "generated";
default_filename(cpp) ->
    "generated";
default_filename(graph) ->
    "type_graph";
default_filename(schema) ->
    "schema".

-spec default_formats() -> [{atom(), string()}].
default_formats() ->
    [{cpp, ".cpp"},
     {so, ".so"},
     {erl, ".erl"},
     {beam, ".beam"},
     {graph, ".png"},
     {schema, ".perc"}].

-spec get_all_x_outputs(options(), atom(), string(), string()) -> [string()].
get_all_x_outputs(Opts, Type, Name, Ext) ->
    [maybe_add_default_name_ext(F, Name, Ext)
     || F <- proplists:get_all_values(Type, Opts)].

-spec get_all_x_output_dirs(options(), atom()) -> [string()].
get_all_x_output_dirs(Opts, Type) ->
    [filename:dirname(F) || F <- proplists:get_all_values(Type, Opts)].

-spec get_path(options(), string()) -> string().
get_path(Opts, Path) ->
    AppName = get_appname(Opts),
    Root =
        case code:lib_dir(AppName) of %% probably not in the code server
            {error, bad_name} ->
                proplists:get_value(lib_dir, Opts);
            R ->
                R
        end,
    case Root of
        undefined ->
            Path; % path is relative to cwd
        _ ->
            filename:join(Root, Path) % relative to lib_dir
    end.

-spec maybe_add_default_name_ext(string(), string(), string()) -> string().
maybe_add_default_name_ext(Filename, DefaultName, DefaultExt) ->
    NewFilename =
        case
            filename:basename(Filename) =:= "" orelse
            lists:suffix("/", Filename) % basename ignores the trailing slash
        of
            true ->
                filename:join(filename:dirname(Filename), DefaultName);
            _ ->
                Filename
        end,
    case filename:extension(NewFilename) of
        [] ->
            NewFilename ++ DefaultExt;
        _ ->
            NewFilename
    end.
