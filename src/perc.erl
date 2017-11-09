-module(perc).

-include_lib("kernel/include/file.hrl").

%% API exports
-export([
    main/1,
    generate_codecs/1
  ]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
-spec main([string()]) -> no_return().
main(Args) ->
    OptSpec = perc_opts:optspec(),
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

-spec generate_codecs(list()) -> ok | no_return().
generate_codecs(Options) ->
    case
        (should_run(Options) or perc_opts:has_force(Options))
        and (perc_opts:get_exported(Options) =/= [])
    of
        true ->
            Gen = gen_from_options(Options),
            Reduced = perc_reduce:reduce(Gen),
            generate_nif(Reduced),
            generate_erl(Reduced),
            case perc_opts:has_graph(Options) of
                true -> save_graph(Reduced, "type_graph.png", "png");
                _ -> ok
            end,
            case perc_opts:get_schema(Options) of
                undefined -> ok;
                Filename ->
                    Defs = perc_gen:get_defs(Reduced),
                    file:write_file(Filename, perc_prettypr:format(Defs))
            end,
            ok;
        _ ->
            ok
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec gen_from_options(perc_opts:options()) -> perc_gen:gen().
gen_from_options(Opts) ->
    {PercInputs, ErlInputs} =
        lists:partition(
          fun(In) ->
                  filename:extension(In) == ".perc"
          end,
          perc_opts:get_inputs(Opts)),
    CodecName = perc_opts:get_codec_name(Opts),
    DefsA = perc_parse_erl:read_all(ErlInputs, CodecName),
    DefsB =
        [begin
             {ok, Tokens, _} = perc_scanner:file(F),
             {ok, Parsed} = perc_rparser:parse_defs(Tokens),
             Parsed
         end || F <- PercInputs],
    Defs = perc_defs:merge([DefsA | DefsB]),
    perc_gen:make(Defs, Opts).

save_graph(Gen, Filename, Format) ->
    Defs = perc_gen:get_defs(Gen),
    RecordDefs = perc_defs:get_records(Defs),
    UserTypeDefs = perc_defs:get_usertypes(Defs),
    Graph = perc_digraph:make(RecordDefs, UserTypeDefs),
    Ret = perc_digraph:save(Graph, Filename, Format),
    perc_digraph:delete(Graph),
    Ret.

should_run(Opts) ->
    try
        InFiles = perc_opts:get_inputs(Opts),
        DateIn = lists:max(
                   [begin
                        {ok, InfoIn} = file:read_file_info(F),
                        InfoIn#file_info.mtime
                    end || F <- InFiles]
                  ),
        SoPath = perc_opts:get_sopath(Opts),
        SoFile = io_lib:format("~s.so", [SoPath]),
        {ok, InfoOut} = file:read_file_info(SoFile),
        DateOut = InfoOut#file_info.mtime,
        DateIn > DateOut
    catch
        _:{badmatch, _} -> true
    end.

-spec generate_nif(perc_gen:gen()) -> ok | no_return().
generate_nif(Gen) ->
    Opts = perc_gen:get_opts(Gen),
    CppStr = perc_backend:generate_nif_source(Gen),
    SoPath = perc_opts:get_sopath(Opts),
    ok = filelib:ensure_dir(SoPath),
    case perc_opts:has_compile_cpp(Opts) of
        true ->
            TempFile = lib:nonl(os:cmd("mktemp --suffix '.cpp'")),
            SoFile = io_lib:format("~s.so", [SoPath]),
            ErlNifIncludeDir = filename:join(code:root_dir(), "usr/include"),
            CIncludeDir = code:priv_dir(perc),
            Flags = perc_opts:get_cpp_flags(Opts),
            Cmd = io_lib:format(
                    "g++ -fvisibility=hidden -nodefaultlibs "
                    "-o ~s -fpic -shared ~s -I ~s -I ~s -I . ~s",
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

-spec generate_erl(perc_gen:gen()) -> ok | no_return().
generate_erl(Gen) ->
    ErlForms = perc_erl_gen:generate(Gen),
    Opts = perc_gen:get_opts(Gen),
    ErlDir = perc_opts:get_erl_dir(Opts),
    filelib:ensure_dir(ErlDir),
    case perc_opts:has_compile_erl(Opts) of
        true ->
            {Module, Binary} =
                case compile:forms(ErlForms, [debug_info]) of
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
            case perc_opts:has_load(Opts) of
                true ->
                    {module, _} = code:load_abs(filename:join(ErlDir, Module)),
                    ok;
                _ ->
                    ok
            end;
        false ->
            Output = erl_prettypr:format(erl_syntax:form_list(ErlForms)),
            ModuleName = perc_opts:get_erl_out(Opts),
            File = filename:join(ErlDir, io_lib:format("~s.erl", [ModuleName])),
            ok = file:write_file(File, Output)
    end.
