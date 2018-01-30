-module(perc).

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
    Opts = perc_opts:normalize(Options),
    case
        should_run(Opts) and (perc_opts:get_exported(Opts) =/= [])
    of
        true ->
            Defs = read_defs(Opts),
            Exports = perc_opts:get_exported(Opts),
            Reduced = perc_reduce:reduce(Defs, Exports),
            generate_nif(Reduced, Opts),
            generate_erl(Opts),
            [save_graph(Reduced, File, filename:extension(File))
             || File <- perc_opts:get_outputs_graph(Opts)],
            output_multiple(
              perc_prettypr:format(Reduced),
              perc_opts:get_outputs_schema(Opts)
             );
        _ ->
            ok
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec read_defs(perc_opts:options()) -> perc_defs:defs().
read_defs(Opts) ->
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
    perc_defs:merge([DefsA | DefsB]).

save_graph(Defs, Filename, Format) ->
    Graph = perc_digraph:make(Defs),
    Ret = perc_digraph:save(Graph, Filename, Format),
    perc_digraph:delete(Graph),
    Ret.

should_run(Opts) ->
    InFiles = perc_opts:get_inputs(Opts),
    OutFiles = perc_opts:get_outputs(Opts),
    case {file_mod_dates(InFiles), file_mod_dates(OutFiles)} of
        {[], _} -> true;
        {_, []} -> true;
        {InDates, OutDates} ->
            DateIn = lists:max(InDates),
            DateOut = lists:min(OutDates),
            DateIn > DateOut
                orelse perc_opts:has_load(Opts)
                orelse perc_opts:has_force(Opts)
    end.

file_mod_dates(Files) ->
    [M || M <- [filelib:last_modified(F) || F <- Files], M =/= 0].

-spec output(iodata(), string()) -> ok | no_return().
output(Data, File) ->
    ok = filelib:ensure_dir(File),
    ok = file:write_file(File, Data).

-spec output_multiple(iodata(), [string()]) -> ok | no_return().
output_multiple(Data, Files) ->
    [output(Data, File) || File <- Files],
    ok.

-spec temp_file(string()) -> string().
temp_file(Ext) ->
    lib:nonl(
      os:cmd(
        lists:flatten(
          io_lib:format("mktemp --suffix '~s'", [Ext])
         )
       )
     ).

-spec temp_dir() -> string().
temp_dir() ->
    lib:nonl(os:cmd("mktemp --directory")).

-spec generate_nif(perc_defs:defs(), perc_opts:options()) -> ok | no_return().
generate_nif(Defs, Opts) ->
    CppStr = perc_backend:generate_nif_source(Defs, Opts),
    output_multiple(CppStr, perc_opts:get_outputs_cpp(Opts)),
    TempFile = temp_file(".cpp"),
    output(CppStr, TempFile),
    ErlNifIncludeDir = filename:join(code:root_dir(), "usr/include"),
    CIncludeDir = code:priv_dir(perc),
    Flags = perc_opts:get_cpp_flags(Opts),
    CXX = perc_opts:get_cxx(Opts),
    [begin
         Cmd = io_lib:format(
                 "~s -fvisibility=hidden -nodefaultlibs "
                 "-o ~s -fpic -shared ~s -I ~s -I ~s -I . ~s",
                 [CXX, So, TempFile, ErlNifIncludeDir, CIncludeDir, Flags]
                ),
         io:format("~s~n", [Cmd]), %% TODO remove
         case os:cmd(Cmd) of
             [] ->
                 ok;
             Error ->
                 BinError = unicode:characters_to_binary(Error),
                 erlang:error({cpp_compiler_output, BinError})
         end
     end || So <- perc_opts:get_outputs_so(Opts)],
    ok.

-spec generate_erl(perc_opts:options()) -> ok | no_return().
generate_erl(Opts) ->
    ErlForms = perc_erl_gen:generate(Opts),
    ErlSource = erl_prettypr:format(erl_syntax:form_list(ErlForms)),
    output_multiple(ErlSource, perc_opts:get_outputs_erl(Opts)),
    {Module, Binary} =
        case compile:forms(ErlForms, [debug_info]) of
            {ok, ModuleName, Bin} when is_binary(Bin) ->
                {ModuleName, Bin};
            {ok, ModuleName, Bin, _Warn} when is_binary(Bin) ->
                {ModuleName, Bin};
            Error ->
                erlang:throw({not_a_binary, Error})
        end,
    output_multiple(Binary, perc_opts:get_outputs_beam(Opts)),
    BeamFile =
        case perc_opts:get_outputs_beam(Opts) of
            [] ->
                Tmp = filename:join(temp_dir(), Module),
                output(Binary, Tmp),
                Tmp;
            [B | _] ->
                B
        end,
    case perc_opts:has_load(Opts) of
        true ->
            {module, _} = code:load_binary(Module, BeamFile, Binary),
            ok;
        _ ->
            ok
    end.
