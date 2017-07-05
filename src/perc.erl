-module(perc).

%% API exports
-export([
    main/1,
    generate_codecs/1,
    get_gen_erl_out/1,
    get_gen_cpp_out/1,
    get_gen_inputs/1,
    get_gen_exported/1,
    get_gen_record_defs/1,
    get_gen_usertype_defs/1,
    set_gen_defs/2
  ]).

-export_type([
    option/0,
    generator/0
  ]).

%%====================================================================
% API Types
%%====================================================================

-type option() :: {erl_out, string()}
                | {cpp_out, string()}
                | {input_file, string()}
                | {record, string()}
                | {backend, string()}.

-record(generator, {
    erl_out :: string(),
    cpp_out :: string(),
    inputs :: [string()],
    exported :: [perc_types:perc_type()],
    record_defs :: [perc_types:record_def()],
    usertype_defs :: [perc_types:usertype_def()]
 }).

-opaque generator() :: #generator{}.

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
-spec main([string()]) -> no_return().
main(Args) ->
    OptSpec =
        [
         {erl_out, $e, "erl-out", {string, "generated"},
          "The generated erlang module name"},
         {cpp_out, $c, "cpp-out", {string, "generated"},
          "The generated cpp file name"},
         {input_file, $i , "in", string,
          "An erlang file containing type and record definitions"},
         {record, $r, "record", string,
          "The records for which we want an encoding function"},
         {usertype, $u, "usertype", string,
          "The user types for which we want an encoding function"}
        ],
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
    Reduced = perc_reduce:reduce(Gen),
    io:format("~p~n", [Reduced]).

-spec get_gen_erl_out(generator()) -> string().
get_gen_erl_out(Gen) ->
    Gen#generator.erl_out.

-spec get_gen_cpp_out(generator()) -> string().
get_gen_cpp_out(Gen) ->
    Gen#generator.cpp_out.

-spec get_gen_inputs(generator()) -> [string()].
get_gen_inputs(Gen) ->
    Gen#generator.inputs.

-spec get_gen_exported(generator()) -> [perc_types:perc_type()].
get_gen_exported(Gen) ->
    Gen#generator.exported.

-spec get_gen_record_defs(generator()) -> [perc_types:record_def()].
get_gen_record_defs(Gen) ->
    Gen#generator.record_defs.

-spec get_gen_usertype_defs(generator()) -> [perc_types:usertype_def()].
get_gen_usertype_defs(Gen) ->
    Gen#generator.usertype_defs.

-spec set_gen_defs(generator(), perc_parse:defs()) -> generator().
set_gen_defs(Gen, {RecordDefs, UserTypeDefs}) ->
    Gen#generator{record_defs=RecordDefs, usertype_defs=UserTypeDefs}.

%%====================================================================
%% Internal functions
%%====================================================================

gen_from_options(Opts) ->
    Inputs = proplists:get_all_values(input_file, Opts),
    RecordNames = proplists:get_all_values(record, Opts),
    UserTypeNames = proplists:get_all_values(usertype, Opts),
    Records =
        [perc_types:make_record(Name) || Name <- RecordNames],
    UserTypes =
        [perc_types:make_usertype(Name) || Name <- UserTypeNames],
    Exported = Records ++ UserTypes,
    {RecordDefs, UserTypeDefs} =
        perc_parse:read_all(Inputs),
    #generator{
       erl_out=proplists:get_value(erl_out, Opts, "generated"),
       cpp_out=proplists:get_value(cpp_out, Opts, "generated"),
       inputs=Inputs,
       exported=Exported,
       record_defs=RecordDefs,
       usertype_defs=UserTypeDefs
      }.
