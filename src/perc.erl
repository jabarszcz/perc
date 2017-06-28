-module(perc).

-define(S, erl_syntax).

-include("perc.hrl").
-include("perc_types.hrl").

%% API exports
-export([main/1,generate_codecs/1]).
-export_type([option/0]).

-type option() :: {erl_out, string()}
                | {cpp_out, string()}
                | {input_file, string()}
                | {record, string()}
                | {backend, string()}.

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
         {backend, $b, "backend", {string, "json"},
          "The codec backends (json, etc.)"}
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
    Module = module_from_options(Options),
    Erl = generate_erlang_module(Module),
    Ccode = perc_backend:generate_nif_source(Module),
    ok = file:write_file(io_lib:format("~s.erl", [Module#nif_module.name]), Erl),
    ok = file:write_file(io_lib:format("~s.cpp", [Module#nif_module.soname]), Ccode).


%%====================================================================
%% Internal functions
%%====================================================================

read(Filename) ->
    case epp:parse_file(Filename, []) of
        {ok, Forms} ->
            Forms;
        {ok, Forms, _} ->
            Forms;
        {error, enoent} ->
            throw({file_not_found, Filename})
    end.

module_from_options(Opts) ->
    Inputs = proplists:get_all_values(input_file, Opts),
    RecordNames = proplists:get_all_values(record, Opts),
    Backends =
        [perc_backend:backend_from_name(Back)
         || Back <- proplists:get_all_values(backend, Opts)],
    Forms = lists:flatmap(fun read/1, Inputs),
    {UserTypes, Records} = perc_analysis:analyse_forms(Forms),
    {ReducedRecords, NonReducedTypes} =
        perc_types:reduce(Records, UserTypes),
    RecordDict =
        dict:from_list([{Rec#record_def.name, Rec} || Rec <- ReducedRecords]),
    Deps = perc_types:get_record_deps(RecordNames, RecordDict),
    #nif_module{
      name=proplists:get_value(erl_out, Opts, "generated"),
      soname=proplists:get_value(cpp_out, Opts, "generated"),
      input_files=Inputs,
      exported_records=RecordNames,
      backends=Backends,
      all_records=Deps,
      record_dict=RecordDict,
      user_types=NonReducedTypes
     }.

generate_erlang_module(Module = #nif_module{}) ->
    ModuleAttr = ?S:attribute(
                    ?S:atom(module),
                    [?S:atom(Module#nif_module.name)]
                   ),
    Init = ?S:function(
              ?S:atom(init),
              [?S:clause(none, [?S:match_expr(
                                   ?S:atom(ok),
                                   ?S:application(
                                      ?S:atom(erlang),
                                      ?S:atom(load_nif),
                                      [?S:string("./" ++ Module#nif_module.soname),
                                       ?S:integer(0)])
                                  )])]
             ),
    Funcs = [?S:function( %% TODO spec
               ?S:atom(lists:flatten(perc_backend:get_nif_name(Backend, Action, RecName))),
               [?S:clause([?S:underscore()],
                          none,
                          [?S:application(none, ?S:atom(throw),
                                          [?S:atom(undefined)])])])
             || RecName <- Module#nif_module.exported_records,
                Action <- ["encode"], %% TODO
                Backend <- Module#nif_module.backends ],
    ExportAttrs = ?S:attribute(
                     ?S:atom(export),
                     [?S:list([?S:arity_qualifier(
                                  ?S:function_name(Func),
                                  ?S:integer(?S:function_arity(Func)))
                      || Func <- [Init | Funcs] ])]
                    ),
    OnloadAttr = ?S:attribute(
                    ?S:atom(on_load),
                    [?S:tuple(
                        [?S:function_name(Init),
                         ?S:integer(?S:function_arity(Init))])]
                   ),
    Forms = ?S:revert_forms(
               [ModuleAttr, ExportAttrs, OnloadAttr, Init | Funcs]),
    forms:from_abstract(Forms).
