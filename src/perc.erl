-module(perc).

-define(S, erl_syntax).

-include("perc.hrl").
-include("perc_types.hrl").

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
-spec main([string()]) -> no_return().
main([Filename, RecordName]) ->
    Res = generate_codecs(Filename, [RecordName]),
    io:format("~s~n", [Res]),
    erlang:halt(0);
main(_) ->
    io:format("usage ...~n"),
    erlang:halt(1).

generate_codecs(Filename, RecordNames) ->
    Forms = forms:read(Filename),
    {UserTypes, Records} = perc_analysis:analyse_forms(Forms),
    {ReducedRecords, NonReducedTypes} =
        perc_types:reduce(Records, UserTypes),
    RecordDict =
        dict:from_list([{Rec#record_def.name, Rec} || Rec <- ReducedRecords]),
    Deps = perc_types:get_record_deps(RecordNames, RecordDict),
    Module = #nif_module{name="generated", %% TODO
                         soname="sogenerated",
                         exported_records=RecordNames,
                         all_records=Deps,
                         user_types=NonReducedTypes,
                         backends=[perc_json]},
    Erl = generate_erlang_module(Module),
    Ccode = perc_backend:generate_nif_source(Module, RecordDict),
    io:format("~s~n~s~n", [Erl, Ccode]),
    ok = file:write_file(io_lib:format("~s.erl", [Module#nif_module.name]), Erl),
    ok = file:write_file(io_lib:format("~s.cpp", [Module#nif_module.soname]), Ccode).


%%====================================================================
%% Internal functions
%%====================================================================

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
