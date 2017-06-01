-module(perc).

-define(S, erl_syntax).

-record(nif_module, {
          name :: string(),
          soname :: string(),
          exported_records :: [string()],
          all_records :: [string()],
          backends :: [string()]
         }).

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
    io:format("~p~n", [Res]),
    erlang:halt(0);
main(_) ->
    io:format("usage ...~n"),
    erlang:halt(1).

generate_codecs(Filename, RecordNames) ->
    Forms = forms:read(Filename),
    Records = perc_types:get_records_from_forms(Forms),
    RecordDict = dict:from_list([{Rec#record_def.name, Rec}
                                 || Rec <- Records]),
    Deps = sets:to_list(record_deps(RecordDict, RecordNames)),
    Module = #nif_module{name="generated", %% TODO
                         soname="sogenerated",
                         exported_records=RecordNames,
                         all_records=Deps,
                         backends=["json"]}.


%%====================================================================
%% Internal functions
%%====================================================================

record_deps(RecordDict, ToVisit) ->
    record_deps_(RecordDict, ToVisit, sets:from_list(ToVisit)).

record_deps_(RecordDict, [Curr | ToVisit], Deps) ->
    CurrRecord = dict:fetch(Curr, RecordDict),
    CurrDeps = sets:to_list(perc_types:get_nested_records(CurrRecord)),
    {NewToVisit, NewDeps} =
        lists:foldl(
          fun(Dep, {NewToVisit, NewDeps}) ->
                  case sets:is_element(Dep, NewDeps) of
                      true ->
                          {NewToVisit, NewDeps};
                      false ->
                          {[Dep | NewToVisit], sets:add_element(Dep, NewDeps)}
                  end
          end,
          {ToVisit, Deps},
          CurrDeps),
    record_deps_(RecordDict, NewToVisit, NewDeps);
record_deps_(_, [], Deps) ->
    Deps.
