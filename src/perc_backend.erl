-module(perc_backend).

-export([backends/0,
         backend_from_name/1,
         get_nif_name/3,
         generate_nif_source/1,
         id_from_name/1]).

-include("perc.hrl").
-include("perc_types.hrl").

-callback name() -> string().
-callback gen_record_enc_func(#record_def{}) -> iolist().
-callback gen_usertype_enc_func(#user_type_def{}) -> iolist().

backends() ->
    [perc_json].

backend_from_name(Name) ->
    hd(lists:filter(
         fun(Backend) ->
                 Backend:name() == Name
         end,
         backends())).

generate_nif_source(Module) ->
    Backends = Module#nif_module.backends,
    UserTypes = [UType#user_type_def.name
                 || UType <- Module#nif_module.user_types],
    NifFuncs = [[{name, get_nif_name(Backend, "encode", Name)},
                 {type_template, io_lib:format("Record<~s>", [id_from_name(Name)])},
                 {defaultsize, "64"},
                 {backend, Backend:name()}]
                || Name <- Module#nif_module.exported_records,
                   Backend <- Backends],
    ModuleDict = [{name, Module#nif_module.name},
                  {records, lists:map(fun id_from_name/1, Module#nif_module.all_records)},
                  {usertypes, lists:map(fun id_from_name/1, UserTypes)},
                  {nif_funcs, NifFuncs}],
    BackendsDict = [[{name, Backend:name()}, {functions, functions(Backend, Module)}]
                    || Backend <- Backends],
    {ok, Source} =
        nif_source_dtl:render([{module, ModuleDict}, {backends, BackendsDict}]),
    Source.

functions(Backend, Module) ->
    iolist_to_binary([[Backend:gen_usertype_enc_func(UserType)
                       || UserType <- Module#nif_module.user_types],
                      [Backend:gen_record_enc_func(Record)
                       || Record <- [dict:fetch(RecName, Module#nif_module.record_dict)
                                     || RecName <- Module#nif_module.all_records]]]).

get_nif_name(Backend, Action, RecordName) ->
    [Backend:name(), "_", Action, "_", RecordName, "_nif"].

id_from_name(Str) ->
    string:to_upper(Str).
