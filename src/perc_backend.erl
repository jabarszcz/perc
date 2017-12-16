-module(perc_backend).

%% API exports
-export([
    backends/0,
    backend_from_name/1,
    generate_nif_source/2,
    get_nif_function_name/3,
    template/2
  ]).

%% Behaviour definition
-callback name() -> string().
-callback gen_record_enc_func(
            perc_defs:record_def(),
            dict:dict(perc_types:perc_type(), integer())
           ) -> iolist().
-callback gen_usertype_enc_func(
            perc_defs:usertype_def(),
            dict:dict(perc_types:perc_type(), integer())
           ) -> iolist().

%%====================================================================
%% API functions
%%====================================================================

-spec backends() -> [atom()].
backends() ->
    [perc_json].

-spec backend_from_name(string()) -> atom().
backend_from_name(Name) ->
    hd(lists:filter(
         fun(Backend) ->
                 Backend:name() == Name
         end,
         backends()
        )).

-spec generate_nif_source(perc_defs:defs(), perc_opts:options()) -> iolist().
generate_nif_source(Defs, Opts) ->
    Backends = perc_opts:get_backends(Opts),
    Records = perc_defs:get_records(Defs),
    UserTypes = perc_defs:get_usertypes(Defs),
    Includes = perc_opts:get_includes(Opts),
    RefsTypes = [perc_defs:def_to_type(D) || D <- Records ++ UserTypes],
    IdMap =
        dict:from_list([{B, A} || {A, B} <- perc_utils:enumerate(RefsTypes)]),
    NifFuncs =
        [[{name, get_nif_function_name(Backend, "encode", Type)},
          {internal, get_nif_internal_name(Backend, "encode", Type, IdMap)},
          {type_template, template(Type, IdMap)},
          {defaultsize, "64"},
          {backend, Backend:name()}]
         || Type <- perc_opts:get_exported(Opts),
            Backend <- Backends],
    ModuleDict =
        [{name, perc_opts:get_module_name(Opts)},
         {records, [def_cid(R, IdMap) || R <- Records]},
         {usertypes, [def_cid(U, IdMap) || U <- UserTypes]},
         {nif_funcs, NifFuncs}],
    BackendsDict =
        [[{name, Backend:name()},
          {functions, functions(Backend, Defs, IdMap)}]
         || Backend <- Backends],
    {ok, Source} =
        nif_source_dtl:render([{module, ModuleDict},
                               {backends, BackendsDict},
                               {includes, Includes}]),
    Source.

-spec get_nif_function_name(
        atom(),
        string(),
        perc_types:perc_type()
       ) -> iolist().
get_nif_function_name(Backend, Action, Type) ->
    TypeStr = type_to_string(Type),
    [Backend:name(), "_", Action, "_", TypeStr].

-spec get_nif_internal_name(
        atom(),
        string(),
        perc_types:perc_type(),
        dict:dict(perc_types:perc_type(), integer())
       ) -> iolist().
get_nif_internal_name(Backend, Action, Type, IdMap) ->
    TypeStr = type_to_cid(Type, IdMap),
    [Backend:name(), "_", Action, "_", TypeStr].

-spec template(
        perc_types:perc_type(),
        dict:dict(perc_types:perc_type(), integer())
       ) -> iolist().
template(Type, IdMap) ->
    case perc_types:get_type(Type) of
        undefined_atom ->
            "Undefined";
        basic ->
            capitalize(atom_to_list(perc_types:get_basic_type(Type)));
        list ->
            ListType = perc_types:get_list_type(Type),
            io_lib:format("List<~s>", [template(ListType, IdMap)]);
        tuple ->
            TupleTypes = perc_types:get_tuple_types(Type),
            io_lib:format(
              "Tuple<~s>",
              [string:join([template(T, IdMap) || T <- TupleTypes], ",")]
             );
        union ->
            UnionTypes = perc_types:get_union_types(Type),
            io_lib:format(
              "Union<~s>",
              [string:join([template(T, IdMap) || T <- UnionTypes], ",")]
             );
        record ->
            io_lib:format("Record<~s>", [type_to_cid(Type, IdMap)]);
        usertype ->
            io_lib:format("UserType<~s>", [type_to_cid(Type, IdMap)]);
        function ->
            TypeArg = perc_types:get_function_arg(Type),
            {Enc, Dec} = perc_types:get_function_names(Type),
            EncStr = function_expr(Enc),
            DecStr = function_expr(Dec),
            io_lib:format(
              "Function<~s,~s,~s>",
              [EncStr, DecStr, template(TypeArg, IdMap)]
             )
    end.

%%====================================================================
%% Internal functions
%%====================================================================

capitalize([H|T]) ->
    [string:to_upper(H)|T].

def_cid(Def, IdMap) ->
    type_to_cid(perc_defs:def_to_type(Def), IdMap).

function_expr(Name) ->
    case Name of
        undefined -> "(trans_func)NULL";
        _ -> Name
    end.

functions(Backend, Defs, IdMap) ->
    Records = perc_defs:get_records(Defs),
    UserTypes = perc_defs:get_usertypes(Defs),
    iolist_to_binary(
      [[Backend:gen_usertype_enc_func(UserType, IdMap)
        || UserType <- UserTypes],
       [Backend:gen_record_enc_func(Record, IdMap)
        || Record <- Records]]
     ).

type_to_cid(Type, IdMap) ->
    case perc_types:get_type(Type) of
        record ->
            io_lib:format(
              "record_~b/*~s*/",
              [dict:fetch(Type, IdMap), perc_types:get_record_name(Type)]
             );
        usertype ->
            io_lib:format(
              "usertype_~b/*~s*/",
              [dict:fetch(Type, IdMap), perc_types:get_usertype_name(Type)]
             )
    end.

type_to_string(Type) ->
    case perc_types:get_type(Type) of
        record ->
            ["record_", perc_types:get_record_name(Type)];
        usertype ->
            ["usertype_", perc_types:get_usertype_name(Type)]
    end.
