-module(perc_backend).

%% API exports
-export([
    backends/0,
    backend_from_name/1,
    generate_nif_source/1,
    get_nif_function_name/3,
    template/1,
    type_to_id/1
  ]).

%% Behaviour definition
-callback name() -> string().
-callback gen_record_enc_func(perc_types:record_def()) -> iolist().
-callback gen_usertype_enc_func(perc_types:usertype_def()) -> iolist().

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

-spec generate_nif_source(perc_gen:gen()) -> iolist().
generate_nif_source(Gen) ->
    Opts = perc_gen:get_opts(Gen),
    Defs = perc_gen:get_defs(Gen),
    Backends = perc_opts:get_backends(Opts),
    Records = perc_defs:get_records(Defs),
    UserTypes = perc_defs:get_usertypes(Defs),
    Includes = perc_opts:get_includes(Opts),
    NifFuncs =
        [[{name, get_nif_function_name(Backend, "encode", Type)},
          {type_template, template(Type)},
          {defaultsize, "64"},
          {backend, Backend:name()}]
         || Type <- perc_opts:get_exported(Opts),
            Backend <- Backends],
    ModuleDict =
        [{name, perc_opts:get_erl_out(Opts)},
         {records, [record_id(R) || R <- Records]},
         {usertypes, [usertype_id(U) || U <- UserTypes]},
         {nif_funcs, NifFuncs}],
    BackendsDict =
        [[{name, Backend:name()},
          {functions, functions(Backend, Defs)}]
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
    TypeStr = type_to_id(Type),
    [Backend:name(), "_", Action, "_", TypeStr].

-spec template(perc_types:perc_type()) -> iolist().
template(Type) ->
    case perc_types:get_type(Type) of
        undefined_atom ->
            "Undefined";
        basic ->
            capitalize(atom_to_list(perc_types:get_basic_type(Type)));
        list ->
            ListType = perc_types:get_list_type(Type),
            io_lib:format("List<~s>", [template(ListType)]);
        tuple ->
            TupleTypes = perc_types:get_tuple_types(Type),
            io_lib:format(
              "Tuple<~s>",
              [string:join([template(T) || T <- TupleTypes], ",")]
             );
        union ->
            UnionTypes = perc_types:get_union_types(Type),
            io_lib:format(
              "Union<~s>",
              [string:join([template(T) || T <- UnionTypes], ",")]
             );
        record ->
            io_lib:format("Record<~s>", [type_to_id(Type)]);
        usertype ->
            io_lib:format("UserType<~s>", [type_to_id(Type)]);
        function ->
            TypeArg = perc_types:get_function_arg(Type),
            {Enc, Dec} = perc_types:get_function_names(Type),
            EncStr = function_expr(Enc),
            DecStr = function_expr(Dec),
            io_lib:format(
              "Function<~s,~s,~s>",
              [EncStr, DecStr, template(TypeArg)]
             )
    end.

-spec type_to_id(perc_types:perc_type()) -> iolist().
type_to_id(Type) ->
    case perc_types:get_type(Type) of
        record ->
            ["record_", perc_types:get_record_name(Type)];
        usertype ->
            ["usertype_", perc_types:get_usertype_name(Type)]
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec record_id(perc_types:record_def()) -> iolist().
record_id(RecordDef) ->
    ["record_", perc_types:get_record_def_name(RecordDef)].

-spec usertype_id(perc_types:usertype_def()) -> iolist().
usertype_id(UserTypeDef) ->
    ["usertype_", perc_types:get_usertype_def_name(UserTypeDef)].

functions(Backend, Defs) ->
    Records = perc_defs:get_records(Defs),
    UserTypes = perc_defs:get_usertypes(Defs),
    iolist_to_binary(
      [[Backend:gen_usertype_enc_func(UserType)
        || UserType <- UserTypes],
       [Backend:gen_record_enc_func(Record)
        || Record <- Records]]
     ).

function_expr(Name) ->
    case Name of
        undefined -> "(trans_func)NULL";
        _ -> Name
    end.

capitalize([H|T]) ->
    [string:to_upper(H)|T].
