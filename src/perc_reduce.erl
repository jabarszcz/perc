-module(perc_reduce).

%% API exports
-export([
    reduce/1
  ]).

%%====================================================================
% API functions
%%====================================================================

-spec reduce(perc:generator()) -> perc:generator().
reduce(Gen) ->
    reduce_unions(
      reduce_deps(
        reduce_usertypes(
          reduce_ignored(Gen)
         )
       )
     ).

%%====================================================================
%% Internal functions
%%====================================================================

-spec reduce_deps(perc:generator()) -> perc:generator().
reduce_deps(Gen) ->
    RecordDefs = perc:get_gen_record_defs(Gen),
    UserTypeDefs = perc:get_gen_usertype_defs(Gen),
    Graph = perc_digraph:make(RecordDefs, UserTypeDefs),
    Deps = perc_digraph:compute_deps(Graph, perc:get_gen_exported(Gen)),
    DepsSet = sets:from_list(Deps),
    NewRecords =
        [R || R <- RecordDefs,
              sets:is_element(
                perc_types:make_record(perc_types:get_record_def_name(R)),
                DepsSet
               )
        ],
    NewUserTypes =
        [U || U <- UserTypeDefs,
              sets:is_element(
                perc_types:make_usertype(perc_types:get_usertype_def_name(U)),
                DepsSet
               )
        ],
    perc_digraph:delete(Graph),
    perc:set_gen_defs(Gen, perc:make_defs(NewRecords, NewUserTypes)).

-spec reduce_ignored(perc:generator()) -> perc:generator().
reduce_ignored(Gen) ->
    Records = perc:get_gen_record_defs(Gen),
    UserTypes = perc:get_gen_usertype_defs(Gen),
    Graph = perc_digraph:make(Records, UserTypes),
    Fun = fun(T) -> perc_digraph:reduce_ignored(Graph, T) end,
    NewRecords = [reduce_types_in_record(Fun, R) || R <- Records],
    NewUserTypes = [reduce_types_in_usertype(Fun, U) || U <- UserTypes],
    perc_digraph:delete(Graph),
    perc:set_gen_defs(Gen, perc:make_defs(NewRecords, NewUserTypes)).

-spec reduce_unions(perc:generator()) -> perc:generator().
reduce_unions(Gen) ->
    Records = perc:get_gen_record_defs(Gen),
    UserTypes = perc:get_gen_usertype_defs(Gen),
    Fun = fun(Type) -> reduce_unions_in_type(Type) end,
    NewRecords =
        [reduce_types_in_record(Fun, R) || R <- Records],
    NewUserTypes =
        [reduce_types_in_usertype(Fun, U) || U <- UserTypes],
    perc:set_gen_defs(Gen, perc:make_defs(NewRecords, NewUserTypes)).

-spec reduce_usertypes(perc:generator()) -> perc:generator().
reduce_usertypes(Gen) ->
    Records = perc:get_gen_record_defs(Gen),
    UserTypes = perc:get_gen_usertype_defs(Gen),
    Graph = perc_digraph:make(Records, UserTypes),
    UserTypeDict =
        dict:from_list(
          [{perc_types:make_usertype(perc_types:get_usertype_def_name(U)), U}
          || U <- UserTypes]
         ),
    Subs = perc_digraph:make_substitutions(Graph, UserTypeDict),
    Fun = fun(Type) -> perc_digraph:reduce_usertypes(Type, Subs) end,
    NewRecords =
        [reduce_types_in_record(Fun, R) || R <- Records],
    NewUserTypes =
        [reduce_types_in_usertype(Fun, U) || U <- UserTypes],
    perc_digraph:delete(Graph),
    perc:set_gen_defs(Gen, perc:make_defs(NewRecords, NewUserTypes)).

-spec reduce_types_in_record(
        fun((perc_types:perc_type()) -> perc_types:perc_type()),
        perc_types:record_def()
       ) -> perc_types:record_def().
reduce_types_in_record(Fun, RecordDef) ->
    Fields = perc_types:get_record_def_fields(RecordDef),
    NewFields = [reduce_types_in_field(Fun, F) || F <- Fields],
    perc_types:set_record_def_fields(RecordDef, NewFields).

-spec reduce_types_in_field(
        fun((perc_types:perc_type()) -> perc_types:perc_type()),
        perc_types:record_field()
       ) -> perc_types:record_field().
reduce_types_in_field(Fun, Field) ->
    Type = perc_types:get_record_field_type(Field),
    NewType = Fun(Type),
    perc_types:set_record_field_type(Field, NewType).

-spec reduce_types_in_usertype(
        fun((perc_types:perc_type()) -> perc_types:perc_type()),
        perc_types:usertype_def()
       ) -> perc_types:usertype_def().
reduce_types_in_usertype(Fun, UserTypeDef) ->
    Type = perc_types:get_usertype_def_type(UserTypeDef),
    NewType = Fun(Type),
    perc_types:set_usertype_def_type(UserTypeDef, NewType).

-spec reduce_unions_in_type(perc_types:perc_type()) -> perc_types:perc_types().
reduce_unions_in_type(Type) ->
    perc_types:fmap(
      fun(Type_) ->
              case perc_types:get_type(Type_) of
                  union ->
                      join_types([Type_]);
                  maybe ->
                      join_types([Type_]);
                  _ ->
                      Type_
              end
      end, Type).

join_types(Types) ->
    join_types(Types, false).

join_types(Types, Maybe) ->
    join_types(Types, Maybe, sets:new()).

join_types([], Maybe, Set) ->
    Union = case sets:size(Set) of
                0 -> empty_union;
                1 -> hd(sets:to_list(Set));
                _ -> perc_types:make_union(sets:to_list(Set))
            end,
    case {Maybe, Union} of
        {true, empty_union} ->
            perc_types:make_basic(atom); % The atom 'undefined'
        {true, _} ->
            perc_types:make_maybe(Union);
        {_, empty_union} ->
            perc_types:make_ignored(empty_union_bug);
        _ ->
            Union
    end;
join_types([Type|Types], Maybe, Set) ->
    case perc_types:get_type(Type) of
        maybe ->
            join_types([perc_types:get_maybe_type(Type) | Types], true, Set);
        basic ->
            case perc_types:get_basic_type(Type) of
                undefined_atom ->
                    join_types(Types, true, Set);
                _ ->
                    join_types(Types, Maybe, sets:add_element(Type, Set))
            end;
        union ->
            join_types(perc_types:get_union_types(Type) ++ Types, Maybe, Set);
        _ ->
            join_types(Types, Maybe, sets:add_element(Type, Set))
    end.
