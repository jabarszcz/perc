-module(perc_reduce).

-include_lib("stdlib/include/assert.hrl").

%% API exports
-export([
    reduce/1
  ]).

%%====================================================================
% API functions
%%====================================================================

-spec reduce(perc_gen:gen()) -> perc_gen:gen().
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

-spec reduce_deps(perc_gen:gen()) -> perc_gen:gen().
reduce_deps(Gen) ->
    Defs = perc_gen:get_defs(Gen),
    Opts = perc_gen:get_opts(Gen),
    RecordDefs = perc_defs:get_records(Defs),
    UserTypeDefs = perc_defs:get_usertypes(Defs),
    Graph = perc_digraph:make(RecordDefs, UserTypeDefs),
    Deps = perc_digraph:compute_deps(Graph, perc_opts:get_exported(Opts)),
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
    perc_gen:set_defs(Gen, perc_defs:make(NewRecords, NewUserTypes)).

-spec reduce_ignored(perc_gen:gen()) -> perc_gen:gen().
reduce_ignored(Gen) ->
    Defs = perc_gen:get_defs(Gen),
    Records = perc_defs:get_records(Defs),
    UserTypes = perc_defs:get_usertypes(Defs),
    Graph = perc_digraph:make(Records, UserTypes),
    Fun = fun(T) -> perc_digraph:reduce_ignored(Graph, T) end,
    NewRecords = [reduce_types_in_record(Fun, R) || R <- Records],
    NewUserTypes = [reduce_types_in_usertype(Fun, U) || U <- UserTypes],
    perc_digraph:delete(Graph),
    perc_gen:set_defs(Gen, perc_defs:make(NewRecords, NewUserTypes)).

-spec reduce_unions(perc_gen:gen()) -> perc_gen:gen().
reduce_unions(Gen) ->
    Defs = perc_gen:get_defs(Gen),
    Records = perc_defs:get_records(Defs),
    UserTypes = perc_defs:get_usertypes(Defs),
    Fun = fun(Type) -> reduce_unions_in_type(Type) end,
    NewRecords =
        [reduce_types_in_record(Fun, R) || R <- Records],
    NewUserTypes =
        [reduce_types_in_usertype(Fun, U) || U <- UserTypes],
    perc_gen:set_defs(Gen, perc_defs:make(NewRecords, NewUserTypes)).

-spec reduce_usertypes(perc_gen:gen()) -> perc_gen:gen().
reduce_usertypes(Gen) ->
    Defs = perc_gen:get_defs(Gen),
    Records = perc_defs:get_records(Defs),
    UserTypes = perc_defs:get_usertypes(Defs),
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
    perc_gen:set_defs(Gen, perc_defs:make(NewRecords, NewUserTypes)).

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
                  _ ->
                      Type_
              end
      end, Type).

join_types(Types) ->
    join_types(Types, sets:new()).

join_types([], Set) ->
    case sets:size(Set) of
        0 -> ?assertNotEqual(0, sets:size(Set));
        1 -> hd(sets:to_list(Set));
        _ -> perc_types:make_union(sets:to_list(Set))
    end;
join_types([Type|Types], Set) ->
    case perc_types:get_type(Type) of
        union ->
            join_types(perc_types:get_union_types(Type) ++ Types, Set);
        _ ->
            join_types(Types, sets:add_element(Type, Set))
    end.
