-module(perc_reduce).

-include_lib("stdlib/include/assert.hrl").

%% API exports
-export([
    reduce/2,
    reduce_deps/2,
    reduce_ignored/1,
    reduce_unions/1,
    reduce_usertypes/1
  ]).

%%====================================================================
% API functions
%%====================================================================

-spec reduce(perc_defs:defs(), [perc_types:perc_types()]) -> perc_defs:defs().
reduce(Defs, Exported) ->
    reduce_unions(
      reduce_deps(
        reduce_usertypes(
          reduce_ignored(Defs)
         ),
        Exported
       )
     ).

-spec reduce_deps(
        perc_defs:defs(),
        [perc_types:perc_type()]
       ) -> perc_defs:defs().
reduce_deps(Defs, Exported) ->
    RecordDefs = perc_defs:get_records(Defs),
    UserTypeDefs = perc_defs:get_usertypes(Defs),
    Graph = perc_digraph:make(Defs),
    Deps = perc_digraph:compute_deps(Graph, Exported),
    DepsSet = sets:from_list(Deps),
    NewRecords =
        [R || R <- RecordDefs,
              sets:is_element(
                perc_defs:def_to_type(R),
                DepsSet
               )
        ],
    NewUserTypes =
        [U || U <- UserTypeDefs,
              sets:is_element(
                perc_defs:def_to_type(U),
                DepsSet
               )
        ],
    perc_digraph:delete(Graph),
    perc_defs:make(NewRecords, NewUserTypes).

-spec reduce_ignored(perc_defs:defs()) -> perc_defs:defs().
reduce_ignored(Defs) ->
    Graph = perc_digraph:make(Defs),
    Fun = fun(T) -> perc_digraph:reduce_ignored(Graph, T) end,
    NewDefs = perc_defs:apply_types(Fun, Defs),
    perc_digraph:delete(Graph),
    NewDefs.

-spec reduce_unions(perc_defs:defs()) -> perc_defs:defs().
reduce_unions(Defs) ->
    Fun = fun(Type) -> reduce_unions_in_type(Type) end,
    perc_defs:apply_types(Fun, Defs).

-spec reduce_usertypes(perc_defs:defs()) -> perc_defs:defs().
reduce_usertypes(Defs) ->
    Graph = perc_digraph:make(Defs),
    UserTypes = perc_defs:get_usertypes(Defs),
    UserTypeDict =
        dict:from_list(
          [{perc_defs:def_to_type(U), U}
           || U <- UserTypes]
         ),
    Subs = perc_digraph:make_substitutions(Graph, UserTypeDict),
    Fun = fun(Type) -> perc_digraph:reduce_usertypes(Type, Subs) end,
    NewDefs = perc_defs:apply_types(Fun, Defs),
    perc_digraph:delete(Graph),
    NewDefs.

%%====================================================================
%% Internal functions
%%====================================================================

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
