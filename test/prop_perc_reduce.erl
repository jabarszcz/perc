-module(prop_perc_reduce).

-export([defs_exported_gen/0]).

-include_lib("proper_eunit.hrl").

-spec all(list(boolean())) -> boolean().
all(List) ->
    lists:all(fun(X) -> X end, List).

-spec any(list(boolean())) -> boolean().
any(List) ->
    lists:any(fun(X) -> X end, List).

-spec types_all(
        fun((perc_types:perc_type()) -> boolean()),
        perc_defs:defs()
       ) -> boolean().
types_all(Fun, Defs) ->
    Records = perc_defs:get_records(Defs),
    Fields = lists:append([perc_defs:get_record_def_fields(R) || R <- Records]),
    FieldTypes = [perc_defs:get_record_field_type(F) || F <- Fields],
    Usertypes = perc_defs:get_usertypes(Defs),
    UsertypeTypes = [perc_defs:get_usertype_def_type(U) || U <- Usertypes],
    lists:all(Fun, FieldTypes) and lists:all(Fun, UsertypeTypes).

defs_exported_gen() ->
    ?LET(Defs, perc_defs:generator(),
         begin
             Types =
                 [perc_defs:def_to_type(D)
                  || D <- perc_defs:get_records(Defs)
                         ++ perc_defs:get_usertypes(Defs)
                 ],
             ?LET(Exported, small_subset_gen(Types),
                  {Defs, Exported}
                 )
         end
        ).

has_no_dangling_refs(Type, DefinedRefsSet) ->
    perc_types:fold(
      fun(T, Acc) ->
              case perc_types:get_type(T) of
                  record ->
                      sets:is_element(T, DefinedRefsSet);
                  usertype ->
                      sets:is_element(T, DefinedRefsSet);
                  _ ->
                      true
              end andalso all(Acc)
      end,
      Type
     ).

defs_no_dangling_references(Defs, Exported) ->
    Dict = perc_defs:type_to_def_dict(Defs),
    DefinedRefsSet = sets:from_list(dict:fetch_keys(Dict)),
    Check =
        fun(Type) ->
                has_no_dangling_refs(Type, DefinedRefsSet)
        end,
    types_all(Check, Defs) and lists:all(Check, Exported).

has_ignored(Type) ->
    perc_types:fold(
      fun(T, Acc) ->
              is_ignored(T) orelse
                  any(Acc)
      end,
      Type
     ).

is_ignore_reduced(Type) ->
    is_ignored(Type) orelse not has_ignored(Type).

is_ignored(Type) ->
    perc_types:get_type(Type) =:= ignored.

prop_reduce_deps() ->
    ?FORALL({D, Exported}, defs_exported_gen(),
            begin
                Reduced = perc_reduce:reduce_deps(D, Exported),
                defs_no_dangling_references(Reduced, Exported)
            end
           ).

prop_reduce_ignored() ->
    ?FORALL(D, perc_defs:generator(),
            begin
                Reduced = perc_reduce:reduce_ignored(D),
                types_all(fun is_ignore_reduced/1, Reduced)
            end
           ).

small_subset_gen(List) ->
    ?LET(Mask,
         vector(
           length(List),
           wunion([{1, true}, {min(10, length(List)), false}])
          ),
         lists:flatmap(
           fun({true, Elem}) -> [Elem]; (_) -> [] end,
           lists:zip(Mask, List)
          )
        ).
