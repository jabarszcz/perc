-module(prop_perc_reduce).

-include_lib("proper_eunit.hrl").

prop_reduce_ignored() ->
    ?FORALL(D, perc_defs:generator(),
            begin
                Reduced = perc_reduce:reduce_ignored(D),
                io:format("~p~n", [Reduced]),
                %% None of the record fields have "ignored" below the root
                Records = perc_defs:get_records(Reduced),
                true = lists:all(fun record_is_ignore_reduced/1, Records),
                %% None of the usertypes have "ignored" below the root
                Usertypes = perc_defs:get_usertypes(Reduced),
                true = lists:all(fun usertype_is_ignore_reduced/1, Usertypes)
            end
           ).

record_is_ignore_reduced(Record) ->
    lists:all(
      fun field_is_ignore_reduced/1,
      perc_defs:get_record_def_fields(Record)
     ).

field_is_ignore_reduced(Field) ->
    Type = perc_defs:get_record_field_type(Field),
    is_ignored(Type) orelse not has_ignored(Type).

usertype_is_ignore_reduced(Usertype) ->
    Type = perc_defs:get_usertype_def_type(Usertype),
    is_ignored(Type) orelse not has_ignored(Type).

has_ignored(Type) ->
    perc_types:fold(
      fun(T, Acc) ->
              is_ignored(T) orelse
                  any(Acc)
      end,
      Type
     ).

is_ignored(Type) ->
    perc_types:get_type(Type) =:= ignored.

any(List) ->
    lists:any(fun(X) -> X end, List).
