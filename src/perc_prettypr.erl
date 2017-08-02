-module(perc_prettypr).

%% API exports
-export([
    format/1
  ]).

%%====================================================================
% API functions
%%====================================================================

-spec format(perc:defs()) -> iolist().
format(Defs) ->
    [[io_lib:format("usertype ~s :: ~s.~n~n",
                    [perc_types:get_usertype_def_name(U),
                     format_perc_type(
                       perc_types:get_usertype_def_type(U)
                      )
                    ]) || U <- perc:get_defs_usertypes(Defs)],
     [io_lib:format("record ~s :: ~s.~n~n",
                    [perc_types:get_record_def_name(R),
                     format_record_fields(R)
                    ]) || R <- perc:get_defs_records(Defs)]
    ].

%%====================================================================
%% Internal functions
%%====================================================================

format_record_fields(RecordDef) ->
    ["{\n",
     [io_lib:format(
        "    ~s :: ~s,~n",
        [perc_types:get_record_field_name(F),
         format_perc_type(
           perc_types:get_record_field_type(F)
          )
        ]
       )
      || F <- perc_types:get_record_def_fields(RecordDef)],
     "}"].

format_perc_type(Type) ->
    case perc_types:get_type(Type) of
        ignored ->
            io_lib:format(
              "ignored /* ~p */",
              [perc_types:get_ignored_reason(Type)]
              );
        basic ->
            atom_to_list(perc_types:get_basic_type(Type));
        maybe ->
            MaybeType = perc_types:get_maybe_type(Type),
            io_lib:format("maybe(~s)", [format_perc_type(MaybeType)]);
        list ->
            ListType = perc_types:get_list_type(Type),
            io_lib:format("list(~s)", [format_perc_type(ListType)]);
        tuple ->
            TupleTypes = perc_types:get_tuple_types(Type),
            io_lib:format(
              "tuple(~s)",
              [string:join([format_perc_type(T) || T <- TupleTypes], ",")]
             );
        union ->
            UnionTypes = perc_types:get_union_types(Type),
            io_lib:format(
              "union(~s)",
              [string:join([format_perc_type(T) || T <- UnionTypes], ",")]
             );
        record ->
            RecordName = perc_types:get_record_name(Type),
            io_lib:format("record<~s>", [RecordName]);
        usertype ->
            UserTypeName = perc_types:get_usertype_name(Type),
            io_lib:format("usertype<~s>", [UserTypeName])
    end.
