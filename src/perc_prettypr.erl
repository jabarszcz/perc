-module(perc_prettypr).

%% API exports
-export([
    format/1
  ]).

%%====================================================================
% API functions
%%====================================================================

-spec format(perc_defs:defs()) -> iolist().
format(Defs) ->
    [[io_lib:format("usertype ~s :: ~s.~n~n",
                    [perc_id:show(perc_defs:get_usertype_def_name(U)),
                     format_perc_type(
                       perc_defs:get_usertype_def_type(U)
                      )
                    ]) || U <- perc_defs:get_usertypes(Defs)],
     [io_lib:format("record ~s :: ~s.~n~n",
                    [perc_id:show(perc_defs:get_record_def_name(R)),
                     format_record_fields(R)
                    ]) || R <- perc_defs:get_records(Defs)]
    ].

%%====================================================================
%% Internal functions
%%====================================================================

format_record_fields(RecordDef) ->
    ["{\n",
     [format_record_field(F)
      || F <- perc_defs:get_record_def_fields(RecordDef)],
     "}"].

format_record_field(Field) ->
    Name = perc_defs:get_record_field_name(Field),
    NameStr =
        case Name of
            undefined -> "_";
            _ -> perc_id:show(Name)
        end,
    TypeStr =
        format_perc_type(
          perc_defs:get_record_field_type(Field)
         ),
    Filters = perc_defs:get_record_field_filters(Field),
    case Filters of
        [] ->
            io_lib:format("    ~s :: ~s,~n", [NameStr, TypeStr]);
        _ ->
            FiltersStr =
                string:join(
                  [perc_id:show(perc_filter:get_name(F)) || F <- Filters],
                  ","
                 ),
            io_lib:format("    ~s :: ~s [~s],~n",
                          [NameStr, TypeStr, FiltersStr])
    end.

format_perc_type(Type) ->
    case perc_types:get_type(Type) of
        ignored ->
            case perc_types:get_ignored_reason(Type) of
                undefined ->
                    "ignored";
                Reason ->
                    CommentSafeReason =
                        re:replace(
                          io_lib:format("~p", [Reason]),
                          "[*][/]", "*\\\\/", [global, {return, list}]
                         ),
                    io_lib:format("ignored /* ~s */", [CommentSafeReason])
            end;
        undefined_atom ->
            "undefined";
        basic ->
            atom_to_list(perc_types:get_basic_type(Type));
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
            io_lib:format("record<~s>", [perc_id:show(RecordName)]);
        usertype ->
            UserTypeName = perc_types:get_usertype_name(Type),
            io_lib:format("usertype<~s>", [perc_id:show(UserTypeName)]);
        function ->
            TypeArg = perc_types:get_function_arg(Type),
            TypeArgStr = format_perc_type(TypeArg),
            {Enc, Dec} = perc_types:get_function_names(Type),
            EncStr = function_to_id(Enc),
            DecStr = function_to_id(Dec),
            io_lib:format("fn<~s,~s>(~s)", [EncStr, DecStr, TypeArgStr])
    end.

function_to_id(undefined) ->
    "_";
function_to_id(Name) ->
    perc_id:show(Name).
