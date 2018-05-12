-module(perc_filter).

%% API exports
-export([
    get_name/1,
    make/1,
    defs_add_automatic_filters/1,
    record_add_automatic_filters/1,
    field_add_automatic_filters/1,
    add_automatic_filters/2
  ]).

-export_type([
    filter/0
  ]).

%%====================================================================
% API types
%%====================================================================

-record(filter, {
          name :: perc_id:id()
         }).

-type filter() :: #filter{}.

%%====================================================================
% API functions
%%====================================================================

-spec get_name(filter()) -> perc_id:id().
get_name(Filter) ->
    Filter#filter.name.

-spec make(perc_id:id()) -> filter().
make(Name) ->
    #filter{name=Name}.

-spec defs_add_automatic_filters(
        perc_defs:defs()
       ) -> perc_defs:defs().
defs_add_automatic_filters(Defs) ->
    Records = perc_defs:get_records(Defs),
    NewRecords = [record_add_automatic_filters(R) || R <- Records],
    perc_defs:set_records(Defs, NewRecords).

-spec record_add_automatic_filters(
        perc_defs:record_def()
       ) -> perc_defs:record_def().
record_add_automatic_filters(Record) ->
    Fields = perc_defs:get_record_def_fields(Record),
    NewFields = [field_add_automatic_filters(F) || F <- Fields],
    perc_defs:set_record_def_fields(Record, NewFields).

-spec field_add_automatic_filters(
        perc_defs:record_field()
       ) -> perc_defs:record_field().
field_add_automatic_filters(Field) ->
    Type = perc_defs:get_record_field_type(Field),
    Filters = perc_defs:get_record_field_filters(Field),
    {Type1, Filters1} = add_automatic_filters(Type, Filters),
    Field1 = perc_defs:set_record_field_type(Field, Type1),
    Field2 = perc_defs:set_record_field_filters(Field1, Filters1),
    Field2.

-spec add_automatic_filters(
        perc_types:perc_type(),
        [perc_filter:filter()]
       ) -> {perc_types:perc_type(), [perc_filter:filter()]}.
add_automatic_filters(Type, Filters) ->
    {Type1, Filters1} = maybe_add_empty_obj_filter(Type, Filters),
    {Type2, Filters2} = maybe_add_undef_filter(Type1, Filters1),
    {Type3, Filters3} = maybe_add_empty_list_filter(Type2, Filters2),
    {Type4, Filters4} = maybe_add_empty_binary_filter(Type3, Filters3),
    {Type4, Filters4}.

%%====================================================================
%% Internal functions
%%====================================================================

maybe_add_undef_filter(Type, Filters) ->
    case perc_types:get_type(Type) of
        union ->
            List = perc_types:get_union_types(Type),
            case {lists:member(perc_types:make_undefined_atom(), List),
                  length(List)} of
                {false, _} -> {Type, Filters};
                {true, 1} -> {Type, Filters};
                {true, _} ->
                    {perc_types:make_union(
                       lists:delete(perc_types:make_undefined_atom(), List)
                      ),
                     [make("no_undef") | Filters]}
            end;
        _ ->
            {Type, Filters}
    end.

maybe_add_empty_list_filter(Type, Filters) ->
    case has(Type, list) of
        true ->
            {Type, [make("no_empty_list") | Filters]};
        _ ->
            {Type, Filters}
    end.

maybe_add_empty_binary_filter(Type, Filters) ->
    case has(Type, binary) of
        true ->
            {Type, [make("no_empty_binary") | Filters]};
        _ ->
            {Type, Filters}
    end.

maybe_add_empty_obj_filter(Type, Filters) ->
    case has(Type, record) of
        true ->
            {Type, [make("no_empty_obj") | Filters]};
        _ ->
            {Type, Filters}
    end.

has(Type, Kind) ->
    case perc_types:get_type(Type) of
        Kind ->
            true;
        basic ->
            Kind == perc_types:get_basic_type(Type);
        union ->
            Members = perc_types:get_union_types(Type),
            lists:any(fun(M) -> has(M, Kind) end, Members);
        _ ->
            false
    end.
