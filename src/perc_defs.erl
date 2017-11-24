-module(perc_defs).

-export([
    apply_types/2,
    get_records/1,
    get_usertypes/1,
    make/2,
    merge/1,
    set_records/2,
    get_record_def_name/1,
    get_record_def_fields/1,
    get_record_field_name/1,
    get_record_field_type/1,
    get_record_field_filters/1,
    get_usertype_def_name/1,
    get_usertype_def_type/1,
    is_record_def/1,
    is_usertype_def/1,
    make_record_def/2,
    make_record_field/2,
    make_record_field/3,
    make_usertype_def/2,
    set_record_def_fields/2,
    set_record_field_type/2,
    set_record_field_filters/2,
    set_usertype_def_type/2,
    def_to_type/1
  ]).

-export_type([
    defs/0,
    record_field/0,
    record_def/0,
    usertype_def/0
  ]).

%%====================================================================
% API Types
%%====================================================================

-record(record_field, {
          name :: undefined | perc_id:id(),
          type :: perc_types:perc_type(),
          filters = [] :: [perc_filter:filter()]
         }).

-type record_field() :: #record_field{}.

-record(record_def, {
          name :: perc_id:id(),
          fields = [] :: [#record_field{}]
         }).

-type record_def() :: #record_def{}.

-record(usertype_def, {
          name :: perc_id:id(),
          type :: perc_types:perc_type()
         }).

-type usertype_def() :: #usertype_def{}.

-record(defs, {
          records = [] :: [record_def()],
          usertypes = [] :: [usertype_def()]
         }).

-type defs() :: #defs{}.

%%====================================================================
%% API functions
%%====================================================================

-spec get_records(defs()) -> [record_def()].
get_records(Defs) ->
    Defs#defs.records.

-spec get_usertypes(defs()) -> [usertype_def()].
get_usertypes(Defs) ->
    Defs#defs.usertypes.

-spec make(
        [record_def()],
        [usertype_def()]
       ) -> defs().
make(RecordDefs, UserTypeDefs) ->
    #defs{
       records=RecordDefs,
       usertypes=UserTypeDefs
      }.

-spec merge([defs()]) -> defs().
merge(DefsList) ->
    {RecordDefsLists, UserTypeDefsLists} =
        lists:unzip(
          [{RecDefs, UserDefs}
           || #defs{records=RecDefs,
                    usertypes=UserDefs} <- DefsList]
         ),
    RecordDefs = lists:append(RecordDefsLists),
    UserTypeDefs = lists:append(UserTypeDefsLists),
    #defs{records=RecordDefs,
          usertypes=UserTypeDefs}.

-spec set_records(defs(), [record_def()]) -> defs().
set_records(Defs, Records) ->
    Defs#defs{records=Records}.

-spec get_record_def_name(record_def()) -> perc_id:id().
get_record_def_name(RecordDef) ->
    RecordDef#record_def.name.

-spec get_record_def_fields(record_def()) -> [record_field()].
get_record_def_fields(RecordDef) ->
    RecordDef#record_def.fields.

-spec get_record_field_name(record_field()) -> undefined | perc_id:id().
get_record_field_name(Field) ->
    Field#record_field.name.

-spec get_record_field_type(record_field()) -> perc_types:perc_type().
get_record_field_type(Field) ->
    Field#record_field.type.

-spec get_record_field_filters(record_field()) -> [perc_filter:filter()].
get_record_field_filters(Field) ->
    Field#record_field.filters.

-spec get_usertype_def_name(usertype_def()) -> perc_id:id().
get_usertype_def_name(UserType) ->
    UserType#usertype_def.name.

-spec get_usertype_def_type(usertype_def()) -> perc_types:perc_type().
get_usertype_def_type(UserType) ->
    UserType#usertype_def.type.

-spec is_record_def(any()) -> boolean().
is_record_def(#record_def{} = _) ->
    true;
is_record_def(_) ->
    false.

-spec is_usertype_def(any()) -> boolean().
is_usertype_def(#usertype_def{} = _) ->
    true;
is_usertype_def(_) ->
    false.

-spec make_record_def(perc_id:id(), [record_field()]) -> record_def().
make_record_def(Name, Fields) ->
    #record_def{name = Name, fields = Fields}.

-spec make_record_field(
        undefined | perc_id:id(),
        perc_types:perc_type()
       ) -> record_field().
make_record_field(Name, Type) ->
    #record_field{name = Name, type = Type}.

-spec make_record_field(
        undefined | perc_id:id(),
        perc_types:perc_type(),
        [perc_filter:filter()]
       ) -> record_field().
make_record_field(Name, Type, Filters) ->
    #record_field{name = Name, type = Type, filters=Filters}.

-spec make_usertype_def(perc_id:id(), perc_types:perc_type()) -> usertype_def().
make_usertype_def(Name, Type) ->
    #usertype_def{name = Name, type = Type}.

-spec set_record_def_fields(record_def(), [record_field()]) ->
                                   record_def().
set_record_def_fields(RecordDef, Fields) ->
    RecordDef#record_def{fields=Fields}.

-spec set_record_field_type(record_field(), perc_types:perc_type()) ->
                                   record_field().
set_record_field_type(RecordField, Type) ->
    RecordField#record_field{type=Type}.

-spec set_record_field_filters(record_field(), [perc_filter:filter()]) ->
                                   record_field().
set_record_field_filters(RecordField, Filters) ->
    RecordField#record_field{filters=Filters}.

-spec set_usertype_def_type(usertype_def(), perc_types:perc_type()) ->
                                   usertype_def().
set_usertype_def_type(UserTypeDef, Type) ->
    UserTypeDef#usertype_def{type=Type}.

-spec def_to_type(usertype_def() | record_def()) -> perc_types:perc_type().
def_to_type(#record_def{name=Name}) ->
    perc_types:make_record(Name);
def_to_type(#usertype_def{name=Name}) ->
    perc_types:make_usertype(Name).

-spec apply_types(
        fun((perc_types:perc_type()) -> perc_types:perc_type()),
        defs()
       ) -> defs().
apply_types(Fun, Defs) ->
    #defs{
       records = [apply_types_record(R, Fun) || R <- Defs#defs.records],
       usertypes = [apply_types_usertype(R, Fun) || R <- Defs#defs.usertypes]
      }.

%%====================================================================
%% Internal functions
%%====================================================================

apply_types_record(Record, Fun) ->
    Record#record_def{
      fields = [apply_types_field(F, Fun) || F <- Record#record_def.fields]
     }.

apply_types_field(Field, Fun) ->
    Field#record_field{
      type = perc_types:fmap(Fun, Field#record_field.type)
     }.

apply_types_usertype(UserType, Fun) ->
    UserType#usertype_def{
      type = perc_types:fmap(Fun, UserType#usertype_def.type)
     }.
