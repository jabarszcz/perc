-module(perc_defs).

-export([
    get_records/1,
    get_usertypes/1,
    make/2,
    merge/1,
    set_records/2
  ]).

-export_type([
    defs/0
  ]).

%%====================================================================
% API Types
%%====================================================================

-record(defs, {
          records = [] :: [perc_types:record_def()],
          usertypes = [] :: [perc_types:usertype_def()]
         }).

-opaque defs() :: #defs{}.

%%====================================================================
%% API functions
%%====================================================================

-spec get_records(defs()) -> [perc_types:record_def()].
get_records(Defs) ->
    Defs#defs.records.

-spec get_usertypes(defs()) -> [perc_types:usertype_def()].
get_usertypes(Defs) ->
    Defs#defs.usertypes.

-spec make(
        [perc_types:record_defs()],
        [perc_types:usertype_defs()]
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

-spec set_records(defs(), [perc_types:record_def()]) -> defs().
set_records(Defs, Records) ->
    Defs#defs{records=Records}.
