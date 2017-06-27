-ifndef(__PERC_TYPES_HRL__).
-define(__PERC_TYPES_HRL__, 1).

-record(record_field, {
          name :: undefined | string(),
          type :: undefined | perc_types:perc_type()
         }).

-record(record_def, {
          name :: undefined | string(),
          fields = [] :: [#record_field{}]
         }).

-record(user_type_def, {
          name :: undefined | string(),
          parameters = [] :: list(), %% TODO
          type :: undefined | perc_types:perc_type()
         }).

-endif.
