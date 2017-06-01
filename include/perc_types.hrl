-ifndef(__PERC_TYPES_HRL__).
-define(__PERC_TYPES_HRL__, 1).

-type perc_type() :: ignored
                   | integer
                   | float
                   | atom
                   | binary
                   | string
                   | boolean
                   | perc_maybe()
                   | perc_list()
                   | perc_tuple()
                   | perc_record().

-type perc_maybe() :: {maybe, perc_type()}.
-type perc_list() :: {list, perc_type()}.
-type perc_tuple() :: {tuple, [perc_type()]}.
-type perc_record() :: {record, string()}.

-record(record_field, {
          name :: undefined | string(),
          type :: undefined | perc_type()
         }).

-record(record_def, {
          name :: undefined | string(),
          fields = [] :: [#record_field{}]
         }).

-endif.
