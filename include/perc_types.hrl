-ifndef(__PERC_TYPES_HRL__).
-define(__PERC_TYPES_HRL__, 1).

-type perc_type() :: perc_ignored()
                   | perc_basic()
                   | perc_maybe()
                   | perc_list()
                   | perc_tuple()
                   | perc_record()
                   | perc_user_type()
                   | perc_union().

-type perc_ignored() :: ignored
                        | {ignored, Reason :: any()}.

-type basic() :: integer
               | float
               | atom
               | binary
               | string
               | boolean
               | undefined_atom.

-type perc_basic() :: {basic, basic()}.
-type perc_maybe() :: {maybe, perc_type()}.
-type perc_list() :: {list, perc_type()}.
-type perc_tuple() :: {tuple, [perc_type()]}.
-type perc_record() :: {record, string()}.
-type perc_user_type() :: {user_type, {string(), [erl_syntax:syntaxtree()]}}.
-type perc_union() :: {union, [perc_type()]}.

-record(record_field, {
          name :: undefined | string(),
          type :: undefined | perc_type()
         }).

-record(record_def, {
          name :: undefined | string(),
          fields = [] :: [#record_field{}]
         }).

-record(user_type_def, {
          name :: undefined | string(),
          parameters = [] :: list(), %% TODO
          type :: undefined | perc_type()
         }).

-endif.
