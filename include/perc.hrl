-include("perc_types.hrl").

-record(nif_module, {
          name :: string(),
          soname :: string(),
          exported_records :: [string()],
          user_types :: [#user_type_def{}],
          all_records :: [string()],
          record_dict :: dict:dict(string(), #record_def{}),
          backends :: [atom()]
         }).

