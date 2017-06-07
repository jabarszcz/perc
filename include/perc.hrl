-record(nif_module, {
          name :: string(),
          soname :: string(),
          exported_records :: [string()],
          all_records :: [string()],
          backends :: [string()]
         }).

