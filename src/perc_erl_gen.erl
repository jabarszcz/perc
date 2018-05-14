-module(perc_erl_gen).

-include_lib("syntax_tools/include/merl.hrl").

%% API exports
-export([
    generate/1
  ]).

%%====================================================================
%% API functions
%%====================================================================

-spec generate(perc_opts:options()) -> iolist().
generate(Opts) ->
    SoPath = filename:rootname(perc_opts:get_sopath_indirect(Opts)),
    AppName = perc_opts:get_appname(Opts),
    ModuleAttr =
        erl_syntax:attribute(
          erl_syntax:atom(module),
          [erl_syntax:atom(perc_opts:get_module_name(Opts))]
         ),
    Init =
        case AppName of
            undefined ->
                merl:tree(
                  ?Q("init() ->"
                     "  ok = erlang:load_nif(\"'@SoPath@\", 0)."
                    )
                  );
            _ ->
                merl:tree(
                  ?Q("init() -> "
                     "  AppStr ="
                     "    case code:lib_dir('@AppName@') of"
                     "        {error, bad_name} ->"
                     "           erlang:error({bad_appname, '@AppName@'});"
                     "        Str -> Str"
                     "    end,"
                     "  ok ="
                     "    erlang:load_nif("
                     "      filename:join("
                     "        AppStr,"
                     "        \"'@SoPath@\""
                     "       ),"
                     "      0"
                     "     )."
                    )
                 )
        end,
    OptBody =
        erl_syntax:abstract(Opts),
    GetOpts =
        merl:tree(
          ?Q("getopts() ->"
             "  _@OptBody."
            )
         ),
    Funcs =
        [erl_syntax:function( %% TODO spec
           erl_syntax:atom(
             lists:flatten(
               perc_backend:get_nif_function_name(Backend, Action, RecName)
              )
            ),
           [erl_syntax:clause(
              [erl_syntax:underscore(),
               erl_syntax:underscore()],
              none,
              [erl_syntax:application(
                 erl_syntax:atom(erlang),
                 erl_syntax:atom(nif_error),
                 [erl_syntax:atom(nif_not_loaded)]
                )]
             )]
          )
         || RecName <- perc_opts:get_exported(Opts),
            Action <- ["encode"], %% TODO
            Backend <- perc_opts:get_backends(Opts)],
    AllFuncs = [Init, GetOpts | Funcs],
    ExportAttrs =
        erl_syntax:attribute(
          erl_syntax:atom(export),
          [erl_syntax:list(
             [erl_syntax:arity_qualifier(
                erl_syntax:function_name(Func),
                erl_syntax:integer(erl_syntax:function_arity(Func))
               )
              || Func <- AllFuncs]
            )]
         ),
    erl_syntax:revert_forms(
      [ModuleAttr, ExportAttrs | AllFuncs]
     ).
