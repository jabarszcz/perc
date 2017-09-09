-module(perc_erl_gen).

%% API exports
-export([
    generate/1
  ]).

%%====================================================================
%% API functions
%%====================================================================

-spec generate(perc:generator()) -> iolist().
generate(Generator) ->
    ModuleAttr =
        erl_syntax:attribute(
          erl_syntax:atom(module),
          [erl_syntax:atom(perc:get_gen_erl_out(Generator))]
         ),
    Init =
        erl_syntax:function(
          erl_syntax:atom(init),
          [erl_syntax:clause(
             none,
             [erl_syntax:match_expr(
                erl_syntax:atom(ok),
                erl_syntax:application(
                  erl_syntax:atom(erlang),
                  erl_syntax:atom(load_nif),
                  [erl_syntax:string(perc:get_gen_sopath(Generator)),
                   erl_syntax:integer(0)]
                 )
               )]
            )]
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
                 none, erl_syntax:atom(throw),
                 [erl_syntax:atom(undefined)]
                )]
             )]
          )
         || RecName <- perc:get_gen_exported(Generator),
            Action <- ["encode"], %% TODO
            Backend <- perc:get_gen_backends(Generator)],
    ExportAttrs =
        erl_syntax:attribute(
          erl_syntax:atom(export),
          [erl_syntax:list(
             [erl_syntax:arity_qualifier(
                erl_syntax:function_name(Func),
                erl_syntax:integer(erl_syntax:function_arity(Func))
               )
              || Func <- [Init | Funcs]]
            )]
         ),
    OnloadAttr =
        erl_syntax:attribute(
          erl_syntax:atom(on_load),
          [erl_syntax:tuple(
             [erl_syntax:function_name(Init),
              erl_syntax:integer(erl_syntax:function_arity(Init))]
            )]
         ),
    erl_syntax:revert_forms(
      [ModuleAttr, ExportAttrs, OnloadAttr, Init | Funcs]
     ).
