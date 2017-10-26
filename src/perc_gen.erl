-module(perc_gen).

%% API exports
-export([
    get_defs/1,
    get_opts/1,
    make/2,
    set_defs/2
  ]).

-export_type([
    gen/0
  ]).

%%====================================================================
% API Types
%%====================================================================

-record(gen, {
    defs :: perc_defs:defs(),
    opts :: [perc:option()]
 }).

-opaque gen() :: #gen{}.

%%====================================================================
%% API functions
%%====================================================================

-spec get_defs(gen()) -> perc:defs().
get_defs(Gen) ->
    Gen#gen.defs.

-spec get_opts(gen()) -> perc:opts().
get_opts(Gen) ->
    Gen#gen.opts.

-spec make(
        perc_defs:defs(),
        perc_opts:options()
       ) -> gen().
make(Defs, Opts) ->
    #gen{
       defs=Defs,
       opts=Opts
      }.

-spec set_defs(gen(), perc_defs:defs()) -> gen().
set_defs(Gen, Defs) ->
    Gen#gen{defs=Defs}.
