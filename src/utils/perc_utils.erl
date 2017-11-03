-module(perc_utils).

-export([
    enumerate/1
  ]).

-spec enumerate([A]) -> [{integer(), A}].
enumerate(List) ->
    lists:zip(lists:seq(1, length(List)), List).
