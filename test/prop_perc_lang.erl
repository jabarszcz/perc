-module(prop_perc_lang).

-include_lib("proper_eunit.hrl").

prop_parse_print_almost_symmetric() ->
    ?FORALL(D, perc_defs:defs(),
            begin
                A = {ok, perc_defs:apply_types(
                           fun (Type) ->
                                   perc_types:fmap(
                                     fun ({ignored, _}) -> ignored;
                                         (Else) -> Else
                                     end,
                                     Type
                                    )
                           end,
                           D
                          )},
                B = perc_rparser:parse_defs_str(
                      lists:flatten(perc_prettypr:format(D))
                     ),
                A == B orelse io:format("~w =/=~n~w~n", [A, B]),
                A == B
            end
           ).
