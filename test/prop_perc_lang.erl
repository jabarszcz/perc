-module(prop_perc_lang).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

prop_im_a_test_tmp() ->
    ?FORALL(D, perc_defs:defs(),
            begin
                A = {ok, perc_defs:apply_types(
                           fun ({ignored, _}) -> ignored;
                               (Else) -> Else
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

proper_test_() ->
    [{atom_to_list(F),
      {timeout, 600,
       fun () -> ?assert(
                    proper:quickcheck(
                      ?MODULE:F(),
                      [long_result, {to_file, user}])
                   )
       end
      }
     }
     || {F, 0} <- ?MODULE:module_info(exports), F > 'prop_', F < 'prop`'].
