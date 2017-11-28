-module(perc_id).

-include_lib("proper/include/proper.hrl").

%% API exports
-export([
    show/1,
    parse/1
  ]).

-export([
    gen/0
   ]).

-export_type([
    id/0
  ]).

%%====================================================================
% API types
%%====================================================================
-type id_char() :: 9..255. % 16#10ffff.
-type id() :: [id_char()].

%%====================================================================
% API functions
%%====================================================================

-spec show(id()) -> string().
show(Id) ->
    case should_quote(Id) of
        true ->
            Escaped = re:replace(
                        Id,
                        "([\\\\'])",
                        "\\\\\\g1",
                        [global, {return, list}]
                       ),
            lists:flatten(["'", Escaped, "'"]);
        _ ->
            Id
    end.

-spec parse(string()) -> id().
parse(Str) ->
    case re:run(Str, "^'(.*)'$", [{capture, [1], list}, dotall]) of
        {match, [Contents]} ->
            re:replace(
              Contents,
              "\\\\([\\\\'])",
              "\\g1",
              [global, {return, list}]
             );
        nomatch ->
            Str
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec should_quote(id()) -> boolean().
should_quote(Id) ->
    re:run(Id, "^[a-zA-Z]+$", [{capture, none}, dollar_endonly]) =:= nomatch
        orelse
        try {ok, [{id, _, _}|_], _} = perc_scanner:string(Id), false
        catch
            _:_ ->
                true
        end.

%%====================================================================
%% Proper Generators
%%====================================================================

gen() ->
    ?LET(I, id(), I).
