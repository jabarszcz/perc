Definitions.

REC = record
UTYPE = usertype
FN = fn
WS = [\f\n\r\s\t\v]
NL = \r?\n
SIMPLE_ID = [a-zA-Z][0-9a-zA-Z_]*
STRING_SQUOT = \'([^\'\\]|\\[\\\'])*\'
ID = {SIMPLE_ID}|{STRING_SQUOT}

Rules.

\%[^\r\n]*{NL}+         : skip_token. %% erlang style comments
/\*([^*]|\*[^/])*\*/    : skip_token. %% c++ style block comment
{WS}+                   : skip_token. %% ignore whitespace
{REC}                   : {token, {record, TokenLine}}.
{UTYPE}                 : {token, {usertype, TokenLine}}.
{FN}                    : {token, {function, TokenLine}}.
{ID}                    : {token, {id, TokenLine, unescape(TokenChars)}}.
_                       : {token, {wildcard, TokenLine}}.
\.                      : {token, {def_sep, TokenLine}}.
\:\:                    : {token, {'::', TokenLine}}.
\{                      : {token, {'{', TokenLine}}.
\}                      : {token, {'}', TokenLine}}.
\(                      : {token, {'(', TokenLine}}.
\)                      : {token, {')', TokenLine}}.
\[                      : {token, {'[', TokenLine}}.
\]                      : {token, {']', TokenLine}}.
<                       : {token, {'<', TokenLine}}.
>                       : {token, {'>', TokenLine}}.
,                       : {token, {',', TokenLine}}.

Erlang code.

-export([
    file/1,
    token_line/1,
    token_val/1,
    unescape/1
  ]).

-export_type([
    token/1,
    tokens/0
  ]).

-type token(V) :: {V, integer()} | {atom(), integer(), V}.

-type tokens() :: [token(any())].

-type string_ret() :: {ok, [tuple()], integer()}
                    | {error, {integer(), module(), any()}, any()}.

%% Silence compiler
-spec format_error(any()) -> string().
-spec string(string()) -> string_ret().
-spec string(string(), integer()) -> string_ret().
-spec token(any(), any()) -> any().
-spec token(any(), any(), any()) -> any().
-spec tokens(any(), any()) -> any().
-spec tokens(any(), any(), any()) -> any().

%% Silence dialyzer
-dialyzer({nowarn_function, yyrev/2}).

-spec file(string()) -> string_ret().
file(Filename) ->
    {ok, In} = file:read_file(Filename),
    string(unicode:characters_to_list(In)).

-spec token_val(token(V)) -> V.
token_val({_Category, _Line, Value}) ->
    Value;
token_val({Value, _Line}) ->
    Value.

-spec token_line(token(any())) -> integer().
token_line(Token) ->
    element(2, Token).

-spec unescape(string()) -> string().
unescape(S) ->
    case re:run(S, "^'(.*)'$", [{capture, [1], list}]) of
        {match, [Contents]} ->
            re:replace(
              Contents,
              "\\\\([\\\\'])",
              "\\g1",
              [global, {return, list}]
             );
        nomatch ->
            S
    end.
