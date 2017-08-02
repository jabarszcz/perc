Nonterminals

defs
def_list
def
record_def
usertype_def
fields
field
type
type_application
applicable
usertype_ref
record_ref
args
args_list.

Terminals

record
usertype
id
wildcard
def_sep
'::'
'{'
'}'
'('
')'
'<'
'>'
','.

Rootsymbol defs.

defs ->
    def_list :
        perc:make_defs(
          lists:filter(fun perc_types:is_record_def/1, value_of('$1')),
          lists:filter(fun perc_types:is_usertype_def/1, value_of('$1'))
         ).

def_list ->
    def def_list : ['$1' | '$2'].
def_list ->
    '$empty' : [].

def ->
    record_def : '$1'.
def ->
    usertype_def : '$1'.

record_def ->
    record id '::' '{' fields '}' def_sep :
        {perc_types:make_record_def(value_of('$2'), value_of('$5')),
         line_of('$1')}.
usertype_def ->
    usertype id '::' type def_sep :
        {perc_types:make_usertype_def(value_of('$2'), value_of('$4')),
         line_of('$1')}.

fields ->
    field : ['$1'].
fields ->
    field ',' fields : ['$1' | '$3'].
fields ->
    '$empty' : [].

field ->
    wildcard :
        {perc_types:make_record_field(undefined, perc_types:make_ignored()),
         line_of('$1')}.
field ->
    id '::' type :
        {perc_types:make_record_field(value_of('$1'), value_of('$3')),
         line_of('$1')}.

type ->
    id : {make_type('$1'), line_of('$1')}.
type ->
    record_ref : {make_type('$1'), line_of('$1')}.
type ->
    type_application : '$1'.

type_application ->
    applicable '(' args ')' : {make_type('$1', value_of('$3')), line_of('$1')}.

applicable ->
    id : '$1'.
applicable ->
    usertype_ref : '$1'.

usertype_ref ->
    usertype '<' id '>' : {{usertype, value_of('$3')}, line_of('$1')}.

record_ref ->
    record '<' id '>' : {{record, value_of('$3')}, line_of('$1')}.

args ->
    '$empty' : [].
args ->
    args_list : '$1'.

args_list ->
    type : ['$1'].
args_list ->
    type ',' args_list : ['$1' | '$3'].

Erlang code.

make_type(IdTuple) ->
    make_type(IdTuple, []).

make_type(IdTuple, Args) ->
    {Id, Line} = {value_of(IdTuple), line_of(IdTuple)},
    case {Id, Args} of
        {"ignored", _} ->
            perc_types:make_ignored();
        {"maybe", [Arg]} ->
            perc_types:make_maybe(Arg);
        {"list", [Arg]} ->
            perc_types:make_list(Arg);
        {"tuple", _} ->
            perc_types:make_tuple(Args);
        {"union", _} ->
            perc_types:make_union(Args);
        {{record, Name}, []} ->
            perc_types:make_record(Name);
        {{usertype, Name}, _} ->
            perc_types:make_usertype(Name); %% TODO Args
        {Basic, []} ->
            BasicSet = sets:from_list([integer, float, atom, binary,
                                       string, boolean, undefined_atom]),
            Atom = list_to_atom(Basic),
            case sets:is_element(Atom, BasicSet) of
                true ->
                    perc_types:make_basic(list_to_atom(Basic));
                _ ->
                    MessageFmt = "Unknown perc type ~s",
                    Message = io_lib:format(MessageFmt, [Id]),
                    return_error(Line, lists:flatten(Message))
            end;
        _ ->
            MessageFmt = "Unknown perc type application ~s/~B",
            Message = io_lib:format(MessageFmt, [Id, length(Args)]),
            return_error(Line, lists:flatten(Message))
    end.

value_of({_Cat, _Line, Value}) ->
    Value;
value_of({Value, _line}) ->
    Value;
value_of(L) when is_list(L) ->
    [value_of(E) || E <- L].
line_of(Token) -> element(2, Token).
