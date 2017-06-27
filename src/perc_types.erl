-module(perc_types).

-include("perc_types.hrl").

-export([
         get_type/1,
         get_ignored_reason/1,
         get_basic_type/1,
         get_maybe_type/1,
         get_list_type/1,
         get_tuple_types/1,
         get_union_types/1,
         get_record_type_name/1,
         get_user_type_name/1,
         make_ignored/0,
         make_ignored/1,
         make_basic/1,
         make_maybe/1,
         make_list/1,
         make_tuple/1,
         make_union/1,
         make_record_type/1,
         make_user_type/2,
         get_record_deps/2,
         reduce/2
        ]).

-export_type([perc_type/0]).


-opaque perc_type() :: perc_ignored()
                     | perc_basic()
                     | perc_maybe()
                     | perc_list()
                     | perc_tuple()
                     | perc_record()
                     | perc_user_type()
                     | perc_union().

-type perc_ignored() :: ignored
                        | {ignored, Reason :: any()}.

-type basic() :: integer
               | float
               | atom
               | binary
               | string
               | boolean
               | undefined_atom.

-type perc_basic() :: {basic, basic()}.
-type perc_maybe() :: {maybe, perc_type()}.
-type perc_list() :: {list, perc_type()}.
-type perc_tuple() :: {tuple, [perc_type()]}.
-type perc_record() :: {record, string()}.
-type perc_user_type() :: {user_type, {string(), [erl_syntax:syntaxtree()]}}.
-type perc_union() :: {union, [perc_type()]}.


% perc_type() manipulation

-spec get_type(perc_type()) -> atom().
get_type(ignored) ->
    ignored;
get_type({Type, _}) ->
    Type.

-spec get_ignored_reason(perc_type()) -> any().
get_ignored_reason(ignored) ->
    undefined;
get_ignored_reason({ignored, Reason}) ->
    Reason.

-spec get_basic_type(perc_type()) -> atom().
get_basic_type({basic, Type}) ->
    Type.

-spec get_maybe_type(perc_type()) -> perc_type().
get_maybe_type({maybe, Type}) ->
    Type.

-spec get_list_type(perc_type()) -> perc_type().
get_list_type({list, Type}) ->
    Type.

-spec get_tuple_types(perc_type()) -> [perc_type()].
get_tuple_types({tuple, Types}) ->
    Types.

-spec get_union_types(perc_type()) -> [perc_type()].
get_union_types({union, Types}) ->
    Types.

-spec get_record_type_name(perc_type()) -> string().
get_record_type_name({record, Name}) ->
    Name.

-spec get_user_type_name(perc_type()) -> string().
get_user_type_name({user_type, {Name, _}}) ->
    Name.

-spec make_ignored() -> perc_type().
make_ignored() ->
    ignored.

-spec make_ignored(any()) -> perc_type().
make_ignored(Reason) ->
    {ignored, Reason}.

-spec make_basic(atom()) -> perc_basic().
make_basic(Type) ->
    {basic, Type}.

-spec make_maybe(perc_type()) -> perc_type().
make_maybe(Type) ->
    {maybe, Type}.

-spec make_list(perc_type()) -> perc_type().
make_list(Type) ->
    {list, Type}.

-spec make_tuple([perc_type()]) -> perc_type().
make_tuple(Types) ->
    {tuple, Types}.

-spec make_union([perc_type()]) -> perc_type().
make_union(Types) ->
    {union, Types}.

-spec make_record_type(string()) -> perc_type().
make_record_type(Name) ->
    {record, Name}.

-spec make_user_type(string(), any()) -> perc_type().
make_user_type(Name, Params) ->
    {user_type, {Name, Params}}.

%% Record dependencies
-spec get_record_deps([string()], dict:dict(string(), #record_def{})) -> [string()].
get_record_deps(RootRecords, RecordDict) ->
    sets:to_list(get_record_deps(RootRecords,
                                 RecordDict,
                                 sets:from_list(RootRecords))).

get_record_deps([Curr | ToVisit], RecordDict, Deps) ->
    CurrRecord = dict:fetch(Curr, RecordDict),
    CurrDeps = sets:to_list(record_deps_from_record(CurrRecord)),
    {NewToVisit, NewDeps} =
        lists:foldl(
          fun(Dep, {NewToVisit, NewDeps}) ->
                  case sets:is_element(Dep, NewDeps) of
                      true ->
                          {NewToVisit, NewDeps};
                      false ->
                          {[Dep | NewToVisit], sets:add_element(Dep, NewDeps)}
                  end
          end,
          {ToVisit, Deps},
          CurrDeps),
    get_record_deps(NewToVisit, RecordDict, NewDeps);
get_record_deps([], _, Deps) ->
    Deps.

record_deps_from_record(#record_def{fields=Fields}) ->
    sets:union([record_deps_from_field(Field) || Field <- Fields]).

record_deps_from_field(Field) ->
    fold(fun
             ({record, Name}, _) ->
                 sets:from_list([Name]);
             (_, Accs) ->
                 sets:union(Accs)
         end, Field#record_field.type).

%% Type reductions (simplifications)
-spec reduce([#record_def{}], [#user_type_def{}]) -> {[#record_def{}], [#user_type_def{}]}.
reduce(Records, UserTypes) ->
    UserTypesDict =
        dict:from_list(
          [{UserType#user_type_def.name, UserType} || UserType <- UserTypes]
          ),
    UserTypeNames = [UserType#user_type_def.name || UserType <- UserTypes],
    Substitutions = make_substitutions(UserTypeNames, UserTypesDict),
    NonReducedTypes =
        [UserType#user_type_def{
           type=reduce_perc_type(
                  UserType#user_type_def.type,
                  Substitutions
                 )}
         || UserType <- UserTypes,
            not dict:is_key(UserType#user_type_def.name,
                            Substitutions)],
    Reduced = reduce_records(Records, Substitutions),
    {Reduced, NonReducedTypes}.

reduce_records(Records, Substitutions) ->
    [reduce_record(Rec, Substitutions) || Rec <- Records].

reduce_record(Record, Substitutions) ->
    ReducedFields =
        [Field#record_field{
           type=reduce_perc_type(Field#record_field.type, Substitutions)
          }
         || Field <- Record#record_def.fields],
    Record#record_def{fields=ReducedFields}.

reduce_perc_type(Type, Substitutions) ->
    reduce_unions(reduce_ignored(reduce_user_types(Type, Substitutions))).

join_types(Types, Maybe) ->
    join_types(Types, Maybe, sets:new()).

join_types([], Maybe, Set) ->
    Union = case sets:size(Set) of
                0 -> {ignored, empty_union};
                1 -> hd(sets:to_list(Set));
                _ -> {union, sets:to_list(Set)}
            end,
    case {Maybe, Union} of
        {true, {ignored, empty_union}} -> {basic, atom};
        {true, _} -> {maybe, Union};
        _ -> Union
    end;
join_types([Type|Types], Maybe, Set) ->
    {NewMaybe, NewType} =
        case Type of
            {maybe, Rest} -> {true, Rest};
            {basic, undefined_atom} -> {true, skip};
            _ -> {Maybe, Type}
        end,
    if NewType == skip ->
            join_types(Types, NewMaybe, Set);
       true ->
            join_types(Types, NewMaybe,
                         sets:add_element(NewType, Set))
    end.

reduce_unions(Type) ->
    fmap(fun
             (Type_={maybe, _}) ->
                 join_types(children(Type_), true);
             (Type_={union, _}) ->
                 join_types(children(Type_), false);
             (Else) ->
                 Else
         end, Type).

is_ignored(ignored) ->
    true;
is_ignored({ignored, _}) ->
    true;
is_ignored(_) ->
    false.

reduce_ignored(Type) ->
    fmap(fun(Type_) ->
                 case lists:filter(fun is_ignored/1, children(Type_)) of
                     [H|_] -> H;
                     _ -> Type_
                 end
         end, Type).

-spec reduce_user_types(perc_type(),
                        dict:dict(string(), perc_type())) ->
                              perc_type().
reduce_user_types(Type, Substitutions) ->
    fmap(fun
             (UserType = {user_type, {UserTypeName, _}}) -> %% TODO args
                 case dict:find(UserTypeName, Substitutions) of
                     {ok, Value} ->
                         Value;
                     _ -> UserType
                 end;
             (Else) ->
                 Else
         end,
         Type).


%% User type substitution

-record(subst_state, {
          visited = sets:new() :: sets:set(string()),
          substitutions = dict:new() :: dict:dict(string(), perc_type())
         }).

-spec make_substitutions([string()],
                         dict:dict(string(), perc_type())) ->
                                dict:dict(string(), perc_type()).
make_substitutions(UserTypes, UserTypesDict) ->
    make_substitutions(UserTypes, UserTypesDict, #subst_state{}).

-spec make_substitutions([string()],
                         dict:dict(string(), perc_type()),
                         #subst_state{}) ->
                                dict:dict(string(), perc_type()).
make_substitutions([UserTypeName|UserTypeNames],
                    UserTypesDict,
                    SubstState =
                       #subst_state{substitutions=Substitutions}) ->
    case dict:is_key(UserTypeName, Substitutions) of
        true ->
            make_substitutions(UserTypeNames, UserTypesDict, SubstState);
        _ ->
            {_, NewState} =
                reduce_and_make_subst(
                  {user_type, {UserTypeName, undefined}}, %% TODO args
                  UserTypesDict,
                  SubstState),
            make_substitutions(UserTypeNames, UserTypesDict, NewState)
    end;
make_substitutions([], _, SubstState) ->
    SubstState#subst_state.substitutions.

reduce_and_make_subst(Type, UserTypeDict, SubstState) ->
    fmap_fold_postorder(
      fun
          (UserType = {user_type, {UserTypeName, _}}, %% TODO args
           SubstStateAcc =
               #subst_state{visited=Visited,
                            substitutions=Substitutions}) ->
              case {dict:find(UserTypeName, Substitutions),
                    sets:is_element(UserTypeName, Visited)} of
                  {{ok, Value}, _} ->
                      %% UserType is already in substitutions
                      {Value, SubstStateAcc};
                  {_, true} ->
                      %% UserType is a recusive or mutually
                      %% recusive type and can't be reduced or
                      %% substituted
                      {UserType, SubstStateAcc};
                  _ ->
                      %% UserType has not been substituted yet
                      NewVisited = sets:add_element(UserTypeName, Visited),
                      #user_type_def{type=UserTypeType} =
                          dict:fetch(UserTypeName, UserTypeDict),
                      {Substituted, NewState} =
                          reduce_and_make_subst(
                            UserTypeType,
                            UserTypeDict,
                            SubstStateAcc#subst_state{visited=NewVisited}
                           ),
                      case contains_user_types(Substituted) of
                          true -> {UserType, NewState};
                          _ ->
                              NewSubstitutions =
                                  dict:store(
                                    UserTypeName,
                                    Substituted,
                                    NewState#subst_state.substitutions
                                   ),
                              {Substituted, NewState#subst_state{
                                              substitutions=NewSubstitutions
                                             }}
                      end
              end;
          (Else, SubstStateAcc) ->
              {Else, SubstStateAcc}
      end,
      SubstState,
      Type).

contains_user_types(Type) ->
    fold(fun
             ({user_type, _}, _) -> true;
             (_, List) -> lists:any(fun(X) -> X==true end, List)
         end, Type).

%% Type tree higher-order functions

-spec children(perc_type()) -> [perc_type()].
children(ignored) -> [];
children({ignored, _}) -> [];
children({basic, _}) -> [];
children({record, _}) -> [];
children({user_type, _}) -> [];
children({maybe, Type}) -> [Type];
children({list, Type}) -> [Type];
children({tuple, Types}) -> Types;
children({union, Types}) -> Types.

-spec update(perc_type(), [perc_type()]) -> perc_type().
update(Type, []) -> Type;
update({maybe, _}, [Type]) -> {maybe, Type};
update({list, _}, [Type]) -> {list, Type};
update({tuple, _}, Types) -> {tuple, Types};
update({union, _}, Types) -> {union, Types}.

%% Fold on the perc type tree
-spec fold(fun((perc_type(), [AccType]) -> AccType), perc_type()) -> AccType.
fold(Function, Type) ->
    apply(Function,
          [Type, [fold(Function, Child)
                  || Child <- children(Type)]
          ]).

%% Map on a perc_type tree to obtain a new perc type tree
-spec fmap(fun((perc_type()) -> perc_type()), perc_type()) -> perc_type().
fmap(Function, Type) ->
    fold(fun(Type_, Children) ->
                 apply(Function, [update(Type_, Children)])
         end, Type).

-spec fmap_fold_postorder(
        fun((perc_type(), AccType) -> {perc_type(), AccType}),
        AccType,
        perc_type()
       ) -> {perc_type(), AccType}.
fmap_fold_postorder(Function, Acc, Type) ->
    {NewChildren, NewAcc} =
        lists:foldl(
          fun(Type_, {OldChildren, OldAcc}) ->
                  {ResType, ResAcc} =
                      fmap_fold_postorder(Function, OldAcc, Type_),
                  NewChildren = [ResType | OldChildren],
                  {NewChildren, ResAcc}
          end, {[], Acc}, children(Type)),
    apply(Function, [update(Type, lists:reverse(NewChildren)), NewAcc]).
