-module(perc_types).

%% API exports
-export([
    children/1,
    fmap/2,
    fold/2,
    fold_fmap_postorder/3,
    get_type/1,
    get_ignored_reason/1,
    get_basic_type/1,
    get_maybe_type/1,
    get_list_type/1,
    get_tuple_types/1,
    get_union_types/1,
    get_record_name/1,
    get_usertype_name/1,
    get_function_names/1,
    get_function_arg/1,
    get_record_def_name/1,
    get_record_def_fields/1,
    get_record_field_name/1,
    get_record_field_type/1,
    get_usertype_def_name/1,
    get_usertype_def_type/1,
    is_record_def/1,
    is_usertype_def/1,
    make_ignored/0,
    make_ignored/1,
    make_basic/1,
    make_maybe/1,
    make_list/1,
    make_tuple/1,
    make_union/1,
    make_record/1,
    make_usertype/1,
    make_function/2,
    make_record_def/2,
    make_record_field/2,
    make_usertype_def/2,
    set_record_def_fields/2,
    set_record_field_type/2,
    set_usertype_def_type/2
  ]).

-export_type([
    perc_type/0,
    record_field/0,
    record_def/0,
    usertype_def/0
  ]).

%%====================================================================
% API types
%%====================================================================

-opaque perc_type() :: perc_ignored()
                     | perc_basic()
                     | perc_maybe()
                     | perc_list()
                     | perc_tuple()
                     | perc_record()
                     | perc_usertype()
                     | perc_union()
                     | perc_function().

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
-type perc_usertype() :: {usertype, {string(), [erl_syntax:syntaxtree()]}}.
-type perc_union() :: {union, [perc_type()]}.
-type perc_function() :: {function, {string(), string()}, perc_type()}.

-record(record_field, {
          name :: undefined | string(),
          type :: undefined | perc_types:perc_type()
         }).

-opaque record_field() :: #record_field{}.

-record(record_def, {
          name :: undefined | string(),
          fields = [] :: [#record_field{}]
         }).

-opaque record_def() :: #record_def{}.

-record(usertype_def, {
          name :: undefined | string(),
          type :: undefined | perc_types:perc_type()
         }).

-opaque usertype_def() :: #usertype_def{}.

%%====================================================================
% API functions
%%====================================================================

%% perc_type() accessors/mutators

-spec get_type(perc_type()) -> atom().
get_type(ignored) ->
    ignored;
get_type({Type, _}) ->
    Type;
get_type({Type, _, _}) ->
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

-spec get_record_name(perc_type()) -> string().
get_record_name({record, Name}) ->
    Name.

-spec get_usertype_name(perc_type()) -> string().
get_usertype_name({usertype, Name}) ->
    Name.

-spec get_function_names(perc_type()) ->
                                {undefined | string(),
                                 undefined | string()}.
get_function_names({function, {Enc, Dec}, _}) ->
    {Enc, Dec}.

-spec get_function_arg(perc_type()) -> perc_type().
get_function_arg({function, {_, _}, Arg}) ->
    Arg.

-spec get_record_def_name(record_def()) -> string().
get_record_def_name(RecordDef) ->
    RecordDef#record_def.name.

-spec get_record_def_fields(record_def()) -> [record_field()].
get_record_def_fields(RecordDef) ->
    RecordDef#record_def.fields.

-spec get_record_field_name(record_field()) -> string().
get_record_field_name(Field) ->
    Field#record_field.name.

-spec get_record_field_type(record_field()) -> perc_type().
get_record_field_type(Field) ->
    Field#record_field.type.

-spec get_usertype_def_name(usertype_def()) -> string().
get_usertype_def_name(UserType) ->
    UserType#usertype_def.name.

-spec get_usertype_def_type(usertype_def()) -> perc_type().
get_usertype_def_type(UserType) ->
    UserType#usertype_def.type.

-spec is_record_def(any()) -> boolean().
is_record_def(#record_def{} = _) ->
    true;
is_record_def(_) ->
    false.

-spec is_usertype_def(any()) -> boolean().
is_usertype_def(#usertype_def{} = _) ->
    true;
is_usertype_def(_) ->
    false.

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

-spec make_record(string()) -> perc_type().
make_record(Name) ->
    {record, Name}.

-spec make_usertype(string()) -> perc_type().
make_usertype(Name) ->
    {usertype, Name}.

-spec make_function({string(), string()}, perc_type()) -> perc_type().
make_function({Enc, Dec}, Arg) ->
    {function, {Enc, Dec}, Arg}.

-spec make_record_def(string(), [record_field()]) -> record_def().
make_record_def(Name, Fields) ->
    #record_def{name = Name, fields = Fields}.

-spec make_record_field(undefined | string(), perc_type()) -> record_field().
make_record_field(Name, Type) ->
    #record_field{name = Name, type = Type}.

-spec make_usertype_def(string(), perc_type()) -> usertype_def().
make_usertype_def(Name, Type) ->
    #usertype_def{name = Name, type = Type}.

-spec set_record_def_fields(record_def(), [record_field()]) ->
                                   record_def().
set_record_def_fields(RecordDef, Fields) ->
    RecordDef#record_def{fields=Fields}.

-spec set_record_field_type(record_field(), perc_types:perc_type()) ->
                                   record_field().
set_record_field_type(RecordField, Type) ->
    RecordField#record_field{type=Type}.

-spec set_usertype_def_type(usertype_def(), perc_types:perc_type()) ->
                                   usertype_def().
set_usertype_def_type(UserTypeDef, Type) ->
    UserTypeDef#usertype_def{type=Type}.

%% Type tree higher-order functions

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

-spec fold_fmap_postorder(
        fun((perc_type(), AccType) -> {perc_type(), AccType}),
        AccType,
        perc_type()
       ) -> {perc_type(), AccType}.
fold_fmap_postorder(Function, Acc, Type) ->
    {NewChildren, NewAcc} =
        lists:foldl(
          fun(Type_, {OldChildren, OldAcc}) ->
                  {ResType, ResAcc} =
                      fold_fmap_postorder(Function, OldAcc, Type_),
                  NewChildren = [ResType | OldChildren],
                  {NewChildren, ResAcc}
          end, {[], Acc}, children(Type)),
    apply(Function, [update(Type, lists:reverse(NewChildren)), NewAcc]).

-spec children(perc_type()) -> [perc_type()].
children(ignored) -> [];
children({ignored, _}) -> [];
children({basic, _}) -> [];
children({record, _}) -> [];
children({usertype, _}) -> [];
children({maybe, Type}) -> [Type];
children({list, Type}) -> [Type];
children({function, _, Type}) -> [Type];
children({tuple, Types}) -> Types;
children({union, Types}) -> Types.

%%====================================================================
%% Internal functions
%%====================================================================

-spec update(perc_type(), [perc_type()]) -> perc_type().
update(Type, []) -> Type;
update({maybe, _}, [Type]) -> {maybe, Type};
update({list, _}, [Type]) -> {list, Type};
update({function, Names, _}, [Type]) -> {function, Names, Type};
update({tuple, _}, Types) -> {tuple, Types};
update({union, _}, Types) -> {union, Types}.
