-module(perc_types).

-include_lib("proper/include/proper.hrl").

%% API exports
-export([
    children/1,
    fmap/2,
    fold/2,
    fold_fmap_postorder/3,
    get_type/1,
    get_ignored_reason/1,
    get_basic_type/1,
    get_list_type/1,
    get_tuple_types/1,
    get_union_types/1,
    get_record_name/1,
    get_usertype_name/1,
    get_function_names/1,
    get_function_arg/1,
    is_reference_type/1,
    make_ignored/0,
    make_ignored/1,
    make_undefined_atom/0,
    make_basic/1,
    make_list/1,
    make_tuple/1,
    make_union/1,
    make_record/1,
    make_usertype/1,
    make_function/2
  ]).

-export([
    generator/1
  ]).

-export_type([
    perc_type/0
  ]).

%%====================================================================
% API types
%%====================================================================

-type perc_type() :: perc_ignored()
                     | undefined_atom
                     | perc_basic()
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
               | boolean.

-type perc_basic() :: {basic, basic()}.
-type perc_list() :: {list, perc_type()}.
-type perc_tuple() :: {tuple, [perc_type()]}.
-type perc_record() :: {record, perc_id:id()}.
-type perc_usertype() :: {usertype, perc_id:id()}.
-type perc_union() :: {union, nonempty_list(perc_type())}.
-type perc_function() :: {function,
                          {undefined | perc_id:id(),
                           undefined | perc_id:id()},
                          perc_type()}.

%%====================================================================
% API functions
%%====================================================================

%% perc_type() accessors/mutators

-spec get_type(perc_type()) -> atom().
get_type(ignored) ->
    ignored;
get_type(undefined_atom) ->
    undefined_atom;
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

-spec get_list_type(perc_type()) -> perc_type().
get_list_type({list, Type}) ->
    Type.

-spec get_tuple_types(perc_type()) -> [perc_type()].
get_tuple_types({tuple, Types}) ->
    Types.

-spec get_union_types(perc_type()) -> [perc_type()].
get_union_types({union, Types}) ->
    Types.

-spec get_record_name(perc_type()) -> perc_id:id().
get_record_name({record, Name}) ->
    Name.

-spec get_usertype_name(perc_type()) -> perc_id:id().
get_usertype_name({usertype, Name}) ->
    Name.

-spec get_function_names(perc_type()) ->
                                {undefined | perc_id:id(),
                                 undefined | perc_id:id()}.
get_function_names({function, {Enc, Dec}, _}) ->
    {Enc, Dec}.

-spec get_function_arg(perc_type()) -> perc_type().
get_function_arg({function, {_, _}, Arg}) ->
    Arg.

-spec is_reference_type(perc_type()) -> boolean().
is_reference_type(T) ->
    case get_type(T) of
        record -> true;
        usertype -> true;
        _ -> false
    end.

-spec make_ignored() -> perc_type().
make_ignored() ->
    ignored.

-spec make_ignored(any()) -> perc_type().
make_ignored(Reason) ->
    {ignored, Reason}.

-spec make_undefined_atom() -> perc_type().
make_undefined_atom() ->
    undefined_atom.

-spec make_basic(atom()) -> perc_basic().
make_basic(Type) ->
    {basic, Type}.

-spec make_list(perc_type()) -> perc_type().
make_list(Type) ->
    {list, Type}.

-spec make_tuple([perc_type()]) -> perc_type().
make_tuple(Types) ->
    {tuple, Types}.

-spec make_union([perc_type()]) -> perc_type().
make_union(Types) ->
    {union, Types}.

-spec make_record(perc_id:id()) -> perc_type().
make_record(Name) ->
    {record, Name}.

-spec make_usertype(perc_id:id()) -> perc_type().
make_usertype(Name) ->
    {usertype, Name}.

-spec make_function({perc_id:id(), perc_id:id()}, perc_type()) -> perc_type().
make_function({Enc, Dec}, Arg) ->
    {function, {Enc, Dec}, Arg}.

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
children(undefined_atom) -> [];
children({basic, _}) -> [];
children({record, _}) -> [];
children({usertype, _}) -> [];
children({list, Type}) -> [Type];
children({function, _, Type}) -> [Type];
children({tuple, Types}) -> Types;
children({union, Types}) -> Types.

%%====================================================================
%% Internal functions
%%====================================================================

-spec update(perc_type(), [perc_type()]) -> perc_type().
update({list, _}, [Type]) -> {list, Type};
update({function, Names, _}, [Type]) -> {function, Names, Type};
update({tuple, _}, Types) -> {tuple, Types};
update({union, _}, Types) -> {union, Types};
update(Type, []) -> Type.

%%====================================================================
%% Proper Generators
%%====================================================================

-spec generator({[perc_id:id()], [perc_id:id()]}) -> proper_types:type().
generator(Names) ->
    ?SIZED(Size, generator_(Names, math:log10(Size))).

-spec generator_(
        {[perc_id:id()], [perc_id:id()]},
        number()
       ) -> proper_types:type().
generator_({[], []}, Depth) when Depth < 1 ->
    basic_ignored_gen();
generator_(Names, Depth) when Depth < 1 ->
    proper_types:oneof([basic_ignored_gen(), ref_gen(Names)]);
generator_(Names, Depth) ->
    proper_types:oneof(
      [
       ?LET(NonEmptyTypes, non_empty(list(generator_(Names, Depth-1))),
            make_union(NonEmptyTypes)
           ),
       ?LET(Types, list(generator_(Names, Depth-1)),
            make_tuple(Types)
           ),
       ?LET(Type, generator_(Names, Depth-1), make_list(Type)),
       func_gen(Names, Depth-1)
      ]
     ).

-spec basic_ignored_gen() -> proper_types:type().
basic_ignored_gen() ->
    ?LET(Basic, proper_types:oneof([perc_basic(), perc_ignored()]), Basic).

-spec func_gen(
        {[perc_id:id()], [perc_id:id()]},
        number()
       ) -> proper_types:type().
func_gen(Names, Depth) ->
    ?LET(Type, generator_(Names, Depth),
         ?LET(Ids, {perc_id:gen(), perc_id:gen()},
              make_function(Ids, Type)
             )
        ).

-spec ref_gen({[perc_id:id()], [perc_id:id()]}) -> proper_types:type().
ref_gen({RecordNames, []}) ->
    ?LET(RName, oneof(RecordNames), perc_types:make_record(RName));
ref_gen({[], UsertypeNames}) ->
    ?LET(UName, oneof(UsertypeNames), perc_types:make_usertype(UName));
ref_gen({RecordNames, UsertypeNames}) ->
    proper_types:oneof(
      [ref_gen({RecordNames, []}), ref_gen({[], UsertypeNames})]
     ).
