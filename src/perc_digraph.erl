-module(perc_digraph).

%% API exports
-export([
    make/2,
    delete/1,
    reduce_ignored/2,
    compute_deps/2,
    make_substitutions/2,
    reduce_usertypes/2,
    save/3
  ]).

-export_type([
    state/0,
    substitutions/0
  ]).

%%====================================================================
% API Types
%%====================================================================

-record(state, {
          graph = digraph:new() :: digraph:graph(),
          dict = dict:new() :: dict:dict(
                                 perc_types:perc_type(),
                                 digraph:vertex()
                                ),
          norec_graph = undefined :: undefined | digraph:graph()
         }).

-opaque state() :: #state{}.

-type substitutions() :: dict:dict(perc_types:perc_type(),
                                   perc_types:perc_type()).

%%====================================================================
%% API functions
%%====================================================================

-spec make(
        [perc_types:record_def()],
        [perc_types:usertype_def()]
       ) -> state().
make(Records, UserTypes) ->
    State1 = #state{},
    State2 = lists:foldl(fun add_record/2, State1, Records),
    State3 = lists:foldl(fun add_usertype/2, State2, UserTypes),
    add_norec_graph(State3).

-spec delete(state()) -> true.
delete(State) ->
    digraph:delete(State#state.graph),
    digraph:delete(State#state.norec_graph).

-spec reduce_ignored(
        state(),
        perc_types:perc_type()
       ) -> perc_types:perc_type().
reduce_ignored(State, Type) ->
    Vertex = dict:fetch(Type, State#state.dict),
    IgnoredVertex = dict:fetch(ignored, State#state.dict),
    Graph = State#state.norec_graph,
    case digraph:get_path(Graph, Vertex, IgnoredVertex) of
        false ->
            Type;
        Path ->
            Stack = filter_ignored_path(get_labels(Graph, Path)),
            perc_types:make_ignored(Stack)
    end.

-spec compute_deps(
        state(),
        [perc_types:perc_type()]
       ) -> [perc_types:perc_type()].
compute_deps(State, Exported) ->
    Graph = State#state.graph,
    Dict = State#state.dict,
    ExportedVertices = [dict:fetch(E, Dict) || E <- Exported],
    Reachable = digraph_utils:reachable(ExportedVertices, Graph),
    [T || T <- get_labels(Graph, Reachable), is_reference_type(T)].

-spec make_substitutions(
        state(),
        dict:dict(
          perc_types:perc_type(),
          perc_types:usertype_def()
         )
       ) -> substitutions().
make_substitutions(State, UserTypeDict) ->
    PostOrderVertices = digraph_utils:postorder(State#state.norec_graph),
    PostOrder = get_labels(State#state.graph, PostOrderVertices),
    Substitutable =
        [U || U <- PostOrder,
              is_usertype(U),
              is_subtstitutable(State, U)],
    make_substitutions(UserTypeDict, Substitutable, dict:new()).

-spec reduce_usertypes(
        perc_types:perc_type(),
        substitutions()
       ) -> perc_types:perc_type().
reduce_usertypes(Type, Substitutions) ->
    perc_types:fmap(
      fun(Type_) ->
              case {perc_types:get_type(Type_),
                    dict:find(Type_, Substitutions)} of
                  {usertype, {ok, Value}} ->
                      Value;
                  _ ->
                      Type_
              end
      end,
      Type).

-spec save(state(), string(), string()) -> ok | {error, any()}.
save(State, Filename, Format) ->
    digraph_viz:export(State#state.graph, [], Filename, Format).

%%====================================================================
%% Internal functions
%%====================================================================

make_substitutions(UserTypeDict, [T|Ts], Subs) ->
    Def = perc_types:get_usertype_def_type(dict:fetch(T, UserTypeDict)),
    Reduced = reduce_usertypes(Def, Subs),
    NewSubs = dict:store(T, Reduced, Subs),
    make_substitutions(UserTypeDict, Ts, NewSubs);
make_substitutions(_, [], Subs) ->
    Subs.

is_usertype(Type) ->
    case perc_types:get_type(Type) of
        usertype ->
            true;
        _ ->
            false
    end.

is_subtstitutable(State, UserType) ->
    Vertex = dict:fetch(UserType, State#state.dict),
    case digraph:get_cycle(State#state.graph, Vertex) of
        false ->
            true;
        _ ->
            false
    end.

-spec add_norec_graph(state()) -> state().
add_norec_graph(State) ->
    Graph = copy_digraph(State#state.graph),
    Labeled = [digraph:vertex(Graph, V) || V <- digraph:vertices(Graph)],
    % Remove records from the new graph because the records are never
    % ignored, only their fields are.
    RecVertices = [V || {V, T} <- Labeled, perc_types:get_type(T) == record],
    digraph:del_vertices(Graph, RecVertices),
    State#state{norec_graph=Graph}.

-spec add_record(perc_types:record_def(), state()) -> state().
add_record(RecordDef, State) ->
    Name = perc_types:get_record_def_name(RecordDef),
    Fields = perc_types:get_record_def_fields(RecordDef),
    Types = [perc_types:get_record_field_type(Field) || Field <- Fields],
    Label = perc_types:make_record(Name),
    {RecordVertex, NewState} = ensure_vertex(Label, State),
    lists:foldl(fun(Type_, State_) ->
                        add_perc_type(RecordVertex, Type_, State_)
                end, NewState, Types).

-spec add_usertype(perc_types:usertype_def(), state()) -> state().
add_usertype(UserTypeDef, State) ->
    Name = perc_types:get_usertype_def_name(UserTypeDef),
    Type = perc_types:get_usertype_def_type(UserTypeDef),
    Label = perc_types:make_usertype(Name),
    {UserTypeVertex, NewState} = ensure_vertex(Label, State),
    add_perc_type(UserTypeVertex, Type, NewState).

-spec add_perc_type(
        digraph:vertex(),
        perc_types:perc_type(),
        state()
       ) -> state().
add_perc_type(ParentVertex, Type, State0) ->
    {Vertex, State1} = ensure_vertex(Type, State0),
    ensure_edge(ParentVertex, Vertex, State1#state.graph),
    State2 = lists:foldl(fun(Type_, State_) ->
                                 add_perc_type(Vertex, Type_, State_)
                         end, State1, perc_types:children(Type)),
    case perc_types:get_type(Type) of
        ignored ->
            {IgnoredVertex, State3} = ensure_vertex(ignored, State2),
            ensure_edge(Vertex, IgnoredVertex, State3#state.graph),
            State3;
        _ ->
            State2
    end.

add_vertex(Graph, Label) ->
    digraph:add_vertex(Graph, digraph:add_vertex(Graph), Label).

ensure_vertex(Label, State) ->
    #state{graph=Graph, dict=Dict} = State,
    case dict:find(Label, Dict) of
        {ok, Vertex} ->
            {Vertex, State};
        error ->
            Vertex = add_vertex(Graph, Label),
            NewDict = dict:store(Label, Vertex, Dict),
            NewState =
                #state{graph=Graph, dict=NewDict},
            {Vertex, NewState}
    end.

ensure_edge(VertexA, VertexB, Graph) ->
    case lists:member(VertexB, digraph:out_neighbours(Graph, VertexA)) of
        true -> true;
        false -> digraph:add_edge(Graph, VertexA, VertexB)
    end.

-spec copy_digraph(digraph:digraph()) -> digraph:digraph().
copy_digraph(Digraph) ->
    digraph_utils:subgraph(Digraph, digraph:vertices(Digraph)).

get_labels(Graph, Vertices) ->
    Labeled = [digraph:vertex(Graph, V) || V <- Vertices],
    [L || {_, L} <- Labeled].

filter_ignored_path(Path) ->
    filter_ignored_path(Path, true, []).

filter_ignored_path([], _, Acc) ->
    Acc;
filter_ignored_path([Elem|Path], Next, Acc) ->
    IsReference = is_reference_type(Elem),
    case IsReference or Next of
        true ->
            filter_ignored_path(Path, IsReference, [Elem | Acc]);
        _ ->
            filter_ignored_path(Path, IsReference, Acc)
    end.

is_reference_type(Type) ->
    case perc_types:get_type(Type) of
        record ->
            true;
        usertype ->
            true;
        _ -> false
    end.
