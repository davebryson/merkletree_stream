%%% @doc
%%% Builds an in-memory merkle tree indexed via a flat tree.
%%% @end
-module(merkletree_stream).

-include("merkletree_stream.hrl").

-export([
    new/0,
    new/1,
    write/2,
    run_test/0,
    run_test_2/0
]).

%% @doc Create a new Tree
new() ->
    #state{}.

%% @doc Create a new tree building off of existing roots
new(Roots) ->
    #state{roots = Roots}.

%% @doc Write data to the tree
write(Data, #state{nodes = Nodes, roots = Roots, blocks = Blocks}) ->
    Index = 2 * Blocks,
    Leaf = #node{
        index = Index,
        parent = flat_tree:parent(Index),
        size = byte_size(Data),
        data = Data,
        hash = leaf(Data)
    },
    {R, N} = build_tree([Leaf | Roots], [Leaf | Nodes]),
    #state{
        roots = R,
        nodes = N,
        blocks = Blocks + 1
    }.

%%%
%%% Private
%%%
build_tree([], Nodes) ->
    %% No roots to process
    {[], Nodes};
build_tree([A], Nodes) ->
    %% Only have 1 root
    {[A], Nodes};
build_tree([R, L | Rest] = Root, Nodes) ->
    case L#node.parent =/= R#node.parent of
        true ->
            {Root, Nodes};
        _ ->
            %% Keep building to the right...
            Leaf = #node{
                index = L#node.parent,
                parent = flat_tree:parent(L#node.parent),
                size = L#node.size + R#node.size,
                data = <<>>,
                hash = parent(L#node.hash, R#node.hash)
            },
            case length(Root) of
                2 -> build_tree([Leaf], [Leaf | Nodes]);
                _ -> build_tree([Leaf | Rest], [Leaf | Nodes])
            end
    end.

run_test() ->
    S = new(),
    S1 = write(<<"hello">>, S),
    S2 = write(<<"washed">>, S1),
    S3 = write(<<"world">>, S2),
    %S4 = write(<<"d">>, S3),
    %S5 = write(<<"e">>, S4),

    %% SHOULD Contains the indices: 0,2,1,4

    %lists:foreach(fun(N) -> io:format("~n~p~n", [N]) end, S5#state.nodes),
    %io:format("~~~~~~~~~~~~~~~~~~~n"),
    %lists:foreach(fun(N) -> io:format("~n~p~n", [N]) end, S5#state.roots).
    lists:foreach(fun(N) -> io:format("~n~p~n", [N]) end, S3#state.nodes),
    io:format("***************~n"),
    lists:foreach(fun(N) -> io:format("~n~p~n", [N]) end, S3#state.roots).

run_test_2() ->
    Data = [<<"c0">>, <<"c1">>, <<"c2">>, <<"c3">>, <<"c4">>, <<"c5">>, <<"c6">>, <<"c7">>],
    S = run_build(Data, new()),
    lists:foreach(fun(N) -> io:format("~n~p~n", [N]) end, S#state.nodes),
    io:format("***************~n"),
    lists:foreach(fun(N) -> io:format("~n~p~n", [N]) end, S#state.roots).

run_build([], State) ->
    State;
run_build([H | T], State) ->
    S = write(H, State),
    run_build(T, S).

%% @private Hash data for a leaf
leaf(Data) when is_binary(Data) ->
    crypto:hash(sha256, Data).

%% @private Hash data for a root node
parent(A, B) when is_binary(A) and is_binary(B) ->
    S0 = crypto:hash_init(sha256),
    S1 = crypto:hash_update(S0, A),
    S2 = crypto:hash_update(S1, B),
    crypto:hash_final(S2).
