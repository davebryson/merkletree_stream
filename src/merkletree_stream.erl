%%% @doc
%%% Builds an in-memory merkle tree indexed via a flat tree.
%%% @end
-module(merkletree_stream).

-include("merkletree_stream.hrl").

-export([
    new/0,
    new/1,
    create_tree/1,
    write/2,
    print_tree/1
]).

%% @doc Create a new Tree
new() ->
    #state{}.

%% @doc Create a new tree building off existing roots
new(Roots) ->
    #state{roots = Roots}.

%% Create a new tree from data
create_tree(Data) when is_list(Data) ->
    make_tree(Data, new()).

%% @doc Write data to a tree
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

%% @doc Create a tree from some data and print out the nodes and roots
print_tree(Data) when is_list(Data) ->
    S = create_tree(Data),
    io:format("~n******* Nodes ********~n"),
    lists:foreach(fun(N) -> io:format("~n~p~n", [N]) end, S#state.nodes),
    io:format("~n******* Roots ********~n"),
    lists:foreach(fun(N) -> io:format("~n~p~n", [N]) end, S#state.roots).

%%%
%%% Private
%%%

%% @private build up the tree during writes
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

%% @private Helper to build a tree from data
make_tree([], State) ->
    State;
make_tree([H | T], State) ->
    S = write(H, State),
    make_tree(T, S).

%% @private Hash data for a leaf
leaf(Data) when is_binary(Data) ->
    crypto:hash(sha256, Data).

%% @private Hash data for a root node
parent(A, B) when is_binary(A) and is_binary(B) ->
    S0 = crypto:hash_init(sha256),
    S1 = crypto:hash_update(S0, A),
    S2 = crypto:hash_update(S1, B),
    crypto:hash_final(S2).
