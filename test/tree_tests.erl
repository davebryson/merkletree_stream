-module(tree_tests).

-include("./src/merkletree_stream.hrl").
-include_lib("eunit/include/eunit.hrl").

create_tree(Data) ->
    buildit(Data, merkletree_stream:new()).
buildit([], State) ->
    State;
buildit([H | T], State) ->
    buildit(T, merkletree_stream:write(H, State)).

%%%
%%% Tests
%%%

build_with_single_root_test() ->
    Data = [<<"a">>, <<"b">>, <<"c">>],
    Tree = create_tree(Data),
    ?assertEqual(2, length(Tree#state.roots)),
    ?assertEqual(4, length(Tree#state.nodes)),
    ?assertEqual(3, Tree#state.blocks),
    ok.

build_more_than_one_root_test() ->
    Data = [<<"a">>, <<"b">>, <<"c">>, <<"d">>],
    Tree = create_tree(Data),
    ?assertEqual(1, length(Tree#state.roots)),
    ?assertEqual(7, length(Tree#state.nodes)),
    ?assertEqual(4, Tree#state.blocks),
    ok.

deterministic_test() ->
    ok.
