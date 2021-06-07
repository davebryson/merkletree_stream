-module(tree_tests).

-include("./src/merkletree_stream.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%
%%% Tests
%%%

build_with_single_root_test() ->
    Data = [<<"a">>, <<"b">>, <<"c">>],
    Tree = merkletree_stream:create_tree(Data),
    ?assertEqual(2, length(Tree#state.roots)),
    ?assertEqual(4, length(Tree#state.nodes)),
    ?assertEqual(3, Tree#state.blocks),
    ok.

build_more_than_one_root_test() ->
    Data = [<<"a">>, <<"b">>, <<"c">>, <<"d">>],
    Tree = merkletree_stream:create_tree(Data),
    ?assertEqual(1, length(Tree#state.roots)),
    ?assertEqual(7, length(Tree#state.nodes)),
    ?assertEqual(4, Tree#state.blocks),
    ok.

deterministic_even_test() ->
    ExpectedNodes = [
        {node, 1, 3,
            <<229, 160, 31, 238, 20, 224, 237, 92, 72, 113, 79, 34, 24, 15, 37, 173, 131, 101, 181,
                63, 151, 121, 247, 157, 196, 163, 215, 233, 57, 99, 249, 74>>,
            <<>>, 2},
        {node, 2, 1,
            <<62, 35, 232, 22, 0, 57, 89, 74, 51, 137, 79, 101, 100, 225, 177, 52, 139, 189, 122, 0,
                136, 212, 44, 74, 203, 115, 238, 174, 213, 156, 0, 157>>,
            <<"b">>, 1},

        {node, 0, 1,
            <<202, 151, 129, 18, 202, 27, 189, 202, 250, 194, 49, 179, 154, 35, 220, 77, 167, 134,
                239, 248, 20, 124, 78, 114, 185, 128, 119, 133, 175, 238, 72, 187>>,
            <<"a">>, 1}
    ],
    ExpectedRoot = [
        {node, 1, 3,
            <<229, 160, 31, 238, 20, 224, 237, 92, 72, 113, 79, 34, 24, 15, 37, 173, 131, 101, 181,
                63, 151, 121, 247, 157, 196, 163, 215, 233, 57, 99, 249, 74>>,
            <<>>, 2}
    ],
    Data = [<<"a">>, <<"b">>],
    Tree = merkletree_stream:create_tree(Data),
    ?assertEqual(ExpectedNodes, Tree#state.nodes),
    ?assertEqual(ExpectedRoot, Tree#state.roots),
    ok.

deterministic_odd_test() ->
    ExpectedNodes = [
        {node, 4, 5,
            <<46, 125, 44, 3, 169, 80, 122, 226, 101, 236, 245, 181, 53, 104, 133, 165, 51, 147,
                162, 2, 157, 36, 19, 148, 153, 114, 101, 161, 162, 90, 239, 198>>,
            <<"c">>, 1},

        {node, 1, 3,
            <<229, 160, 31, 238, 20, 224, 237, 92, 72, 113, 79, 34, 24, 15, 37, 173, 131, 101, 181,
                63, 151, 121, 247, 157, 196, 163, 215, 233, 57, 99, 249, 74>>,
            <<>>, 2},

        {node, 2, 1,
            <<62, 35, 232, 22, 0, 57, 89, 74, 51, 137, 79, 101, 100, 225, 177, 52, 139, 189, 122, 0,
                136, 212, 44, 74, 203, 115, 238, 174, 213, 156, 0, 157>>,
            <<"b">>, 1},

        {node, 0, 1,
            <<202, 151, 129, 18, 202, 27, 189, 202, 250, 194, 49, 179, 154, 35, 220, 77, 167, 134,
                239, 248, 20, 124, 78, 114, 185, 128, 119, 133, 175, 238, 72, 187>>,
            <<"a">>, 1}
    ],
    ExpectedRoots = [
        {node, 4, 5,
            <<46, 125, 44, 3, 169, 80, 122, 226, 101, 236, 245, 181, 53, 104, 133, 165, 51, 147,
                162, 2, 157, 36, 19, 148, 153, 114, 101, 161, 162, 90, 239, 198>>,
            <<"c">>, 1},

        {node, 1, 3,
            <<229, 160, 31, 238, 20, 224, 237, 92, 72, 113, 79, 34, 24, 15, 37, 173, 131, 101, 181,
                63, 151, 121, 247, 157, 196, 163, 215, 233, 57, 99, 249, 74>>,
            <<>>, 2}
    ],

    Data = [<<"a">>, <<"b">>, <<"c">>],
    Tree = merkletree_stream:create_tree(Data),
    ?assertEqual(ExpectedNodes, Tree#state.nodes),
    ?assertEqual(ExpectedRoots, Tree#state.roots),
    ok.
