%% @doc holds the state of the tree
-record(state, {nodes = [], roots = [], blocks = 0}).

%% @doc holds the information about a node in the tree
-record(node, {index = 0, parent = 0, hash = <<>>, data = <<>>, size = 0}).
