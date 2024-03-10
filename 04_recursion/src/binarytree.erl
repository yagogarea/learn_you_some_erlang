-module(binarytree).
-export([new_tree/1, is_empty/1, get_value/1, get_left/1, get_right/1, insert_node/2, search_node/2, print_tree/1]).

-record(node, {value, left, right}).

% Function to create a new node
new_tree(Value) ->
    #node{value=Value, left=nil, right=nil}.

% Function to check if a node is empty
is_empty(nil) ->
    true;
is_empty(_) ->
    false.

% Functions to get the value, left child, and right child of a node
get_value(#node{value=Value}) ->
    Value.
get_left(#node{left=Left}) ->
    Left.
get_right(#node{right=Right}) ->
    Right.

% Function to insert a node into the tree
insert_node(Value, nil) ->
    new_tree(Value);
insert_node(Value, #node{value=V, left=L, right=R}) when Value < V ->
    #node{value=V, left=insert_node(Value, L), right=R};
insert_node(Value, #node{value=V, left=L, right=R}) when Value > V ->
    #node{value=V, left=L, right=insert_node(Value, R)};
insert_node(Value, #node{value=Value, left=L, right=R}) ->
    #node{value=Value, left=L, right=R}.

% Function to search for a value in the tree
search_node(_, nil) ->
    false;
search_node(Value, #node{value=Value, left = _, right = _}) when Value == Value ->
    true;
search_node(Value, #node{value=V, left = L, right = _}) when Value < V ->
    search_node(Value, L);
search_node(Value, #node{value=V, left = _, right = R}) when Value > V ->
    search_node(Value, R).

% Function to print the tree
print_tree(nil) ->
    ok;
print_tree(#node{value=V, left=L, right=R}) ->
    io:format("~p~n", [V]),
    print_tree(L),
    print_tree(R).
