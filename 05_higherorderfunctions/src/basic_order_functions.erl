-module(basic_order_functions).

-export([map/2, filter/2, foldl/3, foldr/3]).

% map/2: apply a function to each element of a list
map(_, []) -> []; % base case
map(F, [H|T]) -> [F(H)|map(F, T)]. % recursive case

% filter/2: filter a list based on a predicate
filter(_, []) -> []; % base case
filter(P, [H|T]) -> 
    case P(H) of
        true -> [H|filter(P, T)];
        false -> filter(P, T)
    end. % recursive case

% foldl/3: left fold
foldl(_, Acc, []) -> Acc; % base case
foldl(F, Acc, [H|T]) -> foldl(F, F(Acc, H), T). % recursive case

% foldr/3: right fold
foldr(_, Acc, []) -> Acc; % base case
foldr(F, Acc, [H|T]) -> F(H, foldr(F, Acc, T)). % recursive case