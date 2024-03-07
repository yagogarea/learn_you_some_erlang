-module(listL). %With Conditional Expressions

-compile(export_all).

% Returns the length of a list
lengthL([]) -> 0;
lengthL([_|T]) -> 1 + lengthL(T).

% Head of a list
headL([]) -> [];
headL([H|_]) -> H.

% Tail of a list
tailL([]) -> [];
tailL([_|T]) -> T.

% Returns the last element of a list
lastL([]) -> [];
lastL([H|[]]) -> H;
lastL([_|T]) -> lastL(T).

% Returns the list with the elements
removeFromListL(_, []) -> [];
removeFromListL(X, [X|T]) -> T;
removeFromListL(X, [H|T]) -> [H|removeFromListL(X, T)].

