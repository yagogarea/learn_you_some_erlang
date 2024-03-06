-module(basic_operations).

-export([add/2, subtract/2, multiply/2, divide/2, is_even/1, is_odd/1]).

add(X, Y) -> X + Y.

subtract(X, Y) -> X - Y.

multiply(X, Y) -> X * Y.

divide(X, Y) when Y =/= 0 -> X / Y;
divide(_, _) -> undefined.  % Handle division by zero.

is_even(X) -> X rem 2 =:= 0.

is_odd(X) -> X rem 2 =:= 1.
