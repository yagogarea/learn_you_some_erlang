-module(positive_negative).

-export([positive_negative/1]).

positive_negative(X) when is_integer(X); is_float(X) -> % We can use is_integer/1 and is_float/1 to check if the input is a number
    case X of
        X when X > 0 -> positive;
        X when X < 0 -> negative;
        _ -> zero
    end.
