# Types

## Dynamic Typing

Erlang is a strongly and dynamically typed language. This means that the type of a variable is determined at runtime, not at compile time and that we cannot operate on a variable that is not of the expected type.

## Type Conversion & Type Guards

Erlang provides a set of built-in functions to convert data from one type to another. These functions are called type conversion functions. For example, the `integer_to_list/1` function converts an integer to a list.

```erlang
1> integer_to_list(123).
"123"
2> integer_to_list(123.0).
** exception error: bad argument
     in function  integer_to_list/1
        called as integer_to_list(123.0)
        *** argument 1: not an integer
``` 
The `is_` functions are used to check the type of a variable. For instance, the `is_integer/1` function checks if a variable is an integer. We can use these functions to guard our code and ensure that we are operating on the expected type.

```erlang
is_a_positive_integer(X) when is_integer(X), X > 0 -> true;
is_a_positive_integer(_) -> false.
ok
```

```erlang
1> is_integer(123).
true
2> is_integer("123").
false
3> is_a_positive_integer(123).
true
4> is_a_positive_integer(-123).
false
```