# Higher Order Functions

## Definition

A higher order function is a function that takes a function as an argument, or returns a function. 

## Anonymus Functions

An anonymous function is a function that is not bound to an identifier. Anonymous functions are often arguments being passed to higher order functions, or used for constructing the result of a higher order function that needs to return a function.
The syntax for an anonymous function is:

```erlang
fun (Patterns1) -> Expression, ..., ExpressionN;
fun (Patterns2) -> Expression, ..., ExpressionN;
...
fun (PatternsN) -> Expression, ..., ExpressionN
end.
```

One thing to note is that the last expression in the function is the return value of the function.

## Utilites

Erlang provides a number of higher order functions in the lists module. These include:

- `map/2` - applies a function to each element of a list
- `filter/2` - returns a list of elements for which a function returns true
- `foldl/3` - applies a function to each element of a list, and accumulates the result from the left
- `foldr/3` - applies a function to each element of a list, and accumulates the result from the right

... and many more

These functions are useful for working with a higher level of abstraction, and can be used to write more concise and readable code. You should use these functions whenever possible, always preferring them over explicit recursion.