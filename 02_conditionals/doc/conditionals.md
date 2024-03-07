# Conditionals : Pattern Matching, Guards, and Flow Control expressions.

Erlang provides a variety of tools to evaluate conditions and control the flow of a program. However, in all cases, it is crucial to **cover all possible cases** to avoid crashing the program. Typically, pattern matching and case expressions with guards are employed for condition evaluation. Although the if expression is available, it is not recommended for general use unless absolutely necessary.

## Pattern Matching

Instead of using the typical if-else statement, Erlang utilizes pattern matching to evaluate conditions. We declare functions for each case we need to evaluate, separating them with a semicolon. The first case that matches the input is the one that gets executed. 

The general syntax is:

```erlang
function_name(Pattern1) -> Expression1;
function_name(Pattern2) -> Expression2;
...
function_name(PatternN) -> ExpressionN.
```

For example, this program will return `false` if the input is 0, and `true` if it's anything else.
```erlang
example(0) -> false; % We can use the semicolon to separate the cases.
example(_) -> true. % The underscore is a wildcard, it matches anything.
``````
Another important aspect is that we can use pattern matching to bind variables to values. For instance, we can employ pattern matching to extract the head and tail of a list.

```erlang
head([H|_]) -> H. % The head of a list is the first element.
tail([_|T]) -> T. % The tail of a list is the list without the first element.
```
## Guards
To enhance the flexibility of our pattern matching, guards can be employed. Guards are additional conditions that must be satisfied for the pattern to match. The `when` keyword is used to introduce a guard to one or more patterns for evaluation. We can use a comma (`,`) or a semicolon (`;`) to separate the conditions, similar to the `andalso` and `orelse` operators, but with the key distinction that guards handle exceptions and return values instead of crashing the program.

The general syntax is: 

`function_name(Pattern) when Guard -> Expression.`

For example, we can use a guard to check if a person was born in the 90s.

```erlang
is_born_in_90s(Y) when Y >= 1990, Y < 2000 -> true; % The comma is used as an AND operator.
is_born_in_90s(_) -> false.

% Another implementation using semicolons might be:
is_born_in_90s(Y) when Y < 1990; Y > 2000 -> false;  % The semicolon is used as an OR operator.
is_born_in_90s(_) -> true.
```

## Flow Control Expressions : IF and CASE OF

### If Statement
If we need to evaluate multiple conditions, we can use the `if` expression. It's important to note that the `if` expression is an expression, not a statement, so it returns a value. While we can use the true condition as a default case, it is not recommended unless absolutely necessary.

The general syntax is:

```erlang
if
    Condition1 -> Expression1;
    Condition2 -> Expression2;
    ...
    true -> ExpressionN % Similar to an else statement or a default case.
end.
```
For example, we can use the `if` expression to check if a number is positive, negative, or zero or we can check if a list is empty.

```erlang
check_sign(X) ->
    if
        X > 0 -> "Positive";
        X < 0 -> "Negative";
        X == 0 -> "Zero"
    end.

check_empty_list(X) -> 
    if
        X == [] -> "Empty";
        true -> "Not Empty"
    end.
```

### Case Of Statement

Alternatively, we can use the `case` expression to evaluate multiple conditions. The `case` expression is more flexible than the `if` expression, as it allows us to match patterns and use guards. The `case` expression is also an expression, so it returns a value.

The general syntax is:  

```erlang
% Without guards
case Expression of
    Case1 -> Expression1;
    Case2 -> Expression2;
    ...
    CaseN -> ExpressionN;
end.

% With guards
case Expression of
    Pattern1 when Guard1 -> Expression1; 
    Pattern2 when Guard2 -> Expression2;
    ... 
    PatternN when GuardN -> ExpressionN;
end.
```
We will use the same examples as before to illustrate the `case` expression.

```erlang
check_sign(X) ->
    case X of
        X when X > 0 -> "Positive";
        X when X < 0 -> "Negative";
        _ -> "Zero"
    end.

check_empty_list(X) ->
    case X of
        [] -> "Empty";
        _ -> "Not Empty"
    end.
```
