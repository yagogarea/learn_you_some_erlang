# Recursion

## What is recursion?

In programming, recursion is a technique where a function calls itself in order to solve a problem. This is a powerful concept that allows us to solve complex problems by breaking them down into smaller, more manageable sub-problems. Erlang is not a exception.

In recursion, we have two main components:
- **Base case**: The simplest form or forms of the problem that can be solved directly without further recursion. It serves as the stopping condition for the recursion.
- **Recursive case**: The part of the problem that can be broken down into smaller sub-problems. It involves calling the function itself.

For example, let's define a function to calculate the factorial of a number.

```erlang
factorial(0) -> 1; % Base case. 
factorial(N) -> N * factorial(N - 1). % Recursive case
```
The proccess of calculating the factorial of 5 would look like this:

```erlang
factorial(5).
120.
```

```
5 * factorial(4) -> 
5 * 4 * factorial(3) -> 
5 * 4 * 3 * factorial(2) -> 
5 * 4 * 3 * 2 * factorial(1) -> 
5 * 4 * 3 * 2 * 1 * factorial(0) -> 
5 * 4 * 3 * 2 * 1 * 1 = 120.
```
## Tail recursion
 
The common issue with recursion is the potential for stack overflow errors when the recursion depth becomes too large. This is due to each recursive call adding a new frame to the call stack, which has a limited size. However, in some cases, this problem can be circumvented by employing tail recursion.

A function is considered tail recursive if the recursive call represents the **last operation in the function**. In such cases, the function doesn't need to keep track of the previous state, enabling the call stack to be optimized away. This optimization is achieved by storing intermediate results in the function arguments, typically using an accumulator.

Let's explore how we can convert the `factorial` function into a tail recursive version:

```erlang
tail_factorial(N) -> tail_factorial(N, 1). % Call the helper function with the accumulator set to 1
% We only expose the tail_factorial/1 function, the helper function is hidden from the user.
tail_factorial(0, Acc) -> Acc; % Base case
tail_factorial(N, Acc) -> tail_factorial(N - 1, N * Acc). % Recursive case
```

The proccess of calculating the factorial of 5 would look like this:

```erlang
tail_factorial(5).
120.
```

``` 
tail_factorial(5, 1) ->
tail_factorial(4, 5) ->
tail_factorial(3, 20) ->
tail_factorial(2, 60) ->
tail_factorial(1, 120) ->
tail_factorial(0, 120) ->
120.
```
We only need to keep track of the current value of the accumulator, so the call stack can be optimized away. This makes tail recursion a powerful tool for solving problems that would otherwise lead to stack overflow errors.

The easiest way to identify tail recursion is to look for the pattern where the recursive call is the last operation in the function. If you find this pattern, you can convert the function into a tail recursive version by adding an accumulator and a helper function.

A problem that can appear when using tail recursion is that the recursive calls can reverse the order of the elements in the list. This is because the recursive call is the last operation in the function, so the elements are added to the list in reverse order. To solve this issue, we can use an accumulator to store the elements in the correct order.

Let's see an example that solves this issue. Let's define a function that returns a list with the N first elements of an another list.

```erlang
sublist(L, N) -> lists:reverse(sublist(L, N, [])). % We reverse the list to solve the issue.

sublist(_, 0, Acc) -> Acc; % Base case
sublist([], _, Acc) -> Acc; % Base case
sublist([H|T], N, Acc) when N > 0 -> sublist(T, N - 1, [H|Acc]). % Recursive case
```