# Expressions

In Erlang, you must put a period at the end of each expression or declaration.

## Operators

The basic operations in Erlang are similar to those in other languages, with the exception of integer division (`div`) and the modulo operator (`rem`). Additionally, Erlang has variables, but they are immutable and have the peculiarity of starting with an uppercase letter.

The `=` operator compares the equality of two elements. If they are equal, it returns the value; otherwise, it raises an exception. If the element on the left is empty, it will store the value on the right and operate the same way, except if it is `_`, in which case the value will not be stored.

When we don't care about an element, we can use the underscore `_` to indicate that the value of this element can be anything.

To perform comparisons, we use `=:=` and `=/=` for equality, or strict equality, but if we don't want to differentiate between integers and floats, we must use `==` and `/=`.


Erlang has boolean operators like `and`, `or`, `xor`, `not`, and their binary counterparts `band`, `bor`, `bxor`, and `bnot`. Importantly, the boolean operators`and`, `or`, `xor`, `not` use eager evaluation, meaning **both sides** are always evaluated.

To achieve lazy evaluation, Erlang offers `andalso` and `orelse` operators, ensuring short-circuit evaluation. **If the first part decides the result, the second part is skipped**.

## Data types 

### Numbers
- **Ints** 
- **Floats** 

**Note:** Be cautious; Erlang has limited precision with floats, and you may need to use approximations. 

```erlang
1> 0.1 + 0.2.
0.30000000000000004
2> 0.1 + 0.2 == 0.3.
false
```
One way to fix this is to use the `abs` function to compare the difference between the two numbers and a very small number.

```erlang
3> abs(0.1 + 0.2 - 0.3) < 0.0000001.
true
```

There are a similar to numbers in other languages but we two new functions:

- `$char` returns the char ASCII value

```erlang
1> $a.
97
```

- `base#number` converts the number in decimal system to base system

```erlang
2> 16#18.
24
```

**Note:** You can use `_` between digits to a improve visibility.

```erlang
1> 1_000_000.
1000000
```
### Fun

In Erlang, functions are similar to those in Ocaml; they are functional objects that can act as anonymous functions and be passed as arguments to other functions. They adhere to the following syntax:

`Function_name = fun (args) -> 
function_body 
end. ` 

```erlang
1> Square = fun (x) -> x*x end.
#Fun<erl_eval.42.105768164>
2> Square(Square(2)).
16
```

### Atoms

The atoms are very lightweight constants with his name as values, allowing only for comparisons. The atoms are enclosed within single quotes (`'`). However, it's worth noting that the single quotes can be omitted if the atom begins with a lowercase letter and consists solely of alphanumeric characters, underscores (_), or @.

```erlang
1> hello_world@mail.
hello_world@mail
2> 'Hello_Wordl15!'.
'Hello_Wordl15!'    % Using or not using quotes is equivalent.
```

#### Boolean
In Erlang, there is no built-in boolean data type. Instead, boolean values are denoted by the atoms `true` and `false`.

### Tuple
Like in Ocaml tuples are a **immutable** data structure that consists of a group of elements written enclosed within curly braces `{}` and separeted by commas. They can have elements with differents types including other tuples.

Values of a tuple can be assigned directly:

```erlang
1> Tuple = {X,Y} = {1,true}.
{1,true}
2> X.
1
3> Y.
true
4> Tuple.
{1,true}
5> {_,true} = Tuple.
{1,true}
```
### Map
Maps are unordered collections of key-value element pairs. You can access the value associated with its key. They are an immutable unordered structure initialized when declared. It's important to note that a map is not a new type definition like a struct; instead, it provides a flexible and dynamic way to associate values with keys without the need for a predefined structure.

We implement maps with the syntax:

`#{key1 => value1, ... keyn => value2}`

```erlang
1> Dog1 = #{name => "Nala", age => 3, breed => chihuahua}.
#{name => "Nala",age => 3,breed => chihuahua}
2> maps:get(name, Dog1).
"Nala"
```

### Record
There are just like structs in C, are data structure for storing different elements with name fields.

`-record(new_type_name, {tuple_of_attributes}). `

**Example:**
```erlang
1> -record(dog, {name, age, breed}).
ok
2> Dog1 = #dog{name = "Wilbur", age = 2, breed = chihuahua}. 
#dog{name = "Wilbur",age = 2,breed = chihuahua}
3> Dog1#dog.name.
"Wilbur"
```

### List
Lists in Erlang are a concatenation of elements with a empty list, these elements can have different data types. You can see a list like the first element concatenate with a list with the rest of the elements or empty if there isn't more (`[Head | Tail]`).

```erlang
1> []
[]
2> ["erl"] = ["erl" | []].
["erl"]
3> ["erl", "exs"] = ["erl" | ["exs"]].
["erl","exs"]
4> [1, 0.33, a, true, {0,0}, [hello, world]].
[1,0.33,a,true,{0,0},[hello,world]]
```

We can apply expressions to our list using the following syntax:

`[expresion || value <- [list], condition]. `

This statement signifies: Return a list with the expressions based on the values that satisfy the condition.

```erlang
1>[X || X <- [-2,-1,0,1,2]].
[-2,-1,0,1,2]
2>[X * -1 || X <- [-2,-1,0,1,2]].
[2,1,0,-1,-2]
3>[X * -1 || X <- [-2,-1,0,1,2], X < 0].
[2,1]
```

**Note:** If a list is componed only by elements that can be chars, it will be represented like a string.

```erlang
1> [98,118]
"bv"
```
#### String
Just like booleans, Erlang doesn't have a specific data type called "String". Instead, strings are an abbreviation for a list containing the ASCII values of individual characters.

```erlang
1> [72,101,108,108,111,44,32,87,111,114,108,100,33].
"Hello, World!"
2> "Hello, World!". % = [$H,$e,...,%!]
"Hello, World!"
```

### Bit Strings And Binaries

Bit strings are sequences of bits, and if the number of bits is divisible by 8, they are also referred to as binaries. They are employed for representing and manipulating binary data and adhere to the following syntax:

`<<pattern1, pattern2, ... , patternN>>`

Where each `pattern` can be a decimal number, a binary expression (using a colon `:` notation to specify the number of bits), or even another binary construction.

```erlang
% Creating a binary with values 2 and 5
1> <<2,5>> 
<<2,5>> 
% Creating a binary with bit-level representations: 
2> <<3:3 , 2:1>>. 
% 3 represent with 3 bits -> 011 
% 2 represented with 1 bit -> 0
<<6:4>> % 0110 -> 6 in decimal ocuppying 4 bits
```

Also we can indicate the type of a Bit String:

```erlang
3> <<72:8/unit:1>>.
<<"H">> 
```

Like in lists we can apply expressions to Bit Strings using the following syntax:

`[expresion || <<value>> <= <<bit_string>>, condition]. `

### References
References are a data type that is guaranteed to be unique among connected nodes in a distributed Erlang system. You can create a reference with `make_ref/0` and check if a data is a reference using `is_reference/1`.

### Port Idenfiers and Pids

Identifies Erlang ports and process.    