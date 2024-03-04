# Introduction 

Although we can utilize Erlang's built-in functions within the Erlang Interactive Environment, creating modules and compiling files becomes indispensable for more extensive program development.

## Code Structure

In Erlang, if we need to define a function or a group of them, we need to define a module. A module is a group of functions stored in a file. To define a module, we need to create a new file in the following form:

`module_name.erl`

The structure of the modules is the following:

**Attributes**

```erlang
-module(module_name).   % This line is mandatory for compiling, with it you are defining the module.
                        % The module name MUST be the same that de file name.
                     
-export([Function/arguments_number],...).   % With this line, you make public functions of your module 
                                            % (resembling public methods in Java)

% And all the attributes that the module uses
```

**Functions Implementation**

```erlang
function_name(arguments_number) ->  % If arg_num is zero, we cannot put the number ().
    implementation.                 % The function implementation must end with a final point.
```

## Work with modules

To compile a file in your shell, execute the following command:

```sh
$ erlc module_name.erl # Generates a executable file: module_name.beam
```

**A common way to work with Erlang is by using the Erlang Interactive Environment** 

We can open it with the following command in your shell:

```sh
$ erl
```
We can use the Erlang functions, but to use ours, we need to run the Interactive Environment in the same path than our file or move to this path using the `cd` function:

```erlang
cd("path").
```

f the module hasn't been compiled previously, you can do so using the `c` function:

```erlang
c(module_name) 
```

Now we can call the functions of our module in this way:

```erlang
module_name:function_name(arguments). % This will run our function and return a value.
```