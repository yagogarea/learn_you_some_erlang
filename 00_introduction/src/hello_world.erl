% This programm implements a simple hello world program.

-module(hello_world). % The module name must match the file name. The module name is used to call the functions in the file.

-export([hello/0]). % This exports the hello/0 function, enabling it to be called externally without any required arguments.

hello() -> 
    io:format("Hello, World!~n").

% io is a module that provides input/output functions.
% io:format is a function that takes a string and prints it to the console. The ~n is a newline character.