-module(useless).
-export([add/2, hello/0, greet_and_add_two/1, add/1, greet/2]).

add(A,B) ->
    A + B.

add(X) when is_tuple(X) ->
    {Y,Z} = X,
    Y + Z.

% Shows greetings.
% io:format/1 is the standard function used to output text.
hello() ->
    io:format("Hello world!~n").

greet_and_add_two(X) ->
    hello(),
    add(X,2).

greet(male, Name) ->
    io:format("Hello, Mr. ~s!", [Name]);
greet(femaile, Name) ->
    io:format("Hello, Mrs. ~s!", [Name]);
greet(_, Name) ->
    io:format("Hello, ~s!", [Name]).
