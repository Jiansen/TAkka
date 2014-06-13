-module(echo).

-export([start/0, say_something/2]).

say_something(_, 0) ->
    done;
say_something(What, Times) ->
    io:format("~p~n", [What]),
    say_something(What, Times - 1).

start() ->
    spawn(echo, say_something, [hello, 3]),
    spawn(echo, say_something, [goodbye, 3]).

%% Terminal Output:
%% 1> c(echo).
%% {ok,echo}
%% 2> echo:say_something(hello, 3).
%% hello
%% hello
%% hello
%% done
%% 3> echo:start().
%% hello
%% goodbye
%% <0.41.0>
%% hello
%% goodbye
%% hello
%% goodbye


