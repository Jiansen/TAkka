-module(echo_actor).

-export([start/0, loop/0, print/0]).

loop() ->
    receive
        Msg -> 
          io:format("loop: ~p~n", [Msg]),
          loop()
    end.

print() ->
    receive
        Msg -> 
          io:format("print: ~p~n", [Msg])
    end.
    
start() ->
    L = spawn(echo_actor, loop, []),
    P = spawn(echo_actor, print, []),
    L ! hello1,
    P ! hello2,
    L ! hello3,
    P ! hello4.


%% Terminal Output:

%% 1> c(echo_actor).     
%% {ok,echo_actor}
%% 2> echo_actor:start().
%% loop: hello1
%% print: hello2
%% hello4
%% loop: hello3


