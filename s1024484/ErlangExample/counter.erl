-module(counter).
-export([start/0, counter/1]).
 
start() ->
    S = spawn(counter, counter, [0]),
    S ! increment,
    send_msgs(S, 3),
    S.
 
counter(Val) ->
    receive
        increment ->
          io:fwrite("increase counter to ~w~n", [Val+1]),
          counter(Val+1);
        Msg -> 
          io:fwrite("~w message(s) that has/have been processed ~n", [Val+1]),
          counter(Val+1)
    end.
 
send_msgs(_, 0) -> true;
send_msgs(S, Count) ->
    S ! "Hello",
    send_msgs(S, Count-1).
 
%% Terminal Output:
%% 1> c(counter).
%% {ok,counter}
%% 2> counter:start().
%% increat counter to 1
%% <0.95.0>
%% 2 message(s) has/have been processed 
%% 3 message(s) has/have been processed 
%% 4 message(s) has/have been processed
