-module(message_order).

-export([act/0, start/0]).

act() ->
    receive
        hello->
          io:format("hello~n", []),
          receive
            bar -> 
              io:format("bar~n", []),
              receive
%            M -> 
%              io:format("~p~n", [M]),
%              act();
              foo -> 
                io:format("foo~n", []),
                act();
              hoo ->
                io:format("hoo~n", []),
                act()
            end
          end
    end.
    
start() ->
    P = spawn(message_order, act, []),
    P ! foo,
    P ! bar,
    P ! hoo,
    P ! hello, true.


%% Terminal Output:

%% 1> c(message_order).     
%% {ok,message_order}
%% 2> message_order:start().
%% hello
%% true
%% bar


