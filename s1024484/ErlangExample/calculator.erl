-module(calculator).

-export([start/0, calculator/0]).

start() ->
    io:fwrite("Starting...~n"),
    S = spawn(calculator, calculator, []),  
    S ! {10, 1}, 
    S ! {10, 0},
    {ok, self}.
    

    
calculator() ->
    timer:sleep(1000),
    receive
%        {M, 0} ->
%          io:fwrite("~w / ~w ~n", [M, 0]),        
%          erlang:raise(exit, "Boom!", []),
%          calculate();
        {M, N} ->
          io:fwrite("~w / ~w is ~w ~n", [M, N, M/N]),
          calculator()
    end.
