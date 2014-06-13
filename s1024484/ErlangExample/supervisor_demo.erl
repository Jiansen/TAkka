-module(supervisor_demo).
-behaviour(supervisor).

-export([start/0, start/1, loop/1, init/1]).

start() ->
    {ok, C} = supervisor:start_link(supervisor_demo, [2]),
    C.
start(Count) ->
  io:fwrite("Starting...~n"),
  Pid=spawn_link(supervisor_demo, loop, [Count]),
  {ok, Pid}.
	
init([Count]) ->
  {ok, {{one_for_one, 2, 60},
          [{supervisor_demo, {supervisor_demo, start, [Count]},
          permanent, brutal_kill, worker, [supervisor_demo]}]}}.
	
loop(Count) ->
  io:fwrite("~w / ~w is ~w ~n", [10, Count, 10/Count]),
  loop(Count-1).
  
%% 1> c(supervisor_demo).
%% {ok,supervisor_demo}
%% 2> supervisor_demo:start().
%% Starting...
%% 10 / 2 is 5.0 
%% 10 / 1 is 10.0 
%% <0.39.0>
%% Starting...
%% 10 / 2 is 5.0 
%% 10 / 1 is 10.0 
%% Starting...
%% 3> 
%% =ERROR REPORT==== 14-Oct-2013::00:03:49 ===
%% Error in process <0.40.0> with exit value: 
{badarith,[{supervisor_demo,loop,1,[{file,"supervisor_demo.erl"},{line,22}]}]}
%% 10 / 2 is 5.0 
%% 3> 
%% =ERROR REPORT==== 14-Oct-2013::00:03:49 ===
%% Error in process <0.42.0> with exit value: 
{badarith,[{supervisor_demo,loop,1,[{file,"supervisor_demo.erl"},{line,22}]}]}
%% 10 / 1 is 10.0 
%% =ERROR REPORT==== 14-Oct-2013::00:03:49 ===
%% Error in process <0.43.0> with exit value: 
{badarith,[{supervisor_demo,loop,1,[{file,"supervisor_demo.erl"},{line,22}]}]}
%% ** exception error: shutdown
