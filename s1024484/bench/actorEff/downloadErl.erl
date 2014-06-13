-module(downloadErl).

-export([start/0]).
-export([actorManager/2, downloadActor/2, indexActor/2, writeActor/1]).

requestloop(NR_REQUESTS, DownloadActor) ->
   if
     NR_REQUESTS > 0 ->
       DownloadActor ! {payload, string:concat("Requested ", integer_to_list(NR_REQUESTS))},
       requestloop(NR_REQUESTS-1, DownloadActor);
     NR_REQUESTS == 0 ->
       DownloadActor ! stopMsg
   end.

actorManager(Num_Actors, Manager) ->
  if
    Num_Actors > 0 -> 
      receive
        decrement ->
          actorManager(Num_Actors-1, Manager)
      end;
    true -> Manager ! stop
  end.


replace(Str, OldHead, NewHead) ->
  RegExp = "\\Q"++OldHead++"\\E",
  re:replace(Str,RegExp,NewHead,[multiline, {return, list}]).

downloadActor(IndexActor, ActorManager) ->
  receive
    {payload, Payload} -> 
          NewPayload = replace(Payload, "Requested", "Downloaded"),
          IndexActor ! {payload, NewPayload},
          downloadActor(IndexActor, ActorManager);
    stopMsg ->
      IndexActor ! stopMsg,
      ActorManager ! decrement
  end.

indexActor(WriteActor, ActorManager) ->
  receive
    {payload, Payload} -> 
          NewPayload = replace(Payload, "Downloaded", "Indexed"),
          WriteActor ! {payload, NewPayload},
          indexActor(WriteActor, ActorManager);
    stopMsg ->
      WriteActor ! stopMsg,
      ActorManager ! decrement
  end.

writeActor(ActorManager) ->
  receive 
    {payload, Payload} ->
          if
            Payload == "Indexed 1" ->
              replace(Payload, "Indexed", "Wrote"),
              ActorManager ! decrement;
%%              writeActor(ActorManager);
            true ->
              replace(Payload, "Indexed", "Wrote"),
              writeActor(ActorManager)
          end;
    stopMsg ->
          true
%%          ActorManager ! decrement
  end.

start() ->
    ActorManager = spawn(downloadErl, actorManager, [3, self()]),
    WriteActor = spawn(downloadErl, writeActor, [ActorManager]),
    IndexActor = spawn(downloadErl, indexActor, [WriteActor, ActorManager]),
    DownloadActor = spawn(downloadErl, downloadActor, [IndexActor, ActorManager]),
    Start = now(),
    requestloop(500000, DownloadActor),

    receive
      stop ->
        End = now(),
        timer:now_diff(End, Start)
    end.

%% NR_REQUESTS      Time(microseconds)
%%    10000             148299
%%   100000            1515986
%%   200000            3112987
%%   300000            4402319
%%   400000            5808968
%%   500000            6912921
%%   600000            8728368
%%   700000           11264396
%%   800000           13041618
%%   900000           14667797
%%  1000000           16148678
%% 10000000          -
