-module(chameneosErl).

-export([start/0]).
-export([mall/3, mallactor/6, chameneo/4]).

start() -> start(1, 4).

start(N, NumChameneos) ->
  Start = now(),
  spawn(chameneosErl, mall, [self(), N, NumChameneos]),
  receive
      stop ->
        End = now(),
        timer:now_diff(End, Start)
  end.

colour(0) -> blue;
colour(1) -> yellow;
colour(2) -> red;
colour(3) -> faded.

startChameneos(NumChameneos, NumChameneos, _Mall) -> exit;
startChameneos(I, NumChameneos, Mall) ->
  spawn(chameneosErl, chameneo, [Mall, colour(I rem 3), I, 0]), 
  startChameneos(I+1, NumChameneos, Mall).

mall(Main, N, NumChameneos) ->
  Mall = spawn(chameneosErl, mallactor, [Main, 0, 0, NumChameneos, N, none]),
  startChameneos(0, NumChameneos, Mall).

mallactor(Main, SumMeetings, NumFaded, NumChameneos, N, WaitingChameneo) ->
  io:format("Mall ~p WaitingChameneo ~p  ~p ~n",[self(), WaitingChameneo, WaitingChameneo == none]),
  receive
    {meetingCount, I} ->
        if
         (NumFaded + 1 == numChameneos) -> Main ! stop
%%         true -> 
        end,
      mallactor(Main, SumMeetings+I, NumFaded+1, NumChameneos, N, WaitingChameneo);
    {meet, Colour, Sender} ->
      if
        N > 0 -> 
          io:format("Hello WaitingChameneo ~p  ~p ~n ",[WaitingChameneo, WaitingChameneo == none ]),
          if
            none == WaitingChameneo-> 
               mallactor(Main, SumMeetings, NumFaded, NumChameneos, N, Sender);
            true ->
               WaitingChameneo ! {meet, Colour, Sender},
               mallactor(Main, SumMeetings, NumFaded, NumChameneos, N-1, none)
          end;
        true ->
          if
            none /= WaitingChameneo ->
               WaitingChameneo ! {exit, self};
            true -> io:format("Hello 4~n",[])
          end,
          Sender ! {exit, self()}
      end; 
    M -> io:format("Mall: Unrecognized msg ~p",[M])
  end.

chameneo(Mall, Colour, ID, Meetings) ->
  chameneoActor(Mall, Colour, ID, Meetings).
chameneoActor(Mall, Colour, ID, Meetings) ->
  Mall ! {meet, Colour, self()}, 
  io:format("This ~p  Colour ~p ID ~p Meetings ~p ~n",[self(), Colour, ID, Meetings]),
  receive
    {meet, OtherColour, Sender} ->
       Sender ! {change, Colour},
       chameneoActor(Mall, complement(Colour, OtherColour), ID, Meetings+1);
    {change, NewColour} ->
       io:format("CHANGE",[]),
       chameneoActor(Mall, NewColour, ID, Meetings+1);
    {exit, _Sender} ->
%%       io:format("chameneo ~p ~p Mall ~P !!!~n",[ID, Colour, Mall]),
%%       exit(self(), normal),
       chameneoActor(Mall, faded, ID, Meetings);
    M -> io:format("CC: Unrecognized msg ~p",[M])
  end.

complement(red, red) -> red;
complement(red, yellow) -> blue;
complement(red, blue) -> blue;
complement(yellow, red) -> blue;
complement(yellow, yellow) -> yellow;
complement(yellow, blue) -> red;
complement(blue, red) -> yellow;
complement(blue, yellow) -> red;
complement(blue, blue) -> blue;
complement(_, faded) -> faded;
complement(faded, _) -> faded.



