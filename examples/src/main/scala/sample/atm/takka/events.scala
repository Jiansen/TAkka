package sample.atm.takka

import akka.actor._

class states extends Actor with akka.actor.ActorLogging {
  def receive = {
    case info:String => log.warning("Received message: {" + info + "}\n")
  }
}

object stats {
  private val system = ActorSystem("statsSystem")
  
  private var log:ActorRef = {
    assert(system.actorFor("/user/log").isTerminated, "stats log has been started.")
    system.actorOf(Props[states], "log")
  }
  
  def logging(info:String) = {
    log ! info
  }
}

/*
-define(EVMGR, {global, ?MODULE}).

-export([start_link/0, stop/0, log/1, add_handler/2, delete_handler/2,
         get_stats/1]).

start_link() ->
    gen_event:start_link(?EVMGR).

stop() ->
    gen_event:stop(?EVMGR).

log(Item) ->
    gen_event:notify(?EVMGR, Item).

add_handler(Handler, Args) ->
    gen_event:add_handler(?EVMGR, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?EVMGR, Handler, Args).

get_stats(Handler) ->
    gen_event:call(?EVMGR, Handler, get_stats).
*/