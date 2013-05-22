/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package takka.routing

import language.implicitConversions
import language.postfixOps
import scala.concurrent.duration._
import takka.actor._
import akka.ConfigurationException
import akka.pattern.pipe
import com.typesafe.config.Config
import scala.collection.JavaConverters.iterableAsScalaIterableConverter
import java.util.concurrent.atomic.{ AtomicLong, AtomicBoolean }
import java.util.concurrent.TimeUnit
import akka.event.Logging.Warning
import scala.concurrent.forkjoin.ThreadLocalRandom
import akka.dispatch.Dispatchers
import scala.annotation.tailrec
import concurrent.ExecutionContext
import akka.event.Logging.Warning
import akka.actor.ActorPath
import akka.actor.SupervisorStrategy
import akka.actor.OneForOneStrategy
import akka.routing.Router


case class Destination[M:Manifest](sender: ActorRef[_], recipient: ActorRef[M]) extends Product with Serializable

trait RouterConfig[M]{
  private var akkaConfig:akka.routing.RouterConfig = _
  
  def fromAkkaRouterConfig[M](config:akka.routing.RouterConfig) = {
    this.akkaConfig = config
  }
  def toAkkaRouterConfig = this.akkaConfig
  
  def createRoute(routeeProvider: RouteeProvider[M]): Route[M]
  
}


class RouteeProvider[M]{
  
}


abstract class NoRouter[M] extends RouterConfig[M]{
  
}
