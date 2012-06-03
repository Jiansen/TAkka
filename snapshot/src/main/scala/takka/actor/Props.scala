package takka.actor

/**
 * Factory for Props instances.
 *
 * Props is a ActorRef configuration object, that is thread safe and fully sharable.
 *
 * Used when creating new actors through; <code>ActorSystem.actorOf</code> and <code>ActorContext.actorOf</code>.
 */
object Props{
  /**
   * Returns a cached default implementation of Props.
   */
  def apply[T](): Props[T] = {
    val p = akka.actor.Props()
    return Props[T](p)
  }
  
  /**
   * Returns a Props that has default values except for "creator" which will be a function that creates an instance
   * of the supplied type using the default constructor.
   */
  def apply[T, A<:Actor[T]] (implicit arg0: ClassManifest[A]): Props[T] = {
    val p = akka.actor.Props[A]
    return Props(p)
  }
  
  /**
   * Returns a Props that has default values except for "creator" which will be a function that creates an instance
   * of the supplied class using the default constructor.
   */
  def apply[T](actorClass: Class[_ <: Actor[T]]): Props[T] = {
    val p = akka.actor.Props(actorClass)
    return Props(p)
  }
  
  /**
   * Returns a Props that has default values except for "creator" which will be a function that creates an instance
   * using the supplied thunk.
   */
  def apply[T](creator: => Actor[T]): Props[T] = {
    val p = akka.actor.Props(creator)
    return Props(p)
  }
}


/**
 * Props is a ActorRef configuration object, that is thread safe and fully sharable.
 * Used when creating new actors through; <code>ActorSystem.actorOf</code> and <code>ActorContext.actorOf</code>.
 *
 * Users should initialise a Props using one of the following APIs:
 * 
 * {{{
 *  val props = Props[M, MyActor]
 *  val props = Props[M](new MyActor)
 *  val props = Props[M](myActor.getClass)
 * }}}
 */
case class Props[-T] (props: akka.actor.Props) {
  def withCreator[T] (c: => Actor[T]): Props[T] ={
    val p = akka.actor.Props(c)
    return Props(p)
  }
}