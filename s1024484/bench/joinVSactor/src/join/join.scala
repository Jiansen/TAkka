
package join

/** A module providing constracts for join patterns.
 *  Based on Philipp Haller's Scala Joins library <a href="http://lamp.epfl.ch/~phaller/joins/index.html"></a> with following improvements
 *  
 *  -- use uniform keyword 'and'
 *  -- support unlimited number of patterns in a single join definition in theory 
 *       (due to the limitation of current Scala compiler, there would be an upper
 *        limit for the number of cases statements in a single block)
 *  -- prevent overriding defined join patterns in a join object
 *  -- support pattern matching on messages
 *  
 *  Limitation
 *  -- does not support subtyping on channel types
 *  -- in each join pattern, we don't check
 *     (a) if the body contains a reply to a synchronous channel which does not appear in the pattern part
 *     (b) if the all synchronous channels in the pattern part will get a reply.
 *  
 *  @author  Jiansen HE
 *  @version 0.3.3, 17/10/2011
 */

import scala.collection.immutable.{HashMap}
import scala.collection.mutable.{Queue, MutableList, Set, HashSet}
import scala.concurrent.{Lock, SyncVar}
import scala.concurrent.ops._
import scala.reflect.Manifest

  // Base class for local channels
trait NameBase{  
  // Type equality test
  def argTypeEqual(t:Any):Boolean
  def pendArg(arg:Any):Unit
  def popArg():Unit
}


  /** Asynchronous local channel
   *
   *  @param owner  the Join object where this channel is defined
   *  @param argT   the descriptor the argument type
   */
class AsyName[Arg](implicit owner: Join, argT:ClassManifest[Arg]) extends NameBase{
  
  def getQ:Queue[Arg] = {argQ}
  // Does not support subtyping
  override def argTypeEqual(t:Any) :Boolean = {
    //t = argT
    t.toString == argT.toString
  }
  
  override def pendArg(arg:Any):Unit = {
    argQ += arg.asInstanceOf[Arg]
  }
  
  override def popArg():Unit = {
    argQ.dequeue()
  }
  
  var argQ = new Queue[Arg] //queue of arguments pending on this name

  /** Pending message on this channel
   *
   *  @param  a  the message pending on this channel
   */
  def apply(a:Arg) :Unit = { 
    if(argQ.contains(a)){
      argQ += a
    }else synchronized {
      owner.trymatch(this, a)// see if the new message will trigger any pattern
    }
  }

  def unapply(attr:Any) : Option[Arg]= attr match { // for pattern matching
    case (ch:NameBase, arg:Any) => {
      if(ch == this){
        Some(arg.asInstanceOf[Arg])
      }else{
        None
      }
    }
    case (nameset: Set[NameBase], pattern:PartialFunction[Any, Any], fixedMsg:HashMap[NameBase, Any], tag:Int, banedName:NameBase) => {
      
      if (this == banedName) {return None}  
      if(fixedMsg.contains(this)){
        Some(fixedMsg(this).asInstanceOf[Arg])
      }else{        
        var checkedMsg = new HashSet[Arg]
        
        def matched(m:Arg):Boolean = {
          if (checkedMsg(m)) {false} // the message has been checked
          else { //   pattern cannot be fired without the presence of current channel  
                 //&& pattern can be fired when m is bound to the current channel 
            checkedMsg += m
            (!(pattern.isDefinedAt(nameset, pattern, fixedMsg+((this, m)), tag+1, this))
            && (pattern.isDefinedAt((nameset, pattern, fixedMsg+((this, m)), tag+1, banedName))))
          }
        }
        
        var returnV:Option[Arg] = None
        argQ.span(m => !matched(m)) match {
           case (_, MutableList()) => { // no message pending on this channel may trigger the pattern
             returnV = None
           }
           case (ums, ms) => {
//  println("\ntag "+tag+": fixedMsg = "+fixedMsg+" this = "+this+" argQ = "+argQ )  // for debugging
             val arg = ms.head // the message could trigger a pattern
             argQ = (((ums.+=:( arg )) ++ ms.tail).toQueue) // pop this message to the head of message queue
             if(/*tag == 1*/ true) {nameset.add(this); /*println(nameset)*/}
             returnV = Some(arg)
           }
        }
        checkedMsg.clear
        returnV
      }
    }
    case attr => {throw new Error("Failure arise when examing patterns "+attr)} // should not arrive here
  }
}

  /** Synchronous local channel
   *
   *  @param owner  the Join object where this channel is defined
   *  @param argT   the descriptor the argument type
   *  @param resT   the descriptor the return type
   */
class SynName[Arg, R](implicit owner: Join, argT:ClassManifest[Arg], resT:ClassManifest[R]) extends NameBase{
  // does not support subtyping
  override def argTypeEqual(t:Any) :Boolean = t match {
    case (aT,rT) => 
      (argT.toString == aT.toString  && resT.toString == rT.toString )
    case _ => false
  }
  
  override def pendArg(arg:Any):Unit = {
    argQ += arg.asInstanceOf[Arg]
  }

  override def popArg():Unit = synchronized {
    val arg = argQ.dequeue()
    setMsgTag(arg)
  }
  
  def setMsgTag(arg:Any) = synchronized {  
    if (msgTag.isSet) msgTag.get
    msgTag.set(arg.asInstanceOf[Arg])
  }
  
  var argQ = new Queue[Arg]  // argument queue
  var msgTag = new SyncVar[Arg] // matched msg
  var resultQ = new Queue[(Arg, R)] // results
  
  /** Pending message on this channel
   *
   *  @param  a  the message pending on this channel
   *  @return    the returned value
   */
  def apply(a:Arg) :R = {
    if(argQ.contains(a)){
      argQ += a
    }else {
      owner.trymatch(this, a)
    }   
    fetch(a)
  }
  
  /** reply value r to this channel
   *
   *  @param r the reply value
   */
  def reply(r:R):Unit = spawn {synchronized {
    resultQ.enqueue((msgTag.get, r))
    notifyAll()
  }}

  private def fetch(a:Arg):R = synchronized {
//    println("fetch " + a + " from "+ resultQ)
    if (resultQ.isEmpty || resultQ.front._1 != a){
      wait(); fetch(a)
    }else{
//println("got")      
      //notifyAll()
      resultQ.dequeue()._2
    }
  }

  def unapply(attr:Any) : Option[Arg]= { attr match{ // for pattern matching
    case (ch:NameBase, arg:Any) => {
      if(ch == this){
        Some(arg.asInstanceOf[Arg])
      }else{
        None
      }
    }
    case (nameset: Set[NameBase], pattern:PartialFunction[Any, Any], fixedMsg:HashMap[NameBase, Any], tag:Int, banedName:NameBase) => {
//  println("\ntag "+tag+": fixedMsg = "+fixedMsg+" this = "+this+" argQ = "+argQ )  // for debugging              
      if (this == banedName) {return None}      

      if(fixedMsg.contains(this)){
        Some(fixedMsg(this).asInstanceOf[Arg])
      }else{        
        var checkedMsg = new HashSet[Arg]
        
        def matched(m:Arg):Boolean = {
          if (checkedMsg(m)) {false} // the message has been checked
          else { //   pattern cannot be fired without the presence of current channel  
                 //&& pattern can be fired when m is bound to the current channel 
            checkedMsg += m
            (!(pattern.isDefinedAt(nameset, pattern, fixedMsg+((this, m)), tag+1, this))
            && (pattern.isDefinedAt((nameset, pattern, fixedMsg+((this, m)), tag+1, banedName))))
          }
        }

        var returnV:Option[Arg] = None
        argQ.span(m => !matched(m)) match {
           case (_, MutableList()) => { // no message pending on this channel may trigger the pattern
             returnV = None
           }
           case (ums, ms) => {
//  println("\ntag "+tag+": fixedMsg = "+fixedMsg+" this = "+this+" argQ = "+argQ )  // for debugging
             val arg = ms.head // the message could trigger a pattern
             argQ = (((ums.+=:( arg )) ++ ms.tail).toQueue) // pop this message to the head of message queue
             if(tag == 1) {nameset.add(this); /*println(nameset)*/}
             setMsgTag(arg)
             returnV = Some(arg)
           }
        }
        checkedMsg.clear
        returnV
      }
    }
    case attr => {throw new Error("Failure arise when examing patterns "+attr)} // should not arrive here
  }
  }
}

object and{
//  def unapply(attr:(Set[NameBase], Queue[(NameBase, Any)], PartialFunction[Any, Any], Int)) = {
  def unapply(attr:Any) = {  
      Some(attr,attr)
  }
}

class Join {
  private var hasDefined = false
//  private var lock : AnyRef = new Object()  
  implicit val joinsOwner = this
  
  private var joinPat: PartialFunction[Any, Any] = _
//  private var joinPat: PartialFunction[(Set[NameBase], Queue[(NameBase, Any)], PartialFunction[Any, Any], Int), Unit] = _
//  def join(joinPat: PartialFunction[(Set[NameBase], Queue[(NameBase, Any)], PartialFunction[Any, Any], Int), Unit]) {
  def join(joinPat: PartialFunction[Any, Any]) { 
   if(!hasDefined){
     this.joinPat = joinPat
     hasDefined = true
   }else{
     throw new Exception("Join definition has been set for"+this)
   }
  }
   
  def trymatch(ch:NameBase, arg:Any) = synchronized {
    var names: Set[NameBase] = new HashSet
//println(ch +"   "+ arg)
    try{
      if(joinPat.isDefinedAt((ch, arg))){// optimization for singleton pattern
        if(ch.isInstanceOf[SynName[Any, Any]]) {ch.asInstanceOf[SynName[Any,Any]]setMsgTag(arg)}
        joinPat((ch,arg))
      }else{
        if(ch.isInstanceOf[SynName[Any, Any]]){
          joinPat((names, this.joinPat, (new HashMap[NameBase, Any]+((ch, arg))), 1, new SynName))            
          ch.asInstanceOf[SynName[Any,Any]].setMsgTag(arg)          
        }else{
          joinPat((names, this.joinPat, (new HashMap[NameBase, Any]+((ch, arg))), 1, new AsyName))
        }
        names.foreach(n => {
          if(n != ch) n.popArg
        })
      }
    }catch{
      case e:MatchError => ch.pendArg(arg)// no pattern is matched
    }
  }
}