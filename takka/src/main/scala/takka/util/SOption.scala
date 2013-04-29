package takka.util

/** 
 * Globally serializable optional value. Instance of SOption is either an instance of SSome or the object SNone.
 *
 *  Copied and Modified from Scala Source Code
 *   
 *  @author  Jiansen HE
 *  @version 1.0, 10/03/2012
 */
//@SerialVersion("serializable Option Type v.1.0")
@SerialVersionUID(10L)
sealed abstract class SOption[+A] extends Product with Serializable {
  self =>

  /** Returns true if the option is $none, false otherwise.
   */
  def isEmpty: Boolean

  /** Returns true if the option is an instance of $some, false otherwise.
   */
  def isDefined: Boolean = !isEmpty

  /** Returns the option's value.
   *  @note The option must be nonEmpty.
   *  @throws Predef.NoSuchElementException if the option is empty.
   */
  def get: A

  /** Returns the option's value if the option is nonempty, otherwise
   * return the result of evaluating `default`.
   *
   *  @param default  the default expression.
   */
  def getOrElse[B >: A](default: => B): B = 
    if (isEmpty) default else this.get

  /** Returns the option's value if it is nonempty,
   * or `null` if it is empty.
   * Although the use of null is discouraged, code written to use 
   * $option must often interface with code that expects and returns nulls.
   * @example {{{
   * val initalText: SOption[String] = getInitialText
   * val textField = new JComponent(initalText.orNull,20)
   * }}}
   */
  def orNull[A1 >: A](implicit ev: Null <:< A1): A1 = this getOrElse null

  /** Returns a $some containing the result of applying $f to this $option's
   * value if this $option is nonempty.
   * Otherwise return $none.
   *
   *  @note This is similar to `flatMap` except here,
   *  $f does not need to wrap its result in an $option.
   *
   *  @param  f   the function to apply
   *  @see flatMap
   *  @see foreach
   */
  def map[B](f: A => B): SOption[B] = 
    if (isEmpty) SNone else SSome(f(this.get))

  /** Returns the result of applying $f to this $option's value if
   * this $option is nonempty.
   * Returns $none if this $option is empty.
   * Slightly different from `map` in that $f is expected to
   * return an $option (which could be $none).
   *
   *  @param  f   the function to apply
   *  @see map
   *  @see foreach
   */
  def flatMap[B](f: A => SOption[B]): SOption[B] = 
    if (isEmpty) SNone else f(this.get)

  /** Returns this $option if it is nonempty '''and''' applying the predicate $p to
   * this $option's value returns true. Otherwise, return $none.
   *
   *  @param  p   the predicate used for testing.
   */
  def filter(p: A => Boolean): SOption[A] = 
    if (isEmpty || p(this.get)) this else SNone
  
  /** Returns this $option if it is nonempty '''and''' applying the predicate $p to
   * this $option's value returns false. Otherwise, return $none.
   *
   *  @param  p   the predicate used for testing.
   */
  def filterNot(p: A => Boolean): SOption[A] =
    if (isEmpty || !p(this.get)) this else SNone

  /** Necessary to keep $option from being implicitly converted to
   *  [[scala.collection.Iterable]] in `for` comprehensions.
   */
  def withFilter(p: A => Boolean): WithFilter = new WithFilter(p)

  /** We need a whole WithFilter class to honor the "doesn't create a new
   *  collection" contract even though it seems unlikely to matter much in a
   *  collection with max size 1.
   */
  class WithFilter(p: A => Boolean) {
    def map[B](f: A => B): SOption[B] = self filter p map f
    def flatMap[B](f: A => SOption[B]): SOption[B] = self filter p flatMap f
    def foreach[U](f: A => U): Unit = self filter p foreach f
    def withFilter(q: A => Boolean): WithFilter = new WithFilter(x => p(x) && q(x))
  }

  /** Returns true if this option is nonempty '''and''' the predicate
   * $p returns true when applied to this $option's value.
   * Otherwise, returns false.
   *
   *  @param  p   the predicate to test
   */
  def exists(p: A => Boolean): Boolean =
    !isEmpty && p(this.get)

  /** Apply the given procedure $f to the option's value,
   *  if it is nonempty. Otherwise, do nothing.
   *
   *  @param  f   the procedure to apply.
   *  @see map
   *  @see flatMap
   */
  def foreach[U](f: A => U) {
    if (!isEmpty) f(this.get)
  }

  /** Returns a $some containing the result of
   * applying `pf` to this $option's contained
   * value, '''if''' this option is
   * nonempty '''and''' `pf` is defined for that value.
   * Returns $none otherwise.
   *
   *  @param  pf   the partial function.
   *  @return the result of applying `pf` to this $option's
   *  value (if possible), or $none.
   */
  def collect[B](pf: PartialFunction[A, B]): SOption[B] =
    if (!isEmpty && pf.isDefinedAt(this.get)) SSome(pf(this.get)) else SNone  

  /** Returns this $option if it is nonempty,
   *  otherwise return the result of evaluating `alternative`.
   *  @param alternative the alternative expression.
   */
  def orElse[B >: A](alternative: => SOption[B]): SOption[B] = 
    if (isEmpty) alternative else this

  /** Returns a singleton iterator returning the $option's value
   * if it is nonempty, or an empty iterator if the option is empty.
   */
  def iterator: Iterator[A] = 
    if (isEmpty) collection.Iterator.empty else collection.Iterator.single(this.get)

  /** Returns a singleton list containing the $option's value
   * if it is nonempty, or the empty list if the $option is empty.
   */
  def toList: List[A] = 
    if (isEmpty) List() else List(this.get)

  /** Returns a [[scala.Left]] containing the given
   * argument `left` if this $option is empty, or
   * a [[scala.Right]] containing this $option's value if
   * this is nonempty.
   *
   * @param left the expression to evaluate and return if this is empty
   * @see toLeft
   */
  def toRight[X](left: => X) =
    if (isEmpty) Left(left) else Right(this.get)

  /** Returns a [[scala.Right]] containing the given
   * argument `right` if this is empty, or
   * a [[scala.Left]] containing this $option's value
   * if this $option is nonempty.
   *
   * @param right the expression to evaluate and return if this is empty
   * @see toRight
   */
  def toLeft[X](right: => X) =
    if (isEmpty) Right(right) else Left(this.get)
}

/** Class `SSome[A]` represents existing values of type
 *  `A`.
 *   
 *  Copied and Modified from Scala Source Code
 *   
 *  @author  Jiansen HE
 *  @version 1.0, 10/03/2012
 */
//@SerialVersion("serializable Some Object v.1.0")
@SerialVersionUID(10L)
final case class SSome[+A](x: A) extends SOption[A] {
  def isEmpty = false
  def get = x
}


/** This case object represents non-existent values.
 *
 *  Copied and Modified from Scala Source Code
 *   
 *  @author  Jiansen HE
 *  @version 1.0, 10/03/2012
 */
//@SerialVersion("serializable None Object v.1.0")
@SerialVersionUID(10L)
case object SNone extends SOption[Nothing] {
  def isEmpty = true
  def get = throw new NoSuchElementException("None.get")
}