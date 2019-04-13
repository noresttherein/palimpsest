package net.turambar.palimpsest.specialty

import net.turambar.palimpsest.specialty.Specialized.Primitives




/** A specialized equivalent of `scala.Option`. Declares the same methods as the latter so can be used as a 'drop in' replacement.
  * Note that a type alias [[net.turambar.palimpsest.specialty.?]] is declared as a shorthand in the same package. A convention adapted by this library is
  * to end names of methods returning an uncertain value with `_?`, ''i.e'' `head_? : ?[E]` for analogues of `headOpt`.
  * @author Marcin Mościcki marcin@moscicki.net
  */
sealed trait Unsure[@specialized(Primitives) +T] extends Serializable {
	def toOption :Option[T]

	def isEmpty :Boolean

	@inline final def nonEmpty :Boolean = isDefined

	@inline final def isDefined :Boolean = !isEmpty

	@inline final def isBlank :Boolean = isEmpty

	@inline final def isSure :Boolean = !isEmpty


	def get :T

	@inline final def getOrElse[U >: T](default: => U): U =
		if (isEmpty) default else this.get

	@inline final def default[U >: T](default :U) :U =
		if (isEmpty) default else this.get




	@inline final def orNull[U >: T](implicit ev: Null <:< U): U = this getOrElse ev(null)

	@inline final def map[@specialized(Primitives) O](f: T => O): Unsure[O] =
		if (isEmpty) Blank else Sure(f(this.get))


	@inline final def flatMap[@specialized(Primitives) O](f: T => Unsure[O]): Unsure[O] =
		if (isEmpty) Blank else f(this.get)

	@inline final def flatten[@specialized(Primitives) O](implicit ev: T <:< Unsure[O]): Unsure[O] =
		if (isEmpty) Blank else ev(this.get)

	/** If the given condition is false, return a `Blank` instance. Otherwise return `this`.
	  * Note that importing implicit conversion [[Unsure$.?:]] will lift any value to a `Sure` instance which can
	  * be then emptied with this method, creating a conditional expression producing an `Unsure` instance.
	  */
	@inline final def ?:(condition :Boolean) :Unsure[T] =
		if (condition && !isEmpty) this
		else Blank

	@inline final def filter(p: T => Boolean): Unsure[T] =
		if (isEmpty || p(this.get)) this else Blank

	@inline final def filterNot(p: T => Boolean): Unsure[T] =
		if (isEmpty || !p(this.get)) this else Blank


	@inline final def withFilter(p: T => Boolean): Unsure[T] =
		if (isEmpty || p(this.get)) this else Blank



	@inline final def contains[U >: T](elem: U): Boolean =
		!isEmpty && this.get == elem

	@inline final def exists(p: T => Boolean): Boolean =
		!isEmpty && p(this.get)

	@inline final def forall(p: T => Boolean): Boolean = isEmpty || p(this.get)

	@inline final def foreach[O](f: T => O) :Unit =
		if (!isEmpty) f(this.get)


	@inline final def collect[O](pf: PartialFunction[T, O]): Unsure[O] =
		if (!isEmpty) pf.lift(this.get) else Blank

	@inline final def orElse[U >: T](alternative: => Unsure[U]): Unsure[U] =
		if (isEmpty) alternative else this

	/** Returns `this` if it contains a value or `alternative` otherwise. The difference from [[orElse]] is that
	  * the argument is evaluated eagerly, guaranteeing that no closure will be created and should have better performance
	  * if the alternative value was computed beforehand.
	  */
	@inline final def ifEmpty[U >: T](alternative :Unsure[U]) :Unsure[U] =
		if (isEmpty) alternative else this


	def canEqual(that :Any) :Boolean = that.isInstanceOf[Unsure[_]]
}


/** Companion object for [[Unsure]] serving as a factory. */
object Unsure {

	@inline final implicit def unsureOption[@specialized(Primitives) T](opt :Option[T]) :Unsure[T] =  opt match {
		case None => Blank
		case Some(x) => Sure(x)
	}

	@inline final def apply[@specialized(Primitives) T](value :T) :Unsure[T] =
		if (value == null) Blank else Sure(value)

	@inline final def some_?[@specialized(Primitives) T](value :Option[T]) : ?[T] =
		if (value.isEmpty) Blank else Sure(value.get)

	/** Implicit conversion adding a conditional factory method for `Unsure` instances to any value.
	  * Importing it allows to write: `condition ?: value` to produce a `Sure(value)` if `condition` is true
	  * or a blank otherwise.
	  */
	@inline final implicit def ?:[@specialized(Primitives) T](value :T) :UnsureConditional[T] =
		new UnsureConditional(new Sure(value))

	class UnsureConditional[T](private val sure :Sure[T]) extends AnyVal {
		/** Returns this value as `Sure(value)` if the left-hand condition evaluates to `true` or `Blank` otherwise. */
		@inline def ?:(condition :Boolean) :Unsure[T] =
			if (!condition || sure.isEmpty) Blank
			else sure
	}


}



/** An equivalent of `scala.None` for use with uncertain values of type [[net.turambar.palimpsest.specialty.Unsure]]`[T]`. */
case object Blank extends Unsure[Nothing] {

	override def toOption :Option[Nothing] = None

	override def isEmpty  = true

	override def get :Nothing = throw new NoSuchElementException("Blank.get")
}




/** A specialized equivalent of `scala.Some`. */
class Sure[@specialized(Primitives) +T] private[specialty] (private[this] val x :T) extends Unsure[T] {

	/** This is the same as `get`, but is not declared by the parent trait, meaning it can only be called
	  * if this instance is statically known to be `Sure`. For this reason it is always preferable to `get`,
	  * as refactors can easily make the latter call unsafe in code which was previously safe.
	  * @return
	  */
	@inline final def value :T = x

	/** Returns the value wrapped by this instance. Prefer using [[Sure.value]] instead whenever `this` is known to by Sure.*/
	override final def get :T = x

	override final def toOption :Option[T] = Some(x)

	override final def isEmpty = false


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Sure[_]]

	override def equals(that :Any) :Boolean = that match {
		case sure :Sure[_] => (sure eq this) || sure.canEqual(this) && sure.value == x
		case _ => false
	}

	override def hashCode :Int = x.hashCode


	override def toString :String = "Sure(" + x + ")"
}





/** Factory of [[Sure]] instances. */
object Sure {

	@inline final def apply[@specialized(Primitives) T](value :T) :Sure[T] = new Sure(value)


	/** Extracts an uncertain value if `mayhaps` is `Sure(x)` by using [[Unsure.toOption]].
	  * Note that '''this method is not specialized and results in double boxing''', so avoid using it in pattern matching
	  * unless performance hit is not a concern. Instead of:
	  * {{{
	  *     mayhaps match {
	  *         case Sure(x) => f(x)
	  *         case _ => g()
	  *     }
	  * }}}
	  * prefer rather:
	  * {{{
	  *     mayhaps match {
	  *         case x :Sure[X] => f(x.value)
	  *         case _ => g()
	  *     }
	  * }}}
	  */
	@inline final def unapply[T](mayhaps :Unsure[T]) :Option[T] = mayhaps.toOption
}