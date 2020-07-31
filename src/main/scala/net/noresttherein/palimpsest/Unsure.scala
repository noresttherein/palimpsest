package net.noresttherein.palimpsest

import net.noresttherein.palimpsest.RuntimeType.Specialized
import net.noresttherein.palimpsest.RuntimeType.Specialized.Primitives
import net.noresttherein.palimpsest.Specialize.Self




/** A specialized equivalent of `scala.Option`. Declares the same methods as the latter so can be used as a 'drop in' replacement.
  * Note that a type alias [[net.noresttherein.palimpsest.?]] is declared as a shorthand in the same package. A convention adapted by this library is
  * to end names of methods returning an uncertain value with `_?`, ''i.e'' `head_? : ?[E]` for analogues of `headOpt`.
  * @author Marcin Mościcki marcin@moscicki.net
  */
sealed trait Unsure[@specialized(Primitives) +T] extends FunctorLike[T, Unsure] with Serializable {
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

	//todo: does the specialization below make sense for types functions are not specialized for?

	@inline final def map[@specialized(Primitives) O](f: T => O): Unsure[O] =
		if (isEmpty) Blank else Sure(f(this.get))


	@inline final def flatMap[@specialized(Primitives) O](f: T => Unsure[O]): Unsure[O] =
		if (isEmpty) Blank else f(this.get)

	@inline final def flatten[@specialized(Primitives) O](implicit ev: T <:< Unsure[O]): Unsure[O] =
		if (isEmpty) Blank else ev(this.get)

	/** If the given condition is false, return a `Blank` instance. Otherwise return `this`.
	  * Note that importing implicit conversion [[Unsure$.?:]] will patch any type with the same method,
	  * creating a conditional expression producing an `Unsure` instance.
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
		new UnsureConditional(Sure(value))

	class UnsureConditional[T](private val sure :Sure[T]) extends AnyVal {
		/** Returns this value as `Sure(value)` if the left-hand condition evaluates to `true` or `Blank` otherwise. */
		@inline def ?:(condition :Boolean) :Unsure[T] =
			if (!condition || sure.isEmpty) Blank
			else sure
	}


}



/** An equivalent of `scala.None` for use with uncertain values of type [[net.noresttherein.palimpsest.Unsure]]`[T]`. */
case object Blank extends Unsure[Nothing] {

	override def toOption :Option[Nothing] = None

	override def isEmpty  = true

	override def get :Nothing = throw new NoSuchElementException("Blank.get")
}




/** A specialized equivalent of `scala.Some`. */ //this is private[palimpsest] only because we can't make it private
final class Sure[@specialized(Primitives) +T] private[palimpsest] (private[this] val x :T) extends Unsure[T] {

	/** This is the same as `get`, but is not declared by the parent trait, meaning it can only be called
	  * if this instance is statically known to be `Sure`. For this reason it is always preferable to `get`,
	  * as refactors can easily make the latter call unsafe in code which was previously safe.
	  */
	@inline def value :T = x

	/** Returns the value wrapped by this instance. Prefer using [[Sure.value]] instead whenever `this` is known to by Sure.*/
	@inline override def get :T = x

	@inline override def toOption :Option[T] = Some(x)

	@inline def isEmpty = false


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Sure[_]]

	override def equals(that :Any) :Boolean = that match {
		case sure :Sure[_] => (sure eq this) ||  sure.value == x
		case _ => false
	}

	override def hashCode :Int = x.hashCode


	override def toString :String = "Sure(" + x + ")"
}





/** Factory of [[Sure]] instances. */
object Sure {

	private[this] val ByteCache = (Byte.MinValue to Byte.MaxValue).map(new Sure(_)).toArray

	private[this] final val CharCacheMin = -127
	private[this] final val CharCacheMax = 128
	private[this] val CharCache = (CharCacheMin.toChar to CharCacheMax.toChar).map(new Sure(_)).toArray

	private[this] final val ShortCacheMin = -127
	private[this] final val ShortCacheMax = 128
	private[this] val ShortCache = (ShortCacheMin.toShort to ShortCacheMax.toShort).map(new Sure(_)).toArray

	private[this] final val IntCacheMin = -255
	private[this] final val IntCacheMax = 256
	private[this] val IntCache = (IntCacheMin to IntCacheMax).map(new Sure(_)).toArray

	private[this] final val LongCacheMin = -255L
	private[this] final val LongCacheMax = 256L
	private[this] val LongCache = (LongCacheMin to LongCacheMax).map(new Sure(_)).toArray
	private[this] val True = new Sure(true)
	private[this] val False = new Sure(false)

	def apply[@specialized(Primitives) T](value :T) :Sure[T] = Specialized[T] match {
		case Specialized.ForInt =>
			val int = value.asInstanceOf[Int]
			if (int >= IntCacheMin && int <= IntCacheMax) IntCache(int - IntCacheMin).asInstanceOf[Sure[T]]
			else new Sure(value)

		case Specialized.ForLong =>
			val long = value.asInstanceOf[Long]
			if (long >= LongCacheMin && long <= LongCacheMax) LongCache((long - LongCacheMin).toInt).asInstanceOf[Sure[T]]
			else new Sure(value)

		case Specialized.ForByte =>
			ByteCache(value.asInstanceOf[Byte].toInt - Byte.MinValue).asInstanceOf[Sure[T]]

		case Specialized.ForChar =>
			val char = value.asInstanceOf[Char]
			if (char >= CharCacheMin && char <= CharCacheMax) CharCache(char - CharCacheMin).asInstanceOf[Sure[T]]
			else new Sure(value)
		case Specialized.ForBoolean =>
			(if (value.asInstanceOf[Boolean]) True else False).asInstanceOf[Sure[T]]
//		case Specialized.ForShort =>
//			val short = value.asInstanceOf[Short]
//			if (short >= ShortCacheMin && short <= ShortCacheMax) ShortCache(short - ShortCacheMin).asInstanceOf[Sure[T]]
//			else new Sure(value)
		case _ => new Sure(value)
	}


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