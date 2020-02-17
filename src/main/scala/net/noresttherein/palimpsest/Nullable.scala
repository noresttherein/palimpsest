package net.noresttherein.palimpsest

/** A value class treating nullable reference types as option-like monads erased in runtime. */
final class Nullable[+T <: AnyRef](/** Wrapped reference value which may be null. */val orNull :T) extends AnyVal {
	@inline def isNull :Boolean = orNull == null

//	@inline def defaultsTo :Boolean = orNull != null

	/** Tests if this value is null representing 'no value'. */
	@inline def isEmpty: Boolean = orNull == null

	/** Tests if this value is not null. */
	@inline def nonEmpty: Boolean = orNull != null

	/** Tests if this value is not null. */
	@inline def isDefined: Boolean = orNull != null


	/** Forces extraction of the value.
	  * @return contained value, if it not null
	  * @throws NoSuchElementException if this value is `null`.
	  */
	@inline def get :T =
		if (orNull==null) throw new NoSuchElementException("Nullable.Empty.get")
		else orNull

	/** Returns this value if it is not null or the lazily computed alternative passed as the argument in the opposite case. */
	@inline def getOrElse[O>:T](alt : =>O) :O =
		if (orNull==null) alt else orNull

	/** Returns this `Nullable` if it is not null or the lazily computed alternative for null values. */
	@inline def orElse[O >: T <: AnyRef](alt : =>Nullable[O]) :Nullable[O] =
		if (orNull==null) alt else this

	@inline def defaultsTo[U >: T](default :U) :U =
		if (orNull==null) default else orNull

	/** Returns `this` if it contains a value or `alternative` otherwise. The difference from [[orElse]] is that
	  * the argument is evaluated eagerly, guaranteeing that no closure will be created and should have better performance
	  * if the alternative value was computed beforehand.
	  */
	@inline def ifNull[U >: T <: AnyRef](alternative :Nullable[U]) :Nullable[U] =
		if (orNull==null) alternative else this


	/** Wraps the value in an option.
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise.
	  */
	@inline def toOption = Option(orNull)

	/** Executes the given block for this value if it is not null. */
	@inline def foreach[O](f :T=>O) :Unit = if (orNull!=null) f(orNull)

	/** Tests if this value is not null and satisfies the given predicate. */
	@inline def exists(p :T=>Boolean): Boolean = orNull!=null && p(orNull)

	/** Tests if this value is null or satisfies the given predicate. */
	@inline def forall(p :T=>Boolean): Boolean = orNull==null || p(orNull)

	/** Tests if this value is not null and equal to the given argument. */
	@inline def contains[O>:T](o :O): Boolean = orNull!=null && orNull == o

	/** Returns a new `Nullable` containing this value if it is not null and satisfies the given predicate,
	  * or a null value otherwise.
	  */
	@inline def filter(p :T=>Boolean) :Nullable[T] =
		if (orNull!=null && p(orNull)) this else new Nullable(null.asInstanceOf[T])

	/** SameKeys to `this.`[[Nullable#filter]]`(p)` - a variant for use in for-comprehensions. */
	@inline def withFilter(p :T=>Boolean) :Nullable[T] =
		if (orNull!=null && p(orNull)) this else new Nullable(null.asInstanceOf[T])

	/** Returns a new `Nullable` which is empty if this value is null or contains the result of applying
	  * the given function to it otherwise.
	  */
	@inline def map[O <: AnyRef](p :T=>O) :Nullable[O] =
		if (orNull==null) new Nullable(null.asInstanceOf[O])
		else new Nullable(p(orNull))

	/** Returns the result of applying the given function to this value if it is not null or
	  * `this` if `this.isEmpty`
	  */
	@inline def flatMap[O <: AnyRef](p :T=>Nullable[O]) :Nullable[O] =
		if (orNull==null) new Nullable(null.asInstanceOf[O])
		else p(orNull)

	/** Flattens `Nullable[Nullable[O] ]` to a single `Nullable[O]`. */
	@inline def flatten[O <: AnyRef](implicit isOpt :T<:<Nullable[O]) :Nullable[O] =
		if (orNull==null) new Nullable(null.asInstanceOf[O])
		else orNull

	/** Returns an empty `Nullable` if this value is null or `f` is not defined for it, otherwise
	  * applying the given partial function and wrapping it in a new `Nullable`.
	  */
	@inline def collect[O <: AnyRef](f :PartialFunction[T, O]) :Nullable[O] =
		if (orNull!=null) new Nullable(f.applyOrElse(orNull, Nullable.GiveNull))
		else new Nullable(null.asInstanceOf[O])

	/** An iterator returning this value as the only element if `this.nonEmpty`. */
	@inline def iterator :Iterator[T] =
		if (orNull==null) Iterator.single(orNull) else Iterator.empty

	/** Returns `Nil` if this value is null or or `this.get::Nil` otherwise. */
	@inline def toList :List[T] = if (orNull==null) Nil else orNull::Nil
}



/** Companion object providing factory methods and extractors working with [[Nullable]]s. */
object Nullable {
	@inline final implicit def nullableToOption[T <: AnyRef](opt :Nullable[T]) :Option[T] = Option(opt.orNull)
	@inline final implicit def optionToNullable[T >: Null <: AnyRef](opt :Option[T]) :Nullable[T] = new Nullable(opt.orNull)

	object ExtraImplicits {
		@inline implicit def AnyRefToNullable[T <: AnyRef](value :T) :Nullable[T] = new Nullable(value)
	}

	/** Wraps the given reference in a purely syntactic option-like object erased in the runtime. */
	@inline final def apply[T <: AnyRef](value :T) :Nullable[T] = new Nullable(value)

	/** Extractor for non-null values of [[Nullable]] to be used in pattern matching. */
	object NotNull {
		@inline final def unapply[T <: AnyRef](opt :Nullable[T]) :Option[T] = Option(opt.orNull)
	}

	@inline final def Null[T <: AnyRef] :Nullable[T] = new Nullable[T](null.asInstanceOf[T])

	/** A `null` value representing an empty [[Nullable]] for any true reference type.
	  */
	final val Empty :Nullable[Nothing] = new Nullable(null.asInstanceOf[Nothing])

	@inline private val GiveNull = {_ : AnyRef => null.asInstanceOf[Nothing] }

	@inline final def nullOf[T <: AnyRef] :T = null.asInstanceOf[T]
}
