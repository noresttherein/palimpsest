package net.turambar.palimpsest.slang

/** A value class treating nullable reference types as option-like monads erased in runtime. */
private[palimpsest] final class Nullable[+T <: AnyRef](/** Wrapped reference value which may be null. */val orNull :T) extends AnyVal {
	/** Tests if this value is null representing 'no value'. */
	@inline final def isEmpty: Boolean = orNull==null

	/** Tests if this value is not null. */
	@inline final def nonEmpty: Boolean = orNull!=null

	/** Tests if this value is not null. */
	@inline final def isDefined: Boolean = orNull!=null

	/** Forces extraction of the value.
	  * @return contained value, if it not null
	  * @throws NoSuchElementException if this value is `null`.
	  */
	@inline final def get :T =
		if (orNull==null) throw new NoSuchElementException("Nullable.Empty.get")
		else orNull

	/** Returns this value if it is not null or the lazily computed alternative passed as the argument in the opposite case. */
	@inline final def getOrElse[O>:T](alt : =>O) :O =
		if (orNull==null) alt else orNull

	/** Returns this `Nullable` if it is not null or the lazily computed alternative for null values. */
	@inline final def orElse[O>:T](alt : =>Nullable[O]) :Nullable[O] =
		if (orNull==null) alt else this

	/** Wraps the value in an option.
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise.
	  */
	@inline final def toOption = Option(orNull)

	/** Executes the given block for this value if it is not null. */
	@inline final def foreach[O](f :T=>O) :Unit = if (orNull!=null) f(orNull)

	/** Tests if this value is not null and satisfies the given predicate. */
	@inline final def exists(p :T=>Boolean): Boolean = orNull!=null && p(orNull)

	/** Tests if this value is null or satisfies the given predicate. */
	@inline final def forall(p :T=>Boolean): Boolean = orNull==null || p(orNull)

	/** Tests if this value is not null and equal to the given argument. */
	@inline final def contains[O>:T](o :O): Boolean = orNull!=null && orNull == o

	/** Returns a new `Nullable` containing this value if it is not null and satisfies the given predicate,
	  * or a null value otherwise.
	  */
	@inline final def filter(p :T=>Boolean) :Nullable[T] =
		if (orNull!=null && p(orNull)) this else new Nullable(null.asInstanceOf[T])

	/** Equivalent to `this.`[[Nullable#filter]]`(p)` - a variant for use in for-comprehensions. */
	@inline final def withFilter(p :T=>Boolean) :Nullable[T] =
		if (orNull!=null && p(orNull)) this else new Nullable(null.asInstanceOf[T])

	/** Returns a new `Nullable` which is empty if this value is null or contains the result of applying
	  * the given function to it otherwise.
	  */
	@inline final def map[O <: AnyRef](p :T=>O) :Nullable[O] =
		if (orNull==null) new Nullable(null.asInstanceOf[O])
		else new Nullable(p(orNull))

	/** Returns the result of applying the given function to this value if it is not null or
	  * `this` if `this.isEmpty`
	  */
	@inline final def flatMap[O <: AnyRef](p :T=>Nullable[O]) :Nullable[O] =
		if (orNull==null) new Nullable(null.asInstanceOf[O])
		else p(orNull)

	/** Flattens `Nullable[Nullable[O] ]` to a single `Nullable[O]`. */
	@inline final def flatten[O <: AnyRef](implicit isOpt :T<:<Nullable[O]) :Nullable[O] =
		if (orNull==null) new Nullable(null.asInstanceOf[O])
		else orNull

	/** Returns an empty `Nullable` if this value is null or `f` is not defined for it, otherwise
	  * applying the given partial function and wrapping it in a new `Nullable`.
	  */
	@inline final def collect[O <: AnyRef](f :PartialFunction[T, O]) :Nullable[O] =
		if (orNull!=null) new Nullable(f.applyOrElse(orNull, Nullable.GiveNull))
		else new Nullable(null.asInstanceOf[O])

	/** An iterator returning this value as the only element if `this.nonEmpty`. */
	@inline final def iterator :Iterator[T] =
		if (orNull==null) Iterator.single(orNull) else Iterator.empty

	/** Returns `Nil` if this value is null or or `this.get::Nil` otherwise. */
	@inline final def toList :List[T] = if (orNull==null) Nil else orNull::Nil
}

/** Companion object providing factory methods and extractors working with [[Nullable]]s. */
private[palimpsest] object Nullable {
	@inline final implicit def nullableToOption[T <: AnyRef](opt :Nullable[T]) :Option[T] = Option(opt.orNull)
	@inline final implicit def optionToNullable[T >: Null <: AnyRef](opt :Option[T]) :Nullable[T] = new Nullable(opt.orNull)
	@inline final implicit def nullableToList[T <: AnyRef](opt :Nullable[T]) :List[T] =
		if (opt.orNull==null) Nil else opt.orNull::Nil

	/** Wraps the given reference in a purely syntactic option-like object erased in the runtime. */
	@inline final def apply[T <: AnyRef](value :T) :Nullable[T] = new Nullable(value)

	/** Extractor for non-null values of [[Nullable]] to be used in pattern matching. */
	object NotNull {
		@inline final def unapply[T <: AnyRef](opt :Nullable[T]) :Option[T] = Option(opt.orNull)
	}

	/** A `null` value representing an empty [[Nullable]] for any true reference type. */
	final val Empty :Nullable[Nothing] = new Nullable(null.asInstanceOf[Nothing])

	@inline private[Nullable] val GiveNull = {_  : AnyRef => null.asInstanceOf[Nothing] }
}
