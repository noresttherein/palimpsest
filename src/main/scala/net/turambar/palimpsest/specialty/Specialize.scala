package net.turambar.palimpsest.specialty

/** Generic specialized callback, allowing to call a specialized method from non-specialized code based on passed
  * implicit specialization information.
  * This is the parameterless version - [[Specialize.With]] is a similar dispatcher accepting a single parameter
  * of a type related to type argument of the created object.
  *
  * @tparam R a type constructor for generic types, either specialized themselves, or requiring a specialized constructor.
  * @see [[Specialize#apply]]
  * @see [[Specialize.SpecializeFor]]
  * @author Marcin Mościcki
  */
trait Specialize[R[X]] {
	
	/** Double dispatch execution of [[Specialize#specialized]] on `this`.
	  * Retrieves implicit specialization information, whatever is available, and invokes the appropriate
	  * specialized variant of [[Specialize#specialized]].
	  * @param specialization information about runtime specialization requested for this call.
	  * @return result of calling the most appropriately specialized variant of `this.specialized[E]`.
	  */
	@inline final def apply[E]()(implicit specialization :Specialized[E]) :R[E] =
		specialization.call(this)
	
	/** Callback specialized method to be implemented by subclasses.
	  * Invoked as a result of calling `this()`, and - as long as any specialization information was present at that point,
	  * either explicitly (specialized code) or implicitly (for example `ClassTag[E]`), an appropriate specialized variant
	  * of this method will be invoked.
	  * @tparam E original type parameter as defined in runtime by the call to [[Specialize#apply]].
	  */
	def specialized[@specialized E :Specialized] :R[E]
}


object Specialize {
	/** Identity type constructor. */
	type Self[X] = X

	/** Type lambda for a type constructor ignoring its argument and simply yielding always type `X`. */
	type Const[X] = ({ type T[Y]=X })

	type Adapt2[R[X, Y]] = {
		type _1[X] = ({ type __[Y] = R[X, Y]})
		type _2[Y] = ({ type __[X] = R[X, Y]})
	}


	/** Generic callback invoking specialized code from non-specialized one based on passed implicit specialization information.
	  *
	  * @tparam R type constructor for a value needing code specialized for some parameter type `E` to compute.
	  * @tparam P type constructor for parameter given to the callback.
	  */
	trait With[R[X], P[X]] {
		/** Retrieves implicit specialization information available at the calling point and delegates to an appropriate
		  * variant of [[With#specialized]].
		  * @param param parameter given to [[With#specialized]]
		  * @param specialization requested specialization
		  * @tparam E type on which the call is specialized
		  * @return result of calling [[With#specialized]]`(param)`.
		  */
		def apply[E](param :P[E])(implicit specialization :Specialized[E]) :R[E] =
			specialization.call(this)(param)

		/** Specialized callback(s) invoked from [[With#apply]] based on requested specialization type. */
		def specialized[@specialized E :Specialized](param :P[E]) :R[E]
	}

	/** Generic callback invoking a specialized method accepting parameter of type `P` and returning a value of `R[X]`
	  * specialized for type `X` specified at call site. This is simply a syntactic wrapper over [[With]].
	  * @tparam R result type constructor
	  * @tparam P parameter type
	  */
	type WithValue[R[X], P] = With[R, Const[P]#T]

	type For[R[X]] = SpecializeFor[R]


	/** A double-dispatch callback invoking different methods (not just synthetic specialized variant of the same method)
	  * based on specialization information for some type `E` given at call site.
	  * Similarly to [[Specialize]], it allows to call specialized call from non-specialized one, but declares
	  * and delegates to a `forE` method for a type `E` specified at call site.
	  * @tparam R type constructor for the returned value.
	  */
	trait SpecializeFor[R[X]] {

		/** Call the appropriate `forE` method for type `E` and return its result. By default,
		  * all these methods simply forward to [[SpecializeFor#specialized]]. If `E` is not a primitive type
		  * or no specialization information is available/a generic implicit value is provided it instead delegates
		  * directly to [[SpecializeFor#specialized]].
		  * @param specialization implicit specialization information for type `E`
		  * @tparam E type specialized for
		  * @return
		  */
		@inline final def apply[E]()(implicit specialization :Specialized[E]) :R[E] =
			specialization.call(this)

		/** Invoked from `this[E]()` if `E` is specified to be `Byte` by the implicit argument to [[SpecializeFor#apply]].
		  * @return by default, result of [[SpecializeFor#specialized]]`[Byte]`
		  */
		def forByte :R[Byte] = specialized

		/** Invoked from `this[E]()` if `E` is specified to be `Short` by the implicit argument to [[SpecializeFor#apply]].
		  * @return by default, result of [[SpecializeFor#specialized]]`[Short]`
		  */
		def forShort :R[Short] = specialized

		/** Invoked from `this[E]()` if `E` is specified to be `Char` by the implicit argument to [[SpecializeFor#apply]].
		  * @return by default, result of [[SpecializeFor#specialized]]`[Char]`
		  */
		def forChar :R[Char] = specialized

		/** Invoked from `this[E]()` if `E` is specified to be `Int` by the implicit argument to [[SpecializeFor#apply]].
		  * @return by default, result of [[SpecializeFor#specialized]]`[Int]`
		  */
		def forInt :R[Int] = specialized

		/** Invoked from `this[E]()` if `E` is specified to be `Long` by the implicit argument to [[SpecializeFor#apply]].
		  * @return by default, result of [[SpecializeFor#specialized]]`[Long]`
		  */
		def forLong :R[Long] = specialized

		/** Invoked from `this[E]()` if `E` is specified to be `Float` by the implicit argument to [[SpecializeFor#apply]].
		  * @return by default, result of [[SpecializeFor#specialized]]`[Float]`
		  */
		def forFloat :R[Float] = specialized

		/** Invoked from `this[E]()` if `E` is specified to be `Double` by the implicit argument to [[SpecializeFor#apply]].
		  * @return by default, result of [[SpecializeFor#specialized]]`[Double]`
		  */
		def forDouble :R[Double] = specialized

		/** Invoked from `this[E]()` if `E` is specified to be `Boolean` by the implicit argument to [[SpecializeFor#apply]].
		  * @return by default, result of [[SpecializeFor#specialized]]`[Boolean]`
		  */
		def forBoolean :R[Boolean] = specialized

		/** Invoked from `this[E]()` if `E` is specified to be `Unit` by the implicit argument to [[SpecializeFor#apply]].
		  * @return by default, result of [[SpecializeFor#specialized]]`[Unit]`
		  */
		def forUnit :R[Unit] = specialized

		/** Invoked from `this[E]()` if `E` is specified to be `Nothing` by the implicit argument to [[SpecializeFor#apply]].
		  * @return by default, result of [[SpecializeFor#specialized]]`[Nothing]`
		  */
		def forNothing :R[Nothing] = specialized[Nothing]

		/** Invoked from `this[E]()` if `E` is specified to be `Null` by the implicit argument to [[SpecializeFor#apply]].
		  * @return by default, result of [[SpecializeFor#specialized]]`[Null]`
		  */
		def forNull :R[Null] = specialized[Null]

		/** Default callback implementation called when no specialization information is available, the specified type
		  * is not a primitive type or the appropriate `forE` method for the given type `E` was not overridden.
		  * @tparam E type specialized for
		  */
		def specialized[@specialized E :Specialized] :R[E]
		
	}

}


