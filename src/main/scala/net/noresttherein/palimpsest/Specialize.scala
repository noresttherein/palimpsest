package net.noresttherein.palimpsest

/** Generic specialized callback, allowing to call a specialized method from non-specialized code based on passed
  * implicit specialization information.
  * This is the parameterless version - [[Specialize.With]] is a similar dispatcher accepting a single parameter
  * of a type related to type argument of the created object.
  *
  * @tparam R a type constructor for generic types, either specialized themselves, or requiring a specialized constructor.
  * @see [[Specialize#apply]]
  * @see [[Specialize.SpecializeIndividually]]
  * @author Marcin Mościcki
  */
trait Specialize[R[X]] {

	/** Double dispatch execution of [[Specialize#specialized]] on `this`.
	  * Retrieves implicit specialization information, whatever is available, and invokes the appropriate
	  * specialized variant of [[Specialize#specialized]].
	  * @param specialization information about runtime specialization requested for this call.
	  * @return result of calling the most appropriately specialized variant of `this.specialized[E]`.
	  */
	def apply[E]()(implicit specialization :RuntimeType[E]) :R[E] =
		specialization.call(this)

	/** Callback specialized method to be implemented by subclasses.
	  * Invoked as a result of calling `this()`, and - as long as any specialization information was present at that point,
	  * either explicitly (specialized code) or implicitly (for example `ClassTag[E]`), an appropriate specialized variant
	  * of this method will be invoked.
	  * @tparam E original type parameter as defined in runtime by the call to [[Specialize#apply]].
	  */
	def specialized[@specialized E :RuntimeType] :R[E]
}





object Specialize {
	/** Identity type constructor. */
	type Self[X] = X

	/** Type lambda for a type constructor ignoring its argument and simply yielding always type `X`. */
	type Const[X] = { type T[Y]=X }

	type Adapt2[R[X, Y]] = {
		type _1[X] = { type __[Y] = R[X, Y] }
		type _2[Y] = { type __[X] = R[X, Y] }
	}


	/** Generic callback invoking specialized single parameter method from non-specialized one based on passed implicit
	  * specialization information.
	  * @tparam R type constructor for the returned value, needing code specialized for some parameter type `E` to compute.
	  * @tparam P type constructor for the parameter given to the callback.
	  */
	trait With[R[X], P[X]] { //todo: reverse the order of parameters
		/** Retrieves implicit specialization information available at the calling point and invokes the appropriate
		  * specialized version of this instance's `specialized` method with the same argument.
		  * @param param the parameter given to [[With#specialized]].
		  * @param specialization the requested specialization.
		  * @tparam E type on which the call is specialized.
		  * @return result of calling [[With#specialized]]`(param)`.
		  */
		@inline final def apply[E](param :P[E])(implicit specialization :RuntimeType[E]) :R[E] =
			specialization.call(this)(param)

		/** Specialized callback invoked from [[With#apply]] based on the requested specialization type. */
		def specialized[@specialized E :RuntimeType](param :P[E]) :R[E]
	}



	/** Generic callback invoking specialized two-parameter method from non-specialized one based on passed implicit
	  * specialization information.
	  * @tparam R  type constructor for the returned value, needing code specialized for some parameter type `E` to compute.
	  * @tparam P1 type constructor for the first parameter given to the callback.
	  * @tparam P2 type constructor for the second parameter given to the callback
	  */
	trait With2[R[X], P1[X], P2[X]] { //todo: reverse the order of parameters
		/** Retrieves implicit specialization information available at the calling point and invokes the appropriate
		  * specialized version of this instance's `specialized` method with the same argument.
		  * @param param1 the first parameter given to [[With2#specialized]].
		  * @param param2 the second parameter given to [[With2#specialized]].
		  * @param specialization requested specialization
		  * @tparam E type on which the call is specialized
		  * @return result of calling [[With#specialized]]`(param)`.
		  */
		@inline final def apply[E](param1 :P1[E], param2 :P2[E])(implicit specialization :RuntimeType[E]) :R[E] =
			specialization.call(this)(param1, param2)

		/** Specialized callback invoked from this instances `apply` method based on the requested specialization type. */
		def specialized[@specialized E :RuntimeType](param1 :P1[E], param2 :P2[E]) :R[E]
	}



	/** Generic callback invoking a specialized method accepting parameter of type `P` and returning a value of `R[X]`
	  * specialized for type `X` specified at call site. This is simply a syntactic wrapper over [[With]].
	  * @tparam R result type constructor
	  * @tparam P parameter type
	  */
	type WithValue[R[X], P] = With[R, Const[P]#T]

	type Individually[R[X]] = SpecializeIndividually[R]



	/** A double-dispatch callback invoking different methods (not just synthetic specialized variant of the same method)
	  * based on specialization information for some type `E` given at call site.
	  * Similarly to [[Specialize]], it allows to call a specialized method from non-specialized one, but declares
	  * and delegates to a `forE` method for a type `E` specified at call site.
	  * If you wish to specialize only for a few types, consider extending [[net.noresttherein.palimpsest.Specialize.SpecializeSome]]
	  * instead, which serves as the default abstract base class, delegating all individual calls back to the single
	  * default abstract method.
	  * @tparam R type constructor for the returned value.
	  * @see [[net.noresttherein.palimpsest.Specialize.SpecializeSome]]
	  */
	trait SpecializeIndividually[R[X]] extends Specialize[R] {

		/** Call the appropriate `forE` method for type `E` and return its result. By default,
		  * all these methods simply forward to [[SpecializeIndividually#specialized]]. If `E` is not a primitive type
		  * or no specialization information is available/a generic implicit value is provided it instead delegates
		  * directly to [[SpecializeIndividually#specialized]].
		  * @param specialization implicit specialization information for type `E`
		  * @tparam E type specialized for
		  * @return
		  */
		@inline final override def apply[E]()(implicit specialization :RuntimeType[E]) :R[E] =
			specialization.call(this)


		/** Invoked from `this[E]()` if `E` is specified to be `Byte` by the implicit argument to [[SpecializeIndividually#apply]]. */
		def forByte :R[Byte]

		/** Invoked from `this[E]()` if `E` is specified to be `Short` by the implicit argument to [[SpecializeIndividually#apply]]. */
		def forShort :R[Short]

		/** Invoked from `this[E]()` if `E` is specified to be `Char` by the implicit argument to [[SpecializeIndividually#apply]]. */
		def forChar :R[Char]

		/** Invoked from `this[E]()` if `E` is specified to be `Int` by the implicit argument to [[SpecializeIndividually#apply]]. */
		def forInt :R[Int]

		/** Invoked from `this[E]()` if `E` is specified to be `Long` by the implicit argument to [[SpecializeIndividually#apply]]. */
		def forLong :R[Long]

		/** Invoked from `this[E]()` if `E` is specified to be `Float` by the implicit argument to [[SpecializeIndividually#apply]]. */
		def forFloat :R[Float]

		/** Invoked from `this[E]()` if `E` is specified to be `Double` by the implicit argument to [[SpecializeIndividually#apply]]. */
		def forDouble :R[Double]

		/** Invoked from `this[E]()` if `E` is specified to be `Boolean` by the implicit argument to [[SpecializeIndividually#apply]]. */
		def forBoolean :R[Boolean]

		/** Invoked from `this[E]()` if `E` is specified to be `Unit` by the implicit argument to [[SpecializeIndividually#apply]]. */
		def forUnit :R[Unit]

		/** Invoked from `this[E]()` if `E` is specified to be `Nothing` by the implicit argument to [[SpecializeIndividually#apply]]. */
		def forNothing :R[Nothing]

		/** Invoked from `this[E]()` if `E` is either a reference type or is erased and boxed at the point of calling.
		  * Implicit argument gives all available information about type `E`.
		  */
		def forRef[E :RuntimeType] :R[E]

		/** This method is no longer invoked as a specialized callback; instead, the appropriate method for the
		  * specialized type is called directly.
		  * @throws UnsupportedOperationException unless overriden.
		  */
		override def specialized[@specialized E :RuntimeType] :R[E] = throw new UnsupportedOperationException("("+this+" :SpecializeSome).specialized")
	}



	/** A convenience base class of [[SpecializeIndividually]] which delegates all methods to the single
	  * [[net.noresttherein.palimpsest.Specialize.SpecializeSome#specialized]], allowing subclasses to provide
	  * a distinct implementation only for a selected few types and default to a common method for all others.
	  * In order to avoid the generation of unwanted variants of the `specialized` method in subclasses, this class
	  * implements `specialized` by forwarding the call to the non-specialized method
	  * [[net.noresttherein.palimpsest.Specialize.SpecializeSome#generic]].
	  */
	abstract class SpecializeSome[R[X]] extends SpecializeIndividually[R] {

		override def forByte :R[Byte] = specialized

		override def forShort :R[Short] = specialized

		override def forChar :R[Char] = specialized

		override def forInt :R[Int] = specialized

		override def forLong :R[Long] = specialized

		override def forFloat :R[Float] = specialized

		override def forDouble :R[Double] = specialized

		override def forBoolean :R[Boolean] = specialized

		override def forUnit :R[Unit] = specialized

		override def forNothing :R[Nothing] = specialized[Nothing]

		override def forRef[E :RuntimeType] :R[E] = generic[E]

		/** Default target for all `for`''T'' methods. Directly invokes non-specialized method `generic`. */
		override def specialized[@specialized E :RuntimeType] :R[E] = generic

		/** The final defender method used when no specialization is required for type `E`. Invoked directly from `specialized`.
		  */
		protected[this] def generic[E :RuntimeType] :R[E]
	}


	/** This is class is similar to `SpecializeSome` in behavior, but overrides all type-specific methods with constants
	  * initialized by a call to `specialized`.
	  */
	abstract class SpecializedVals[R[X]] extends SpecializeIndividually[R] {
		override final val forByte = specialized[Byte]
		override final val forShort = specialized[Short]
		override final val forInt = specialized[Int]
		override final val forLong = specialized[Long]
		override final val forFloat = specialized[Float]
		override final val forDouble = specialized[Double]
		override final val forChar = specialized[Char]
		override final val forBoolean = specialized[Boolean]
		override final val forUnit = specialized[Unit]
		override final val forNothing = specialized[Nothing]
		override def forRef[E :RuntimeType] :R[E] = specialized[E]
	}
	
}





