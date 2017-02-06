package net.turambar.palimpsest.specialty

/** Generic specialized callback, allowing to call specialized call out of non-specialized one.
  * This is a parameterless version - a simililar
 *
  * @tparam R a type constructor for generic types, either specialized themselves, or requiring a specialized constructor.
  * @see [[Specialize#apply]]
  * @see [[Specialize.SpecializeFor]]
  * @author Marcin Mościcki
  */
trait Specialize[R[X]] {
	
	/** Double dispatch execution of [[Specialize#specialized]] on `this`.
	  * Retrieves implicit specialization information, whatever is available, and invokes
	  * @param specialization information about runtime specialization requested for this call.
	  * @return result of calling the most appropriately specialized variant of `this.specialized[E]`.
	  */
	@inline def apply[E]()(implicit specialization :Specialized[E]) :R[E] =
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
	type Self[X] = X
	type Const[X] = ({ type T[Y]=X })

	type Adapt2[R[X, Y]] = {
		type _1[X] = ({ type __[Y] = R[X, Y]})
		type _2[Y] = ({ type __[X] = R[X, Y]})
	}



	trait With[R[X], P[X]] {
		def apply[E](param :P[E])(implicit specialization :Specialized[E]) :R[E] =
			specialization.call(this)(param)

		def specialized[@specialized E :Specialized](param :P[E]) :R[E]
	}

	type WithValue[R[X], P] = With[R, Const[P]#T]

	type For[R[X]] = SpecializeFor[R]
	
	
	
	trait SpecializeFor[R[X]] {
		@inline final def apply[E]()(implicit specialization :Specialized[E]) :R[E] =
			specialization.call(this)
		
		def forByte :R[Byte] = specialized
		def forShort :R[Short] = specialized
		def forChar :R[Char] = specialized
		def forInt :R[Int] = specialized
		def forLong :R[Long] = specialized
		def forFloat :R[Float] = specialized
		def forDouble :R[Double] = specialized
		def forBoolean :R[Boolean] = specialized
		def forUnit :R[Unit] = specialized
		def forNothing :R[Nothing] = specialized[Nothing]
		def forNull :R[Null] = specialized[Null]
		
		def specialized[@specialized E :Specialized] :R[E]
		
		//override def specialized[E: Specialized]: R[E] =
//			throw new UnsupportedOperationException(s"$this: no default or specialized implementation for ${Specialized[E]}")
	}

}


