package net.turambar.palimpsest.specialty

import scala.annotation.tailrec
import scala.collection.generic.{CanBuildFrom, GenTraversableFactory}
import net.turambar.palimpsest.specialty.FitBuilder.RetardedFitBuilder
import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.Specialize.SpecializeIndividually



/** Specialized counterpart of scala `GenTraversableFactory` to be extended by companions of specialized collections.
  * Provides specialized versions of standard factory methods as well as a default specialized [[net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom]] implementation.
  *
  * @tparam S type constructor for specialized collections.
  * @author Marcin Mościcki
  */
trait FitIterableFactory[S[@specialized(Elements) X] <: SpecializableIterable[X, S] with FitIterable[X]]
	extends GenTraversableFactory[S] with FitCompanion[S]
{ factory =>
	
	override def fill[@specialized(Elements) E](n: Int)(elem: => E): S[E] = {
		var i=0
		val builder = newBuilder[E]
		while (i<n) { builder += elem; i +=1 }
		builder.result()
	}


	override def tabulate[@specialized(Elements) E](n: Int)(f: Int => E): S[E] = {
		var i=0
		val builder = newBuilder[E]
		while (i<n) { builder += f(i); i += 1 }
		builder.result()
	}


	override def iterate[@specialized(Elements) E](start: E, len: Int)(f: (E) => E): S[E] =
		if (len<=0) empty[E]
		else {
			var i=1
			val builder = newBuilder[E]
			builder sizeHint len
			builder += start
			var last = start
			while(i<len) {
				i += 1
				last = { val next = f(last); builder += next; next }
			}
			builder.result()
		}


/*	class ErasedCanBuildFrom[E] extends GenericCanBuildFrom[E] with FitCanBuildFrom[S[_], E, S[E]] {

		final override def apply(from: S[_]): FitBuilder[E, S[E]] = from.genericBuilder[E]

		final override def apply(): FitBuilder[E, S[E]] = newBuilder[E]


		override private[specialty] def companion: Any = factory

		override def specialization: Specialized[_] = Specialized.OfAnyRef

		override def canEqual(that :Any) = that.isInstanceOf[ErasedCanBuildFrom[_]]

		override def equals(that :Any) = that match {
			case cbf :ErasedCanBuildFrom[_] => (cbf eq this) ||
			cbf.canEqual(this) && cbf.companion == companion
		}

		override def hashCode = companion.hashCode

		override def toString = companion+".ReusableCBF"
	}*/




	/** A `CanBuildFrom` factory which takes use of its specialization for `E` to call the specialized
	  * builder provider methods (either on `From` - `S[_]` - or the companion object) in order to
	  * build a specialized instance of `To =:= S[E]`.
      * @tparam E specialized element type of target collection.
	  */
	class CanBuildSpecialized[@specialized(Elements) E](implicit override val specialization :RuntimeType[E])
		extends GenericCanBuildFrom[E] with CanFitFrom[S[_], E, S[E]]
	{ outer =>

		private[this] final val mapper = new Specialize.With2[
			({ type L[X] = FitBuilder[X, S[E]]})#L, ({ type L[X] = X => E})#L, ({ type L[X] = FitBuilder[E, S[E]]})#L
		]{
			override def specialized[@specialized X :RuntimeType](f :X => E, builder :FitBuilder[E, S[E]]) =
				builder.mapInput(f)
		}


		override def apply(from: S[_]): FitBuilder[E, S[E]] = from.genericBuilder[E]

		override def apply(): FitBuilder[E, S[E]] = newBuilder[E]

		override def mapped[O](from :S[_], f :O => E) :FitBuilder[O, S[E]] =
			mapper(f, apply(from))(from.specialization.asInstanceOf[RuntimeType[O]])

		override def mapped[O :RuntimeType](f :O => E) :FitBuilder[O, S[E]] =
			mapper(f, apply())

//		override def specialization: Specialized[E] = Specialized[E]


		override private[specialty] def companion: Any = factory

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[CanBuildSpecialized[_]]

		override def toString = s"$factory.CBF[$specialization]"
	}

	
	class GenericFitBuilder[E] extends RetardedFitBuilder[E, S[E]] {
		override def result(): S[E] = {
			if (hint < 0)
				hint = guessSize()
			val spec = guessSpecialization
			val b = fitBuilder(spec)
			if (hint>=0)
				b.sizeHint(hint)
			
			@tailrec def append(from :List[TraversableOnce[E]]=inOrder) :Unit = from match {
				case head::tail => b ++= head; append(tail)
				case _ => ()
			}
			append()
			b.result()
		}
	}




	override val ReusableCBF :CanBuildSpecialized[Nothing] = new CanBuildSpecialized[Nothing]()(RuntimeType.erased[Nothing]) {
		override def mapped[O](from :S[_], f :O => Nothing)= from.genericBuilder[Nothing].mapInput(f)

		override def mapped[O :RuntimeType](f :O => Nothing) = newBuilder[Nothing].mapInput(f)
	}


	type CBF[E] = CanFitFrom[S[_], E, S[E]]


	implicit def CanBuildSpecialized[E :RuntimeType] :CBF[E] = CBF()

//	private[this] val CBF = new Specialize[CBF] {
//		override def specialized[@specialized E: Specialized]: CBF[E] = canBuildSpecialized[E] //new CanBuildSpecialized[E]
//	}
//	protected def canBuildSpecialized[@specialized E :Specialized] = new CanBuildSpecialized[E]

	private[this] val CBF = new SpecializeIndividually[CBF] {
		override val forByte = new CanBuildSpecialized[Byte]
		override val forShort = new CanBuildSpecialized[Short]
		override val forChar = new CanBuildSpecialized[Char]
		override val forInt = new CanBuildSpecialized[Int]
		override val forLong = new CanBuildSpecialized[Long]
		override val forFloat = new CanBuildSpecialized[Float]
		override val forDouble = new CanBuildSpecialized[Double]
		override val forBoolean = new CanBuildSpecialized[Boolean]
		override val forUnit = new CanBuildSpecialized[Unit]
		override def forRef[E :RuntimeType] = canFitFromRef[E]
	}

	/** Default `CanFitFrom` used for reference types and erased types. */
	protected def canFitFromRef[E :RuntimeType] :CBF[E] = ReusableCBF.asInstanceOf[CBF[E]]



	override val toString :String = {
		val className = getClass.getName
		val start = className.lastIndexOf('.')
		val end = className.indexOf('$', start+1)
		if (end<0) className.substring(start+1)
		else className.substring(start+1, end)
	}
	
}







trait SpecializedIterableFactory[S[@specialized(Elements) X] <: SpecializableIterable[X, S] with FitIterable[X]]
	extends FitIterableFactory[S]
{
	implicit def canBuildFrom[E](implicit fit :CanFitFrom[S[_], E, S[E]]) :CanBuildFrom[S[_], E, S[E]] //=
//		fit.cbf
	
	implicit val CanBuildBytes: CBF[Byte] = new CanBuildSpecialized[Byte]
	implicit val CanBuildShorts: CBF[Short] = new CanBuildSpecialized[Short]
	implicit val CanBuildInts: CBF[Int] = new CanBuildSpecialized[Int]
	implicit val CanBuildLongs: CBF[Long] = new CanBuildSpecialized[Long]
	implicit val CanBuildFloats: CBF[Float] = new CanBuildSpecialized[Float]
	implicit val CanBuildDoubles: CBF[Double] = new CanBuildSpecialized[Double]
	implicit val CanBuildChars: CBF[Char] = new CanBuildSpecialized[Char]
	implicit val CanBuildBooleans: CBF[Boolean] = new CanBuildSpecialized[Boolean]


}


/** Base class for companion objects of specialized 'interface' traits which delegate all constructor (and builder)
  * methods eventually to the companion of the default implementation type.
  */
abstract class InterfaceIterableFactory[S[@specialized(Elements) X] <: SpecializableIterable[X, S] with FitIterable[X]]
	extends SpecializedIterableFactory[S]
{
	
//	override val Empty: S[Nothing] = default.Empty
	
	protected[this] type RealType[@specialized(Elements) X] <: S[X] with SpecializableIterable[X, RealType]
	protected[this] def default :FitIterableFactory[RealType]
	private[this] val impl = default
	
	@inline final override def emptyOf[E :RuntimeType] :S[E] = impl.emptyOf[E]
	
	@inline final override def empty[@specialized(Elements) E]: S[E] = impl.empty[E]

	@inline final override def newBuilder[@specialized(Elements) E]: FitBuilder[E, S[E]] = impl.newBuilder[E]

	@inline final override def fitBuilder[E: RuntimeType]: FitBuilder[E, S[E]] = impl.fitBuilder[E]
	
//	@inline override final def specializedBuilder[@specialized(Elements) E: Specialized]: FitBuilder[E, S[E]] = impl.specializedBuilder[E]


	override def fill[@specialized(Elements) E](n: Int)(elem: => E): S[E] = impl.fill(n)(elem)

	override def tabulate[@specialized(Elements) E](n: Int)(f: Int => E): S[E] = impl.tabulate(n)(f)

	override def iterate[@specialized(Elements) E](start: E, len: Int)(f: E => E): S[E] = impl.iterate(start, len)(f)

}




abstract class ImplementationIterableFactory[S[@specialized(Elements) X] <: SpecializableIterable[X, S] with FitIterable[X]]
	extends SpecializedIterableFactory[S]
{
//	override val Empty :S[Nothing] = empty
	
//	override def newBuilder[E]: FitBuilder[E, S[E]] = ???
//
//	override def specializedBuilder[E: Specialized]: FitBuilder[E, S[E]] = ???
}

