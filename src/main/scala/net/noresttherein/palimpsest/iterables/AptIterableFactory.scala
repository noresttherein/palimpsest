package net.noresttherein.palimpsest.iterables

import net.noresttherein.palimpsest.{iterables, ItemTypes, AptBuilder, RuntimeType, Specialize}
import net.noresttherein.palimpsest.AptBuilder.RetardedAptBuilder
import net.noresttherein.palimpsest.RuntimeType.Specialized.{Fun0, Fun1, Fun1Vals}
import net.noresttherein.palimpsest.iterables.AptCompanion.CanFitFrom
import net.noresttherein.palimpsest.Specialize.{SpecializedVals, SpecializeIndividually}

import scala.collection.generic.{CanBuildFrom, GenTraversableFactory}



/** Specialized counterpart of scala `GenTraversableFactory` to be extended by companions of specialized collections.
  * Provides specialized versions of standard factory methods as well as a default specialized
  * [[net.noresttherein.palimpsest.iterables.AptCompanion.CanFitFrom]] implementation. Collection companion objects
  * will generally implement its subclass [[net.noresttherein.palimpsest.iterables.SpecializableIterableFactory]],
  * which additionally declares implicit constants for all inbuilt value types, or
  * [[net.noresttherein.palimpsest.iterables.InterfaceIterableFactory]] for companions of collection interfaces.
  *
  * @tparam S type constructor for specialized collections.
  * @author Marcin Mościcki
  */
abstract class AptIterableFactory[S[@specialized(ItemTypes) X] <: SpecializableIterable[X, S] with AptIterable[X]]
	extends GenTraversableFactory[S] with AptCompanion[S]
{ factory =>


	override def generic[E] :S[E] = empty[E]

	override def of[E :RuntimeType] :S[E] = NewEmpty()

	override def empty[@specialized(ItemTypes) E] :S[E] = newBuilder[E].result()

	private[this] final val NewEmpty :Specialize[S] = new Specialize[S] {
		override def specialized[@specialized E : RuntimeType]: S[E] = empty[E]
	}


	override def one[@specialized(ItemTypes) E](singleton :E) :S[E] = (newBuilder[E] += singleton).result()


	override def apply[@specialized(ItemTypes) E](elems: E*): S[E] =
		if (elems.isEmpty) empty
		else (newBuilder[E] ++= elems).result()



	override def builder[E :RuntimeType] :AptBuilder[E, S[E]] = NewBuilder()

//	override def newBuilder[@specialized E] :AptBuilder[E, S[E]]

	protected[this] type SpecializedBuilder[E] = AptBuilder[E, S[E]]

	private[this] final val NewBuilder :Specialize[SpecializedBuilder] = new Specialize[SpecializedBuilder] {
		override def specialized[@specialized E : RuntimeType]: SpecializedBuilder[E] = newBuilder[E] //specializedBuilder[E]
	}





	override def fill[@specialized(ItemTypes) E](n: Int)(elem: => E): S[E] = {
		var i=0
		val builder = newBuilder[E]
		while (i<n) { builder += elem; i +=1 }
		builder.result()
	}


	override def tabulate[@specialized(Fun1Vals) E](n: Int)(f: Int => E): S[E] = {
		var i=0
		val builder = newBuilder[E]
		while (i<n) { builder += f(i); i += 1 }
		builder.result()
	}


	override def iterate[@specialized(Fun1Vals) E](start: E, len: Int)(f: E => E): S[E] =
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






	class GenericAptBuilder[E] extends RetardedAptBuilder[E, S[E]] {
		override def typeHint[L <: E](implicit tpe :RuntimeType[L]) :AptBuilder[E, S[E]] =
			builder(tpe.asInstanceOf[RuntimeType[E]])

		override protected def resultBuilder(implicit runtimeType :RuntimeType[E]) :AptBuilder[E, S[E]] =
			builder
	}



	/** A `CanBuildFrom` factory which takes use of its specialization for `E` to call the specialized
	  * builder provider methods (either on `From` - `S[_]` - or the companion object) in order to
	  * build a specialized instance of `To =:= S[E]`.
      * @tparam E specialized element type of target collection.
	  */
	class CanBuildSpecialized[@specialized(ItemTypes) E](implicit override final val specialization :RuntimeType[E])
		extends GenericCanBuildFrom[E] with CanFitFrom[S[_], E, S[E]]
	{ outer =>

		override def apply(from: S[_]): AptBuilder[E, S[E]] = from.genericBuilder[E]

		override def apply(): AptBuilder[E, S[E]] = newBuilder[E]


		override private[palimpsest] def companion: Any = factory

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[CanBuildSpecialized[_]]

		override def toString = s"$factory.CBF[$runtimeType]"
	}



	override val ReusableCBF :CanBuildSpecialized[Nothing] = new CanBuildSpecialized[Nothing]()(RuntimeType.erased[Nothing])



	type CFF[E] = CanFitFrom[S[_], E, S[E]]


	/** Fallback implicit `CanFitFrom` which will use any available information about type `E` to return the appropriate
	  * specialized version.
	  */
	implicit def CanBuildSpecialized[E :RuntimeType] :CFF[E] = CBF()




	private[this] final val CBF = new SpecializedVals[CFF] {
		override def forRef[E :RuntimeType] = canFitFromRef[E]

		override def specialized[@specialized E :RuntimeType] = new CanBuildSpecialized[E]
	}

	/** Default `CanFitFrom` used for reference types and erased types. */
	protected def canFitFromRef[E :RuntimeType] :CFF[E] = ReusableCBF.asInstanceOf[CFF[E]]



	override val toString :String = {
		val className = getClass.getName
		val start = className.lastIndexOf('.')
		val end = className.indexOf('$', start+1)
		if (end<0) className.substring(start+1)
		else className.substring(start+1, end)
	}

}





/** Actual base class extended by companions of collection implementation classes. Defines implicit `CanFitFrom` values
  * for all primitives which should take precedence over the generic implicit inherited from `AptIterableFactory`.
  * @tparam S type constructor for specialized collections.
  */
abstract class SpecializableIterableFactory[S[@specialized(ItemTypes) X] <: SpecializableIterable[X, S] with AptIterable[X]]
	extends AptIterableFactory[S]
{
	/** This method needs to be implemented in each final companion object independently in order to take precedence
	  * over standard scala `CanBuildFrom` implicits. Each implementation should simply return `fit.cbf`
	  */
	implicit def canBuildFrom[E](implicit fit :CanFitFrom[S[_], E, S[E]]) :CanBuildFrom[S[_], E, S[E]]

	implicit val CanBuildBytes: CFF[Byte] = new CanBuildSpecialized[Byte]
	implicit val CanBuildShorts: CFF[Short] = new CanBuildSpecialized[Short]
	implicit val CanBuildInts: CFF[Int] = new CanBuildSpecialized[Int]
	implicit val CanBuildLongs: CFF[Long] = new CanBuildSpecialized[Long]
	implicit val CanBuildFloats: CFF[Float] = new CanBuildSpecialized[Float]
	implicit val CanBuildDoubles: CFF[Double] = new CanBuildSpecialized[Double]
	implicit val CanBuildChars: CFF[Char] = new CanBuildSpecialized[Char]
	implicit val CanBuildBooleans: CFF[Boolean] = new CanBuildSpecialized[Boolean]

}





/** Base class for companion objects of specialized 'interface' traits which delegate all constructor (and builder)
  * methods eventually to the companion of the default implementation type.
  */
abstract class InterfaceIterableFactory[S[@specialized(ItemTypes) X] <: SpecializableIterable[X, S] with AptIterable[X]]
	extends SpecializableIterableFactory[S]
{

	/** The type constructor for the generic collection class used as actual implementation for `S[X]`. */
	protected[this] type RealType[@specialized(ItemTypes) X] <: S[X] with SpecializableIterable[X, RealType]

	/** The generic companion object to the implementation collection. This method is used to initialize a field
	  * of this instance, ergo implementations must remain a method (and not a `val`) and must return a constant
	  * which is not a member field (that is, which was initialized before the constructor of this class or its subclass).
	  */
	protected[this] def default :AptIterableFactory[RealType]

	private[this] final val Impl = default


	@inline final override def generic[E] :S[E] = Impl.generic[E]

	@inline final override def of[E :RuntimeType] :S[E] = Impl.of[E]
	
	@inline final override def empty[@specialized(ItemTypes) E]: S[E] = Impl.empty[E]

	@inline final override def newBuilder[@specialized(ItemTypes) E]: AptBuilder[E, S[E]] = Impl.newBuilder[E]

	@inline final override def builder[E: RuntimeType]: AptBuilder[E, S[E]] = Impl.builder[E]

	override def one[@specialized(ItemTypes) E](elem :E) :S[E] = Impl.one(elem)

	@inline final override def apply[@specialized(ItemTypes) E](elems :E*) :S[E] = Impl(elems : _*)

	@inline final override def fill[@specialized(ItemTypes) E](n: Int)(elem: => E): S[E] = Impl.fill(n)(elem)

	@inline final override def tabulate[@specialized(Fun1Vals) E](n: Int)(f: Int => E): S[E] = Impl.tabulate(n)(f)

	@inline final override def iterate[@specialized(Fun1Vals) E](start: E, len: Int)(f: E => E): S[E] = Impl.iterate(start, len)(f)

}




