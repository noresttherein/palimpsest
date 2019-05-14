package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.seqs.{FitBuffer, FitSeq}
import net.turambar.palimpsest.specialty._
import net.turambar.palimpsest.specialty.iterables.{FitIterable, FitIterableFactory, InterfaceIterableFactory, IterableAdapter, SpecializableIterable}

import scala.annotation.unspecialized
import scala.collection.immutable.LongMap
import scala.collection.generic.{CanBuildFrom, GenericCompanion, GenericSetTemplate, Shrinkable, Subtractable}
import scala.collection.{mutable, GenIterable, GenSet, GenTraversableOnce, SetLike}


/** A generic type constructor of a higher kind shared by all specialized set implementations in this package.
  * @tparam E element type
  * @tparam S type constructor accepting element type and giving a specialized set type specific to concrete implementing class.
  */
//todo: is this needed at all?
trait SpecializableSet[@specialized(Elements) E, +S[@specialized(Elements) X]<:SpecializableSet[X, S] with ValSet[X]]
	extends GenericSetTemplate[E, S] with SetLike[E, S[E]]
	   with SpecializableIterable[E, S] with SetSpecialization[E, S[E]]
{
//	override def newBuilder :FitBuilder[E, S[E]] = new StableSetBuilder[E, S[E]](empty)
	override def empty :S[E] = companion.empty
}




/**
  * @author Marcin Mościcki
  */
trait ValSet[@specialized(Elements) E]
	extends collection.Set[E] with FitIterable[E] with SpecializableSet[E, ValSet]
{

	override def companion: FitCompanion[ValSet] = ValSet

//	override def toSeq = toFitSeq
//	override def toFitSeq :FitSeq[E] = (FitSeq.newBuilder[E] ++= this).result()

//	def stable :FitSet.Stable[E] = this
//	def mutable :FitSet.Mutable[E] = MutableSet.from(this)
//	override def newBuilder :FitBuilder[E, FitSet[E]] = companion.newBuilder

	override def typeStringPrefix = "Set"
	override def stringPrefix :String = super[FitIterable].stringPrefix //typeStringPrefix + "[" + specialization.classTag + "]"
}



object ValSet extends InterfaceIterableFactory[ValSet] {

	override protected[this] type RealType[@specialized(Elements) E] = StableSet[E]
	override protected[this] final def default :FitIterableFactory[StableSet] = StableSet


	type Sorted[@specialized(Elements) E] = OrderedSet[E]
	type MakeSorted[@specialized(Elements) E] = OrderedSet[E]

	type Mutable[@specialized(Elements) E] = MutableSet[E]
	type Stable[@specialized(Elements) E] = StableSet[E]

	/** Factory and companion for generic, specialized, mutable sets. */
	final val Mutable = MutableSet

	/** Factory and companion for generic, specialized and immutable sets. */
	final val Stable = StableSet

	/** Specialized ordered set types (using ordering default for the given element type). */
	object Sorted {
		/** Immutable specialized sets sorted by natural ordering for the given element type. */
		type Stable[@specialized(Elements) E] = OrderedSet.Stable[E]

		/** Mutable specialized sets sorted by natural ordering for the given element type. */
		type Mutable[@specialized(Elements) E] = OrderedSet.Mutable[E]
	}




	override implicit def canBuildFrom[E](implicit fit: CanFitFrom[ValSet[_], E, ValSet[E]]): CanBuildFrom[ValSet[_], E, ValSet[E]] =
		fit.cbf













	abstract class SetAdapter[+Source <: ValSet[E] with SetSpecialization[E, Source], E, +This <: ValSet[E] with SetSpecialization[E, This]]
			(final protected[this] var source :Source)
		extends IterableAdapter[Source, E, This] with SetTemplate[E, This] //with ValSet[E] with SetSpecialization[E, This]
	{
		override def stable :Stable[E] = source.stable
		override def mutable :Mutable[E] = source.mutable

		override def ++(xs: GenTraversableOnce[E]) :This = fromSource(source ++ xs)

		override def ++(elems: FitTraversableOnce[E]) :This  = fromSource(source ++ elems)

		override def --(xs: GenTraversableOnce[E]) :This  = fromSource(source -- xs)

		override def --(elems: FitTraversableOnce[E]) :This  = fromSource(source -- elems)





		override def intersect(that: GenSet[E]) :This = fromSource(source intersect that)

		override def union(that: GenSet[E]) :This = fromSource(source union that)

		override def diff(that: GenSet[E]) :This = fromSource(source diff that)

		override def &(that: GenSet[E]) :This = fromSource(source & that)

		override def |(that: GenSet[E]) :This = fromSource(source | that)

		override def &~(that: GenSet[E]) :This = fromSource(source &~ that)

		override def ^(that :GenSet[E]) :This = fromSource(source ^ that)

		override def subsetOf(that: GenSet[E]) :Boolean = source.subsetOf(that)

		override def empty :This = fromSource((source :SetSpecialization[E, Source]).empty)

		override def newBuilder :FitBuilder[E, This] = source.newBuilder.mapResult(fromSource)

		override def toBuffer[U >: E] :FitBuffer[U] = source.toBuffer

		override def toSeq :FitSeq[E] = source.toSeq

		override def clone() :This = fromSource(source.clone())

		//these could do with specialization
		override def contains(elem: E): Boolean = source.contains(elem)

		override def +(elem: E): This = fromSource(source + elem)

		override def -(elem: E): This = fromSource(source - elem)
	}


	/** A builder of set `S` utilising internally a mutable set `M`. This may be faster than the default
	  * [[net.turambar.palimpsest.specialty.sets.ValSet.StableSetBuilder]] if the passed map function `build`
	  * used to obtain the result is able to reuse internal contents of the mutable set.
	  * @param set initial empty set to which all methods delegate.
	  * @param build a function mapping the mutable set to the final result used on the call to `result()`.
	  */
	class ValSetBuilder[@specialized(Elements) E, M <: MutableSet[E], +S](set :M, build :M => S)
		extends FitBuilder[E, S]
	{
		override def +=(elem :E) :this.type = {
			set += elem; this
		}

		override def ++=(elems :FitTraversableOnce[E]) :this.type = {
			set ++= elems; this
		}

		override def clear() :Unit = set.clear()

		override def result() :S = build(set)

		override def mapResult[O](f :S => O) :FitBuilder[E, O] = new ValSetBuilder(set, build andThen f)

	}





	/** A `FitBuilder` building any type of set using it's own (copying) `+`/`++` methods, starting from an empty set.
	  * This is the default builder for `ValSet`s.
	  * @param set initial contents of this builder, usually an empty set of an appropriate type, determining also the final result type
	  * @tparam E element type of the built set.
	  * @tparam S built `To` set type.
	  */
	private[sets] class StableSetBuilder[@specialized(Elements) E, +S<:ValSet[E] with SetSpecialization[E, S]]
			(private[this] var set :S)
		extends FitBuilder[E, S]
	{
		override def +=(elem1: E, elem2: E, elems: E*) :this.type = { set = set + (elem1, elem2, elems:_*); this }

		override def ++=(xs: FitTraversableOnce[E]) :this.type = { set = set ++ xs; this }

		override def +=(elem: E): this.type = { set += elem; this }

		override def result(): S = set

		override def clear(): Unit = set = (set :SetSpecialization[E, S]).empty


		override def origin :AnyRef = ValSet
	}





//	@inline final private[palimpsest] def friendCopy[E](set :ValSet[E], xs :Array[E], start :Int, total :Int) :Int =
//		SetSpecialization.friendCopy(set, xs, start, total)


}
