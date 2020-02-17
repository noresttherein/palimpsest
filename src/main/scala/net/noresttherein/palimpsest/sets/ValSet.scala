package net.noresttherein.palimpsest.sets

import net.noresttherein.palimpsest.iterables.AptCompanion.CanFitFrom
import net.noresttherein.palimpsest.seqs.{AptBuffer, AptSeq}
import net.noresttherein.palimpsest._
import net.noresttherein.palimpsest.iterables.{CloneableIterable, AptCompanion, AptIterable, AptIterableFactory, InterfaceIterableFactory, IterableAdapter, SpecializableIterable}

import scala.annotation.unspecialized
import scala.collection.immutable.LongMap
import scala.collection.generic.{CanBuildFrom, GenericCompanion, GenericSetTemplate, Shrinkable, Subtractable}
import scala.collection.{mutable, GenIterable, GenSet, GenTraversableOnce, SetLike}


/** A generic type constructor of a higher kind shared by all specialized set implementations in this package.
  * @tparam E element type
  * @tparam S type constructor accepting element type and giving a specialized set type specific to concrete implementing class.
  */
//todo: is this needed at all?
trait SpecializableSet[@specialized(ItemTypes) E, +S[@specialized(ItemTypes) X]<:SpecializableSet[X, S] with ValSet[X]]
	extends GenericSetTemplate[E, S] with SetLike[E, S[E]]
	   with SpecializableIterable[E, S] with SetSpecialization[E, S[E]]
{
//	override def newBuilder :AptBuilder[E, S[E]] = new StableSetBuilder[E, S[E]](empty)
	@unspecialized
	override def empty :S[E] = companion.of(specialization)
}




/**
  * @author Marcin Mościcki
  */
trait ValSet[@specialized(ItemTypes) E]
	extends collection.Set[E] with AptIterable[E] with SpecializableSet[E, ValSet]
{

	override def companion: AptCompanion[ValSet] = ValSet


	override def mutable :MutableSet[E] = MutableSet.empty[E] ++= this
//	override def toSeq = toFitSeq
//	override def toFitSeq :AptSeq[E] = (AptSeq.newBuilder[E] ++= this).result()

//	def stable :FitSet.Stable[E] = this
//	def mutable :FitSet.Mutable[E] = MutableSet.from(this)
//	override def newBuilder :AptBuilder[E, FitSet[E]] = companion.newBuilder

	protected[this] override def typeStringPrefix = "Set"

	override def stringPrefix :String = super[AptIterable].stringPrefix //typeStringPrefix + "[" + specialization.classTag + "]"
}



object ValSet extends InterfaceIterableFactory[ValSet] {

	override protected[this] type RealType[@specialized(ItemTypes) E] = StableSet[E]
	override protected[this] final def default :AptIterableFactory[StableSet] = StableSet


//	type Sorted[@specialized(Elements) E] = OrderedSet[E]
//	type MakeSorted[@specialized(Elements) E] = OrderedSet[E]
//
//	type Mutable[@specialized(Elements) E] = MutableSet[E]
//	type Stable[@specialized(Elements) E] = StableSet[E]
//
//	/** Factory and companion for generic, specialized, mutable sets. */
//	final val Mutable = MutableSet
//
//	/** Factory and companion for generic, specialized and immutable sets. */
//	final val Stable = StableSet
//
//	/** Specialized ordered set types (using ordering default for the given element type). */
//	object Sorted {
//		/** Immutable specialized sets sorted by natural ordering for the given element type. */
//		type Stable[@specialized(Elements) E] = StableOrderedSet[E]
//
//		/** Mutable specialized sets sorted by natural ordering for the given element type. */
//		type Mutable[@specialized(Elements) E] = MutableOrderedSet[E]
//	}




	override implicit def canBuildFrom[E](implicit fit: CanFitFrom[ValSet[_], E, ValSet[E]]): CanBuildFrom[ValSet[_], E, ValSet[E]] =
		fit.cbf













	abstract class SetAdapter[+Source <: ValSet[E] with SetSpecialization[E, Source], E, +This <: ValSet[E] with SetSpecialization[E, This]]
			(final protected[this] var source :Source)
		extends IterableAdapter[Source, E, This] with SetTemplate[E, This] //with ValSet[E] with SetSpecialization[E, This]
	{
		override def stable :StableSet[E] = source.stable
		override def mutable :MutableSet[E] = source.mutable

		override def ++(xs: GenTraversableOnce[E]) :This = fromSource(source ++ xs)

		override def ++(elems: Vals[E]) :This  = fromSource(source ++ elems)

		override def --(xs: GenTraversableOnce[E]) :This  = fromSource(source -- xs)

		override def --(elems: Vals[E]) :This  = fromSource(source -- elems)





		override def intersect(that: GenSet[E]) :This = fromSource(source intersect that)

		override def union(that: GenSet[E]) :This = fromSource(source union that)

		override def diff(that: GenSet[E]) :This = fromSource(source diff that)

		override def &(that: GenSet[E]) :This = fromSource(source & that)

		override def |(that: GenSet[E]) :This = fromSource(source | that)

		override def &~(that: GenSet[E]) :This = fromSource(source &~ that)

		override def ^(that :GenSet[E]) :This = fromSource(source ^ that)

		override def subsetOf(that: GenSet[E]) :Boolean = source.subsetOf(that)

		override def empty :This = fromSource((source :SetSpecialization[E, Source]).empty)

		override def newBuilder :AptBuilder[E, This] = source.newBuilder.mapResult(fromSource)

		override def toBuffer[U >: E] :AptBuffer[U] = source.toBuffer

		override def toSeq :AptSeq[E] = source.toSeq

		override def clone() :This = fromSource(source.clone())

		//these could do with specialization
		override def contains(elem: E): Boolean = source.contains(elem)

		override def +(elem: E): This = fromSource(source + elem)

		override def -(elem: E): This = fromSource(source - elem)
	}


	/** A builder of set `S` utilising internally a mutable set `M`. This may be faster than the default
	  * [[net.noresttherein.palimpsest.sets.ValSet.StableSetBuilder]] if the passed map function `build`
	  * used to obtain the result is able to reuse internal contents of the mutable set.
	  * @param set initial empty set to which all methods delegate.
	  * @param build a function mapping the mutable set to the final result used on the call to `result()`.
	  */
	class ValSetBuilder[@specialized(ItemTypes) E, M <: MutableSet[E], +S](set :M, build :M => S)
		extends AptBuilder[E, S]
	{
		override def +=(elem :E) :this.type = {
			set += elem; this
		}

		override def ++=(elems :Vals[E]) :this.type = {
			set ++= elems; this
		}

		override def clear() :Unit = set.clear()

		override def result() :S = build(set)

		override def mapResult[O](f :S => O) :AptBuilder[E, O] = new ValSetBuilder(set, build andThen f)

	}





	/** A `AptBuilder` building any type of set using it's own (copying) `+`/`++` methods, starting from an empty set.
	  * This is the default builder for `ValSet`s.
	  * @param set initial contents of this builder, usually an empty set of an appropriate type, determining also the final result type
	  * @tparam E element type of the built set.
	  * @tparam S built `To` set type.
	  */
	private[sets] class StableSetBuilder[@specialized(ItemTypes) E, +S<:ValSet[E] with SetSpecialization[E, S]]
			(private[this] var set :S)
		extends AptBuilder[E, S]
	{
		override def +=(elem1: E, elem2: E, elems: E*) :this.type = { set = set + (elem1, elem2, elems:_*); this }

		override def ++=(xs: Vals[E]) :this.type = { set = set ++ xs; this }

		override def +=(elem: E): this.type = { set += elem; this }

		override def result(): S = set

		override def clear(): Unit = set = (set :SetSpecialization[E, S]).empty


		override def origin :AnyRef = ValSet
	}






	/** A trait providing implementation
	  */
	trait ConvertingSet[@specialized(ItemTypes) E, +S <: SetSpecialization[E, S] with ValSet[E]]
		extends SetSpecialization[E, ValSet[E]]
	{
		override def empty :S

		override def +(elem :E) :S = empty ++ this + elem //(build ++= this + elem).result()

		override def -(elem :E) :S = empty ++ this - elem //((build ++= this).result() :SetSpecialization[E, S]) - elem

		override def ^(elem :E) :S = empty ++ this ^ elem //((build ++= this).result() :SetSpecialization[E, S]) ^ elem

		override def clone() :S = empty ++ this //(build ++= this).result()

//		@unspecialized
//		protected[this] def build :AptBuilder[E, S]
	}

}
