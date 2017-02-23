package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.FitIterable.{IterableAdapter}
import net.turambar.palimpsest.specialty.iterables.EmptyIterable

import net.turambar.palimpsest.specialty.FitIterator.BaseIterator
import net.turambar.palimpsest.specialty.seqs.FitSeq
import net.turambar.palimpsest.specialty.sets.FitSet.ImmutableSetBuilder
import net.turambar.palimpsest.specialty.sets.MutableSet.AbstractMutableSetAdapter
import net.turambar.palimpsest.specialty.{Elements, FitBuilder, FitCompanion, FitIterable, FitIterator, FitTraversableOnce, ImplementationIterableFactory, IterableSpecialization, SpecializableIterable, Specialize, Specialized, SpecializedIterableFactory}

import scala.annotation.unspecialized
import scala.collection.immutable.LongMap
import scala.collection.generic.{CanBuildFrom, GenericCompanion, GenericSetTemplate, Shrinkable, Subtractable}
import scala.collection.{GenSet, GenTraversableOnce, SetLike, mutable}




trait SpecializableSet[@specialized(Elements) E, +S[@specialized(Elements) X]<:SpecializableSet[X, S] with FitSet[X]]
	extends GenericSetTemplate[E, S] with SetLike[E, S[E]]
			with SpecializableIterable[E, S] with SetSpecialization[E, S[E]]
{
//	override def newBuilder :FitBuilder[E, S[E]] = new ImmutableSetBuilder[E, S[E]](empty)
	override def empty :S[E] = companion.empty
}



trait SetSpecialization[@specialized(Elements) E, +This <: SetSpecialization[E, This] with FitSet[E]]
	extends SetLike[E, This] with IterableSpecialization[E, This] with mutable.Cloneable[This]
{

	override def specialization :Specialized[E] = mySpecialization

	override def clone() :This = repr //(newBuilder ++= this).result()

	def stable :FitSet.Stable[E]
	def mutable :FitSet.Mutable[E]

	override def empty :This //= newBuilder.result()
	override def newBuilder :FitBuilder[E, This] = new ImmutableSetBuilder[E, This](empty)

	override def apply(elem: E) = contains(elem)
	override def contains(elem :E) :Boolean

	override def +(elem1: E, elem2: E, elems: E*) :This = this + elem1 + elem2 ++ elems

	override def -(elem1: E, elem2: E, elems: E*) :This = this - elem1 - elem2 -- elems

	def ++(elems :FitTraversableOnce[E]) :This =
		elems.foldLeft(repr)(_ + _)

	def --(elems :FitTraversableOnce[E]) :This = {
		var res = repr; val it = elems.fitIterator
		while(it.hasNext && res.nonEmpty)
			res = res - it.next()
		res
	}

	override def +(elem :E) :This

	override def -(elem :E) :This

}

/**
  * @author Marcin Mościcki
  */
trait FitSet[@specialized(Elements) E]
	extends collection.Set[E] with FitIterable[E] with SpecializableSet[E, FitSet]
{

	override def companion: FitCompanion[FitSet] = FitSet

	override def toSeq = toFitSeq
	override def toFitSeq :FitSeq[E] = (FitSeq.newBuilder[E] ++= this).result()

	def stable :FitSet.Stable[E] = this
	def mutable :FitSet.Mutable[E] = MutableSet.adapt(this)
//	override def newBuilder :FitBuilder[E, FitSet[E]] = companion.newBuilder

	override def typeStringPrefix = "Set"
	override def stringPrefix = super[FitIterable].stringPrefix //typeStringPrefix + "[" + mySpecialization.classTag + "]"
}



object FitSet extends ImplementationIterableFactory[FitSet] {
	type Sorted[@specialized(Elements) E] = SortedFitSet[E]
	type MakeSorted[@specialized(Elements) E] = SortedFitSet[E]

	type Mutable[@specialized(Elements) E] = MutableSet[E]
	type Stable[@specialized(Elements) E] = FitSet[E] //todo

	object Sorted {
		type Stable[@specialized(Elements) E] = SortedFitSet.Stable[E]
		type Mutable[@specialized(Elements) E] = SortedFitSet.Mutable[E]
	}

	override final val Empty :FitSet[Nothing] = new EmptyIterable[Nothing, FitSet[Nothing]] with FitSet[Nothing] {
		override def contains(elem: Nothing): Boolean = false
		override def +(elem: Nothing): FitSet[Nothing] =
			throw new UnsupportedOperationException(s"FitSet[Nothing] + $elem")
		override def -(elem: Nothing): FitSet[Nothing] = this
		override def toString = "FitSet.Empty"
	}
	
	override def empty[@specialized(Elements) E] :FitSet[E] = EmptySet()
	
	override def apply[@specialized(Elements) E](elems :E*) :FitSet[E] =
		(newBuilder[E] ++= elems).result()


	def newBuilder[@specialized(Elements) E] :FitBuilder[E, FitSet[E]] = SetBuilder()

	override def specializedBuilder[@specialized(Elements) E: Specialized]: FitBuilder[E, FitSet[E]] =
		SetBuilder()

	override def fitBuilder[E: Specialized]: FitBuilder[E, FitSet[E]] = SetBuilder()



	override implicit def canBuildFrom[E](implicit fit: CanFitFrom[FitSet[_], E, FitSet[E]]): CanBuildFrom[FitSet[_], E, FitSet[E]] =
		fit.cbf




	private type SetBuilder[E] = FitBuilder[E, FitSet[E]]
	
	final private val SetBuilder = new Specialize.For[SetBuilder] {
		override def forBoolean: SetBuilder[Boolean] = BooleanSet.newBuilder
		override def forByte: SetBuilder[Byte] = ByteSet.newBuilder
		override def forShort :SetBuilder[Short] = ShortSet.newBuilder
		override def forInt: SetBuilder[Int] = IntSet.newBuilder
		override def forLong :SetBuilder[Long] = LongSet.newBuilder
		override def forFloat :SetBuilder[Float] = FloatSet.newBuilder
		override def forDouble :SetBuilder[Double] = DoubleSet.newBuilder
		override def forChar :SetBuilder[Char] = CharSet.newBuilder

		override def specialized[@specialized E : Specialized]: SetBuilder[E] = ???
	}
	
	final private val EmptySet = new Specialize.For[FitSet] {
		override def forBoolean = BooleanSet.Empty
		override def forByte = ByteSet.Empty
		override def forShort = ShortSet.Empty
		override def forInt = IntSet.Empty
		override def forLong = LongSet.Empty
		override def forChar = CharSet.Empty
		override def forFloat = FloatSet.Empty
		override def forDouble = DoubleSet.Empty
		override def specialized[@specialized E : Specialized]: FitSet[E] = ???
	}


	abstract class SetAdapter[+Source <: FitSet[E] with SetSpecialization[E, Source], E, +This <: FitSet[E] with SetSpecialization[E, This]](final protected[this] var source :Source)
		extends IterableAdapter[Source, E, This] with FitSet[E] with SetSpecialization[E, This]
	{
		override def stable = source.stable
		override def mutable = source.mutable

		override def ++(xs: GenTraversableOnce[E]) :This = fromSource(source ++ xs)

		override def ++(elems: FitTraversableOnce[E]) :This  = fromSource(source ++ elems)

		override def --(xs: GenTraversableOnce[E]) :This  = fromSource(source -- xs)

		override def --(elems: FitTraversableOnce[E]) :This  = fromSource(source -- elems)





		override def intersect(that: GenSet[E]) :This = fromSource(source intersect that)

		override def union(that: GenSet[E]) = fromSource(source union that)

		override def diff(that: GenSet[E]) = fromSource(source diff that)

		override def &(that: GenSet[E]) :This = fromSource(source & that)

		override def |(that: GenSet[E]) :This = fromSource(source | that)

		override def &~(that: GenSet[E]) :This = fromSource(source &~ that)

		override def subsetOf(that: GenSet[E]) :Boolean = source.subsetOf(that)

		override def empty :This = fromSource((source :SetSpecialization[E, Source]).empty)

//		override def newBuilder :FitBuilder[E, This] = source.newBuilder.mapResult(fromSource)

		override def clone() :This = fromSource(source.clone())

		//these could do with specialization
		override def contains(elem: E): Boolean = source.contains(elem)

		override def +(elem: E): This = fromSource(source + elem)

		override def -(elem: E): This = fromSource(source - elem)
	}




	/** A `FitBuilder` building any type of set using it's own (copying) `+`/`++` methods, starting from an empty set.
	  * This is the default builder for `FitSet`s.
	  * @param set initial contents of this builder, usually an empty set of an appropriate type, determining also the final result type
	  * @tparam E element type of the built set.
	  * @tparam S built `To` set type.
	  */
	private[sets] class ImmutableSetBuilder[@specialized(Elements) E, +S<:FitSet[E] with SetSpecialization[E, S]](private[this] var set :S)
		extends FitBuilder[E, S]
	{
		override def +=(elem1: E, elem2: E, elems: E*) = { set = set + (elem1, elem2, elems:_*); this }

		override def ++=(xs: FitTraversableOnce[E]) = { set = set ++ xs; this }

		override def +=(elem: E): this.type = { set += elem; this }

		override def result(): S = set

		override def clear(): Unit = set = (set :SetSpecialization[E, S]).empty

		override def count: Int = set.size
	}

}
