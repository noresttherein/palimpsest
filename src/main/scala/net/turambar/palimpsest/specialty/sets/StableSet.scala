package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.iterables.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.iterables._
import net.turambar.palimpsest.specialty.{Elements, FitBuilder, RuntimeType, Specialize}
import net.turambar.palimpsest.specialty.seqs.FitSeq
import net.turambar.palimpsest.specialty.sets.ValSet.{StableSetBuilder, ValSetBuilder}

import scala.collection.generic.CanBuildFrom
import scala.collection.{immutable, GenTraversableOnce, SetLike}


//todo: maybe make not specialized and introduce type alias with specialization?
/**
  * @author Marcin Mościcki
  */
trait StableSet[@specialized(Elements) E]
	extends immutable.Set[E] //with IsStable[E] with StableIterableTemplate[E, StableSet[E]]
	   with ValSet[E] with SpecializableSet[E, StableSet]
	   with StableIterable[E] with StableIterableTemplate[E, StableSet[E]]
{
	override def companion :FitCompanion[StableSet] = StableSet

	protected[this] override def debugPrefix = "StableSet"
}







object StableSet extends SpecializableIterableFactory[StableSet] {

	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[StableSet[_], E, StableSet[E]]): CanBuildFrom[StableSet[_], E, StableSet[E]] =
		fit.cbf



	override def empty[@specialized(Elements) E] :StableSet[E] = EmptySet()

	override def one[@specialized(Elements) E](elem :E) :StableSet[E] = new SingletonSet(elem)

	def two[@specialized(Elements) E](elem1 :E, elem2 :E) :StableSet[E] =
		if (elem1 == elem2) new SingletonSet(elem1)
		else new DoubletonSet(elem1, elem2)



	override def newBuilder[@specialized(Elements) E]: FitBuilder[E, StableSet[E]] = Builder()



	//todo
	final private[this] val EmptySet = new Specialize.Individually[StableSet] {
		override final val forBoolean = BooleanSet.Empty
		override final val forByte = ByteSet.Empty
		override final def forShort = ??? //ShortSet.Empty
		override final def forInt = ??? //IntSet.Empty
		override final val forLong = StableLongSet.Empty
		override final def forChar = ??? //CharSet.Empty
		override final def forFloat = ???
		override final val forDouble = StableDoubleSet.Empty
		override final val forUnit = UnitSet.Empty
		override def forRef[E :RuntimeType] = ???
	}


	private type Builder[E] = FitBuilder[E, StableSet[E]]


	/**  Specialized factory for builders returning a new builder for a set implementation best fitting
	  *  a given element type. Delegates via double dispatch to the set factory object associated with
	  *  requested element type.
	  */
	final private[this] val Builder = new Specialize.Individually[Builder] {
		private[this] val stabilize = (set :MutableSet[Any]) => set.stable
		@inline final private[this] def stable[E] = stabilize.asInstanceOf[MutableSet[E] => StableSet[E]]

		override def forBoolean: Builder[Boolean] = BooleanSet.newBuilder
		override def forByte: Builder[Byte] = ByteSet.newBuilder
		override def forShort :Builder[Short] = ??? //ShortSet.newBuilder
		override def forInt: Builder[Int] = ??? //IntSet.newBuilder
		override def forLong :Builder[Long] = new ValSetBuilder(MutableLongSet.empty, stable)
		override def forFloat :Builder[Float] = ??? //FloatSet.newBuilder
		override def forDouble :Builder[Double] = new ValSetBuilder(MutableDoubleSet.empty, stable)
		override def forChar :Builder[Char] = ??? //CharSet.newBuilder
		override def forUnit = new StableSetBuilder[Unit, StableSet[Unit]](UnitSet.Empty)
		override def forRef[E :RuntimeType] = ???
	}




	//todo: all classes below have their dedicated methods overriden by IterableSpecialization!
	private class EmptySet[@specialized(Elements) E]
		extends EmptyIterableFoundation[E, StableSet[E]] with StableSet[E]
	{
		override def contains(elem :E) :Boolean = false

		override def +(elem :E) :StableSet[E] = new SingletonSet(elem)

		override def -(elem :E) :StableSet[E] = this

		override def mutable :MutableSet[E] = MutableSet.empty[E]

		//todo: conflict with damn SetLike
		override def toSeq :FitSeq[E] = FitSeq.empty[E]
	}





	private class SingletonSet[@specialized(Elements) E](final override val head :E)
		extends SingletonFoundation[E, StableSet[E]] with SingletonSpecialization[E, StableSet[E]] with StableSet[E]
	{
		override def contains(elem :E) :Boolean = elem == head

		override def +(elem :E) :StableSet[E] =
			if (elem == head) this
			else new DoubletonSet(head, elem)

		override def -(elem :E) :StableSet[E] =
			if (elem == head) EmptySet()
			else this


		override def mutable :MutableSet[E] = MutableSet.empty[E] += head

		//todo: resolve this override conflict
		override def toSeq :FitSeq[E] = FitSeq.one(head)
	}





	private class DoubletonSet[@specialized(Elements) E](final override val head :E, final override val last :E)
		extends DoubletonFoundation[E, StableSet[E]] with DoubletonSpecialization[E, StableSet[E]] with StableSet[E]
	{
		override def contains(elem :E) :Boolean = elem == head || elem == last

		override def +(elem :E) :StableSet[E] =
			if (elem == head || elem == last) this
			else (builder[E] += head += last += elem).result()

		override def -(elem :E) :StableSet[E] =
			if (elem == head) new SingletonSet(last)
			else if (elem == last) new SingletonSet(head)
			else this

		override def mutable :MutableSet[E] = MutableSet.empty[E] += head += last

		//todo resolve this override
		override def toSeq :FitSeq[E] = FitSeq.two(head, last)
	}


	/** A trait for sets which aren't themselves stable, but operations on which return stable sets nevertheless.
	  * Used primarily by views on mutable sets.
	  */
	trait MakesStableSets[@specialized(Elements) E] extends SpecializableSet[E, StableSet] {
		//todo: make sure there is no infinite recursion with empty set
		override def +(elem :E) :StableSet[E] = (StableSet.newBuilder[E] ++= this + elem).result()
		override def -(elem :E) :StableSet[E] = (StableSet.newBuilder[E] ++= this).result() - elem
		override def ^(elem :E) :StableSet[E] = (StableSet.newBuilder[E] ++= this).result() ^ elem

		override def clone() :StableSet[E] = (StableSet.newBuilder[E] ++= this).result()

		override def stable :StableSet[E] = (StableSet.newBuilder[E] ++= this).result()

		override def companion :FitCompanion[StableSet] = StableSet
	}
}
