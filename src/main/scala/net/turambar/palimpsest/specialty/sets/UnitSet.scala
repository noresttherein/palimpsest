package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.iterables.{CloneableIterable, EmptyIterableFoundation, EmptyIterableTemplate, SingletonFoundation, SingletonSpecialization, StableIterableTemplate}
import net.turambar.palimpsest.specialty.sets.UnitSet.MutableUnitSet
import net.turambar.palimpsest.specialty.{?, Blank, Sure}
import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize
import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.seqs.{FitSeq, StableSeq}


/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
sealed trait UnitSet extends StableSet[Unit] with SetSpecialization[Unit, UnitSet]
	with CloneableIterable[Unit, UnitSet] with StableIterableTemplate[Unit, UnitSet] with OfKnownSize
{
	override def empty :UnitSet = UnitSet.Empty
	override def mutable :MutableUnitSet = new MutableUnitSet(nonEmpty)
}



object UnitSet {

	object Empty extends EmptyIterableFoundation[Unit, UnitSet] with UnitSet {
		override def contains(elem :Unit) :Boolean = false
		override def +(elem :Unit) :UnitSet = Full
		override def -(elem :Unit) :UnitSet = this
		override def ^(elem :Unit) :UnitSet = Full
		override def empty :UnitSet = this
		override val toSeq :FitSeq[Unit] = FitSeq.Empty

	}

	object Full extends SingletonFoundation[Unit, UnitSet] with SingletonSpecialization[Unit, UnitSet] with UnitSet {
		override def contains(elem :Unit) :Boolean = true
		override def +(elem :Unit) :UnitSet = this
		override def -(elem :Unit) :UnitSet = Empty
		override def ^(elem :Unit) :UnitSet = Empty
		override val toSeq :StableSeq[Unit] = FitSeq.one(())
	}



	class MutableUnitSet private[UnitSet] (private[this] var full :Boolean)
		extends MutableSet[Unit] with MutableSetSpecialization[Unit, MutableUnitSet] with OfKnownSize
	{
		override def size :Int = if (full) 1 else 0
		override def nonEmpty :Boolean = full
		override def isEmpty :Boolean = !full

		override def empty = new MutableUnitSet(false)

		override def +=(elem :Unit) :this.type =  { full = true; this }

		override def -=(elem :Unit) :this.type = { full = false; this }

		override def flip(elem :Unit) :Boolean = { full = !full; full }

		override def contains(elem :Unit) :Boolean = full

		override def filter(p :Unit => Boolean, where :Boolean) :MutableUnitSet =
			new MutableUnitSet(full && p(())==where)

		override def foreach[@specialized(Unit) U](p :Unit => U) :Unit =
			if (full) p(())

		override protected def reverseForeach(f :Unit => Unit) :Unit =
			if (full) f(())

		override def find_?(p :Unit => Boolean, where :Boolean) : ?[Unit] =
			if (full && p(())) Sure(())
			else Blank

		override def iterator :FitIterator[Unit] =
			if (full) FitIterator.one(())
			else FitIterator.Empty


		override def clone() = new MutableUnitSet(full)

	}
}
