package net.turambar.palimpsest.specialty.sets

import scala.collection.{BitSet, mutable}

import net.turambar.palimpsest.specialty.FitIterator.BaseIterator
import net.turambar.palimpsest.specialty.sets.IntSet.IntSetIterator
import net.turambar.palimpsest.specialty.{FitBuilder, FitIterator}

/**
  * @author Marcin Mościcki
  */
class IntSet private[IntSet](negative :BitSet, positive :BitSet, override val size :Int) extends FitSet[Int] {
	
	override def contains(elem: Int): Boolean =
		elem>=0 && positive.contains(elem) || negative.contains(-elem)
	
	override def +(elem: Int): FitSet[Int] =
		if (elem>=0)
			if (positive.contains(elem)) this
			else new IntSet(negative, positive + elem, size+1)
		else
			if (negative.contains(-elem)) this
			else new IntSet(negative + (-elem), positive, size+1)
	
	override def -(elem: Int): FitSet[Int] =
		if (elem>=0)
			if(positive.contains(elem)) new IntSet(negative, positive-elem, size - 1)
			else this
		else
			if (negative.contains(elem)) new IntSet(negative - (-elem), positive, size-1)
			else this
	
	override def iterator: FitIterator[Int] = new IntSetIterator(negative, positive, size)
		
	
}


object IntSet {
	final val Empty = new IntSet(BitSet.empty, BitSet.empty, 0)
	@inline final def empty = Empty
	
	def newBuilder :FitBuilder[Int, IntSet] = new IntSetBuilder(mutable.BitSet.empty, mutable.BitSet.empty, 0)
	
	private class IntSetIterator(negative :BitSet, positive :BitSet, private[this] var left :Int) extends BaseIterator[Int] {
		override def hasFastSize: Boolean = true
		override def hasDefiniteSize = true
		override def size = left
		override def hasNext = left>0
		
		var head :Int =
			if (left<=0) 0
			else if (negative.nonEmpty) -negative.last
			else positive.head
		
		
		override def next(): Int = {
			val res = head
			left -= 1
			if (left>0) {
				while (head < 0) {
					head += 1
					if (negative.contains(-head)) return res
				}
				while (!positive.contains(head)) head += 1
			}
			res
		}
		

		override def skip(): Unit = {
			left -= 1
			if (left>0) {
				do head += 1 while (head < 0 && !negative.contains(-head))
				if (head >= 0)
					while (!positive.contains(head)) head += 1
			}
		}
	}
	
	private class IntSetBuilder(negative :mutable.BitSet, positive :mutable.BitSet, private[this] var size :Int)
		extends FitBuilder[Int, IntSet]
	{
		override def +=(elem: Int): this.type = {
			if (elem >= 0)
				if (!positive.contains(elem)){
					positive += elem
					size += 1
				}
			else if (!negative.contains(-elem)) {
				negative += -elem
				size += 1
			}
			this
		}
		
		override def result(): IntSet = new IntSet(negative, positive, size)
		
		override def count: Int = size
		
		override def clear(): Unit = {
			negative.clear()
			positive.clear()
			size=0
		}
	}
}
