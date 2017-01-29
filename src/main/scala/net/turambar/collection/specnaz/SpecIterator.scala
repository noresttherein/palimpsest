package net.turambar.collection.specnaz

import scala.annotation.unspecialized
import scala.collection.{immutable, BufferedIterator, Iterator}

import net.turambar.collection.specnaz.SpecIterator.LimitedIterator
import net.turambar.collection.{AbstractIndexedIterator, AbstractReverseIndexedIterator, AbstractStepIterator}

/**
  * @author Marcin Mościcki
  */
trait SpecIterator[@specialized(Reified) +E] extends BufferedIterator[E] {
	private[this] val spec = Specialized[E]

	override def head :E
	override def next(): E

	@inline def specialization :Specialized[_] = spec

	override def drop(n: Int): SpecIterator[E] = { var i=n; while(i>0 && hasNext) { next(); i-=1 }; this }

	override def take(n :Int) :SpecIterator[E] = new LimitedIterator(this, n)
	
	override def slice(from: Int, until: Int): Iterator[E] = drop(from).take(until-from)
	
	override def copyToArray[B >: E](xs: Array[B], start: Int, len: Int): Unit =
		if (start<0)
			throw new IndexOutOfBoundsException(s"$this.copyToArray(${xs.getClass.getComponentType.getSimpleName}[${xs.length}], $start, $len)")
		else {
			val end = start + (len min xs.length-start max 0)
			var i = start
			while (i<end && hasNext) { xs(i) = next(); i+=1 }
		}


	override def sameElements(that: Iterator[_]): Boolean = that match {
		case s :SpecIterator[_] if s.specialization==specialization =>
			val other = s.asInstanceOf[SpecIterator[E]]
			while (hasNext && other.hasNext && next()==other.next()) {}
			!hasNext && !other.hasNext

		case _ =>
			while (hasNext && that.hasNext && next()==that.next()) {}
			!hasNext && !that.hasNext
	}

	@unspecialized
	override def buffered = this
	
	def hasFastSize :Boolean = isEmpty
	
	override def toSeq :SpecSeq[E] = SpecSeq(super.toSeq:_*)
//	override def toIndexedSeq :SpecSeq[E]

}

object SpecIterator {
	def empty[E] :SpecIterator[E] = Empty
	
	def singleton[@specialized(Reified) E](value :E) :SpecIterator[E] = new SingletonIterator[E] {
		val head = value
	}
	
	
	def adapt[@specialized(Reified) E](iter :Iterator[E]) :SpecIterator[E] = iter match {
		case spec :SpecIterator[_] => spec.asInstanceOf[SpecIterator[E]]
		case i => new AdaptedSpecIterator(iter.buffered)
	}
	
	object Empty extends SpecIterator[Nothing] {
		override def head: Nothing = throw new NoSuchElementException(s"SpecIterator.empty.head")
		override def next(): Nothing = head
		override def hasNext: Boolean = false

		override def drop(n: Int): SpecIterator[Nothing] = this
		override def take(n: Int): SpecIterator[Nothing] = this
		override def slice(from: Int, until: Int): Iterator[Nothing] = this

		override def size = 0
		override def length = 0
		override def hasDefiniteSize = true
		override def hasFastSize = true
		
		override def copyToArray[B >: Nothing](xs: Array[B], start: Int, len: Int): Unit = ()
		
		override def toSeq :SpecSeq[Nothing] = SpecSeq.Empty
		override def toIndexedSeq = immutable.IndexedSeq.empty
		
		override def sameElements(that: Iterator[_]): Boolean = that.isEmpty
		
		override def toString = "SpecIterator.empty"
	}
	


	
	trait SpecSeqIterator[@specialized(Reified) +E] extends SpecIterator[E] with AbstractIndexedIterator[E] {
		override def hasDefiniteSize = true
		override def hasFastSize = true
	}
	
	trait ReverseSpecSeqIterator[@specialized(Reified) +E] extends SpecIterator[E] with AbstractReverseIndexedIterator[E] {
		override def hasDefiniteSize = true
		override def hasFastSize = true
	}
	
//	trait SpecStepIterator[@specialized(Reified) E] extends SpecIterator[E] with AbstractStepIterator[E]
	
	
	private class AdaptedSpecIterator[+E](source :BufferedIterator[E]) extends SpecIterator[E] {
		override def head: E = source.head

		override def next(): E = source.next

		override def hasNext: Boolean = source.hasNext

		override def copyToArray[B >: E](xs: Array[B], start: Int, len: Int): Unit = source.copyToArray(xs, start, len)
	}
	
	trait SingletonIterator[@specialized(Reified) +E] extends SpecIterator[E] {
		private[this] var left = 1
		override def next() = { left=0; head }
		
		override def hasNext = left>0
		
		override def hasFastSize = true
		override def hasDefiniteSize = true
		override def size = left
		
		override def drop(n: Int): SpecIterator[E] = {
			if (n>0)
				left = 0
			this
		}
		
		override def take(n :Int) :SpecIterator[E] = {
			if (n<left)
				left = 0
			this
		}
		
		override def copyToArray[B >: E](xs: Array[B], start: Int, len: Int): Unit =
			if (left>0 && len>0 && xs.length-start>0)
				xs(start) = head
		
		override def toString = if (!isEmpty) s"SpecIterator($head)" else "SpecIterator()"
	}
	
	
	class LimitedIterator[@specialized(Reified) +E](source :SpecIterator[E], private[this] var limit :Int) extends SpecIterator[E] {
		override def head: E = source.head
//			if (limit<=0) throw new NoSuchElementException(s"$this.head")
//			else source.head
		
		override def next(): E = { limit-=1; source.next() }
		
		override def hasNext() = limit>0 && source.hasNext
		
		override def take(n: Int): SpecIterator[E] = {
			limit = limit min n
			this
		}
		
		override def drop(n :Int) :SpecIterator[E] = {
			if (n > 0) {
				source.drop(n)
				limit -= n
			}
			this
		}
		
		override def hasDefiniteSize = true
		override def hasFastSize = limit<=0 || source.hasFastSize
		override def size = if (limit<=0) 0 else limit min source.size
		override def isEmpty = limit<=0 || source.isEmpty
		
		override def copyToArray[B >: E](xs: Array[B], start: Int, len: Int): Unit =
			source.copyToArray(xs, start, len min limit)
	}
}

