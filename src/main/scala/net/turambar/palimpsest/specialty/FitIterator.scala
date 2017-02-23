package net.turambar.palimpsest.specialty

import scala.annotation.unspecialized
import scala.collection.generic.{CanBuildFrom, FilterMonadic}
import scala.collection.{AbstractIterator, BufferedIterator, GenTraversableOnce, Iterator, immutable, mutable}
import net.turambar.palimpsest.{IndexedIteratorLike, ReverseIndexedIteratorLike}
import net.turambar.palimpsest.specialty.FitIterator.{FilterIterator, LimitedIterator, MappedIterator, ScanLeftIterator, TakeWhileIterator}
import Specialized.{Fun1, Fun1Vals, Fun2, Fun2Vals}
import net.turambar.palimpsest.specialty.seqs.{FitBuffer, FitSeq, StableArray, StableSeq$}



sealed trait IteratorTemplate[+E] extends Iterator[E] { self :FitIterator[E] =>
	protected[this] def mySpecialization :Specialized[E]
	
	@inline override final def /:[@specialized(Fun2) B](z: B)(op: (B, E) => B): B = foldLeft(z)(op)
}


/** This is a `@specialized` version of scala `Iterator`, which allows to eliminate boxing of each element
  * for primitives and automatically forces the compiler to generate specialized versions of
  * methods with this interface in their signature (assuming their declaring class/trait is specialized on `E`).
  * Apart from being already a `BufferedIterator` (to eliminate number of types duplicated from generic scala APIs),
  * it declares [[FitIterator#hasFastSize]] which can be used in concatenation or comparison code to check
  * if calling `size` is not only safe but also sensible.
  *
  * It is recommended that in order to minimize overhead of static forwarding implementors:
  *   1.  extend [[net.turambar.palimpsest.specialty.FitIterator.BaseIterator]]
  *       to inherit default implementations of methods which don't need specializations, or can reuse minimized
  *       specialized API of this interface
  *   2.  implement [[FitIterator#head]], [[FitIterator#next]], [[FitIterator#hasNext]] and optionally [[FitIterator#skip]]
  *       in the final concrete class, rather than another inherited trait
  *
  * @author Marcin Mościcki
  */
trait FitIterator[@specialized(Elements) +E]
	extends FitTraversableOnce[E] with BufferedIterator[E] //with FilterMonadic[E, FitIterator[E]]
{ self =>
	protected[this] def mySpecialization :Specialized[E] = Specialized[E]
	
	@unspecialized
	override def specialization :Specialized[_<:E] = mySpecialization
	
	@unspecialized
	override def fitIterator :FitIterator[E] = this
	
	def hasFastSize :Boolean
	
	/** First element of the iterator, if not empty.
	  * Note that for efficiency the result of calling it on an empty iterator is undefined (doesn't necessarily throw an exception).
	  * '''Always''' check if the iterator has the next element with [[hasNext]]
      * @return
	  */
	override def head :E
	
	/** Returns the next element of the iterator, advancing over it.
	  * Implemented as returning [[head]] and then calling [[skip]].
	  * Note that for efficiency, the result of calling this method on an empty iterator is undefined
	  * and may in particular result in an endless loop. Always ensure that there is an element to return with [[hasNext]].
	  */
	override def next(): E //= { val res = head; skip(); head }
	
	/** Equivalent to `next()` and discarding the value, simply skips the next element of the iterator without returning it.
	  * Streamlines buffered iterator implementations as specialized code can be confined to default [[next]] implementation
	  * (this method is not specialized). Note that this is not necessarily the same as `drop(1)`, as the result of calling
	  * it on an empty iterator is undefined.
	  */
	def skip() :Unit
	
	
	@unspecialized
	override def drop(n: Int): FitIterator[E] =
		if (n<=0) this
		else if (hasFastSize && n>=size) FitIterator.Empty
		else {
			var i=n
			while(i>0 && hasNext) { skip(); i-=1 }
			this
		}
	
	override def take(n :Int) :FitIterator[E] =
		if (n<=0) FitIterator.empty[E]
		else if (hasFastSize && size <= n) this
		else new LimitedIterator(this, n)
	
	@unspecialized
	override def slice(from: Int, until: Int): FitIterator[E] = drop(from).take(until-from)
	
	
	override def dropWhile(p: (E) => Boolean): FitIterator[E] =
		{ while(hasNext && p(head)) skip(); this }
	
	override def takeWhile(p: (E) => Boolean): FitIterator[E] =
		if (!hasNext) this
		else new TakeWhileIterator[E](this, p)
		
	def filter(p :E=>Boolean, ourTruth :Boolean) :FitIterator[E] =
		if (!hasNext) this
		else new FilterIterator(p, ourTruth)(this)
	
	@unspecialized
	override def filter(p :E=>Boolean) :FitIterator[E] = filter(p, ourTruth=true)
	@unspecialized
	override def filterNot(p :E=>Boolean) :FitIterator[E] = filter(p, ourTruth=false)
	@unspecialized
	override def withFilter(p :E=>Boolean) :FitIterator[E] = filter(p, ourTruth=true)
	


	override def foreach[@specialized(Unit) U](f: (E) => U): Unit =
		{ while (hasNext) f(next()) }
	
	@unspecialized
	override def traverse(f :E => Unit) :Unit = foreach(f)//= { while (hasNext) f(next()) }
	

	protected def forall(p :E=>Boolean, ourTruth :Boolean) :Boolean = {
		while(hasNext) {
			if (p(next())!=ourTruth) return false
		}
		true
	}
	
	@unspecialized
	override def forall(p: (E) => Boolean): Boolean = forall(p, ourTruth=true)
	@unspecialized
	override def exists(p: (E) => Boolean): Boolean = !forall(p, ourTruth=false)
	@unspecialized
	override def indexWhere(p: (E) => Boolean): Int = indexWhere(p, ourTruth=true)
	
	
	protected[specialty] def indexWhere(p :E=>Boolean, ourTruth :Boolean) :Int = {
		var i = 0
		while(hasNext) {
			if (p(next())==ourTruth) return i
			i += 1
		}
		-1
	}
	
	@unspecialized
	override def indexOf[B >: E](elem: B): Int =
		if (mySpecialization.boxType.isAssignableFrom(elem.getClass))
			positionOf(elem.asInstanceOf[E])
		else {
			var i = 0
			while(hasNext) {
				if (next() == elem)
					return i
				i += 1
			}
			-1
		}

	protected[this] def positionOf(elem :E) :Int = {
		var i = 0
		while(hasNext) {
			if (next()==elem)
				return i
			i += 1
		}
		-1
	}
	
	
	override def find(p: (E) => Boolean): Option[E] = {
		while(hasNext) {
			val e = next()
			if (p(e)) return Some(e)
		}
		None
	}
	
	override def count(p: (E) => Boolean): Int = {
		var t = 0
		while(hasNext) if (p(next())) t+=1
		t
	}
	
	
	override def map[@specialized(Fun1Vals) B](f: (E) => B): FitIterator[B] = new MappedIterator(f)(this)
	
	
	
	
	override def corresponds[@specialized(Fun2) O](that: GenTraversableOnce[O])(p: (E, O) => Boolean): Boolean =
		that.toIterator match {
			case fit :FitIterator[O] =>
				while (hasNext && fit.hasNext)
					if (!p(next(), fit.next())) return false
				hasNext == fit.hasNext
			case i =>
				while (hasNext && i.hasNext)
					if (!p(next(), i.next())) return false
				
				hasNext == i.hasNext
		}
	
	
	
	
	override def foldLeft[@specialized(Fun2) B](z: B)(op: (B, E) => B): B = {
		var acc = z
		while(hasNext) { acc = op(acc, next()) }
		acc
	}
	
	//todo: unspecialize the two
	override def scanLeft[@specialized(Fun2) A](z: A)(op: (A, E) => A): FitIterator[A] =
		new ScanLeftIterator(z, op)(this)
	
	override def scanRight[@specialized(Fun2) O](z: O)(op: (E, O) => O): FitIterator[O] =
		(FitBuffer.empty[E] ++= this).scanRight(z)(op).iterator
//		toFitBuffer[E].scanRight(z)(op).iterator
	
// these can't be sensibly/efficiently implemented:
//	override def :\[@specialized(Specialized.Fun2) B](z: B)(op: (E, B) => B): B = foldRight(z)(op)
//	override def foldRight[B](z: B)(op: (E, B) => B): B = super.foldRight(z)(op)
	
//these can't be specialized:
//	override def reduceLeft[B >: E](op: (B, E) => B): B =
//	override def reduceRight[B >: E](op: (E, B) => B): B = super.reduceRight(op)
//	override def reduceLeftOption[B >: E](op: (B, E) => B): Option[B] = super.reduceLeftOption(op)
//	override def reduceRightOption[B >: E](op: (E, B) => B): Option[B] = super.reduceRightOption(op)
//	override def reduce[A1 >: E](op: (A1, A1) => A1): A1 = super.reduce(op)
//	override def reduceOption[A1 >: E](op: (A1, A1) => A1): Option[A1] = super.reduceOption(op)
//	override def fold[A1 >: E](z: A1)(op: (A1, A1) => A1): A1 = super.fold(z)(op)
//	def ++[B >: E](that: => GenTraversableOnce[B]): Iterator[B] = super.++(that)
//	def padTo[A1 >: A](len: Int, elem: A1): Iterator[A1]

//too much effort or little gain in specializing:
//	override def flatMap[B](f: (E) => GenTraversableOnce[B]): Iterator[B] = super.flatMap(f)
//	def partition(p: E => Boolean): (Iterator[E], Iterator[E]) = {
//	override def collect[B](pf: PartialFunction[E, B]): Iterator[B] = super.collect(pf)
//	def span(p: E => Boolean): (Iterator[E], Iterator[E]) = super.span(p)
	



	override def sameElements(that: Iterator[_]): Boolean = that match {
		case s :FitIterator[_] if s.specialization==specialization =>
			sameElements(that.asInstanceOf[FitIterator[E]])
		case _ =>
			while (hasNext && that.hasNext && next()==that.next()) {}
			!hasNext && !that.hasNext
	}
	
	/** Specialized variant of [[sameElements(Iterator)]]. */
	protected[this] def sameElements(other :FitIterator[E]) :Boolean = {
		while (hasNext && other.hasNext && next()==other.next())
			()
		!hasNext && !other.hasNext
	}
	
	@unspecialized
	override def toSeq :FitSeq[E] = (FitSeq.fitBuilder(mySpecialization) ++= this).result()
	
	//todo: 'optimistic specialization'
	@unspecialized
	override def toBuffer[U>:E] :FitBuffer[U] = toFitBuffer
	
	@unspecialized
	def toFitBuffer[U>:E :Specialized] :FitBuffer[U] = //(FitBuffer.fitBuilder(mySpecialization) ++= this).result()
		FitBuffer.like[U] ++= this
	
	
	@unspecialized
	override def copyToArray[B >: E](xs: Array[B], start: Int, len: Int): Unit =
		if (start<0)
			throw new IllegalArgumentException(s"$this.copyToArray(${xs.getClass.getComponentType.getSimpleName}[${xs.length}], $start, $len)")
		else if (mySpecialization.emptyArray.getClass.isAssignableFrom(xs.getClass)) //todo
		     specializedCopy(xs.asInstanceOf[Array[E]], start, len)
		else {
			var i = start; val end = math.min(xs.length, start+len)
			while(i<end && hasNext) { xs(i) = next(); i+=1 }
		}
	
	/** Copies elements to the array, assuming their specializations are equal. */
	protected[this] def specializedCopy(xs :Array[E], start :Int, len :Int) :Unit = {
		val end = math.min(xs.length, start + len)
		var i = start
		while (i<end && hasNext) { val e :E = next(); xs(i) = e; i+=1 }
	}
	
}





object FitIterator {
	def empty[E] :FitIterator[E] = Empty
	
	def single[@specialized(Elements) E](value :E) :FitIterator[E] = new SingletonIterator[E](value)
	
	
	def adapt[@specialized(Elements) E](iter :Iterator[E]) :FitIterator[E] = iter match {
		case spec :FitIterator[_] => spec.asInstanceOf[FitIterator[E]]
		case i => new ErasedIterator(iter.buffered)
	}
	
	
	/** Checks if the given instance can be converted to a [[FitIterator]].
	  * Matching succeeds if the instance already is a `FitIterator`,
	  * is a [[FitIterable]] (in which case its iterator is returned),
	  * or is an instance of `mutable.WrappedArray`, in which case a properly
	  * specialized iterator is created.
	  */
	def unapply[E](items :GenTraversableOnce[E]) :Option[FitIterator[E]] = items match {
		case it :FitIterator[E] => Some(it)
		case es :IterableSpecialization[E, _] => Some(es.iterator)
		case arr :mutable.WrappedArray[E] => Some(ArrayIterator(arr.array))
		case _ => None
	}
	
	
	/** Base class for [[FitIterator]] instances introduced to minimize static forwarding of trait methods.
	  * This class is ''unspecialized'' (so can be extended by specialized classes)
	  * and contains only methods which don't need specializations (to be provided by concrete implementations).
	  * Note that this class doesn't implement [[FitIterator]] in order to enforce subclasses to explicitly
	  * extend it, as otherwise they would inherit just the unspecialized version
	  */
	abstract class BaseIterator[+E] extends AbstractIterator[E] with FitIterator[E] { //this :FitIterator[E] =>
		
		override def specialization :Specialized[_<:E] = mySpecialization
		
		override def hasFastSize = !hasNext
		override def isEmpty = !hasNext
		override def nonEmpty = hasNext
		
		final override def fitIterator :FitIterator[E] = this
		
		override def drop(n: Int): FitIterator[E] =
			if (n<=0) this
			else if (hasFastSize && n>=size) FitIterator.Empty
			else {
				var i=n
				while(i>0 && hasNext) { skip(); i-=1 }
				this
			}
		
		override def slice(from: Int, until: Int): FitIterator[E] = drop(from).take(until-from)
		
		override def filter(p :E=>Boolean) :FitIterator[E] = filter(p, ourTruth=true)
		override def filterNot(p :E=>Boolean) :FitIterator[E] = filter(p, ourTruth=false)
		override def withFilter(p :E=>Boolean) :FitIterator[E] = filter(p, ourTruth=true)
		
		override def forall(p: (E) => Boolean): Boolean = forall(p, ourTruth=true)
		
		override def exists(p: (E) => Boolean): Boolean = !forall(p, ourTruth=false)
		
		override def indexWhere(p: (E) => Boolean): Int = indexWhere(p, ourTruth=true)
		
		
		override def traverse(f: (E) => Unit): Unit = foreach[Unit](f :E=>Unit) //dear compiler, please use specialized f.apply
		
		override def toString =
			if (isEmpty) s"Iterator[$mySpecialization]()"
			else s"Iterator[$mySpecialization]($head, ???)"
		
	}

//	abstract class ProxyIterator[E] extends BaseIterator[E] {
//		protected[this] def source :Iterator
//	}
	
	/** An empty, erased iterator. */
	object Empty extends FitIterator[Nothing] {
		override val specialization = Specialized[Nothing]
		
		override def head: Nothing = throw new NoSuchElementException(s"FitIterator.empty.head")
		override def next(): Nothing = head
		override def skip() :Unit = ()
		override def hasNext: Boolean = false
		override def isEmpty = true
		override def nonEmpty = false
		
		override def size = 0
		override def length = 0
		override def hasDefiniteSize = true
		override def hasFastSize = true
		
		override def drop(n: Int): FitIterator[Nothing] = this
		override def take(n: Int): FitIterator[Nothing] = this
		override def slice(from: Int, until: Int): FitIterator[Nothing] = this

		
		override def filter(p: (Nothing) => Boolean, ourTruth: Boolean): FitIterator[Nothing] = this
//		override def filter(p: (Nothing) => Boolean): FitIterator[Nothing] = this
//		override def filterNot(p: (Nothing) => Boolean): FitIterator[Nothing] = this
		
		override def foreach[@specialized(Unit) U](f: (Nothing) => U): Unit = ()
		override def traverse(f: (Nothing) => Unit): Unit = ()
		
		override def map[@specialized(Fun1Vals) B](f: (Nothing) => B): FitIterator[B] = this
		override def flatMap[B](f: (Nothing) => GenTraversableOnce[B]): FitIterator[B] = this
		
		
		override def copyToArray[B >: Nothing](xs: Array[B], start: Int, len: Int): Unit = ()
		
		override def toSeq :FitSeq[Nothing] = FitSeq.Empty
		override def toIndexedSeq = StableArray.Empty
		override def toBuffer[U >: Nothing]: FitBuffer[U] = FitBuffer.empty[U]
		override def toFitBuffer[U >: Nothing : Specialized]: FitBuffer[U] = FitBuffer.empty[U]
		
		override def sameElements(that: Iterator[_]): Boolean = that.isEmpty
		
		override def toString = "FitIterator[Nothing]()"
	}
	
	
	
	/** Unspecialized base class for [[FitIterator]] implementations which iterate over
	  * some kind of indexed sequences (in a generic sense). It is assumed that [[index]]
	  * describes the position of the current element ([[FitIterator#head]]) and that advancing
	  * the iterator can be represented by increasing the index by one in [[skip]], until a
	  * predefined (but mutable) end index is reached (exclusive).
	  * Note that implementations of [[FitIterator#head]] and [[FitIterator#next]] are left to the
	  * subclasses in order to be specializable.
	  * @param index current position of the `head` element
	  * @param end end index for the iteration, `end>index` for non-empty iterators.
	  */
	abstract class IndexedIterator[+E](protected[this] final var index :Int, protected[this] final var end :Int)
		extends BaseIterator[E]
	{ this :FitIterator[E] =>
		override def hasDefiniteSize = true
		override def hasFastSize = true
		override def size = if (index>=end) 0 else end-index
		
		override def hasNext = index<end
		
		override def skip() :Unit = index+=1
		
		override def drop(n: Int): FitIterator[E] = {
			if (n>0)
				index += n
			this
		}
		
		override def take(n: Int): FitIterator[E] = {
			if (index+n<end)
				end = index+n
			this
		}
		
		override def slice(from: Int, until: Int): FitIterator[E] = {
			if (index + until < end)
				end = index + until
			if (from>0)
				index += from
			this
		}
		
		
		override def toString =
			if (isEmpty) s"Iterator[$mySpecialization]#$index>>()"
			else s"Iterator[$mySpecialization]#$index>>($head, ?)"
	}
	
	
	/** Unspecialized base class for [[FitIterator]] implementations which iterate over
	  * some kind of indexed sequences in reverse order (in a generic sense). It is assumed that [[index]]
	  * describes the position of the current element ([[FitIterator#head]]) and that advancing
	  * the iterator can be represented by decreasing the index by one in [[skip]], until a
	  * predefined (but mutable) end index is passed.
	  * Note that implementations of [[FitIterator#head]] and [[FitIterator#next]] are left to the
	  * subclasses in order to be specializable.
	  * @param index current index of this iterator to be used by [[FitIterator#head]]
	  * @param end modifiable end index of iteration (inclusive), (`end<=index` for non-empty iterators)
	  */
	abstract class ReverseIndexedIterator[+E](protected[this] var index :Int, protected[this] var end :Int)
		extends BaseIterator[E]
	{ this :FitIterator[E] =>
		
		override def hasFastSize = true
		override def hasDefiniteSize = true
		override def size = if (end>index) 0 else index-end +1
		
		override def hasNext: Boolean = index >= end
		
		override def skip() :Unit = index-=1
		
		override def drop(n: Int): FitIterator[E] = {
			if (n>0)
				index -= n
			this
		}
		
		override def take(n: Int): FitIterator[E] = {
			if (index-n > end)
				end = index-n
			this
		}
		
		override def slice(from: Int, until: Int): FitIterator[E] = {
			if (index - until > end)
				end = index - until
			if (from>0)
				index -= from
			this
		}
		
		override def toString =
			if (isEmpty) s"Iterator[$mySpecialization]#$index>>()"
			else s"Iterator[$mySpecialization]#$index>>($head, ?)"
	}
	

	
	
	private class ErasedIterator[+E](source :BufferedIterator[E]) extends BaseIterator[E] with FitIterator[E] {
		override def hasDefiniteSize = source.hasDefiniteSize
		
		override def head: E = source.head

		override def next(): E = source.next()
		
		override def skip() = source.next()

		override def hasNext: Boolean = source.hasNext
		
		override def copyToArray[B >: E](xs: Array[B], start: Int, len: Int): Unit = source.copyToArray(xs, start, len)
	}
	
	
	
	private class SingletonIterator[@specialized(Elements) +E](val head :E) extends FitIterator[E] {
		override def specialization: Specialized[_ <: E] = mySpecialization
		
		private[this] var left = 1
		override def skip() :Unit = left = 0
		override def next() = { left=0; head }
		override def hasNext = left>0
		
		override def hasFastSize = true
		override def hasDefiniteSize = true
		override def size = left
		
		
		override def foreach[@specialized(Unit) U](f: (E) => U): Unit = f(head)
		override def traverse(f: (E) => Unit): Unit = f(head)
		
		@unspecialized
		override def drop(n: Int): FitIterator[E] = {
			if (n>0)
				left = 0
			this
		}
		
		@unspecialized
		override def take(n :Int) :FitIterator[E] = {
			if (n<left)
				left = 0
			this
		}
		
		override def copyToArray[B >: E](xs: Array[B], start: Int, len: Int): Unit =
			if (left>0 && len>0 && xs.length-start>0)
				xs(start) = head
		
		override def toString =
			if (isEmpty) s"FitIterator[$mySpecialization]()"
			else s"FitIterator[$mySpecialization]($head)"
	}
	
	
	abstract class CountdownIterator[+E](protected[this] final var limit :Int) extends BaseIterator[E] {
		override def size = limit
		override def hasFastSize = true
		override def hasDefiniteSize = true
		override def hasNext = limit > 0
		
		override def take(n :Int) :FitIterator[E] =
			if (n>size) this
			else if (n<=0) Empty
			else {
				limit -= n; this
			}
		
		override def drop(n :Int) :FitIterator[E] =
			if (n<=0) this
			else if (n>size) Empty
			else {
				val until = size-n
				do { skip() } while (size > until)
				this
			}
	}
	
	
	class LimitedIterator[@specialized(Elements) +E](source :FitIterator[E], private[this] var limit :Int)
		extends BaseIterator[E] with FitIterator[E]
	{
		override def head: E = source.head
		
		override def skip() :Unit = { limit-=1; source.skip() }
		
		override def next(): E = { limit-=1; source.next() }
		
		override def hasNext = limit>0 && source.hasNext
		
		@unspecialized
		override def take(n: Int): FitIterator[E] = {
			limit = limit min n
			this
		}
		
		@unspecialized
		override def drop(n :Int) :FitIterator[E] = {
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
		
		override def toString = s"$source.take($limit)"
	}

	
	
	class TakeWhileIterator[@specialized +E](source :FitIterator[E], p :E=>Boolean)
		extends BaseIterator[E] with FitIterator[E]
	{
		override def hasDefiniteSize = source.hasDefiniteSize
		
		private[this] var hd: E = source.head
		var hasNext = p(hd)
		
		override def head = hd
		
		override def next(): E = {
			val res = hd
			if (source.hasNext) hd=source.next()
			else hasNext = false
			res
		}
		
		override def skip(): Unit = {
			if (source.hasNext) hd = source.next()
			else hasNext = false
		}
		
	}
	

	
	class MappedIterator[@specialized(Fun1) X, @specialized(Fun1Vals) +E](f :X=>E)(protected var source :FitIterator[X])
		extends BaseIterator[E] with FitIterator[E]
	{
		override def head: E = f(source.head)
		override def next(): E = f(source.next())
		override def skip() :Unit = source.skip()
		override def hasNext = source.hasNext
		
		override def size = source.size
		override def hasDefiniteSize: Boolean = source.hasDefiniteSize
		override def hasFastSize :Boolean = source.hasFastSize
		override def isEmpty = source.isEmpty
		override def nonEmpty = source.nonEmpty
		
		@unspecialized
		override def drop(n: Int): FitIterator[E] = {
			source = source.drop(n); this
		}
		
		@unspecialized
		override def take(n: Int): FitIterator[E] = {
			source = source.take(n); this
		}
		
		@unspecialized
		override def slice(from: Int, until: Int): FitIterator[E] = {
			source = source.slice(from, until); this
		}
		
		override def map[@specialized(Fun1Vals) O](g: (E) => O): FitIterator[O] =
			new MappedIterator[X, O]({ x :X => g(f(x)) })(source)
	}
	
	
	class ScanLeftIterator[@specialized(Fun2) +A, @specialized(Fun2) +E](private[this] var acc :A, op :(A, E)=>A)(source :FitIterator[E])
		extends BaseIterator[A] with FitIterator[A]
	{
		override def hasFastSize = source.hasFastSize
		override def hasDefiniteSize: Boolean =  source.hasDefiniteSize
		override def size = 1 + source.size
		var hasNext = true
		
		override def head: A = acc //op(acc, source.head)
		
		override def next(): A = {
			val res = acc
			if (source.hasNext) acc = op(res, source.next())
			else hasNext = false
			res
		}
		
		override def skip() :Unit = next() //avoid boxed calls on op
		
	}
	
	
	class FilterIterator[@specialized(Fun1) E](p :E=>Boolean, ourTruth :Boolean = true)(source :FitIterator[E])
		extends BaseIterator[E] with FitIterator[E]
	{
		override def hasDefiniteSize = source.hasDefiniteSize
		var head :E = source.next()
		private[this] var hasMore = true
		if (p(head)!=ourTruth) next()
		
		def hasNext = hasMore
		
		/** Delegate to a specialized method. */
		def skip() :Unit = next()
		
		override def next() :E = {
			val res = head
			while(source.hasNext) {
				val hd = source.next()
				if (p(hd)==ourTruth) {
					head = hd; return res
				}
			}
			hasMore = false
			res
		}
	
	}
}

