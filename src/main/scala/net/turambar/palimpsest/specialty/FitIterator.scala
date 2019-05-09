package net.turambar.palimpsest.specialty

import java.lang.Math

import scala.annotation.{tailrec, unspecialized}
import scala.collection.generic.{CanBuildFrom, FilterMonadic}
import scala.collection.{immutable, mutable, AbstractIterator, BufferedIterator, GenTraversableOnce, Iterator}
import net.turambar.palimpsest.{IndexedIteratorLike, ReverseIndexedIteratorLike}
import net.turambar.palimpsest.specialty.FitIterator.{ConcatIterator, FilterIterator, LimitedIterator, MappedIterator, ScanLeftIterator, TakeWhileIterator}
import Specialized.{Fun1, Fun1Vals, Fun2, Fun2Vals}
import net.turambar.palimpsest.specialty.seqs.{FitBuffer, FitIndexedSeq, FitSeq, StableArray}



sealed trait IteratorTemplate[+E] extends Iterator[E] with FunctorLike[E, FitIterator] { self :FitIterator[E] =>
	protected[this] def mySpecialization :Specialized[E]

//	def hasFastSize :Boolean = hasNext

//	override def ofAtLeast(size :Int) :Boolean = {
//		@tailrec def enumerate(n :Int) :Boolean =
//			n<=0 || hasNext && (n==1 || { skip(); enumerate(n-1) })
//		enumerate(size)
//	}

	def headOption :Option[E] = if (hasNext) Some(head) else None

	override def traverse(f :E => Unit) :Unit = foreach(f)

	override def forall(p: E => Boolean): Boolean = { dropWhile(p); isEmpty }
	override def exists(p: E => Boolean): Boolean = { dropWhile(p, false); hasNext }
	override def indexWhere(p: E => Boolean): Int = indexWhere(p, ourTruth=true)

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

	@inline override final def /:[@specialized(Fun2) B](z: B)(op: (B, E) => B): B = foldLeft(z)(op)

//	override def toIterator :FitIterator[E] = this

	def skip() :Unit

	protected def empty :FitIterator[E]

//	override def drop(n: Int): FitIterator[E] = {
//		var i=n
//		while(i>0 && hasNext) { skip(); i-=1 }
//		this
//	}


	override def drop(n: Int): FitIterator[E] =
		if (n<=0) this
		else if (hasFastSize && n>=size) empty //FitIterator.empty[E]
		else {
			var i=n
			while(i>0 && hasNext) { skip(); i-=1 }
			this
		}


	override def take(n :Int) :FitIterator[E] =
		if (n<=0) empty
		else if (hasFastSize && size <= n) this
		else new LimitedIterator(this, n)


	protected def dropWhile(p :E => Boolean, is :Boolean) :FitIterator[E]

	override def dropWhile(p: E => Boolean): FitIterator[E] = dropWhile(p, true)


	override def slice(from: Int, until: Int): FitIterator[E] = drop(from).take(until-from)

	override def filter(p :E=>Boolean) :FitIterator[E] = filter(p, ourTruth=true)
	override def filterNot(p :E=>Boolean) :FitIterator[E] = filter(p, ourTruth=false)
	override def withFilter(p :E=>Boolean) :FitIterator[E] = filter(p, ourTruth=true)

	override def map[@specialized(Fun1Vals) O](f :E => O) :FitIterator[O] = new MappedIterator[E, O](f)(this)

//	@migration("release", "use :++ as ++ cannot be specialized")
	override def ++[U >: E](andThen: => GenTraversableOnce[U]) :Iterator[U] = super.++(andThen)


	override def toSeq :FitSeq[E] = (FitSeq.fitBuilder(mySpecialization) ++= this).result()

	//todo: 'optimistic specialization'
	override def toBuffer[U>:E] :FitBuffer[U] = toFitBuffer

	override def toIterator :FitIterator[E] = this

	def toFitBuffer[U>:E :Specialized] :FitBuffer[U] = //(FitBuffer.fitBuilder(mySpecialization) ++= this).result()
		FitBuffer.of[U] ++= this


	override def copyToArray[B >: E](xs: Array[B], start: Int, len: Int): Unit =
		if (start<0)
			throw new IllegalArgumentException(s"$this.copyToArray(${xs.getClass.getComponentType.getSimpleName}[${xs.length}], $start, $len)")
		else if (mySpecialization.emptyArray.getClass.isAssignableFrom(xs.getClass)) //todo
			specializedCopy(xs.asInstanceOf[Array[E]], start, len)
		else {
			var i = start; val end = Math.min(xs.length, start+len)
			while(i<end && hasNext) { xs(i) = next(); i+=1 }
		}

}



/** This is a `@specialized` version of scala `Iterator`, which allows to eliminate boxing of each element
  * for primitives and automatically forces the compiler to generate specialized versions of
  * methods with this interface in their signature (assuming their declaring class/trait is specialized on `E`).
  * Apart mine being already a `BufferedIterator` (to eliminate number of types duplicated mine generic scala APIs),
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
	extends BufferedIterator[E] with FitTraversableOnce[E] with IteratorTemplate[E]//with FilterMonadic[E, FitIterator[E]]
{ self =>
	protected[this] def mySpecialization :Specialized[E] = Specialized[E]

	@unspecialized
	override final def specialization :Specialized[_<:E] = mySpecialization
	

	
	override def hasFastSize :Boolean = !hasNext
	override def isEmpty :Boolean = !hasNext
	override def nonEmpty :Boolean = hasNext

	override def ofAtLeast(size :Int) :Boolean = {
		@tailrec def enumerate(n :Int) :Boolean =
			n<=0 || hasNext && (n==1 || { skip(); enumerate(n-1) })
		enumerate(size)
	}

	protected def empty :FitIterator[E] =
		if (!hasNext) this
		else FitIterator.empty[E]

	def head_? : ?[E] = if (hasNext) Sure(head) else Blank

//	def next_? : ?[E] = if (hasNext) Sure(next()) else Blank

	/** First element of the iterator, if not empty.
	  * Note that for efficiency reason the result of calling it on an empty iterator is undefined (doesn't necessarily
	  * throw an exception). '''Always''' check if the iterator has the next element with [[hasNext]].
	  */
	override def head :E
	
	override def next(): E //= { val res = head; skip(); head }

	/** Applies the given function to `head` element of this iterator. Exists for the benefit of
	  * [[net.turambar.palimpsest.specialty.FitIterator.MappedIterator]], so it can be specialized only on its
	  * return type, rather than all `(E=>O)` pairs.
	  */
	private[specialty] def mapHead[@specialized(Fun1Vals) O](f :E=>O) :O = f(head)

	/** Applies the given function to `next()` element of this iterator, advancing it. Exists for the benefit of
	  * [[net.turambar.palimpsest.specialty.FitIterator.MappedIterator]], so it can be specialized only on its
	  * return type, rather than all `(E=>O)` pairs.
	  */
	private[specialty] def mapNext[@specialized(Fun1Vals) O](f :E=>O) :O = f(next())

	/** Applies the given function to the passed first argument and the next element of this iterator, advancing it.
	  * Exists for the benefit of [[FitIterator.ScanLeftIterator]], to avoid specializing it on both source and
	  * result element types.
	  */
	private[specialty]def scanNext[@specialized(Fun2) O](acc :O, f :(O, E) => O) :O = f(acc, next())




	

	protected def dropWhile(p :E => Boolean, is :Boolean) :FitIterator[E] = {
		while(hasNext && p(head) == is) skip(); this
	}
	
//	override def dropWhile(p: E => Boolean): FitIterator[E] = {
//		while(hasNext && p(head)) skip(); this
//	}
	
	override def takeWhile(p: E => Boolean): FitIterator[E] =
		if (!hasNext) this
		else new TakeWhileIterator[E](this, p)
		
	
	def filter(p :E=>Boolean, ourTruth :Boolean) :FitIterator[E] =
		if (!hasNext) this
		else new FilterIterator(p, ourTruth)(this)
	



	override def foreach[@specialized(Unit) U](f: E => U): Unit = { 
		while (hasNext) f(next()) 
	}
	

	

//	protected def forall(p :E=>Boolean, ourTruth :Boolean) :Boolean = {
//		while(hasNext) {
//			if (p(next())!=ourTruth) return false
//		}
//		true
//	}
	

	
	protected[specialty] def indexWhere(p :E=>Boolean, ourTruth :Boolean) :Int = {
		var i = 0
		while(hasNext) {
			if (p(next())==ourTruth) return i
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
	
	
	override def find(p: E => Boolean): Option[E] = {
		while(hasNext) {
			val e = next()
			if (p(e)) return Some(e)
		}
		None
	}
	
	def find_?(p :E => Boolean, where :Boolean): ?[E] = {
		while(hasNext) {
			val e = next()
			if (p(e) == where) return Sure(e)
		}
		Blank
	}
	
	override def count(p: E => Boolean): Int = {
		var t = 0
		while(hasNext) if (p(next())) t+=1
		t
	}
	
	
	override def map[@specialized(Fun1Vals) B](f: E => B): FitIterator[B] = new MappedIterator(f)(this)


	override def flatMap[@specialized(Elements) B](f :E => GenTraversableOnce[B]) :FitIterator[B] =
		if (!hasNext)
			FitIterator.empty[B]
		else
			new ConcatIterator[B](FitIterator.adapt(f(next()).toIterator), flatMap(f))


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
	

	/** Copies elements to the array, assuming their specializations are equal. */
	protected[this] def specializedCopy(xs :Array[E], start :Int, len :Int) :Unit = {
		val end = Math.min(xs.length, start + len)
		var i = start
		while (i<end && hasNext) { val e :E = next(); xs(i) = e; i+=1 }
	}
	
}





object FitIterator {

	/** An empty iterator, specialized for its declared element type.
	  * @see [[FitIterator.Empty]] an empty iterator with erased element type.
	  */
	def empty[@specialized(Elements) E] :FitIterator[E] = new EmptyIterator[E] //Empty

	/** An iterator over `times` copies of `elem`. */
	def repeated[@specialized(Elements) E](elem :E, times :Int) :FitIterator[E] = new ConstIterator[E](elem, times)

	/** An iterator over a single element. */
	def apply[@specialized(Elements) E](value :E) :FitIterator[E] = new SingletonIterator[E](value)

	/** An iterator over two values. */
	def apply[@specialized(Elements) E](first :E, second :E) :FitIterator[E] = new DoubletonIterator[E](first, second)
	
	/** Wraps the given iterator in an erased `FitIterator`. */
	def adapt[@specialized(Elements) E](iter :Iterator[E]) :FitIterator[E] = iter match {
		case spec :FitIterator[_] => spec.asInstanceOf[FitIterator[E]]
		case _ => new ErasedIterator(iter.buffered)
	}
	
	
	/** Checks if the given instance can be converted to a [[FitIterator]].
	  * Matching succeeds if the instance already is a `Fitterator`,
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


	/** Implicit conversion mine `FitIterator[E]` which adds a concatenation operation `:++`. Note that standard
	  * iterator's method `++` is generic with a lower bound for element type which makes it impossible to specialize.
	  */
	implicit class IteratorConcatenation[@specialized(Elements) E](first :FitIterator[E]) {
		def :++(andThen: =>GenTraversableOnce[E]) :FitIterator[E] = new ConcatIterator[E](first, andThen)
	}




	/** Base class for [[FitIterator]] instances introduced to minimize static forwarding of trait methods.
	  * This class is ''unspecialized'' (so can be extended by specialized classes)
	  * and contains only methods which don't need specializations (to be provided by concrete implementations).
	  * Note that this class doesn't implement [[FitIterator]] in order to enforce subclasses to explicitly
	  * extend it, as otherwise they would inherit just the unspecialized version
	  */
	abstract class BaseIterator[+E] extends AbstractIterator[E] with IteratorTemplate[E] { this :FitIterator[E] =>



//		override def specialization :Specialized[_<:E] = mySpecialization

		override def traverse(f: E => Unit): Unit = foreach[Unit](f :E=>Unit) //dear compiler, please use specialized f.apply
		
		override def toString :String =
			if (isEmpty) s"Iterator[$mySpecialization]()"
			else s"Iterator[$mySpecialization]($head, ???)"
		
	}

	/** An iterator of known size, computable in `O(1)` or close enough to make little difference.
	  * Implements size-related methods using its `size`.
	  */
	abstract class FastSizeIterator[+E] extends BaseIterator[E] { this :FitIterator[E] =>
		override def hasFastSize = true
		override def hasDefiniteSize = true
//		override def isEmpty :Boolean = size == 0
//		override def nonEmpty :Boolean = size > 0
		override def ofAtLeast(items :Int) :Boolean = size >= items
		override def hasNext :Boolean = size > 0
	}


	/** A ''single abstract method'' iterator base trait which implements all required methods using abstract `next_?`
	  * returning an uncertain value. Having a single abstract method allows concrete subclasses to be defined
	  * in-line using function syntax.
	  */
	trait SAMIterator[@specialized(Elements) +E] extends FitIterator[E] {
		private[this] var future: ?[E] = _

		/** If non-empty, returns the next element as a `Sure` value, immediately advancing over it.
		  * If empty, returns simply `Blank`. It must be equivalent to:
		  * {{
		  *      if (hasNext) Sure(next())
		  *      else Blank
		  * }}
		  */
		def next_? : ?[E]

		override def hasNext :Boolean = {
			if (future == null)
				future = next_?
			future.isDefined
		}


		override def head :E = {
			if (future == null)
				future = next_?
			future.get
		}

		override def next() :E = {
			if (future == null)
				future = next_?
			val res = future.get
			future = next_?
			res
		}

		override def skip() :Unit = next()

	}


	private class EmptyIterator[@specialized(Elements) E] extends FastSizeIterator[E] with FitIterator[E]  {
		override def size = 0
		override def length = 0
		override def ofAtLeast(items :Int) :Boolean = items <= 0
		override def hasNext = false
		override def isEmpty = true
		override def nonEmpty = false

		override protected def empty :FitIterator[E] = this

		override def head: E = throw new NoSuchElementException(s"FitIterator.empty.head")
		override def next() :E = head
		override def skip() :Unit = ()

		override def drop(n :Int) :FitIterator[E] = this
		override def take(n :Int) :FitIterator[E] = this
		override def slice(from :Int, until :Int) :FitIterator[E] = this

		override def filter(p :E => Boolean, where :Boolean) :FitIterator[E] = this

		override def foreach[@specialized(Unit) U](f :E => U) :Unit = ()
		override def traverse(f :E => Unit) :Unit = ()

		override def map[@specialized(Fun1Vals) O](f :E => O) :FitIterator[O] = FitIterator.empty[O]
		override def flatMap[@specialized(Elements) O](f :E => GenTraversableOnce[O]) :FitIterator[O] = FitIterator.empty[O]

		override def copyToArray[B >: E](xs: Array[B], start: Int, len: Int): Unit = ()

		override def toSeq :FitSeq[E] = FitSeq.empty[E]
		override def toIndexedSeq :immutable.IndexedSeq[E] = StableArray.empty[E]
		override def toBuffer[U >: E]: FitBuffer[U] = FitBuffer.empty[U]
		override def toFitBuffer[U >: E : Specialized]: FitBuffer[U] = FitBuffer.empty[U]

		override def sameElements(that: Iterator[_]): Boolean = that.isEmpty

		override def toString :String = "FitIterator[" + mySpecialization + "]()"
	}


	/** An empty, erased iterator. */
	val Empty :FitIterator[Nothing] = new EmptyIterator[Nothing] { //todo: do we need this specialization info?
		override val mySpecialization :Specialized[Nothing] = Specialized[Nothing]
	}

	
	
	/** Not specialized base class for [[FitIterator]] implementations which iterates over
	  * some kind of indexed sequences (in a generic sense). It is assumed that [[index]]
	  * describes the position of the current element ([[FitIterator#head]]) and that advancing
	  * the iterator can be represented by increasing the index by one in [[skip]], until a
	  * predefined (but mutable) end index is reached (exclusive).
	  * Implementation of [[FitIterator#head]] and [[FitIterator#next]] is left to the
	  * subclasses in order to be specialized.
	  * @param index current position of the `head` element
	  * @param end end index for the iteration, `end>index` for non-empty iterators.
	  */
	abstract class IndexedIterator[+E](protected[this] final var index :Int, protected[this] final var end :Int)
		extends FastSizeIterator[E]
	{ this :FitIterator[E] =>
		@inline final override def size :Int = if (index>=end) 0 else end-index

		override def hasNext :Boolean = index<end
		
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
		
		
		override def toString :String =
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
		extends FastSizeIterator[E]
	{ this :FitIterator[E] =>
		
		@inline final override def size :Int = if (end>index) 0 else index-end +1

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
		
		override def toString :String =
			if (isEmpty) s"Iterator[$mySpecialization]#$index>>()"
			else s"Iterator[$mySpecialization]#$index>>($head, ?)"
	}
	

	
	
	private class ErasedIterator[+E](source :BufferedIterator[E]) extends BaseIterator[E] with FitIterator[E] {
		override def hasDefiniteSize :Boolean = source.hasDefiniteSize
		
		override def head: E = source.head

		override def next(): E = source.next()
		
		override def skip() :Unit = source.next()

		override def hasNext: Boolean = source.hasNext
		
		override def copyToArray[B >: E](xs: Array[B], start: Int, len: Int): Unit = source.copyToArray(xs, start, len)
	}
	
	
	
	private class SingletonIterator[@specialized(Elements) +E](val head :E) extends FastSizeIterator[E] with FitIterator[E] {
//		override def specialization: Specialized[_ <: E] = mySpecialization
		
		private[this] var left = 1
		override def skip() :Unit = left = 0
		override def next() :E = { left=0; head }
		override def hasNext :Boolean = left>0
		
		override def size :Int = left
		
		
		override def foreach[@specialized(Unit) U](f: E => U): Unit = f(head)
		override def traverse(f: E => Unit): Unit = f(head)
		
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
		
		override def toString :String =
			if (isEmpty) s"FitIterator[$mySpecialization]()"
			else s"FitIterator[$mySpecialization]($head)"
	}



	private final class DoubletonIterator[@specialized(Elements) +E](first :E, second :E) extends CountdownIterator[E](2) with FitIterator[E] {
		override def head: E = limit match {
			case 2 => first
			case 1 => second
			case _ => throw new NoSuchElementException(s"$this.head")
		}

		override def next(): E = { val res = head; skip(); res }

		override def skip(): Unit = limit-=1
	}


	abstract class CountdownIterator[+E](protected[this] final var limit :Int) extends FastSizeIterator[E] { this :FitIterator[E] =>
		override def size :Int = Math.max(0, limit)
		override def hasNext :Boolean = limit > 0
		override def ofAtLeast(items :Int) :Boolean = limit >= items
		
		override def take(n :Int) :FitIterator[E] =
			if (n>size) this
			else if (n<=0) empty
			else {
				limit -= n; this
			}
		
		override def drop(n :Int) :FitIterator[E] =
			if (n<=0) this
			else if (n>size) empty
			else {
				val until = size-n
				do { skip() } while (size > until)
				this
			}
	}



	class ConstIterator[@specialized(Elements) +E](override val head :E, count :Int) extends CountdownIterator[E](count) with FitIterator[E] {
		override def next(): E = { limit-= 1; head }
		override def skip(): Unit = limit -= 1
	}



	/** Iterator implementing a `take` operation on another iterator. Returns elements of `source` for as long as
	  * next element exists or a predefined element limit has been reached.
	  */
	class LimitedIterator[@specialized(Elements) +E](source :FitIterator[E], private[this] var limit :Int)
		extends BaseIterator[E] with FitIterator[E]
	{
		override def head: E = source.head
		
		override def skip() :Unit = { limit-=1; source.skip() }
		
		override def next(): E = { limit-=1; source.next() }
		
		override def hasNext :Boolean = limit>0 && source.hasNext
		
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
		override def hasFastSize :Boolean = limit<=1 || source.hasFastSize
		override def size :Int = if (limit<=0) 0 else limit min source.size
		override def isEmpty :Boolean = limit<=0 || source.isEmpty
		override def nonEmpty :Boolean = limit > 0 && source.nonEmpty
		override def ofAtLeast(elems :Int) :Boolean = limit >= elems && source.ofAtLeast(elems)


		override def copyToArray[B >: E](xs: Array[B], start: Int, len: Int): Unit =
			source.copyToArray(xs, start, len min limit)


		override def toString = s"$source.take($limit)"
	}

	
	
	class TakeWhileIterator[@specialized +E](source :FitIterator[E], p :E=>Boolean)
		extends BaseIterator[E] with FitIterator[E]
	{
		override def hasDefiniteSize :Boolean = source.hasDefiniteSize
		
		private[this] var hd: E = source.head
		private[this] var takeHead = p(hd)

		def hasNext :Boolean = takeHead
		
		override def head :E = hd
		
		override def next(): E = {
			val res = hd
			if (source.hasNext) hd = source.next()
			else takeHead = false
			res
		}
		
		override def skip(): Unit = {
			if (source.hasNext) hd = source.next()
			else takeHead = false
		}
		
	}
	

	class MappedIterator[X, @specialized(Fun1Vals) +E](f :X=>E)(protected var source :FitIterator[X])
		extends BaseIterator[E] with FitIterator[E]
	{
		override def head: E = source.mapHead(f) //f(source.head)
		override def next(): E = source.mapNext(f) //f(source.next())
		override def skip() :Unit = source.skip()
		override def hasNext :Boolean = source.hasNext

		override def size :Int = source.size
		override def hasDefiniteSize: Boolean = source.hasDefiniteSize
		override def hasFastSize :Boolean = source.hasFastSize
		override def isEmpty :Boolean = source.isEmpty
		override def nonEmpty :Boolean = source.nonEmpty
		override def ofAtLeast(size :Int) :Boolean = source.ofAtLeast(size)

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

		override def map[@specialized(Fun1Vals) O](g: E => O): FitIterator[O] =
			new MappedIterator[X, O]((x :X) => g(f(x)))(source)
	}





	private class ScanLeftIterator[@specialized(Fun2) +A, E](private[this] var acc :A, op :(A, E)=>A)(source :FitIterator[E])
		extends BaseIterator[A] with FitIterator[A]
	{
		override def hasFastSize :Boolean = source.hasFastSize
		override def hasDefiniteSize: Boolean =  source.hasDefiniteSize
		override def size :Int = 1 + source.size
		override def ofAtLeast(n :Int) :Boolean = n<=1 || source.ofAtLeast(n-1)
		override def hasNext :Boolean = hasMore

		private[this] var hasMore = true

		override def head: A = acc //op(acc, source.head)
		
		override def next(): A = {
			val res = acc
			if (source.hasNext) acc = source.scanNext(res, op) //op(res, source.next())
			else hasMore = false
			res
		}
		
		override def skip() :Unit = next() //avoid boxed calls on op
		
	}
	


	class FilterIterator[@specialized(Fun1) +E](p :E=>Boolean, ourTruth :Boolean = true)(source :FitIterator[E])
		extends BaseIterator[E] with FitIterator[E]
	{
		override def hasDefiniteSize :Boolean = source.hasDefiniteSize

		private[this] var hd :E = source.next()
		private[this] var hasMore = true

		if (p(head)!=ourTruth) next()

		def head :E = hd
		def hasNext :Boolean = hasMore
		
		/** Delegate to a specialized method. */
		def skip() :Unit = next()
		
		override def next() :E = {
			val res = hd
			while(source.hasNext) {
				val e = source.next()
				if (p(e)==ourTruth) {
					hd = e; return res
				}
			}
			hasMore = false
			res
		}
	
	}



	private class ConcatIterator[@specialized(Elements) +E](first :FitIterator[E], andThen: =>GenTraversableOnce[E])
		extends BaseIterator[E] with FitIterator[E]
	{
		private[this] var second :FitIterator[E] = _
		/** Iterated over all elements mine the first iterator. Needed because `size` might evaluate `second`
		  * before actually advancing to it. If true, implies that `second` has been initialized. */
		private[this] var firstDone = false

		private def evalSecond :FitIterator[E] = {
			if (second == null)
				second = FitIterator.adapt(andThen.toIterator)
			second
		}

		override def hasDefiniteSize :Boolean = first.hasDefiniteSize && second!=null && second.hasDefiniteSize
		override def hasFastSize :Boolean = first.hasFastSize && second!=null && second.hasFastSize

		override def size :Int = first.size + evalSecond.size

		override def hasNext :Boolean =
			if (firstDone) second.hasNext
			else if (first.hasNext) true
			else {
				firstDone = true
				second = FitIterator.adapt(andThen.toIterator)
				second.hasNext
			}

		override def head :E =
			if (firstDone)
				second.head
			else if (first.hasNext) first.head
			else {
				firstDone = true
				if (second==null)
					second = adapt(andThen.toIterator)
				second.head
			}

		override def next() :E =
			if (firstDone)
				second.next()
			else if (first.hasNext)
				first.next()
			else {
				firstDone = true
				if (second==null)
					second = FitIterator.adapt(andThen.toIterator)
				second.next()
			}

		override def skip() :Unit =
			if (firstDone) second.skip()
			else if (first.hasNext) first.skip()
			else {
				firstDone = true
				if (second==null)
					second = adapt(andThen.toIterator)
				second.skip()
			}
	}
}

