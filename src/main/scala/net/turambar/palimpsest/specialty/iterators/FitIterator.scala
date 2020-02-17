package net.turambar.palimpsest.specialty.iterators

import net.turambar.palimpsest.specialty.{?, iterators, Blank, ItemTypes, FitTraversableOnce, FunctorLike, RuntimeType, Sure}
import net.turambar.palimpsest.specialty.RuntimeType.Specialized.{Fun1, Fun1Vals, Fun2}
import net.turambar.palimpsest.specialty.iterables.{FitIterable, IterableSpecialization}
import net.turambar.palimpsest.specialty.seqs.{FitBuffer, FitSeq, StableArray}
import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize
import net.turambar.palimpsest.specialty.iterators.FitIterator.{ConcatIterator, FilterIterator, LimitedIterator, MappedIterator, ScanLeftIterator, TakeWhileIterator}

import scala.annotation.{tailrec, unspecialized}
import scala.collection.{immutable, mutable, AbstractIterator, BufferedIterator, GenTraversableOnce, Iterator}



trait IteratorTemplate[+E] extends Iterator[E] with FitTraversableOnce[E] with FunctorLike[E, FitIterator] { self :FitIterator[E] =>

	override def hasFastSize :Boolean = !hasNext
	override def isEmpty :Boolean = !hasNext
	override def nonEmpty :Boolean = hasNext

	override def ofAtLeast(size :Int) :Boolean = {
		@tailrec def enumerate(n :Int) :Boolean =
			n<=0 || hasNext && (n==1 || { skip(); enumerate(n-1) })
		enumerate(size)
	}


	def headOption :Option[E] = if (hasNext) Some(head) else None



	override def traverse(f :E => Unit) :Unit = foreach[Unit](f) //dear compiler, please use the specialized version!

	override def forall(p: E => Boolean): Boolean = { dropWhile(p); isEmpty }
	override def exists(p: E => Boolean): Boolean = { dropWhile(p, false); hasNext }
	override def indexWhere(p: E => Boolean): Int = indexWhere(p, ourTruth=true)


	override def indexOf[B >: E](elem: B): Int =
		if (specialization.boxType.isAssignableFrom(elem.getClass))
			positionOf(elem.asInstanceOf[E])
		else
			super[Iterator].indexOf(elem)

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

	override def flatMap[@specialized(ItemTypes) B](f :E => GenTraversableOnce[B]) :FitIterator[B] =
		if (!hasNext)
			FitIterator.empty[B]
		else //todo: will mapHead specialize on the argument if result type is a reference?
			new ConcatIterator[B](FitIterator.adapt(mapHead(f).toIterator), flatMap(f))

//	@migration("release", "use :++ as ++ cannot be specialized")
//	override def ++[U >: E](andThen: => GenTraversableOnce[U]) :Iterator[U] = super.++(andThen)



	override def toSeq :FitSeq[E] = (FitSeq.builder(specialization) ++= this).result()

	//todo: 'optimistic specialization'
	override def toBuffer[U>:E] :FitBuffer[U] = toFitBuffer

	override def toIterator :FitIterator[E] = this

	def toFitBuffer[U>:E :RuntimeType] :FitBuffer[U] = FitBuffer.of[U] ++= this



	override def copyToArray[B >: E](xs: Array[B], start: Int, len: Int): Unit =
		if (start<0)
			throw new IllegalArgumentException(s"$this.copyToArray(${xs.getClass.getComponentType.getSimpleName}[${xs.length}], $start, $len)")
		else if (specialization.emptyArray.getClass.isAssignableFrom(xs.getClass)) //todo
			specializedCopy(xs.asInstanceOf[Array[E]], start, len)
		else {
			var i = start; val end = Math.min(xs.length, start+len)
			while(i<end && hasNext) { xs(i) = next(); i+=1 }
		}



	override def toString :String =
		if (isEmpty) s"Iterator[$specialization]()"
		else s"Iterator[$specialization]($head, ???)"

}





/** This is a `@specialized` version of scala `Iterator`, which allows to eliminate boxing of each element
  * for primitives and automatically forces the compiler to generate specialized versions of
  * methods with this interface in their signature (assuming their declaring class/trait is specialized on `E`).
  * Apart from being already a `BufferedIterator` (to eliminate number of types duplicated from generic scala APIs),
  * it declares [[FitIterator#hasFastSize]] which can be used in concatenation or comparison code to check
  * if calling `size` is not only safe but also sensible.
  *
  * It is recommended that in order to minimize overhead of static forwarding implementors:
  *   1.  extend [[iterators.BaseIterator]]
  *       to inherit default implementations of methods which don't need specializations, or can reuse minimized
  *       specialized API of this interface
  *   2.  implement [[FitIterator#head]], [[FitIterator#next]], [[FitIterator#hasNext]] and optionally [[FitIterator#skip]]
  *       in the final concrete class, rather than another inherited trait
  *
  * @author Marcin Mościcki
  */
trait FitIterator[@specialized(ItemTypes) +E]
	extends BufferedIterator[E] with FitTraversableOnce[E] with IteratorTemplate[E]//with FilterMonadic[E, FitIterator[E]]
{ self =>

	@unspecialized
	override final def runtimeType :RuntimeType[_<:E] = specialization

	protected[this] def specialization :RuntimeType[E] = RuntimeType.specialized[E]



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

	/** Returns the `n`-th element of this iterator. The state of the iterator after the method returns is undefined
	  * and it should be discarded.
	  * @throws IndexOutOfBoundsException if n < 0 or this iterator has fewer than n elements.
	  */
	def get(n :Int) :E = {
		if (n < 0)
			throw new IndexOutOfBoundsException(n)
		val i = drop(n)
		if (i.hasNext)
			i.next()
		else
			throw new IndexOutOfBoundsException(n)
	}



	/** Applies the given function to `head` element of this iterator. Exists for the benefit of
	  * [[iterators.FitIterator.MappedIterator]], so it can be specialized only on its
	  * return type, rather than all `(E=>O)` pairs.
	  */
	private[specialty] def mapHead[@specialized(Fun1Vals) O](f :E=>O) :O = f(head)

	/** Applies the given function to `next()` element of this iterator, advancing it. Exists for the benefit of
	  * [[iterators.FitIterator.MappedIterator]], so it can be specialized only on its
	  * return type, rather than all `(E=>O)` pairs.
	  */
	private[specialty] def mapNext[@specialized(Fun1Vals) O](f :E=>O) :O = f(next())

	/** Applies the given function to the passed first argument and the next element of this iterator, advancing it.
	  * Exists for the benefit of [[FitIterator.ScanLeftIterator]], to avoid specializing it on both source and
	  * result element types.
	  */
	private[specialty]def scanNext[@specialized(Fun2) O](acc :O, f :(O, E) => O) :O = f(acc, next())




	//overriden for specialization
	override def take(n :Int) :FitIterator[E] =
		if (n<=0) empty
		else if (hasFastSize && size <= n) this
		else new LimitedIterator(this, n)


	protected def dropWhile(p :E => Boolean, is :Boolean) :FitIterator[E] = {
		while(hasNext && p(head) == is) skip(); this
	}
	

	override def takeWhile(p: E => Boolean): FitIterator[E] =
		if (!hasNext) this
		else new TakeWhileIterator[E](this, p)
		
	
	def filter(p :E=>Boolean, ourTruth :Boolean) :FitIterator[E] =
		if (!hasNext) this
		else new FilterIterator(p, ourTruth)(this)


//	override def map[@specialized(Fun1Vals) B](f: E => B): FitIterator[B] = new MappedIterator(f)(this)




	override def foreach[@specialized(Unit) U](f: E => U): Unit = {
		while (hasNext) f(next())
	}


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
		case s :FitIterator[_] if s.runtimeType==runtimeType =>
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





/** An iterator allowing update of the iterated collection. */
trait MutableIterator[@specialized(ItemTypes) E] extends FitIterator[E] {

	/** Changes the element that would be returned by the call to [[FitIterator.head]].
	  * The iterator will remain at the same position. This will update the underlying collection and a subsequent call
	  * to `next()` will return the passed value.
	  * @param elem a replacement for the next element in the iterated collection.
	  * @throws `NoSuchElementException` if the iterator is empty.
	  */
	def head_=(elem :E) :Unit

	/** Changes the element that would be returned by the call to [[net.turambar.palimpsest,specialty.FitIterator.head]]
	  * and advances over in the same as a call to [[FitIterator.next()]] would.
	  * @param elem a replacement for the next element in the iterated collection.
	  * @throws `NoSuchElementException` if the iterator is empty.
	  */
	def next_=(elem :E) :Unit = { head = elem; skip() }

}





object FitIterator {


	/** An empty, erased iterator. */
	val Empty :FitIterator[Nothing] = new EmptyIterator[Nothing]

	/** An empty iterator, specialized for its declared element type.
	  * @see [[FitIterator.Empty]] an empty iterator with erased element type.
	  */
	def empty[@specialized(ItemTypes) E] :FitIterator[E] = new EmptyIterator[E] //Empty

	/** An iterator over a single element. */
	def one[@specialized(ItemTypes) E](value :E) :FitIterator[E] = new SingletonIterator[E](value)

	/** An iterator over two values. */
	def two[@specialized(ItemTypes) E](first :E, second :E) :FitIterator[E] = new DoubletonIterator[E](first, second)


	/** An iterator which returns the results of recursively applying the given function to its own result. */
	def iterate[@specialized(Fun1) E](f :E => E)(head :E) :FitIterator[E] = new RecursiveIterator[E](head, f)

	/** An iterator over `times` copies of `elem`. */
	def repeated[@specialized(ItemTypes) E](elem :E, times :Int) :FitIterator[E] = new ConstIterator[E](elem, times)



	/** Wraps the given iterator in an erased `FitIterator` unless it already is a `FitIterator`. */
	def adapt[@specialized(ItemTypes) E](iter :Iterator[E]) :FitIterator[E] = iter match {
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





	/** Implicit conversion from `FitIterator[E]` which adds a concatenation operation `:++`. Note that standard
	  * iterator's method `++` is generic with a lower bound for element type which makes it impossible to specialize.
	  */
	implicit class IteratorConcatenation[@specialized(ItemTypes) E](first :FitIterator[E]) {
		def :++(andThen: =>GenTraversableOnce[E]) :FitIterator[E] = new ConcatIterator[E](first, andThen)
	}





	private class EmptyIterator[@specialized(ItemTypes) E] extends FastSizeIterator[E] with FitIterator[E] {
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
		override def flatMap[@specialized(ItemTypes) O](f :E => GenTraversableOnce[O]) :FitIterator[O] = FitIterator.empty[O]

		override def copyToArray[B >: E](xs: Array[B], start: Int, len: Int): Unit = ()

		override def toSeq :FitSeq[E] = FitSeq.empty[E]
		override def toIndexedSeq :immutable.IndexedSeq[E] = StableArray.empty[E]
		override def toBuffer[U >: E]: FitBuffer[U] = FitBuffer.empty[U]
		override def toFitBuffer[U >: E : RuntimeType]: FitBuffer[U] = FitBuffer.empty[U]

		override def sameElements(that: Iterator[_]): Boolean = that.isEmpty

		override def toString :String = "FitIterator[" + specialization + "]()"
	}





	private class SingletonIterator[@specialized(ItemTypes) +E](val head :E) extends FastSizeIterator[E] with FitIterator[E] {

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
			if (isEmpty) s"FitIterator[$specialization]()"
			else s"FitIterator[$specialization]($head)"
	}



	private final class DoubletonIterator[@specialized(ItemTypes) +E](first :E, second :E) extends CountdownIterator[E](2) with FitIterator[E] {
		override def head: E = limit match {
			case 2 => first
			case 1 => second
			case _ => throw new NoSuchElementException(s"$this.head")
		}

		override def next(): E = { val res = head; skip(); res }

		override def skip(): Unit = limit-=1
	}





	private class ConstIterator[@specialized(ItemTypes) +E](override final val head :E, count :Int)
		extends CountdownIterator[E](count) with FitIterator[E]
	{
		override def next(): E = { limit-= 1; head }
		override def skip(): Unit = limit -= 1
	}



	private class RecursiveIterator[@specialized(Fun1) +E](private[this] var hd :E, f :E => E)
		extends BaseIterator[E] with FitIterator[E]
	{
		override def hasDefiniteSize = false
		override def hasFastSize = false
		override def hasNext = true

		override def head :E = hd

		override def next() :E = { val res = hd; hd = f(hd); res }

		override def skip() :Unit = skip(this) //use speciaized code

		private[this] def skip(it :FitIterator[E]) :Unit = { hd = f(hd) }
	}









	private class ErasedIterator[+E](source :BufferedIterator[E]) extends BaseIterator[E] with FitIterator[E] {
		override def hasDefiniteSize :Boolean = source.hasDefiniteSize
		
		override def head: E = source.head

		override def next(): E = source.next()
		
		override def skip() :Unit = source.next()

		override def hasNext: Boolean = source.hasNext
		
		override def copyToArray[B >: E](xs: Array[B], start: Int, len: Int): Unit = source.copyToArray(xs, start, len)
	}





	/** Iterator implementing a `take` operation on another iterator. Returns elements of `source` for as long as
	  * next element exists or a predefined element limit has been reached.
	  */
	private[iterators] class LimitedIterator[@specialized(ItemTypes) +E](source :FitIterator[E], private[this] var limit :Int)
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

	



	private class TakeWhileIterator[@specialized +E](source :FitIterator[E], p :E=>Boolean)
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
	




	class MappedIterator[X, @specialized(Fun1Vals) +E](f :X=>E)(protected final var source :FitIterator[X])
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

		override def skip() :Unit = skip(this) //for specialization

		private[this] def skip(it :FitIterator[A]) :Unit =
			if (source.hasNext) acc = source.scanNext(acc, op)
			else hasMore = false
	}





	final class FilterIterator[@specialized(Fun1) +E](p :E=>Boolean, ourTruth :Boolean = true)(source :FitIterator[E])
		extends BaseIterator[E] with FitIterator[E]
	{
		override def hasDefiniteSize :Boolean = source.hasDefiniteSize

		private[this] var hd :E = source.next()
		private[this] var hasMore = true

		if (p(head)!=ourTruth) next()

		def head :E = hd
		def hasNext :Boolean = hasMore
		

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

		override def skip() :Unit = skip(this) //for specialization

		private[this] def skip(it :FitIterator[E]) :Unit = next()
	
	}



	private[iterators] final class ConcatIterator[@specialized(ItemTypes) +E](first :FitIterator[E], andThen: =>GenTraversableOnce[E])
		extends BaseIterator[E] with FitIterator[E]
	{
		private[this] var second :FitIterator[E] = _
		/** Iterated over all elements from the first iterator. Needed because `size` might evaluate `second`
		  * before actually advancing to it. If true, implies that `second` has been initialized. */
		private[this] var firstDone = false

		@unspecialized
		@inline private def evalSecond :FitIterator[E] = {
			if (second == null)
				second = FitIterator.adapt(andThen.toIterator)
			second
		}

		@unspecialized
		private def hop() :FitIterator[E] = {
			firstDone = true
			evalSecond
		}

		override def hasDefiniteSize :Boolean = first.hasDefiniteSize && second!=null && second.hasDefiniteSize
		override def hasFastSize :Boolean = first.hasFastSize && second!=null && second.hasFastSize

		override def size :Int = first.size + evalSecond.size


		override def hasNext :Boolean =
			if (firstDone) second.hasNext
			else if (first.hasNext) true
			else hop().hasNext


		override def head :E =
			if (firstDone) second.head
			else if (first.hasNext) first.head
			else hop().head


		override def next() :E =
			if (firstDone) second.next()
			else if (first.hasNext) first.next()
			else hop().next()



		override def skip() :Unit =
			if (firstDone) second.skip()
			else if (first.hasNext) first.skip()
			else hop().skip()

	}

}

