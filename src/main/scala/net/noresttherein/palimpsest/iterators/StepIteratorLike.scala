package net.noresttherein.palimpsest.iterators

/** A generic base trait for iterators over indexed collections with fast random access.
  * Each step of the iteration (call to [[Iterator#next]]) results in increasing the current index
  * by the predefined, constant step, until the end index is reached. This allows for iteration
  * in both directions, as well as skipping elements.
  *
  * @author Marcin Mościcki
  */
trait StepIteratorLike[+E, +I<:Iterator[E]] extends BufferedIterator[E]
{ this :I =>
	
	/** Index of the current element (`head`). */
	protected[this] var index :Int
	
	/** Index which, when reached, signals the end of the iteration:
	  * [[hasNext]] is defined as `index!=end`.
	  * May be larger or smaller than [[index]], depending on the direction of iteration and whether this iterator
	  * has definite size. Subclasses should ensure that the difference between `index` and and `end` is divisible by
	  * [[step]] (that is, that this index will be 'stepped on' and not 'stepped over'.
	  */
	protected[this] var end :Int
	
	/** Amount by which [[index]] should be increased with every step (in every [[next]] call). May be negative. */
	protected[this] val step :Int
	
	/** Adjusts [[end]] so that its distance to [[index]] is the largest multiplication of [[step]]
	  * not larger than current distance. Assumes non-zero `step`.
	  * @return `this`
	  */
	protected def aligned :I = { end = (end-index) / step * step; this }
	
	override def size :Int = (end-index)/step
	
	override def hasDefiniteSize: Boolean = (end-index) / step >= 0
	
	override def hasNext :Boolean = index != end
	
	override def next :E = { val res = head; index += step; res }
	
	override def take(n: Int): I = {
		if (n <= 0)
			end = index
		else if (n < size)
			end = index + n*step
		this
	}
	
	override def drop(n: Int): I = {
		if (n>0)
			if (n<size)
				index += step*n
			else
	            index = end
		this
	}
	
	override def slice(from: Int, until: Int): I = {
		if (until<=0 || from >= until)
			end = index
		else if (until<size) {
			end = index + until * step
			if (from>0)
				index += from*step
		}
		this
	}
	
	override def toString = s"Iter[>$step>#$index>|$end]"
}





object StepIteratorLike {
	
	@inline def apply[E](from :Int, until :Int, by :Int=1)(item :Int => E) :Iterator[E] =
		new SubFunctionIterator[E](from, until, by)(item).aligned
		
	
	
	@inline def apply[E](range :Range)(item :Int=>E) :Iterator[E] =
		apply(range.start, range.end, range.step)(item)
	
	
	@inline def reverse[E](lowBound :Int, upperBound :Int, step :Int = -1)(item :Int => E) :Iterator[E] =
		apply(upperBound, lowBound, step)(item)
	
	
	
	class SubFunctionIterator[+E](final protected[this] var index :Int, final protected[this] var end :Int, final protected[this] val step :Int)(f :Int=>E)
		extends StepIteratorLike[E, Iterator[E]]
	{
		override def head: E = f(index)
	}
}
