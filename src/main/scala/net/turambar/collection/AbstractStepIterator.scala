package net.turambar.collection

/**
  * @author Marcin Mościcki
  */
trait AbstractStepIterator[E] extends BufferedIterator[E]
{
	protected[this] var index :Int
	protected[this] var end :Int
	protected[this] val step :Int
	
	protected def aligned :this.type = { end = (end-index) / step * step; this }
	
	override def size :Int = (end-index)/step
	
	override def hasDefiniteSize: Boolean = (end-index) / step >= 0
	
	override def hasNext :Boolean = index != end
	
	override def next :E = { val res = head; index += step; res }
	
	override def take(n: Int): this.type = {
		if (n <= 0)
			end = index
		else if (n < size)
			end = index + n*step
		this
	}
	
	override def drop(n: Int): this.type = {
		if (n>0)
			if (n<size)
				index += step*n
			else
	            index = end
		this
	}
	
	override def slice(from: Int, until: Int): this.type = {
		if (until<=0 || from >= until)
			end = index
		else if (until<size) {
			end = index + until * step
			if (from>0)
				index += from*step
		}
		this
	}
	
	override def toString = s"Iterator[$index]($step->$end)"
}


object AbstractStepIterator {
	
	@inline def apply[E](from :Int, until :Int, by :Int=1)(item :Int => E) :Iterator[E] =
		new SubFunctionIterator[E](from, until, by)(item).aligned
		
	
	
	@inline def apply[E](range :Range)(item :Int=>E) :Iterator[E] =
		apply(range.start, range.end, range.step)(item)
	
	
	@inline def reverse[E](lowBound :Int, upperBound :Int, step :Int = -1)(item :Int => E) :Iterator[E] =
		apply(upperBound, lowBound, step)(item)
	
	
	
	class SubFunctionIterator[E](final protected[this] var index :Int, final protected[this] var end :Int, final protected[this] val step :Int)(f :Int=>E)
		extends AbstractStepIterator[E]
	{
		override def head: E = f(index)
	}
}