package net.turambar.palimpsest.specialty.iterators



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
	@inline final override def size :Int = if (index >= end) 0 else end - index

	override def hasNext :Boolean = index < end

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
		if (isEmpty) s"Iterator[$specialization]#$index>>()"
		else s"Iterator[$specialization]#$index>>($head, ?)"
}





/** Non specialized base class for [[FitIterator]] implementations which iterate over
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
		if (isEmpty) s"Iterator[$specialization]#$index>>()"
		else s"Iterator[$specialization]#$index>>($head, ?)"
}





