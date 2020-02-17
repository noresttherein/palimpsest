package net.noresttherein.palimpsest.iterators

import net.noresttherein.palimpsest.ItemTypes


/** A bidirectional iterator, declaring the methods for moving in the opposite direction in the iterated collection.
  * As with iterator, a cursor always points between two elements of the collection (or between an element and a boundary
  * of the collection, or nowhere in case of empty iterators). When moving in the opposite direction, [[AptIterator.head]]
  * will return the previously returned element by [[net.noresttherein.palimpsest.iterators.Cursor#prev()]]
  * (or, more correctly, the element following the element which would be returned by the next call to `prev()`).
  * The `head` method itself does not have an equivalent. As with normal iterators, `size`, if defined, specifies the
  * number of elements in front of the cursor, but will generally advance to the 'forward' element of the collection while doing so.
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait Cursor[@specialized(ItemTypes) +E] extends AptIterator[E] {
	/** The opposite of the standard `next()` method of iterators: moves the cursor back in the structure by one element
	  * and returns it.
	  * @return element preceding `this.head` in the iterated collection.
	  */
	def prev() :E

	/** The opposite of the standard `hasNext` method of iterators: verifies that there are more elements before this
	  * cursor. If this method returns  `true`, [[net.noresttherein.palimpsest.Cursor#prev()]] will safely return
	  * the element preceding [[AptIterator#head]]
	  * @return `true` if `prev()`
	  */
	def hasPrev :Boolean

	/** The opposite of the standard `skip()` method of iterators: moves the cursor back in the structure by one element,
	  * backtracking over the element which would be returned by [[net.noresttherein.palimpsest.Cursor#prev]]
 	  */
	def back() :Unit
}
