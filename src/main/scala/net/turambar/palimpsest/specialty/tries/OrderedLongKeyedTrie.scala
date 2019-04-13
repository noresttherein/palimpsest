package net.turambar.palimpsest.specialty.tries


/*
import net.turambar.palimpsest.specialty.{?, FitIterator, Sure}
import net.turambar.palimpsest.specialty
import net.turambar.palimpsest.specialty.ordered.{OrderedAs, OrderedBy, ValOrdering}
import net.turambar.palimpsest.specialty.ordered.OrderedBy.{OrderedEmpty, OrderedSingleton}
import net.turambar.palimpsest.specialty.ordered.ValOrdering.LongOrdering
import net.turambar.palimpsest.specialty.tries.LongTrieKeys.{flipSign, EmptyLongKeys, LongKeyBranch, LongTrie, LongKeyLeaf}
import net.turambar.palimpsest.specialty.tries.BinaryTrie.{seedStack, LeafKeyIterator}
import net.turambar.palimpsest.specialty.Var


/** A [[LongTrieKeys]] where all keys have their sign (highest) bit flipped before inserting to the trie.
  * As `LongTrieKeys` stores the keys according to 'unsigned' ordering of its keys, flipping their sign bit
  * leads to a trie which is ordered according to natural (two's complement) ordering of `Long`. This results in
  * getting an `OrderedSet`/`OrderedMap` essentially for free. An unfortunate effect is that the term 'key' may now
  * refer to both the internal, flipped key and its public value. In order to differentiate between the two,
  * all methods declared by a standard collection interface (or within [[OrderedBy]]) are assumed to refer to the keys
  * as naturally ordered, while all trie-specific methods work with the flipped values.
  *
  * This trait doesn't take a self type as a parameter in order to be able to serve as a concrete type argument,
  * being a fixed point `OrderedLongKeyedTrie &lt;: LongTrieKeys[OrderedLongKeyedTrie]`. All subclasses
  * representing empty trie, leaves and branches however do take one, which might be thus stated simply as this trait.
  *
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait OrderedLongKeyedTrie extends LongTrieKeys[OrderedLongKeyedTrie] with (OrderedLongKeyedTrie OrderedBy Long)
{  this :LongTrie =>
	/** Natural ordering on `Long` representing the order of keys in the trie after flipping their sign bit. */
	override implicit def ordering :ValOrdering[Long] = LongOrdering

	override def compare(e1 :Long, e2 :Long) :Int = if (e1 < e2) -1 else if (e1 == e2) 0 else 1
	override protected def lte(x :Long, y :Long) :Boolean = x <= y
	override protected def lt(x :Long, y :Long) :Boolean = x < y
	override protected def equiv(x :Long, y :Long) :Boolean = x == y
}


object OrderedLongKeyedTrie {
	import java.lang.Long.{MIN_VALUE => SignBit}

	abstract class FlippedEmptyLongTrie[+This <: LongTrieKeys[This] with OrderedBy[This, Long] with OrderedLongKeyedTrie]
		extends EmptyLongKeys[This] with OrderedLongKeyedTrie with OrderedEmpty[Long, This] //with LongOrdering
	{ this :This with LongTrie => }


	/** While all leaves in a [[LongTrieKeys]] followDown essentially unsigned ordering of their keys,
	  * it would often be more convenient if they followed natural (signed) ordering of the keys instead.
	  * It can be easily achieved by storing all keys with their sign (highest) bit flipped to the opposite value,
	  * thus swapping the left and right children of the root node responsible for the sign bit (if values of both signs are present in the trie).
	  * This trait is a mixin for leaves of such tries which wish to expose the keys as `key + Long.MinValue` instead.
	  * The `key` property is assumed to be already flipped, and all methods defined by [[Trie]]/[[LongTrieKeys]] are assumed
	  * to work for keys with their sign bit already flipped to avoid problems resulting from flipping it several times.
	  * Instead, the [[OrderedAs]] methods flip the sign bit of all their key arguments. This provides a discrepancy between
	  * what's stored and exposed by [[Trie#key]], as well as potential other methods, and the values accepted by order-related methods.
	  * It is up to implementing classes to resolve and hide it.
	  */
	trait FlippedLongLeaf[+This <: LongTrieKeys[This] with OrderedBy[This, Long] with OrderedLongKeyedTrie]
		extends LongKeyLeaf[Any, This] with OrderedLongKeyedTrie with OrderedSingleton[Long, This] //with LongOrdering
	{ this :This =>

		override def firstKey :Long = key ^ 0x8000000000000000L

/*
		override def contains(key :Long) :Boolean = key == firstKey


		/** The `n-th` 'virtual' key in this set, with the only valid value of `n` being `0` as a leaf
		  * is essentially a singleton collection.
		  * @param n index of the required element in the collection. If not `0`, an `IndexOutOfBoundsException` will be thrown.
		  * @return `key + Long.MinValue`
		  */
		override def keyAt(n: Int): Long =
			if (n==0) firstKey else throw new IndexOutOfBoundsException(s"$typeStringPrefix.keyAt($n)")

		/** An iterator over 'virtual' key(s), flipping the sign of the actual key for purpose of comparison with `start`. */
		override def keysIteratorFrom(start: Long): FitIterator[Long] = {
			val v = firstKey
			if (start <= v) FitIterator(v) else FitIterator.empty
		}
*/

		override def rangeImpl(from: ?[Long], until: ?[Long]): This = {
			val v = firstKey
			if ((from.isEmpty || from.get <= v) && (until.isEmpty || v < until.get))
				this
			else empty
		}

		override def from(from: Long) :This = if (from <= firstKey) this else empty

		override def until(until: Long) :This = if (firstKey < until) this else empty

		override def to(to :Long) :This = if (firstKey <= to) this else empty

		override def range(from: Long, until: Long) :This = {
			val v = firstKey
			if (from <= v && v < until) this else empty
		}
	}




	/** This variant of the `LongTrieKeys` treats all keys as if they had their sign (highest) bit flipped.
	  * This essentially swaps the 'negative' and 'positive' branch positions at the root, with negative values
	  * (now stored with the sign bit of `0`) sorting before all positive values. In effect, it changes
	  * the [[FlippedLongBranch#ordering]] to natural `Ordering.Long` instead of unsigned comparison
	  * default to `LongTrieKeys`. All ordering-related methods inherited from `Sorted` accept
	  * the 'virtual' key values, which get their sign bit flipped before delegating to implementations
	  * inherited from `LongKeyBranch`.
	  */
	trait FlippedLongBranch[+This <: LongTrieKeys[This] with OrderedBy[This, Long] with OrderedLongKeyedTrie]
		extends LongKeyBranch[This] with OrderedLongKeyedTrie with OrderedBy[This, Long] //with LongOrdering
	{ this :This =>
//		/** Natural ordering on `Long`, that is one defined by `Ordering.Long`. */
//		override implicit def ordering: ValOrdering[Long] = LongOrdering

//		protected[this] def newBranch(splitPath :Long, left :This, right :This) :This

		override def firstKey :Long = flipSign(headNode.key)
		override def lastKey :Long = flipSign(lastNode.key)


		/** n-th key in this collection treated as a sequence sorted by 'virtual' (flipped) keys.
		  * @param idx index of the leaf/element to retrieve.
		  * @return `flipSign(leaf(idx).key)`
		  */
		override def keyAt(idx: Int) :Long = try {
			flipSign(keyNode(Var[Int](idx)).key)
		} catch {
			case _ :NoSuchElementException => throw new IndexOutOfBoundsException(s"$typeStringPrefix.keyAt($idx)")
		}


		/** Checks if this trie contains a leaf for the corresponding key. */
		override protected def contains(key :Long) :Boolean = hasKey(flipSign(key))


		override def rangeImpl(from: ?[Long], until: ?[Long]): This =
			until match {
				case ub :Sure[Long] => range(from default Long.MinValue, ub.value)
				case _ => from match {
					case lb :Sure[Long] => this.from(lb.value)
					case _ => this
				}
			}

		override def from(from: Long) :This = fromKey(flipSign(from))

		override def to(end :Long) :This = toKey(flipSign(end))

		override def until(until: Long) :This = keyRange(0, flipSign(until))

		override def range(from: Long, until: Long) :This = keyRange(flipSign(from), flipSign(until))

		/** An iterator over 'virtual' key(s), flipping the sign of the actual key for purpose of comparison with `start`. */
		override def keysIteratorFrom(start: Long): FitIterator[Long] = {
			val flipped = flipSign(start)
			if (center >= 0 && (flipped < 0 || flipped > upperBound))
				FitIterator.empty
			else {
				val stack = new Array[BinaryTrie[Long, This]](64)
				val top = fillIteratorStack(flipped, stack)
				if (top < 0)
					FitIterator.empty
				else
					new FlippedKeyIterator[This](stack, top)
			}
		}

/*
			if (center >= 0 && start >= 0) //all our 'real' keys are negative
				FitIterator.empty
			else if (center < 0 && center != SignBit && start < 0) //all our 'real' keys are non-negative and `start` is negative
			     new FlippedKeyIterator(this)
			else {
				val keyStart = flipSign(start)
				val stack = new Array[BinaryTrie[Long, This]](64)
				//numerical comparisons of keys are equivalent to unsigned ordering of a trie iff compared numbers have the same sign.
				//Therefore, we need to ensure that start is of the same sign as values in the node we descend to.
				val depth =
					if (center == SignBit && keyStart >= 0) { //a singularity, center is the smallest long and not larger than left trie keys
						stack(0) = this //push ourselves so the iterator retraces back to the right subtrie once it finishes the left one
						fillIteratorStack(left, keyStart, stack, 1) //force search in the left subtree (containing keys with `0` sign bit like start)
					} else //now we are sure that keyStart will have the same sign as all values within requested range
	                    fillIteratorStack(this, keyStart, stack, 0) //handles also case path=0x8000000000000000L && start >=0
				if (depth<0)
					FitIterator.empty
				else
					new FlippedKeyIterator[This](stack, depth)
			}
*/


	}


	/** An iterator over keys in a `LongTrieKeys`, flipping the sign of each key before returning it.
	  * Extracted here to facilitate reuse.
	  */
	class FlippedKeyIterator[T<:BinaryTrie[Long, T] with LongTrieKeys[T]](stack :Array[BinaryTrie[Long, T]], top :Int)
		extends LeafKeyIterator[Long, T](stack, top)
	{
		def this(trie :LongTrieKeys[T]) = this(seedStack(trie, 64), 0)
		override def head :Long = flipSign(topNode.key)
		override def next() :Long = { val res = flipSign(topNode.key); skip(); res }
	}

}
*/
