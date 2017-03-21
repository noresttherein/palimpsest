package net.turambar.palimpsest.specialty.tries

import net.turambar.palimpsest.specialty.IterableTemplate
import net.turambar.palimpsest.specialty.tries.Trie.TrieOperator
import net.turambar.palimpsest.specialty.tries.TrieAdapter.Counter

/**
  * @author Marcin Mościcki
  */
trait MappingTrie[+K, +V, +E, +T <: TrieTemplate[_, _, _, T], +This <: TrieTemplate[K, V, E, This]] extends TrieTemplate[K, V, E, This] {

	private[this] type SourceBranch = BinaryTrie[_, _, T]
	private[this] type SourceLeaf = TrieLeaf[_, _, T]
	private[this] type EmptySource = EmptyTrie[_, _, T]

	protected[this] def source :T
	protected[this] def fromSource(adaptee :T) :This
//	protected[this] def fromSource(adaptee :T, sizeDelta :Int) :This
//	protected[this] def fromSource(parent :BinaryTrie[_, _, T], left :This, right :This) :This
	protected[this] def fromSource(parent :T, left :This, right :This) :This
//	protected[this] def fromSource(parent :BinaryTrie[_, _, T], left :This, right :This, size :Int) :This



	@inline final protected[this] def forSource[@specialized(Unit, Boolean) O](f :This => O) :T=>O =
		leaf => f(fromSource(leaf))

	protected[this] def compose[@specialized(Unit, Boolean) O](f :E=>O) :T=>O


	override def plurality: Int = source.plurality
	override def size = source.size
	override def ofAtLeast(size: Int): Boolean = source.ofAtLeast(size)


	override def leaf(n: Int): This = fromSource(source.leaf(n))

	override def trieHead: This = fromSource(source.trieHead)
	override def trieLast: This = fromSource(source.trieLast)


	override def foreach[@specialized(Unit) U](f: (E) => U) :Unit =
		source.forEachLeaf(compose(f).asInstanceOf[T=>Unit])

	override def forEachLeaf(f: (This) => Unit): Unit = source.forEachLeaf(forSource(f))


	override def forall(p: (E) => Boolean) :Boolean = source.forAllLeaves(compose(p))

	override def forAllLeaves(f: (This) => Boolean): Boolean = source.forAllLeaves(forSource(f))

	override def exists(p :E => Boolean) :Boolean = source.existsLeaf(compose(p))

	override def existsLeaf(p :This => Boolean) :Boolean = source.existsLeaf(forSource(p))

	override def find(p: (E) => Boolean) :Option[E] = {
		val leaf = source.findLeaf(compose(p))
		if (leaf.isEmpty) None
		else Some(compose(identity[E])(leaf))
	}

	override def findLeaf(p :This => Boolean) :This = fromSource(source.findLeaf(forSource(p)))

	override def tail :This = {
		def drop1(trie :T) :This = trie match {
			case bin :SourceBranch =>
				val l = drop1(bin.left)
				if (l eq null) fromSource(bin.right)
				else fromSource(trie, l, fromSource(bin.right))
			case _ :SourceLeaf => null.asInstanceOf[This]
			case _ => //handles empty tries in particular
				val res = trie.tail
				if (res.isEmpty) null.asInstanceOf[This]
				else fromSource(res)
		}
		drop1(source)
	}

	override def init :This = {
		def dropRight1(trie :T) :This = trie match {
			case bin :SourceBranch =>
				val r = dropRight1(bin.right)
				if (r eq null) fromSource(bin.left)
				else fromSource(trie, fromSource(bin.left), r)
			case _ :SourceLeaf => null.asInstanceOf[This]
			case _ => //handles empty tries in particular
				val res = trie.init
				if (res.isEmpty) null.asInstanceOf[This]
				else fromSource(res)
		}
		dropRight1(source)
	}




	/** Private recursive `drop` implementation lifting result from `T` to `This`.
	  * @param trie trie to drop from
	  * @param n number of leaves to drop, `0 < n < size`
	  */
	protected[this] final def fastDrop(trie :T, n :Int) :This = trie match {
		case bin :SourceBranch =>
			val lsize = bin.left.size
			if (n < lsize) fromSource(trie, fastDrop(bin.left, n), fromSource(bin.right))
			else if (n==lsize) fromSource(bin.right)
			else fastDrop(bin.right, n - lsize)
//			case _ :TrieLeaf[K, V, T] => can't happen because of invariant n > 0 && n < trie.size
		case _ =>
			fromSource(trie.drop(n))
	}

	/** Recursive, linear/sequential dropping of front leaves which avoids calls to `size`.
	  * Implementation for `drop` used when `!this.hasFastSize`.
	  * @param trie trie to drop from
	  * @param countdown in/out counter of front leaves to drop, `0 < n`
	  * @return a trie resulting from dropping `countdown` leafs from `trie` and converting to `This`,
	  *         or `null` to denote empty result (dropping the whole trie) for efficiency.
	  */
	protected[this] final def slowDrop(trie :T, countdown :Counter) :This = trie match {
		case bin :SourceBranch =>
			val l = slowDrop(bin.left, countdown)
			if (countdown.get == 0)
				if (l eq null) fromSource(bin.right)
				else fromSource(trie, l, fromSource(bin.right))
			else
				slowDrop(bin.right, countdown)
		case _ :SourceLeaf => //countdown > 0
			countdown.--; null.asInstanceOf[This]
		case _ :EmptySource => null.asInstanceOf[This]
//		case _ if trie.plurality==1 =>
//			countdown.--; null.asInstanceOf[This]
		case _ => //won't happen if the trie consists only of proper inner nodes with arity 2, but you can't trust the subclasses
			val t = trie.drop(countdown.get)
			if (t.nonEmpty) {
				countdown := 0; fromSource(t)
			} else {
				countdown -= trie.size; null.asInstanceOf[This]
			}
	}

	override def drop(n: Int) :This =
		if (n<=0) asTrie
		else if (source.hasFastSize)
			if (n >= source.size) empty else fastDrop(source, n)
		else if (hasFastSize && n >= size)
			empty
		else {
			val res = slowDrop(source, new Counter(n))
			if (res eq null) empty else res
		}


	protected[this] final def slowTakeRight(trie :T, countdown :Counter) :This = trie match {
		case bin :SourceBranch =>
			val r = slowTakeRight(bin.right, countdown)
			if (countdown.get == 0) r
			else if (r eq null) slowTakeRight(bin.right, countdown) //shouldn't happen if there are no empty nodes in non-empty tries
			else {
				val l = slowTakeRight(bin.left, countdown)
				if (l eq null) r //shouldn't happen if there are no empty nodes in non-empty tries
				else fromSource(trie, l, r)
			}
		case _ :SourceLeaf => //countdown > 0
			countdown.--; fromSource(trie)
		case _ :EmptySource => null.asInstanceOf[This] //shouldn't happen if there are no empty nodes in non-empty tries
		case _ =>
			val t = trie.takeRight(countdown.get)
			countdown -= t.size; fromSource(t)
	}

	override def takeRight(n :Int) :This =
		if (n<=0) empty
		else if (source.hasFastSize)
			if (n >= source.size) asTrie else fastDrop(source, source.size - n)
		else if (hasFastSize)
			if (n >= size) asTrie else slowDrop(source, new Counter(size-n))
		else slowTakeRight(source, new Counter(n))



	protected[this] final def fastTake(trie :T, n :Int) :This = trie match {
		case bin :SourceBranch =>
			val lsize = bin.left.size
			if (n < lsize) fastTake(bin.left, n)
			else if (n==lsize) fromSource(bin.left)
			else fromSource(trie, fromSource(bin.left), fastTake(bin.right, n - lsize))
//			case _ :TrieLeaf[K, V, T] => //can't happen as n > 0 && n < trie.size
		case _ =>
			fromSource(trie.take(n))
	}

	protected[this] final def slowTake(trie :T, counter :Counter) :This = trie match {
		case bin :SourceBranch =>
			val l = slowTake(bin.left, counter)
			if (counter.get == 0) l
			else if (l eq null) //bin.left.isEmpty
				slowTake(bin.right, counter)
			else fromSource(trie, l, slowTake(bin.right, counter))
		case _ :SourceLeaf => //counter > 0
			counter.--; fromSource(trie)
		case _ :EmptySource => null.asInstanceOf[This]
		case _ =>
			val res = trie.take(counter.get)
			counter -= res.size
			fromSource(res)
	}


	override def take(n: Int) :This =
		if (n<=0) empty
		else if (source.hasFastSize)
			if (n>=source.size) empty else fastTake(source, n)
		else if (hasFastSize && n>=size)
			asTrie
		else {
			val res = slowTake(source, new Counter(n))
			if (res eq null) empty else res
		}

	protected[this] final def slowDropRight(trie :T, countdown :Counter) :This = trie match {
		case bin :SourceBranch =>
			val r = slowDropRight(bin.right, countdown)
			if (r eq null)
				if (countdown.get == 0) fromSource(bin.left)
				else slowDropRight(bin.left, countdown)
			else
				fromSource(trie, fromSource(bin.left), r) //countdown == 0
		case _ :SourceLeaf => //countdown > 0
			countdown.--; null.asInstanceOf[This]
		case _ :EmptySource => null.asInstanceOf[This]

		case _ =>
			val res = trie.dropRight(countdown.get)
			if (trie.nonEmpty) {
				countdown := 0; fromSource(res)
			} else {
				countdown -= trie.size; null.asInstanceOf[This]
			}
	}



	override def dropRight(n :Int) :This =
		if (n <= 0) asTrie
		else if (source.hasFastSize) {
			val total = source.size
			if (total <= n) empty else fastTake(source, total-n)
		} else if (hasFastSize) {
			val total = size
			if (total <= n) empty else slowTake(source, new Counter(total-n))
		} else
			slowDropRight(source, new Counter(n))




	protected[this] def fastSlice(trie :T, from :Int, until :Int) :This = trie match {
		case bin :SourceBranch =>
			val lsize = bin.left.size
			if (until <= lsize) fastSlice(bin.left, from, until)
			else if (from >= lsize) fastSlice(bin.right, from-lsize, until-lsize)
			else fromSource(trie, fastDrop(bin.left, from), fastTake(bin.right, until-lsize))
		case _ :SourceLeaf => fromSource(trie) //as until-from > 0
		case _ => fromSource(trie.slice(from, until))
	}

	protected[this] def slowSlice(trie :T, from :Counter, length :Counter) :This = trie match {
		case bin: SourceBranch =>
			val front = slowSlice(bin.left, from, length)
			if (length.get == 0) //from.get==0 || front==null
				front
			else if (front eq null) //original from > bin.left.size
				slowSlice(bin.right, from, length)
			else
				fromSource(trie, front, slowSlice(bin.right, from, length))
		case _: SourceLeaf =>
			if (from.get > 0) {
				from.--; null.asInstanceOf[This]
			} else { //invariant length.get > 0
				length.--; fromSource(trie)
			}
		case _: EmptySource =>
			null.asInstanceOf[This]

		case _ =>
			val res = trie.slice(from.get, from.get + length.get)
			if (res.isEmpty) {
				from -= trie.size; null.asInstanceOf[This]
			} else {
				from := 0; length -= res.size
				fromSource(res)
			}




	}

	override def slice(from: Int, until: Int) :This =
		if (until<=0 || until<=from)
			empty
		else if (source.hasFastSize)
			if (source.size <= from) empty
			else fastSlice(source, from, until)
		else if (hasFastSize && from >= size)
			empty
		else slowSlice(source, new Counter(if (from <= 0) 0 else from), new Counter(until-from))


	protected[this] final def fastSplit(trie :T, pos :Int) :(This, This) = trie match {
		case bin :SourceBranch =>
			val lsize = bin.left.size
			if (pos < lsize) {
				val (l, rem) = fastSplit(bin.left, pos)
				(l, fromSource(trie, rem, fromSource(bin.right)))
			} else if (pos==lsize)
				(fromSource(bin.left), fromSource(bin.right))
			else {
				val (init, r) = fastSplit(bin.right, pos-lsize)
				(fromSource(trie, fromSource(bin.left), init), r)
			}
		//below code should theoretically be dead, as we handle cases of full trie and empty trie before descending deeper
		case _ :SourceLeaf => (fromSource(trie), empty)
		case _ =>
			val (l, r) = trie.splitAt(pos)
			(fromSource(l), fromSource(r))
	}

	protected[this] def slowSplit(trie :T, countdown :Counter) :(This, This) = trie match {
		case bin: SourceBranch =>
//			val at = countdown.get
			val (first, second) = slowSplit(bin.left, countdown)
			if (countdown.get == 0)
				if (second == null) (first, fromSource(bin.right))
				else (first, fromSource(trie, second, fromSource(bin.right)))
			else if (first==null) //<=> bin.left.isEmpty => second==null
				slowSplit(bin.right, countdown)
			else { //second.isEmpty
				val (tail, last) = slowSplit(bin.right, countdown)
				(fromSource(trie, first, tail), last)
			}
		case _ :SourceLeaf => //counter > 0
			countdown.--; (fromSource(trie), null.asInstanceOf[This])
		case _ :EmptySource => val e =  empty; (e, e)
		case _ =>
			val (l, r) = trie.splitAt(countdown.get)
			if (r.isEmpty) {
				countdown := 0
				(fromSource(l), null.asInstanceOf[This])
			} else {
				countdown -= l.size
				(fromSource(l), fromSource(r))
			}

	}

	override def splitAt(idx: Int) :(This, This) =
		if (idx<=0) (empty, asTrie)
		else if (hasFastSize && idx >= size) (asTrie, empty)
		else if (source.hasFastSize) fastSplit(source, idx)
		else slowSplit(source, new Counter(idx))





	private[palimpsest] override def properPrefixOrNull(f: (This) => Boolean): This = takePrefixOrNull(forSource(f))


	@inline final protected[this] def takePrefixOrNull(p :T => Boolean) :This = {
		var tookAll = false
		def takeFrom(trie :T) :This = trie match {
			case bin: SourceBranch =>
				val l = takeFrom(bin.left)
				if (tookAll) {
					val r = takeFrom(bin.right)
					if (r eq null) l
					else if (l eq null) r
					else fromSource(trie, l, r)
				} else l
			case _: SourceLeaf =>
				if (p(trie)) {
					tookAll = true; fromSource(trie)
				} else {
					tookAll = false; null.asInstanceOf[This]
				}
			case _: EmptySource =>
				tookAll = true; null.asInstanceOf[This]
			case _ =>
				val res = trie.properPrefixOrNull(p)
				if (res eq null) {
					tookAll = true
					if (trie.isEmpty) null.asInstanceOf[This]
					else fromSource(trie)
				} else {
					tookAll = false
					fromSource(res)
				}
		}
		val res = takeFrom(source)
		if (tookAll) null.asInstanceOf[This]
		else if (res eq null) empty
		else res
	}

	override def takeWhile(p: (E) => Boolean) :This = {
		val res = takePrefixOrNull(compose(p))
		if (res eq null) asTrie
		else res
	}

	override def takeLeaves(p :This => Boolean) :This = {
		val res = takePrefixOrNull(forSource(p))
		if (res == null) asTrie else res
	}


	final private[this] def dropWhile(trie :T, p :T=>Boolean) :This = trie match {
		case bin :SourceBranch =>
			val l = dropWhile(bin.left, p)
			if (l eq null) dropWhile(bin.right, p)
			else fromSource(trie, l, fromSource(bin.right))
		case _ :SourceLeaf =>
			if (p(trie)) null.asInstanceOf[This] else fromSource(trie)
		case _ :EmptySource =>
			null.asInstanceOf[This]
		case _ =>
			fromSource(trie.dropLeaves(p))
	}

	override def dropWhile(p: (E) => Boolean) :This = dropWhile(source, compose(p))

	override def dropLeaves(p :This => Boolean) :This = dropWhile(source, forSource(p))

	protected[this] final def span(trie :T, p :T=>Boolean) :(This, This) = trie match {
		case bin :SourceBranch =>
			val l = span(bin.left, p)
			if (l._2 ne null)
				(l._1, fromSource(trie, l._2, fromSource(bin.right)))
			else {
				val r = span(bin.right, p)
				if (r._1 eq null) (l._1, r._2)
				else (fromSource(trie, l._1, r._1), r._2)
			}
		case _ :SourceLeaf => //fromSource(trie).span(p)
			if (p(trie)) (fromSource(trie), null.asInstanceOf[This])
			else (null.asInstanceOf[This], fromSource(trie))

		case _ :EmptySource =>
			(null.asInstanceOf[This], null.asInstanceOf[This])

		case _ =>
			val res = trie.leafSpan(p); (
			if (res._1.isEmpty) null.asInstanceOf[This] else fromSource(res._1), // _1
			if (res._2.isEmpty) null.asInstanceOf[This] else fromSource(res._2) // _2
		)
	}

	override def leafSpan(p :This=>Boolean) :(This, This) = {
		val adapted = forSource(p)
		val res = span(source, adapted)
		if ((res._1 ne null) && (res._2 ne null)) res
		else if (res._1 ne null) (res._1, empty)
		else if (res._2 ne null) (empty, res._2)
		else {
			val e = empty; (e, e)
		}
	}

	override def span(p: (E) => Boolean) :(This, This) = {
		val res = span(source, compose(p))
		if ((res._1 ne null) && (res._2 ne null)) res
		else if (res._1 ne null) //then res._2 eq null
			(res._1, empty)
		else if (res._2 ne null)  //res._1 eq null
			(empty, res._2)
		else {
			val e = empty; (e, e)
		}
	}


}



trait AdaptingTrie[+K, +V, +E, +T <: TrieTemplate[_, _, E, T], +This <: TrieTemplate[K, V, E, This]]
	extends MappingTrie[K, V, E, T, This] with IterableTemplate[E, This]
{
	override protected[this] def compose[@specialized(Unit, Boolean) O](f: E => O) =
		(leaf :T) => f(leaf.head)
}


object TrieAdapter {


	class Counter(private[this] var n :Int) {
		def this() = this(0)
		@inline final def get :Int = n

		@inline final def :=(value :Int) :Unit = n = value
		@inline final def +=(x :Int) :Unit = n += x
		@inline final def -=(x :Int) :Unit = n -= x
		@inline final def ++ :Unit = n += 1
		@inline final def -- :Unit = n -= 1
	}

}