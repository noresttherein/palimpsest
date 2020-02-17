package net.turambar.palimpsest.specialty.tries

import net.turambar.palimpsest.specialty.{?, Vals, Var}
import net.turambar.palimpsest.specialty.RuntimeType.Specialized.Fun2
import net.turambar.palimpsest.specialty.iterables.{AptIterable, IterableSpecialization, IterableTemplate}
import net.turambar.palimpsest.specialty.iterables.AptCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.iterators.AptIterator
import net.turambar.palimpsest.specialty.seqs.ValList
import net.turambar.palimpsest.specialty.sets.{SetSpecialization, ValSet}
import net.turambar.palimpsest.specialty.tries.BinaryTrieKeySetFactory.SharingTrieOp
import net.turambar.palimpsest.specialty.tries.GenericBinaryTrie.BinaryTriePatch
import net.turambar.palimpsest.specialty.tries.Trie.MutableTrieOwner
import net.turambar.palimpsest.specialty.tries.TrieElements.{ElementCounter, ElementOf}

import scala.annotation.unspecialized
import scala.collection.{GenTraversableOnce, LinearSeq}
import scala.collection.generic.CanBuildFrom


/** A container of a single trie of type `T`. Base type for collections backed by tries. */
trait TriePot[+T] {
	protected def trie :T
//	protected[specialty] def contents :T = trie
}



trait MutableTriePot[T] extends TriePot[T] with MutableTrieOwner[T] {
	protected[this] def trie_=(t :T) :Unit

	override private[tries] def updateTrie(root :T) :Unit = trie = root
}





trait TriePotIterable[+K, +F, +T <: TrieElements[K, F, T] with F, +E, +R] extends TriePot[T] with IterableTemplate[E, R] {
	//this :R =>

	protected[this] def empty :R = plant(trie.emptyTrie, 0)

//	protected[this] def copy :R

	protected[this] def copy(trieCopy :T, size :Int = -1) :R = plant(trieCopy, size)

	protected[this] def plant(trie :T, trieSize :Int = -1) :R

	protected[this] def replant(trie :T, deltaSize :Int) :R

	protected[this] def elements :ElementOf[E, F]

	protected[this] def countingElements :ElementCounter[E, F]

	override def size :Int = trie.size

	protected[this] def size_=(n :Int) :Unit


	override def ofAtLeast(count :Int) :Boolean =
		hasFastSize && count <= size || trie.ofAtLeast(count)

	override def isEmpty :Boolean = size==0 || trie.isEmpty

	override def nonEmpty :Boolean = size > 0 || trie.nonEmpty





	override def tail :R = replant(trie.tail, -1)

	override def init :R = replant(trie.init, -1)


	override def drop(n :Int) :R =
		if (n <= 0)
			carbon
		else if (hasFastSize && size <= n)
			empty
		else {
			val count = Var[Int](n)
			val res = trie.dropTrie(count)
			if (count.get > 0) { //n was greater than our size
				size = n - count.get
				empty
			} else
				replant(res, -n)
		}


	override def take(n :Int) :R =
		if (n <= 0)
			empty
		else if (hasFastSize && size <= n)
			carbon
		else {
			val count = Var[Int](n)
			val res = trie.takeTrie(count)
			if (count.get > 0) { //n was greater than our size
				val elems = n - count
				size = elems
				copy(res, n - count)
			} else
				plant(res, n)
		}


	override def dropRight(n :Int) :R =
		if (n <= 0)
			carbon
		else if (hasFastSize && size <= n)
			empty
		else {
			val count = Var(n)
			val res = trie.dropRightTrie(count)
			if (count > 0) { //n was greater than our size
				size = n - count
				empty
			} else
				replant(res, -n)
		}


	override def takeRight(n :Int) :R =
		if (n <= 0)
			empty
		else if (hasFastSize && size <= n)
			carbon
		else {
			val count = Var(n)
			val res = trie.takeRightTrie(count)
			if (count > 0) {
				val elems = n - count
				size = elems
				copy(res, elems)
			} else
				plant(res, n)
		}


	override def slice(from :Int, until :Int) :R =
		if (until <= 0 || until <= from || hasFastSize && size <= from)
			empty
		else if (from <= 0)
			take(until) //plant(trie.takeTrie(Var[Int](until)))
		else {
			val start = Var(from)
			val requested = until - from
			val count = Var(requested)
			val res = trie.sliceTrie(start, count)
			if (start > 0) {
				size = from - start
				empty
			} else if (count > 0) {
				size = until - count
				plant(res, requested - count)
			} else
				plant(res, requested)
		}


	override def splitAt(n :Int) :(R, R) =
		if (n <= 0)
			(empty, carbon)
		else if (hasFastSize && size <= n)
			(carbon, empty)
		else {
			val i = Var(n)
			val s = trie.splitTrie(i)
			if (i.get > 0) {
				val count = n - i.get
				size = count
				(copy(s._1, count), empty)
			} else if (hasFastSize)
	            (plant(s._1, n), plant(s._2, size - n))
			else
				(plant(s._1, n), plant(s._2))
		}



	override def takeWhile(p :E => Boolean) :R = plant(trie.takeWhile(elements)(p))

	override def dropWhile(p :E => Boolean) :R = plant(trie.dropWhile(elements)(p))

	override def span(p :E => Boolean) :(R, R) = {
		val split = trie.span(elements)(p)
		(plant(split._1), plant(split._2))
	}


	override def filter(p :E => Boolean, where :Boolean) :R =
		if (hasFastSize)
			plant(trie.filter(elements)(p, where))
		else {
			val counter = countingElements
			val res = plant(trie.filter(counter)(p, where))
			size = counter.count
			res
		}

	override def partition(p :E => Boolean) :(R, R) =
		if (hasFastSize) {
			val res = trie.partition(elements)(p)
			(plant(res._1), plant(res._2))
		} else {
			val counter = countingElements
			val res = trie.partition(counter)(p)
			size = counter.count
			(plant(res._1), plant(res._2))
		}




	override def find_?(p :E => Boolean, where :Boolean) : ?[E] = trie.find_?(elements)(p, where)

	override def forall(p :E=>Boolean) :Boolean =
		if (hasFastSize)
			trie.forall(elements)(p)
		else {
			val counter = countingElements
			val res = trie.forall(counter)(p)
			if (res)
				size = counter.count
			res
		}

	override def exists(p :E=>Boolean) :Boolean =
		if (hasFastSize)
			trie.exists(elements)(p)
		else {
			val counter = countingElements
			val res = trie.exists(counter)(p)
			if (!res)
				size = counter.count
			res
		}

	override def count(p :E => Boolean) :Int =
		if (hasFastSize)
			trie.count(elements)(p)
		else {
			val counter = countingElements
			val res = trie.count(counter)(p)
			size = counter.count
			res
		}
	override def inverse :AptIterable[E] = trie.foldRight(elements)(ValList.empty[E]) { (e, res) => e::res }
}





trait TriePotIterableSpecialization[+K, +F, +T <: TrieElements[K, F, T] with F, @specialized(TrieElements.Types) +E, +R]
	extends TriePotIterable[K, F, T, E, R] with IterableSpecialization[E, R]
{ //this :R =>

	override def isEmpty :Boolean = trie.isEmpty

//	protected[this] def elementOf(t :T) :E = elements.elementOf(t)
	//todo: viewHead/viewLast (headNode and lastNode may have to create a new object to conform to expected type
	override def head :E = elements.elementOf(trie.headNode)

	override def last :E = elements.elementOf(trie.lastNode)



	override def foreach[@specialized(Unit) U](f :E => U) :Unit =
		if (hasFastSize)
			trie.foreach(elements)(f)
		else {
			val counter = countingElements
			trie.foreach(counter)(f)
			size = counter.count
		}

	override protected def reverseForeach(f :E => Unit) :Unit =
		if (hasFastSize)
			trie.reverseForeach(elements)(f)
		else {
			val counter = countingElements
			trie.reverseForeach(counter)(f)
			size = counter.count
		}



	override def foldLeft[@specialized(Fun2) O](z :O)(op :(O, E) => O) :O =
		if (hasFastSize)
			trie.foldLeft(elements)(z)(op)
		else {
			val counter = countingElements
			val res = trie.foldLeft(counter)(z)(op)
			size = counter.count
			res
		}

	override def foldRight[@specialized(Fun2) O](z :O)(op :(E, O) => O) :O =
		if (hasFastSize)
			trie.foldRight(elements)(z)(op)
		else {
			val counter = countingElements
			val res = trie.foldRight(counter)(z)(op)
			size = counter.count
			res
		}


	override protected[this] def trustedCopyTo(xs :Array[E], start :Int, total :Int) :Int =
		trie.copyToArray(elements)(xs, start, total)


	override protected def manualCopyToArray[U >: E](xs :Array[U], start :Int, len :Int) :Unit =
		trie.copyToArray[U](elements)(xs, start, len)


	override def iterator :AptIterator[E] = trie.iterator(elements)

//	override def reverseIterator :FitIterator[E] = trie.reverseIterator(elements)
}









abstract class TriePotIterableFoundation[+K, +F, +T <: TrieElements[K, F, T] with F, +E, +R]
                                        (private[this] var root :T, trieSize :Int = -1)
	extends TriePotIterable[K, F, T, E, R] //with MutableTriePot[K, T, R]
{ //this :R =>
	@volatile private[this] var _size = trieSize

	@inline override final def trie :T = root

	@inline final protected[this] def trie_=(t :T) :Unit = root = t

//	@inline override final protected[specialty] def contents :T = root


	override protected[this] def replant(trie :T, deltaSize :Int) :R =
		if (_size < 0) plant(trie, -1)
		else plant(trie, _size + deltaSize)


	@inline final override def size :Int = {
		if (_size < 0)
			_size = root.size
		_size
	}

	@inline final protected[this] def unsureSize :Int = _size

	@inline final protected[this] def size_=(updatedSize :Int) :Unit = _size = updatedSize

	@inline final protected[this] def size_++ :Unit = _size += 1

	@inline final protected[this] def size_-- :Unit = _size -= 1

	@inline final protected[this] def size_+=(delta :Int) :Unit = _size += delta


	@inline final override def hasFastSize :Boolean = _size >= 0




	override def ofAtLeast(elems :Int) :Boolean =
		elems <= 0 || (_size >= 0 && _size >= elems) || root.ofAtLeast(elems)


	override def isEmpty :Boolean = root.isEmpty

	override def nonEmpty :Boolean = root.nonEmpty


}





/*
trait BinaryTriePotIterableSpecialization[K, F <: BinaryTrie[K, F],
                                          T <: GenericBinaryTrie[K, F, T] with TrieFriends[K, F, F] with TrieElements[K, F, T] with F,
                                          @specialized(TrieElements.Types) +E, +R]
	extends TriePotIterableFoundation[K, F, T, E, R] with TriePotIterableSpecialization[K, F, T, E, R] with ElementOf[E, F]
{

	override def head :E = elementOf(trie.viewHead)

	override def last :E = elementOf(trie.viewLast)


	//overriden to narrow down result type - elements now must accept tries of supertype F
	@unspecialized
	override protected[this] def elements :ElementOf[E, F] = this

	//overriden to narrow down result type - elements now must accept tries of supertype F
	override protected[this] def countingElements :ElementCounter[E, F]

	protected[this] def plantFriend(trie :F, size :Int = -1) :R //= plant(trie.stable, size)


	/** Check if the given collection is of a compatible type as this instance and, if so, retrieve its backing trie. */
	protected[this] def friendTrie(elems :GenTraversableOnce[_]) :Option[F]


//	protected[this] def ops :TrieKeySetOps[K, F, T]


	protected[this] def patchTrie(t :T, element :E)(patch :BinaryTriePatch[K, F, T]) :T

	protected[this] def patchTrie(t :T, elems :FitTraversableOnce[E])(patch :BinaryTriePatch[K, F, T]) :T = {
		var res = t; val it = elems.toIterator
		while (it.hasNext)
			res = patchTrie(res, it.next())(patch)
		res
	}

	protected[this] def patchTrie(t :T, elems :GenTraversableOnce[E])(patch :BinaryTriePatch[K, F, T]) :T = {
		var res = t
		elems match {
			case list :LinearSeq[E] =>
				var l = list
				while (l.nonEmpty) {
					res = patchTrie(res, l.head)(patch); l = l.tail
				}
			case it :Iterator[E] =>
				while (it.hasNext)
					res = patchTrie(res, it.next)(patch)
			case _ =>
				elems foreach { e => res = patchTrie(res, e)(patch) }
		}
		res
	}



	protected[this] def juxtaposition(other :F)(op :SharingTrieOp[F, F]) :R



}
*/

