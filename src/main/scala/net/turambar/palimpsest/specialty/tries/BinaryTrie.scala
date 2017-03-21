package net.turambar.palimpsest.specialty.tries

import net.turambar.palimpsest.specialty.Specialized.Fun2
import net.turambar.palimpsest.specialty.{IterableSpecialization, IterableTemplate}
import net.turambar.palimpsest.specialty.tries.Trie.{MutableTrieRoot, TriePatch}

import scala.annotation.tailrec


/** Base class for trie nodes with exactly two children, which - unless explicitly stated - are assumed to be non-empty.
  * Doesn't carry any information about the nature of the paths in the trie itself, but rather implements common `Iterable`
  * methods which can be defined recursively solely using corresponding methods of the [[Trie]] trait of its children.
  * Generally, any traversing method will follow recursively the path down the trie until first node which is not an instance of [[BinaryTrie]]
  * is found, at which point defers the calculation of intermediate result to its corresponding method.
  */
abstract class BinaryTrieFoundation[+K, +V, +E, +S <: T with TrieTemplate[K, V, E, S], +T<:TrieTemplate[K, V, E, T]](private[this] var _left :S, private[this] var _right :S)
	extends TrieTemplate[K, V, E, T]
{ //this :Trie[K, V, T] =>
	def keyOpt :Option[K] = None
	def valueOpt :Option[V] = None

	/** The left subtrie, first in the traversing order. */
	@inline final def left :S = _left
	/** The right subtrie, second in the traversing order. */
	@inline final def right :S = _right

	@inline final protected[this] def left_=(subtrie :S) :Unit = _left = subtrie
	@inline final protected[this] def right_=(subtrie: S) :Unit = _right = subtrie

	/** Fixed to return [[Trie.BranchNode]], equal to `2` - denotes the magnitude of leaves (as in 'one, two, many') in this trie. */
	override def plurality :Trie.NodeType = 2
	override def size = _left.size + _right.size
	override def nonEmpty = true
	override def isEmpty = false
	override def ofAtLeast(items :Int) = items <= 0 || {
		val lsize = left.size
		right.ofAtLeast(items-lsize)
	}


	override def leaf(idx :Int) :T = {
		//		def skip(trie :T, n :Int) :T = trie match {
		//			case branch :BinaryTrie[K, V, T] =>
		//				val inLeft = skip(branch.left, n)
		//				if (inLeft!=null) inLeft else skip(branch.right, n-branch.left.size)
		//		}
		val lsize = left.size
		if (idx<lsize) left.leaf(idx) else right.leaf(idx - lsize)
	}



	override def trieHead: T = {
		@tailrec def goLeft(trie :T) :T = trie match {
			case b :BinaryTrie[K, V, T] => goLeft(b.left)
			case t => t
		}
		goLeft(_left)
	}

	override def trieLast: T = {
		@tailrec def goRight(trie :T) :T = trie match {
			case b :BinaryTrie[K, V, T] => goRight(b.right)
			case t => t
		}
		goRight(right)
	}



	override def tail :T = filteredLeft(left.tail)
	override def init :T = filteredRight(right.init)

	override def drop(n: Int): T = {
		val lsize = left.size
		if (n>=lsize) right.drop(n-lsize)
		else if (n <= 0) asTrie
		//			else if (n >= lsize + right.size) empty
		else filteredLeft(left.drop(n)) //don't invoke right.size, may be O(n)
	}

	override def take(n: Int): T =  {
		val lsize = left.size
		if (n<=lsize) left.take(n)
		//			else if (n >= lsize+right.size) asTrie
		else filteredRight(right.take(n-lsize)) //don't invoke right.size, may be O(n)
	}

	override def dropRight(n: Int): T = {
		val rsize = right.size
		if (n >= rsize) left.dropRight(n-rsize)
		else if (n <= 0) asTrie
		//			else if (n >= left.size + rsize) empty
		else filteredRight(right.dropRight(n)) //don't invoke left.size, may be O(n)
	}

	override def takeRight(n: Int): T =  {
		val rsize = right.size
		if (n <= rsize) right.takeRight(n)
		//			else if (n >= rsize + left.size) asTrie
		else filteredLeft(left.takeRight(n-rsize)) //don't invoke left.size, may be O(n)
	}


	override def slice(from: Int, until: Int): T =
		if (until <= 0 || until <= from) empty
		else {
			val lsize = left.size
			if (from >= lsize) right.slice(from - lsize, until - lsize)
			else if (until<=lsize) left.slice(from, until)
			//				else if (from<=0 && until >= lsize + right.size) asTrie
			else if (from<=0) filteredRight(right.take(until-lsize))
			else copy(left.drop(from), right.take(until - from -lsize))
		}


	override def splitAt(idx: Int): (T, T) =  {
		if (idx <= 0) (empty, asTrie)
		else if (idx >= size) (asTrie, empty)
		else {
			val lsize = left.size
			if (idx == lsize) (left, right)
			else if (idx < lsize) {
				val lsplit = left.splitAt(idx)
				(lsplit._1, copy(lsplit._2, right))
			} else {
				val rsplit = right.splitAt(idx - lsize)
				(copy(left, rsplit._1), rsplit._2)
			}
		}
	}


	private[palimpsest] override def properPrefixOrNull(f: (T) => Boolean): T = {
		val l = left.properPrefixOrNull(f)
		if (l ne null) l
		else {
			val r = right.properPrefixOrNull(f)
			if (r eq null) r
			else if (r.isEmpty) left
			else copy(left, r)
		}
	}

	override def takeLeaves(f: (T) => Boolean) :T = {
		val l = left.properPrefixOrNull(f)
		if (l ne null) l
		else {
			val r = right.properPrefixOrNull(f)
			if (r eq null) asTrie
			else if (r.isEmpty) left
			else copy(left, r)
		}
	}


	override def dropLeaves(f: (T) => Boolean) :T = {
		val l = left.dropLeaves(f)
		if (l.isEmpty) right.dropLeaves(f)
		else if (l eq left) asTrie
		else copy(l, right)
	}

	override def leafSpan(f: (T) => Boolean): (T, T) = {
		val l = left.leafSpan(f)
		if (l._1.isEmpty) (l._1, asTrie)
		else if (l._2.nonEmpty)
			(l._1, copy(l._2, right))
		else {
			val r = right.leafSpan(f)
			if (r._1.isEmpty) (left, right)
			else if (r._2.isEmpty) (asTrie, r._2)
			else (copy(l._1, r._1), r._2)
		}
	}


	override def filterLeaves(f: (T) => Boolean): T = filtered(left.filterLeaves(f), right.filterLeaves(f))

	override def filterKeys(f: (K) => Boolean): T = filtered(left.filterKeys(f), right.filterKeys(f))
	override def filterKeysNot(f :K=>Boolean) :T = filtered(left.filterKeysNot(f), right.filterKeysNot(f))
	override def filterValues(f: (V) => Boolean): T = filtered(left.filterValues(f), right.filterValues(f))
	override def filterValuesNot(f :V=>Boolean) :T = filtered(left.filterValuesNot(f), right.filterValuesNot(f))


	override def forEachLeaf(f: (T) => Unit): Unit = {
		def executeLeaf(trie :T) :Unit = trie match {
			case bin :BinaryTrie[K, V, T] =>
				executeLeaf(bin.left); executeLeaf(bin.right)
			case _ :TrieLeaf[K, V, T] => f(trie)
			case _ :EmptyTrie[K, V, T] => ()
			case _ => trie.forEachLeaf(f)
		}
		executeLeaf(left); executeLeaf(right)
	}

	override def forEachKey(f: (K) => Unit): Unit = {
		def executeKey(trie :T) :Unit = trie match {
			case branch :BinaryTrie[K, V, T] => executeKey(branch.left); executeKey(branch.right)
			case t => t.forEachKey(f)
		}
		executeKey(left); executeKey(right)
	}
	def forEachKeysReversed(f :K=>Unit) :Unit = {
		def reversedKeys(trie :T) :Unit = trie match {
			case branch :BinaryTrie[K, V, T] => reversedKeys(branch.right); reversedKeys(branch.left)
			case t => t.forEachKeysReversed(f)
		}
		reversedKeys(right); reversedKeys(left)
	}

	override def forEachValue(f: (V) => Unit): Unit = {
		def executeValue(trie :T) :Unit = trie match {
			case branch :BinaryTrie[K, V, T] => executeValue(branch.left); executeValue(branch.right)
			case t => t.forEachValue(f)
		}
		executeValue(left); executeValue(right)
	}
	def forEachValueReversed(f :V=>Unit) :Unit = {
		def valuesReversed(trie :T) :Unit = trie match {
			case branch :BinaryTrie[K, V, T] => valuesReversed(branch.right); valuesReversed(branch.left)
			case t => t.forEachValueReversed(f)
		}
		valuesReversed(right); valuesReversed(left)
	}

	override def existsLeaf(f: T => Boolean): Boolean = {
		def satisfy(trie :T) :Boolean = trie match {
			case branch :BinaryTrie[K, V, T] => satisfy(branch.left) || satisfy(branch.right)
			case _ :TrieLeaf[K, V, T] => f(trie)
			case _ :EmptyTrie[K, V, T] => false
			case other => other.existsLeaf(f)
		}
		satisfy(left) || satisfy(right)
	}

	override def existsKey(f: (K) => Boolean): Boolean = {
		def satisfyKey(trie :T) :Boolean = trie match {
			case branch :BinaryTrie[K, V, T] => satisfyKey(branch.left) || satisfyKey(branch.right)
			case t => t.existsKey(f)
		}
		satisfyKey(left) || satisfyKey(right)
	}

	override def existsValue(f: (V) => Boolean): Boolean = {
		def satisfyValue(trie :T) :Boolean = trie match {
			case branch :BinaryTrie[K, V, T] => satisfyValue(branch.left) || satisfyValue(branch.right)
			case t => t.existsValue(f)
		}
		satisfyValue(left) || satisfyValue(right)
	}

	override def forAllLeaves(f: (T) => Boolean): Boolean = {
		def validateLeaves(trie :T) :Boolean = trie match {
			case branch :BinaryTrie[K, V, T] => validateLeaves(branch.left) && validateLeaves(branch.right)
			case _ :TrieLeaf[K, V, T] => f(trie)
			case _ :EmptyTrie[K, V, T] => true
			case other => other.forAllLeaves(f)
		}
		validateLeaves(left) && validateLeaves(right)
	}

	def forAllKeys(f :K=>Boolean) :Boolean = {
		def assertKeys(trie :T) :Boolean = trie match {
			case branch :BinaryTrie[K, V, T] => assertKeys(branch.left) && assertKeys(branch.right)
			case other => other.forAllKeys(f)
		}
		assertKeys(left) && assertKeys(right)
	}
	def forAllValues(f :V=>Boolean) :Boolean = {
		def assertValues(trie :T) :Boolean = trie match {
			case branch :BinaryTrie[K, V, T] => assertValues(branch.left) && assertValues(branch.right)
			case other => left.forAllValues(f)
		}
		assertValues(left) && assertValues(right)
	}


	override def findLeaf(f: (T) => Boolean): T = {
		def searchLeaf(trie :T) :T = trie match {
			case branch :BinaryTrie[K, V, T] =>
				val l = searchLeaf(branch.left)
				if (l ne null) l else searchLeaf(branch.right)
			case _ :TrieLeaf[K, V, T] =>
				if (f(trie)) trie else null.asInstanceOf[T]
			case _ :EmptyTrie[K, V, T] => null.asInstanceOf[T]
			case other =>
				val res = other.findLeaf(f)
				if (res.nonEmpty) res else null.asInstanceOf[T]
		}
		val l = searchLeaf(left)
		if (l ne null) l
		else {
			val r = searchLeaf(right)
			if (r ne null) r else empty
		}
	}

	override def findKey(f: (K) => Boolean): Option[K] = {
		def searchKey(trie :T) :Option[K] = trie match {
			case branch :BinaryTrie[K, V, T] => searchKey(branch.left) match {
				case some :Some[K] => some
				case _ => searchKey(branch.right)
			}
			case other => other.findKey(f)
		}
		searchKey(asTrie)
	}
	override def findValue(f: (V) => Boolean): Option[V] = {
		def searchValue(trie :T) :Option[V] = trie match {
			case branch :BinaryTrie[K, V, T] => searchValue(branch.left) match {
				case some :Some[V] => some
				case _ => searchValue(branch.right)
			}
			case other => other.findValue(f)
		}
		searchValue(asTrie)
	}




	/** Joins two tries, resulting from some filtering of `this.left` and `this.right`,
	  * into a single trie. If any of the arguments is empty, the opposite is returned.
	  * Otherwise clones any of this instance's properties into a new [[BinaryTrie]]
	  * created with [[copy]]
	  */
	@inline final protected[this] def filtered(l :S, r :S) :T =
		if ((l eq left) && (r eq right)) asTrie
		else if (l.isEmpty) r
		else if (r.isEmpty) l
		else copy(l, r)

	@inline final protected[this] def filteredRight(r :S) :T =
		if (r eq right) asTrie
		else if (r.isEmpty) left
		else copy(left, r)

	@inline final protected[this] def filteredLeft(l :S) :T =
		if (l eq left) asTrie
		else if (l.isEmpty) right
		else copy(l, right)


	protected[this] def copy(l :S, r :S) :T


	/** Given a subtrie resulting from selecting/filtering leaves from another trie, verify
	  * if the subtrie is a proper subtrie, i.e. doesn't include all elements from the original trie.
	  * @param subtrie subtrie resulting from filtering/slicing `supertrie`.
	  * @param supertrie original supertrie.
	  * @return `true` iff `supertrie` contains elements not present in `subtrie` (assumes `subtrie` doesn't contain leafs not present in `supertrie`).
	  */
	@inline final protected[this] def isProperSubtrie(subtrie :T, supertrie :T) :Boolean = !isFullSubtrie(subtrie, supertrie)

	/** Given a subtrie resulting from filtering/slicing another trie, verify if it is the full subtrie (i.e. contains all leaves from the original trie).
	  * @param subtrie subtrie resulting from filtering/slicing `supertrie`.
	  * @param supertrie original supertrie.
	  * @return `true` iff `subtrie` contains all elements from `supertrie` (assumes `subtrie` doesn't contain leaves not present in supertrie).
	  */
	@inline final protected[this] def isFullSubtrie(subtrie :T, supertrie :T) :Boolean =
		(subtrie eq supertrie) || {
			if (subtrie.hasFastSize && supertrie.hasFastSize) subtrie.size == supertrie.size
			else //if calculating size requires traversal of the collection, it'll be faster to just search for the first difference, as comparing tries with the same structure is (relatively) fast.
				subtrie == supertrie
		}
}





object BinaryTrieFoundation {

	trait MutableTrieBranch[@specialized(Int, Long) K, V, T <: Trie[K, V, T]]
		extends BinaryTrie[K, V, T] with Trie[K, V, T] with MutableTrieRoot[T]
	{
		protected def hangRight(trie :T) :Unit = right = trie
		protected def hangLeft(trie :T) :Unit = left = trie

//		override protected[this] def mutate(root :MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]): Unit =
//			mutate(root, root, key, mutant)

//		override protected[this] def mutate(root :MutableTrieRoot[T], parent :MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]): Unit


	}

	trait ValueTrieTemplate[+K, +V, +T<:TrieTemplate[K, V, V, T]]
		extends BinaryTrieFoundation[K, V, V, T, T] //with IterableTemplate[V, T] //IterableSpecialization[V, T]
	{ this :Trie[K, V, T] =>

		override def headOption :Option[V] = left.headOption

		override def lastOption :Option[V] = right.lastOption

		override def foreach[@specialized(Unit) U](f: (V) => U) = {
			def foreachValue(trie :T) :Unit = trie match {
				case branch :BinaryTrie[K, V, T] =>
					foreachValue(branch.left); foreachValue(branch.right)
				case weirdo => weirdo.foreach(f)
			}
			foreachValue(left); foreachValue(right)
		}

		override protected def reverseForeach(f: (V) => Unit): Unit = {
			def valuesReversed(trie :T) :Unit = trie match {
				case branch :BinaryTrie[K, V, T] =>
					valuesReversed(branch.right); valuesReversed(branch.left)
				case other => other.reverseTraverse(f)
			}
			valuesReversed(right); valuesReversed(left)
		}

		override def forall(p: (V) => Boolean) :Boolean = {
			def validateValues(trie :T) :Boolean = trie match {
				case branch :BinaryTrie[K, V, T] =>
					validateValues(branch.left) && validateValues(branch.right)
				case other => other.forall(p)
			}
			validateValues(left) && validateValues(right)
		}

		override def exists(p: (V) => Boolean) :Boolean = {
			def searchValues(trie :T) :Boolean = trie match {
				case branch :BinaryTrie[K, V, T] =>
					searchValues(branch.left) || searchValues(branch.right)
				case other => other.exists(p)
			}
			searchValues(left) || searchValues(right)
		}

		override def count(p: (V) => Boolean) :Int = {
			def countValues(trie :T) :Int = trie match {
				case branch :BinaryTrie[K, V, T] =>
					countValues(branch.left) + countValues(branch.right)
				case other => other.count(p)
			}
			countValues(left) + countValues(right)
		}

		override def find(p: (V) => Boolean) :Option[V] = {
			def firstValue(trie :T) :Option[V] = trie match {
				case branch :BinaryTrie[K, V, T] => firstValue(branch.left) match {
					case some :Some[V] => some
					case _ => firstValue(branch.right)
				}
				case other => other.find(p)
			}
			firstValue(asTrie)
		}

		override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, V) => O) :O = {
			def foldl(trie :T, acc :O) :O = trie match {
				case branch :BinaryTrie[K, V, T] =>
					foldl(branch.right, foldl(branch.left, acc))
				case other => other.foldLeft(acc)(op)
			}
			foldl(right, foldl(left, z))
		}

		override def foldRight[@specialized(Fun2) O](z: O)(op: (V, O) => O) : O = {
			def foldr(trie :T, acc :O) :O = trie match {
				case branch :BinaryTrie[K, V, T] =>
					foldr(branch.left, foldr(branch.right, acc))
				case other => other.foldRight(acc)(op)
			}
			foldr(left, foldr(right, z))
		}


		override def takeWhile(p: (V) => Boolean) :T = left.takeWhile(p) match {
			case all if isFullSubtrie(all, left) =>
				right takeWhile p match {
					case e if e.isEmpty => all
					case reallyAll if isFullSubtrie(reallyAll, right) => asTrie
					case tail => copy(left, tail)
				}
			case some => some
		}

		override def dropWhile(p: (V) => Boolean) :T = left.dropWhile(p) match {
			case e if e.isEmpty => right dropWhile p
			case none if isFullSubtrie(none, left) => asTrie
			case some => copy(some, right)
		}

		override def span(p: (V) => Boolean) :(T, T) = {
			val splitLeft = left.span(p)
			if (splitLeft._1.isEmpty) (empty, asTrie)
			else if (splitLeft._2.nonEmpty) (splitLeft._1, copy(splitLeft._2, right))
			else {
				val splitRight = right.span(p)
				if (splitRight._2.isEmpty) (asTrie, splitRight._2)
				else if (splitRight._1.isEmpty) (left, right)
				else (copy(left, splitRight._1), splitRight._2)
			}
		}


		override protected[this] def filter(p: (V) => Boolean, ourTruth: Boolean): T =
			if (ourTruth) filter(p) else filterNot(p)

		override def filter(p: (V) => Boolean) :T = filtered(left.filter(p), right.filter(p))

		override def filterNot(p: (V) => Boolean) :T = filtered(left.filterNot(p), right.filterNot(p))


		override def collectFirst[B](pf: PartialFunction[V, B]) = left.collectFirst(pf) match {
			case some :Some[B] => some
			case _ => right.collectFirst(pf)
		}

	}


	/** Trait of binary branch of trie `This` serving as an adapter to another trie type `T` sharing a common root `Root` with `This`.
	  * Used to directly embed tries of adapted type `T` in tries of type `This` without initial conversion.
	  * Instead, adapted branches are converted on demand to the public trie type `This` - any function
	  * rebuilding the trie (such as filtering or slicing) will create new nodes as instances of `This` instead of `T`,
	  * reusing however if possible unmodified fragments. In this way, the subtrie with this root will be gradually
	  * converted to a 'regular' instance of `This` as individual leaves/subtries are accessed.
	  *
	  * Instead of introducing a new trie node being a conventional adapter for a single trie `T`, we extend [[BinaryTrie]]
	  * with branch type `T`, so that any external function recursing over the container trie seamlessly treats this branch as any other
	  * branch in the super trie.
	  * @tparam This public trie type of this branch
	  * @tparam T adapted trie type
	  * @tparam Root common root Trie type shared between `This` and `T`.
	  */
	trait AdaptingTrie[+K, +V, +E, +This <: Trie[K, V, This] with Root , +T <: Trie[K, V, T] with Root, +Root <: Trie[K, V, Root] with TrieTemplate[K, V, E, Root]]
		extends BinaryTrieFoundation[K, V, E, T, Root] //with TrieTemplate[K, V, E, This]
	{ //this :Trie[K, V, This] =>
		override def asTrie :This
		override def empty :This
		private[this] type Branch = BinaryTrieFoundation[K, V, E, T, Root]

		protected[this] def fromSource(subtrie :T) :This
		protected[this] def fromSource(subtrie :BinaryTrie[K, V, T]) :This

		protected[this] def filterLeft(subtrie :BinaryTrie[K, V, T])(filteredLeft :This) :This =
			if (filteredLeft.isEmpty) fromSource(subtrie.right)
//			else if (filteredLeft eq subtrie.left) fromSource(subtrie)
			else copyOf(subtrie)(filteredLeft, fromSource(subtrie.right))

		protected[this] def filterRight(subtrie :BinaryTrie[K, V, T])(filteredRight :This) :This =
			if (filteredRight.isEmpty) fromSource(subtrie.left)
//			else if (filteredRight eq subtrie.right) fromSource(subtrie)
			else copyOf(subtrie)(fromSource(subtrie.left), filteredRight)

		protected[this] def filterSource(subtrie :BinaryTrie[K, V, T])(filteredLeft :This, filteredRight :This) :This =
			if (filteredLeft.isEmpty) filteredRight //fromSource(filteredRight)
			else if (filteredRight.isEmpty) filteredLeft //fromSource(filteredLeft)
//			else if ((filteredLeft eq subtrie.left) && (filteredRight eq subtrie.right)) fromSource(subtrie)
			else copyOf(subtrie)(filteredLeft, filteredRight)

		protected[this] def copyOf(subtrie :BinaryTrie[K, V, Root])(l :This, r :This) :This

		override protected[this] def copy(l :T, r :T) :This =
			if ((l eq left) && (r eq right)) asTrie
			else copyOf(this)(fromSource(l), fromSource(r))



		override def trieHead :This = fromSource(left.trieHead)
		override def trieLast = fromSource(right.trieHead)

		override def leaf(idx: Int) :This =
			if (idx < 0 || hasFastSize && idx>=size)
				empty
//				throw new IndexOutOfBoundsException(s"$typeStringPrefix<$size>.leaf($idx)")
			else if (left.hasFastSize)
				if (idx < left.size) fromSource(left.leaf(idx))
				else fromSource(right.leaf(idx - left.size))
			else {
				var result :This = empty

				def slowLeaf(trie :T, pos :Int) :Int = trie match {
					case bin :BinaryTrie[K, V, T] =>
						val remaining = slowLeaf(bin.left, pos)
						if (remaining < 0) remaining
						else slowLeaf(bin.right, remaining)

					case _ :TrieLeaf[K, V, T] =>
						if (pos==0) {
							result = fromSource(trie)
							-1
						} else pos-1
					case  _ =>
						val l = trie.leaf(pos)
						if (l.isEmpty) pos - trie.size
						else {
							result = fromSource(l); -1
						}
					}
				val remaining = slowLeaf(left, idx)
				if (remaining >= 0)
					slowLeaf(right, remaining)
				result
			}




		override def tail :This = {
			def drop1(trie :Branch) :This = trie.left match {
				case bin :Branch =>
					copyOf(trie)(drop1(bin), fromSource(trie.right))
				case _ :TrieLeaf[K, V, T] => fromSource(trie.right)
				case other => other.tail match {
					case e if e.isEmpty => fromSource(trie.right)
					case t => copyOf(trie)(fromSource(t), fromSource(trie.right))
				}
			}
			drop1(this)
		}

		override def init :This = {
			def dropRight1(trie :Branch) :This = trie.right match {
				case bin :Branch =>
					copyOf(trie)(fromSource(trie.left), dropRight1(bin))
				case _ :TrieLeaf[K, V, T] => fromSource(trie.left)
				case other => other.init match {
					case e if e.isEmpty => fromSource(trie.left)
					case i => copyOf(trie)(fromSource(trie.left), fromSource(i))
				}
			}
			dropRight1(this)
		}

		/** Private recursive `drop` implementation lifting result from `T` to `This`.
		  * @param trie trie to drop from
		  * @param n number of leaves to drop, `0 < n < size`
		  */
		protected[this] final def fastDrop(trie :T, n :Int) :This = trie match {
			case bin :BinaryTrie[K, V, T] =>
				val lsize = bin.left.size
				if (n < lsize) copyOf(bin)(fastDrop(bin.left, n), fromSource(bin.right))
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
			case bin :BinaryTrie[K, V, T] =>
				val l = slowDrop(bin.left, countdown)
				if (countdown.get == 0)
					if (l==null) fromSource(bin.right)
					else copyOf(bin)(l, fromSource(bin.right))
				else
					slowDrop(bin.right, countdown)
			case _ :TrieLeaf[K, V, T] => //countdown > 0
				countdown.--; null.asInstanceOf[This]
			case _ if trie.plurality==1 =>
				countdown.--; null.asInstanceOf[This]
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
			else if (left.hasFastSize) {
				val lsize = left.size
				if (n < lsize) copyOf(this)(fastDrop(left, n), fromSource(right))
				else if (n==lsize) fromSource(right)
				else if (right.hasFastSize)
					if (n >= lsize + right.size) empty
					else fastDrop(right, n - lsize)
				else
					slowDrop(right, new Counter(n-lsize))
			} else if (hasFastSize && n >= size)
				empty
			else {
				val counter = new Counter(n)
				val l = slowDrop(left, counter)
				if (counter.get==0)
					if (l==null) fromSource(right)
					else copyOf(this)(l, fromSource(right))
				else
					slowDrop(right, counter)
			}


		protected[this] final def fastTake(trie :T, n :Int) :This = trie match {
			case bin :BinaryTrie[K, V, T] =>
				val lsize = bin.left.size
				if (n < lsize) fastTake(bin.left, n)
				else if (n==lsize) fromSource(bin.left)
				else copyOf(bin)(fromSource(bin.left), fastTake(bin.right, n - lsize))
//			case _ :TrieLeaf[K, V, T] => //can't happen as n > 0 && n < trie.size
			case _ =>
				fromSource(trie.take(n))
		}

		protected[this] final def slowTake(trie :T, counter :Counter) :This = trie match {
			case bin :BinaryTrie[K, V, T] =>
				val l = slowTake(bin.left, counter)
				if (counter.get == 0) l
				else copyOf(bin)(l, slowTake(bin.right, counter))
			case _ :TrieLeaf[K, V, T] => //counter > 0
				counter.--; fromSource(trie)
			case _ =>
				val res = trie.take(counter.get)
				counter -= res.size
				fromSource(res)
		}


		override def take(n: Int) :This =
			if (n<=0) empty
			else if (left.hasFastSize) {
				val lsize = left.size
				if (n < lsize) fastTake(left, n)
				else if (n==lsize) fromSource(left)
				else if (right.hasFastSize)
					if (n >= lsize + right.size) asTrie
					else copyOf(this)(fromSource(left), fastTake(right, n - lsize))
				else
					copyOf(this)(fromSource(left), slowDrop(right, new Counter(n - lsize)))
			} else if (hasFastSize && size <= n)
				asTrie
			else {
				var remaining = new Counter(n)
				val l = slowTake(left, remaining)
				if (remaining.get == 0) l
				else copyOf(this)(l, slowTake(right, remaining))
			}


		final protected[this] def slowDropRight(trie :T, countdown :Counter) :This = trie match {
			case bin :BinaryTrie[K, V, T] =>
				val r = slowDropRight(bin.right, countdown)
				if (countdown.get == 0)
					if (r == null) fromSource(bin.left)
					else copyOf(bin)(fromSource(bin.left), r)
				else
					slowDropRight(bin.left, countdown)
			case _ :TrieLeaf[K, V, T] => //countdown > 0
				countdown.--; null.asInstanceOf[This]
			case _ =>
				val res = trie.dropRight(countdown.get)
				if (res.nonEmpty) countdown := 0
				else countdown -= trie.size
				fromSource(res)
		}

		override def dropRight(n: Int) :This =
			if (n<=0) asTrie
			else if (hasFastSize) take(size-n)
			else if (right.hasFastSize) {
				val rsize = right.size
				if (n < rsize)
					copyOf(this)(fromSource(left), fastTake(right, rsize-n))
				else if (n==rsize)
					fromSource(left)
				else if (left.hasFastSize)
					if (n >= left.size + rsize) empty
					else fastTake(left, left.size + rsize - n)
				else
					slowDropRight(left, new Counter(n - rsize))
			} else {
				val counter = new Counter(n)
				val r = slowDropRight(right, counter)
				if (counter.get == 0)
					if (r == null) fromSource(left)
					else copyOf(this)(fromSource(left), r)
				else
					slowDropRight(left, counter)
			}


		final protected[this] def slowTakeRight(trie :T, counter :Counter) :This = trie match {
			case bin :BinaryTrie[K, V, T] =>
				val r = slowTakeRight(bin.right, counter)
				if (counter.get==0) r
				else copyOf(bin)(slowTakeRight(bin.left, counter), r)
			case _ :TrieLeaf[K, V, T] => //counter > 0
				counter.--; fromSource(trie)
			case _ =>
				val res = trie.takeRight(counter.get)
				counter -= res.size
				fromSource(res)
		}

		override def takeRight(n: Int) :This =
			if (n<=0) empty
			else if (hasFastSize) drop(size-n)
			else if (right.hasFastSize) {
				val rsize = right.size
				if (n < rsize) fastDrop(right, rsize-n)
				else if (rsize==n) fromSource(right)
				else if (left.hasFastSize)
					if (n >= left.size + rsize) asTrie
					else copyOf(this)(fastDrop(left, left.size + rsize - n), fromSource(right))
				else
					copyOf(this)(slowTakeRight(left, new Counter(n - rsize)), fromSource(right))
			} else {
				val counter = new Counter(n)
				val r = slowTakeRight(right, counter)
				if (counter.get==0) r
				else copyOf(this)(slowTakeRight(left, counter), r)
			}


		protected[this] def fastSlice(trie :T, from :Int, until :Int) :This = trie match {
			case bin :BinaryTrie[K, V, T] =>
				val lsize = bin.left.size
				if (until <= lsize) fastSlice(bin.left, from, until)
				else if (from >= lsize) fastSlice(bin.right, from-lsize, until-lsize)
				else copyOf(bin)(fastDrop(bin.left, from), fastTake(bin.right, until-lsize))
			case _ :TrieLeaf[K, V, T] => fromSource(trie) //as until-from > 0
			case _ => fromSource(trie.slice(from, until))
		}

		override def slice(from: Int, until: Int) :This =
			if (until<=0 || until<=from) empty
			else if (left.hasFastSize) {
				val lsize = left.size
				if (until <= lsize) fastSlice(left, from, until)
				else if (right.hasFastSize)
					if (from >= lsize + right.size) empty
					else if (from >= lsize) fastSlice(right, from-lsize, until-lsize)
					else copyOf(this)(fastDrop(left, from), fastTake(right, until-lsize))
				else if (from >= lsize)
					fastDrop(right, from-lsize).take(until-from)
				else
					copyOf(this)(fastDrop(left, from), slowTake(right, new Counter(until-lsize)))
			} else if (hasFastSize && from >= size)
				empty
			else drop(from).take(until-from)


		protected[this] final def fastSplit(trie :T, pos :Int) :(This, This) = trie match {
			case bin :BinaryTrie[K, V, T] =>
				val lsize = bin.left.size
				if (pos < lsize) {
					val (l, rem) = fastSplit(bin.left, pos)
					(l, copyOf(bin)(rem, fromSource(bin.right)))
				} else if (pos==lsize)
					(fromSource(bin.left), fromSource(bin.right))
				else {
					val (init, r) = fastSplit(bin.right, pos-lsize)
					(copyOf(bin)(fromSource(bin.left), init), r)
				}
			//below code should theoretically be dead, as we handle cases of full trie and empty trie before descending deeper
			case _ :TrieLeaf[K, V, T] => (fromSource(trie), empty)
			case _ =>
				val (l, r) = trie.splitAt(pos)
				(fromSource(l), fromSource(r))
		}

		protected[this] def slowSplit(trie :T, countdown :Counter) :(This, This) = trie match {
			case bin :BinaryTrie[K, V, T] =>
				val (first, second) = slowSplit(bin.left, countdown)
				if (countdown.get==0)
					if (second == null) (first, fromSource(bin.right))
					else (first, copyOf(bin)(second, fromSource(bin.right)))
				else { //second.isEmpty
					val (tail, last) = slowSplit(bin.right, countdown)
					(copyOf(bin)(first, tail), last)
				}
			case _ :TrieLeaf[K, V, T] => //counter > 0
				countdown.--; (fromSource(trie), null.asInstanceOf[This])
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
			else if (left.hasFastSize) {
				val lsize = left.size
				if (idx < lsize) {
					val (l, r) = fastSplit(left, idx)
					(l, copyOf(this)(r, fromSource(right)))
				} else if (idx == lsize)
					(fromSource(left), fromSource(right))
				else if (right.hasFastSize)
					if (idx >= lsize + right.size) (asTrie, empty)
					else {
						val (tail, rem) = fastSplit(right, idx - lsize)
						(copyOf(this)(fromSource(left), tail), rem)
					}
				else {
					val (init, r) = slowSplit(right, new Counter(idx-lsize))
					(copyOf(this)(fromSource(left), init), r)
				}
			} else {
				val pos = new Counter(idx)
				val (first, second) = slowSplit(left, pos)
				if (pos.get==0)
					if (second==null) (first, fromSource(right))
					else (first, copyOf(this)(second, fromSource(right)))
				else {
					val (tail, last) = slowSplit(right, pos)
					if (last==null) (copyOf(this)(first, tail), empty)
					else (copyOf(this)(first, tail), last)
				}
			}


		override def forEachLeaf(f: Root => Unit) :Unit = {
//			left.forEachLeaf(f); right.forEachLeaf(f)
		}

		override def existsLeaf(f: Root => Boolean) :Boolean = left.existsLeaf(f) || right.existsLeaf(f)

		override def forAllLeaves(f :Root => Boolean) :Boolean = left.forAllLeaves(f) && right.forAllLeaves(f)

		override private[palimpsest] def properPrefixOrNull(f: Root => Boolean) :This = {
			//todo: manual recursion
			val l = left.properPrefixOrNull(f)
			if (l ne null) fromSource(l)
			else {
				val r = right.properPrefixOrNull(f)
				if (r eq null) null.asInstanceOf[This]
				else if (r.isEmpty) fromSource(left)
				else copy(left, r)
			}
		}

		override def takeLeaves(f: Root => Boolean) :This = {
			val res = properPrefixOrNull(f)
			if (res eq null) asTrie else res
		}

		override def dropLeaves(f: Root => Boolean) :This = {
			//todo: manual recursion
			val l = left.dropLeaves(f)
			if (l.isEmpty) fromSource(right.dropLeaves(f))
			else if (l eq left) asTrie
			else fromSource(l)
		}


		override def leafSpan(f: (Root) => Boolean) :(This, This) = {
			val l = left.leafSpan(f)
			if (l._1.isEmpty) (empty, asTrie)
			else if (l._2.nonEmpty) (fromSource(l._1), copy(l._2, right))
			else {
				val r = right.leafSpan(f)
				if (r._1.isEmpty) (fromSource(left), fromSource(right))
				else if (r._2.isEmpty) (asTrie, empty)
				else (copy(left, r._1), fromSource(r._2))
			}
		}

		override def filterLeaves(f: Root => Boolean) :This = {
			val Empty = empty
			def filt(trie :T) :This = trie match {
				case bin :BinaryTrie[K, V, T] =>
					filterSource(bin)(filt(bin.left), filt(bin.right))

				case _ :TrieLeaf[K, V, T] =>
					val leaf = fromSource(trie)
					if (f(leaf)) leaf
					else Empty
//				case _ =>
//					fromSource(trie.filterLeaves { t :T => f(fromSource(t)) })
			}
			val l = filt(left); val r = filt(right)
			if (l.isEmpty) r
			else if (r.isEmpty) l
			else copyOf(this)(l, r)
		}


//		protected[this] def forLeaf[@specialized(Unit, Boolean) O](f :This => O) :T => O =
//			{ t :T => f(fromSource(t)) }

//		override def forLeaves(f: This => Unit) = {
//			val g = forLeaf(f)
//			left.forLeaves(g); right.forLeaves(g)
//		}
//
//		override def forLeavesReversed(f: This => Unit) = {
//			val g = forLeaf(f)
//			right.forLeavesReversed(g); left.forLeavesReversed(g)
//		}
//
//		override def existsLeaf(f: This => Boolean) = {
//			val g = forLeaf(f)
//			left.existsLeaf(g) || right.existsLeaf(g)
//		}
//
//		override def forAllLeaves(f: This => Boolean) = {
//			val g = forLeaf(f)
//			left.forAllLeaves(g) && right.forAllLeaves(g)
//		}
//
//		override def findLeaf(f: This => Boolean) = {
//			val g = forLeaf(f)
//			val l = left.findLeaf(g)
//			if (l.nonEmpty) fromSource(l)
//			else fromSource(right.findLeaf(g))
//		}


		//todo:
		override def filterKeys(f: (K) => Boolean) :This = filterLeaves(TrieLeaf.forKey[K, V, Root, Boolean](f))

		override def filterKeysNot(f: (K) => Boolean) = filterLeaves { leaf :Root => !f(leaf.key) }

		override def filterValues(f: (V) => Boolean) = filterLeaves(TrieLeaf.forValue[K, V, Root, Boolean](f))

		override def filterValuesNot(f: (V) => Boolean) = filterLeaves{ leaf :Root => !f(leaf.value) }

	}




	class Counter(private[this] var n :Int) {
		@inline final def get :Int = n

		@inline final def :=(value :Int) :Unit = n = value
		@inline final def +=(x :Int) :Unit = n += x
		@inline final def -=(x :Int) :Unit = n -= x
		@inline final def ++ :Unit = n += 1
		@inline final def -- :Unit = n -= 1
	}
//	private class Break[T](val value :T) extends Exception
}