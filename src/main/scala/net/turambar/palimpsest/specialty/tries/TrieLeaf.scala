package net.turambar.palimpsest.specialty.tries

import net.turambar.palimpsest.specialty.{Elements, FitIterator, IterableSpecialization, IterableTemplate}
import net.turambar.palimpsest.specialty.iterables.{SingletonFoundation, SingletonTemplate}
import net.turambar.palimpsest.specialty.tries.Trie.{MutableTrieRoot, TriePatch}

/** Base, minimal implementation of single-element tries. Represents both a singleton collection of type `T`
  * and a leaf in a larger trie instance.
  * Doesn't attempt to implement actual collection operations itself, assuming derived classes will inherit their
  * appropriate behaviour separately from either their root collection type or base traits such as [[SingletonTemplate]].
  *
  * @tparam K type of the keys in this trie used during lookup, associated with leaves and defining their location in the trie.
  *           Most often an internal type, not exposed as part of the public interface (such as with hash codes).
  * @tparam V optional values stored in leaves beside keys, one value per leaf/key. Their meaning is implementation dependent.
  * @tparam T 'self type' of this trie, that is the `Repr` in `InterableLike`. This interface doesn't pose any bounds
  *           on this type or the relationship between 'this' and `T` to simplify implementation,
  *           but actual collections will generally assume that `this.type <: T`.
  */
trait TrieLeaf[@specialized(Int, Long) +K, +V, +T/* <: Trie[K, V, T]*/] extends Trie[K, V, T]  { //this :T =>
	/** Fixed to return [[Trie.LeafNode]], equal to `1` - denotes the magnitude of leaves (as in 'one, two, many') in this trie. */
	@inline final def plurality :Trie.NodeType = 1

	def asTrie :T = this.asInstanceOf[T]

	override def trieHead: T = asTrie
	override def trieLast: T = asTrie



	override protected[this] def leafFor(key :K) :T = if (this.key==key) asTrie else empty
	override protected[this] def hasLeaf(key :K) = this.key == key
	override def leaf(idx :Int) :T =
		if (idx==0) asTrie else empty //throw new IndexOutOfBoundsException(s"$typeStringPrefix.leaf($idx)")

	override protected[this] def belongs(key: K): Boolean = this.key == key



	override def keyOpt :Option[K] = Some(key)
	override def valueOpt :Option[V] = Some(value)

	override def forEachLeaf(f: T => Unit): Unit = f(asTrie)

//	override def forLeavesReversed(f: T => Unit): Unit = f(asTrie)
	override def forEachKey(f: (K) => Unit): Unit = f(key)
	override def forEachKeysReversed(f: (K) => Unit): Unit = f(key)
	//		override def forValues(f: V => Unit): Unit = f(value)
	//		override def forValuesReversed(f: V => Unit): Unit = f(value)

	override def existsLeaf(f: T => Boolean): Boolean = f(asTrie)
	override def forAllLeaves(f: T => Boolean): Boolean = f(asTrie)

	override def existsKey(f: (K) => Boolean): Boolean = f(key)
	override def forAllKeys(f: (K) => Boolean): Boolean = f(key)
	//		override def existsValue(f: (V) => Boolean): Boolean = f(value)
	//		override def forAllValues(f: (V) => Boolean): Boolean = f(value)


	override def findLeaf(f: (T) => Boolean): T = if (f(asTrie)) asTrie else empty
	override def findKey(f: K => Boolean): Option[K] = if (f(key)) Some(key) else None


	override def filterLeaves(f: (T) => Boolean): T = {
		val self = asTrie; if (f(self)) self else empty
	}

	override def filterKeys(f: K => Boolean): T = if (f(key)) asTrie else empty
	override def filterKeysNot(f : K => Boolean): T = if (f(key)) empty else asTrie
	//		override def filterValues(f: V => Boolean): T = if (f(value)) asTrie else empty
	//		override def filterValuesNot(f :V => Boolean) :T = if (f(value)) empty else asTrie


	override def leafSpan(f: (T) => Boolean): (T, T) = {
		val self = asTrie; if (f(self)) (self, empty) else (empty, self)
	}

	private[palimpsest] override def properPrefixOrNull(f: (T) => Boolean): T =
		if (f(asTrie)) null.asInstanceOf[T] else empty

	override def takeLeaves(f :T=>Boolean) :T = {
		val self = asTrie; if (f(self)) self else empty
	}
	override def dropLeaves(f :T=>Boolean) :T = {
		val self = asTrie; if (f(self)) empty else self
	}

	override protected[this] def without(key :K) :T =
		if (this.key==key) empty else asTrie

/*
	override protected[this] def patch(key: K, mutant: TriePatch[K, V, T]) :T =
		if (key==this.key) mutant.updateLeaf(this)
		else join(mutant.notFound(key, this))

	override protected[this] def patch(root: MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]): T =
		if (key==this.key) mutant.updateLeaf(this) match {
//			case e if e.isEmpty => root.size_--(); e
			case e if e == empty => root.size_--(); e
			case replaced => replaced
		} else mutant.notFound(key, this) match {
//			case e if e.isEmpty => asTrie
			case e if e == empty => asTrie
			case newLeaf => root.size_++(); disjoint(newLeaf)
		}

	protected[this] def join(brother :T) :T =
		if (brother.isEmpty) asTrie
		else disjoint(brother)

	protected[this] def disjoint(brother :T) :T
*/
}






object TrieLeaf {

	@inline final def unapply[K, V, T](trie :Trie[K, V, T]) :Option[(K, V)] = trie match {
		case leaf :TrieLeaf[K, V, T] => Some(leaf.key, leaf.value)
		case _ => None
	}

	object IsTrieLeaf {
		@inline final def unapply(trie :Trie[_, _, _]) :Boolean = trie.isInstanceOf[TrieLeaf[_, _, _]]
		@inline final def apply(trie :Trie[_, _, _]) :Boolean = trie.isInstanceOf[TrieLeaf[_, _, _]]
	}

	object LeafKey {
		@inline final def unapply[K, V, T](trie :Trie[K, V, T]) :Option[K] = trie match {
			case leaf :SingletonLeaf[K, V, _, T] => Some(leaf.key)
			case leaf :TrieLeaf[K, V, T] => Some(leaf.key)
			case _ => None
		}
		@inline final def apply[K, V, T](trie :Trie[K, V, T]) :Option[K] = unapply(trie)
	}

	object LeafValue {
		@inline final def unapply[K, V, T](trie :Trie[K, V, T]) :Option[V] = trie match {
			case leaf :TrieLeaf[K, V, T] => Some(leaf.value)
			case _ => None
		}
		@inline final def apply[K, V, T](trie :Trie[K, V, T]) :Option[V] = unapply(trie)
	}



	trait ValueLeaf[+K, @specialized(Elements) +V, +T<:Trie[K, V, T] with IterableTemplate[V, T]]
		extends TrieTemplate[K, V, V, T] with IterableSpecialization[V, T]
	{ //this :TrieLeaf[K, V, T] =>
		def value :V

		override def forEachValue(f: (V) => Unit): Unit = f(value)
		override def forEachValueReversed(f: (V) => Unit): Unit = f(value)
		override def existsValue(f: (V) => Boolean): Boolean = f(value)
		override def forAllValues(f: (V) => Boolean): Boolean = f(value)
		override def filterValues(f: (V) => Boolean): T = if (f(value)) asTrie else empty
		override def filterValuesNot(f: (V) => Boolean): T = if (f(value)) empty else asTrie
		override def findValue(f: (V) => Boolean): Option[V] = if(f(value)) Some(value) else None


		override def foreach[@specialized(Unit) U](f :V=>U) :Unit = f(value)
		override protected def reverseForeach(f: (V) => Unit): Unit = f(value)
		override protected[this] def filter(p: (V) => Boolean, ourTruth: Boolean): T =
			if (p(value)==ourTruth) asTrie else empty

		override def filter(p: (V) => Boolean) = if (p(value)) asTrie else empty
		override def filterNot(p: (V) => Boolean) = if (p(value)) empty else asTrie

		override def iterator = FitIterator(value)
	}





	abstract class SingletonLeaf[@specialized(Int, Long) +K, +V, +E, +T <: Trie[K, V, T]]
		extends SingletonFoundation[E, T] with TrieLeaf[K, V, T]
	{
		def this(key :K) = { this(); thisKey = key }

		private[this] var thisKey :K = _

		@inline final override def key :K = thisKey
		@inline final protected[this] def key_=(key :K) :Unit = thisKey = key


		override protected[this] def patch(key: K, mutant: TriePatch[K, V, T]) :T =
			if (key==this.key) mutant.updateLeaf(this)
			else join(mutant.notFound(key, this))

		override protected[this] def patch(root: MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]): T =
			if (key==this.key) mutant.updateLeaf(this) match {
				case e if e.isEmpty => root.size_--(); e
//				case e if e == empty => root.size_--(); e
				case replaced => replaced
			} else mutant.notFound(key, this) match {
				case e if e.isEmpty => asTrie
//				case e if e == empty => asTrie
				case newLeaf => root.size_++(); disjoint(newLeaf)
			}


		override protected[this] def mutate(root: MutableTrieRoot[T], parent: MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]): Unit =
			if (key==this.key) mutant.updateLeaf(this) match {
				case empty if empty.isEmpty => root.size_--(); parent.hang(empty) //fixme: hanging empty tries is fishy
				case replacement => parent.hang(replacement)
			} else mutant.notFound(key, this) match {
				case e if e.isEmpty => ()
				case other => root.size_++(); parent.hang(disjoint(other))
			}


		override protected[this] def mutate(root: MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]) =
			if (key==this.key) mutant.updateLeaf(this) match {
				case empty if empty.isEmpty => root.size_--(); root.hang(empty)
				case replacement => root.hang(replacement)
			} else mutant.notFound(key, this) match {
				case e if e.isEmpty => ()
				case other => root.size_++(); root.hang(disjoint(other))
			}


		protected[this] def join(brother :T) :T =
			if (brother.isEmpty) asTrie
			else disjoint(brother)

		protected[this] def disjoint(brother :T) :T
	}



	abstract class AbstractTrieLeaf[@specialized(Int, Long) +K, +V, +E, +T <: Trie[K, V, T]]
		extends SingletonFoundation[E, T] with TrieLeaf[K, V, T]
	{

		override protected[this] def patch(key: K, mutant: TriePatch[K, V, T]) :T =
			if (key==this.key) mutant.updateLeaf(this)
			else join(mutant.notFound(key, this))

		override protected[this] def patch(root: MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]): T =
			if (key==this.key) mutant.updateLeaf(this) match {
				case e if e.isEmpty => root.size_--(); e
//				case e if e == empty => root.size_--(); e
				case replaced => replaced
			} else mutant.notFound(key, this) match {
				case e if e.isEmpty => asTrie
//				case e if e == empty => asTrie
				case newLeaf => root.size_++(); disjoint(newLeaf)
			}


		override protected[this] def mutate(root: MutableTrieRoot[T], parent: MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]): Unit =
			if (key==this.key) mutant.updateLeaf(this) match {
				case empty if empty.isEmpty => root.size_--(); parent.hang(empty) //fixme: hanging empty tries is fishy
				case replacement => parent.hang(replacement)
			} else mutant.notFound(key, this) match {
				case e if e.isEmpty => ()
				case other => root.size_++(); parent.hang(disjoint(other))
			}


		override protected[this] def mutate(root: MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]) =
			if (key==this.key) mutant.updateLeaf(this) match {
				case empty if empty.isEmpty => root.size_--(); root.hang(empty)
				case replacement => root.hang(replacement)
			} else mutant.notFound(key, this) match {
				case e if e.isEmpty => ()
				case other => root.size_++(); root.hang(disjoint(other))
			}


		protected[this] def join(brother :T) :T =
			if (brother.isEmpty) asTrie
			else disjoint(brother)

		protected[this] def disjoint(brother :T) :T
	}



	private[tries] def forKey[@specialized(Int, Long) K, V, T<:Trie[K, V, T], @specialized(Boolean, Unit) O](f :K=>O) :T=>O =
		{ leaf :T => f(leaf.key) }

	private[tries] def forValue[K, @specialized(Elements) V, T<:Trie[K, V, T], @specialized(Boolean, Unit) O](f :V=>O) :T=>O =
		{ leaf :T => f(leaf.value) }

}
