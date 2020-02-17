package net.turambar.palimpsest.specialty.sets

/*
import net.turambar.palimpsest.specialty.AptIterable.IterableAdapter
import net.turambar.palimpsest.specialty.{?, Elements, AptBuilder, FitIterator, FitTraversableOnce, Specialized}
import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize
import net.turambar.palimpsest.specialty.ordered.{OrderedAs, ValOrdering}
import net.turambar.palimpsest.specialty.seqs.AptBuffer
import net.turambar.palimpsest.specialty.Specialized.Fun2
import net.turambar.palimpsest.specialty.tries.Trie.{MutableTrieRoot, TrieCombinator, AbstractTriePatch}
import net.turambar.palimpsest.specialty.tries.{Trie, TrieKeys, TrieLeaf}

import scala.annotation.unspecialized
import scala.collection.{breakOut, GenSet}
import scala.collection.generic.CanBuildFrom


/** Base, unspecialized class providing most of the implementation of a mutable trie set
  * backed by a friendly, pseudo-immutable trie set `T`.
  * @param root the 'officially, but not really' immutable trie with current contents of this set.
  *             Inner nodes and leaves in this trie will be substituted to reflect applied changes to this set.
  *             Some changes may result instead in creating a new trie instance, reusing the contents
  *             of this trie, and updating this reference, substituting the old trie.
  * @param leaves size of this set.
  * @tparam K key type of the associated trie type
  * @tparam V element type and leaf value type in the associated trie
  * @tparam T the [[Trie]] providing actual implementation and storage for this instance.
  * @tparam This self type for this mutable set.
  */
abstract class MutableTrieValueSetFoundation[
			K, 
			V, 
			T <: StableSet[V] with TrieKeys[K, V, T] with TrieSetValues[K, V, T],
			+This <: MutableSet[V] with MutableSetSpecialization[V, This] with SetSpecialization[V, This]
		] private[sets] (
			private[this] var root :T, 
			private[this] var leaves :Int
		)
	extends IterableAdapter[T, V, This] with collection.mutable.Set[V] with collection.mutable.SetLike[V, This]
			with MutableTrieRoot[T] with AbstractTriePatch[K, V, T] with TrieCombinator[T] with OfKnownSize //with MutableSetSpecialization[V, This]
{ //this :This =>
	def this(contents :T) = this(contents, contents.size)

	override protected[this] def fromSource(other: T): This //= new MutableTrieValueSet[K, V, T](other)
	protected[this] def fromSource(other :T, size :Int) :This //= new MutableTrieValueSet[K, V, T](other, size)

	override def clone() :This = fromSource(source.clone(), size)

	override def empty :This = fromSource((root :SetSpecialization[V, T]).empty :T, 0)

//	override def stable :StableSet[V] = root.stable

	override def newBuilder :AptBuilder[V, This] = fromSource((root :SetSpecialization[V, T]).empty :T, 0)

	override def toBuffer[U>:V] :AptBuffer[U] = root.toBuffer


	@inline final private[palimpsest] def trie :T = root

	@inline override final protected[this] def source: T = root


	override def hang(replacement: T): Unit = root = replacement
	override def size_++() :Unit = leaves += 1
	override def size_--() :Unit = leaves -= 1

	@inline final override def size = leaves
	@inline final protected[this] def size_=(newSize :Int) :Unit = size = newSize



	override def tail =
		if (size==0) throw new UnsupportedOperationException(s"$typeStringPrefix().tail")
		else fromSource(root.tail.clone(), size-1)

	override def init =
		if (size==0) throw new UnsupportedOperationException(s"$typeStringPrefix().init")
		else fromSource(root.init.clone(), size-1)


	override protected[this] def dropTake(from: Int, until: Int) :This =
		if (until<=from || from>=size) fromSource((root :SetSpecialization[V, T]).empty :T, 0)
		else fromSource(root.slice(from, until).clone(), math.min(until, size) - from)

	override def drop(n :Int) = dropTake(math.max(0, n), size)
	override def take(n :Int) = dropTake(0, n)
	override def dropRight(n :Int) = dropTake(0, size - math.min(n, size))
	override def takeRight(n :Int) = dropTake(size - math.min(n, size), size)
	override def slice(from :Int, until :Int) = dropTake(math.max(from, 0), until)

	override def splitAt(n: Int) =
		if (n <= 0) (fromSource((root :SetSpecialization[V, T]).empty :T, 0), clone())
		else if (n>=size) (clone(), fromSource((root :SetSpecialization[V, T]).empty :T, 0))
		else {
			val split = root.splitAt(n)
			(fromSource(split._1.clone(), n), fromSource(split._2.clone(), size-n))
		}


	//todo: size of this
	override def filter(p: V => Boolean) :This = fromSource(root.filter(p))
	override def filterNot(p: V => Boolean) :This = fromSource(root.filterNot(p))



	override def intersect(that: GenSet[V]) :This = that match {
		case trie :TrieSetValues[K, V, T] =>
			fromSource(root.merge(trie.asTrie)(TrieSetValues.mutableIntersection[K, V, T]))
		case mute :MutableTrieValueSetFoundation[K, V, T, _] =>
			fromSource(root.merge(mute.trie)(TrieSetValues.mutableIntersection[K, V, T]))
		case _ => filter(that)
	}

	override def subsetOf(that: GenSet[V]) = root subsetOf that



	override def retain(p: V => Boolean) :Unit = {
		root = root.filter(p); size = root.size
	}

	override def clear() :Unit = { root = (root :Trie[K, V, T]).empty; size = 0 }


	/** Callback from [[remove]]/ [[-=]] when the key being deleted is not present in this trie. */
	override def patchMissing[S >: T](key: K, sibling: Trie[K, V, S]): S = sibling.empty

	/** Callback from [[remove]]/ [[-=]] deleting the leaf for the requested key/element. */
	override def patchLeaf[S >: T](oldLeaf: TrieLeaf[K, V, S]): S = oldLeaf.empty





	//callbacks from ++=
	override def emptyFirst(right: T): T = { size += right.size; right.editableCopy }
	override def emptySecond(left: T): T = left
	override def matched(left: T, right: T): T = left

	override def disjoint(first: T, second: T): T = {
		size += second.size
		root.disjoint(first, second.editableCopy)
	}

	override def reduced(res1: T, res2: T): T = root.reduced(res1, res2)

	override def reduced(original: T)(left: T, right: T) = root.reduced(original)(left, right)


//	override def stringPrefix = root.stringPrefix
//	override def typeStringPrefix = "Mutable" + root.typeStringPrefix

}



trait SpecializedMutableTrieSet[
		K,
		@specialized(Elements) V,
		T <: StableSet[V] with TrieKeys[K, V, T] with TrieSetValues[K, V, T],
		+This <: MutableSet[V] with MutableSetSpecialization[V, This] with SetSpecialization[V, This]
	] extends MutableTrieValueSetFoundation[K, V, T, This] with MutableSet[V] with MutableSetSpecialization[V, This] with SetSpecialization[V, This]
{ //this :MutableTrieValueSetFoundation[K, V, T, This] =>

	@unspecialized
	override def stable :T = source.stable

	override def empty :This = fromSource((trie :SetSpecialization[V, T]).empty :T, 0)

	protected[this] def singleton(value :V) :T = (trie :Trie[K, V, T]).empty + value



	@inline final override def head = trie.head
	@inline final override def last = trie.last


	override def contains(elem: V): Boolean = source.contains(elem)



	override def foldRight[@specialized(Fun2) O](z: O)(@unspecialized op: (V, O) => O) = source.foldRight(z)(op)

	@unspecialized
	override def scanLeft[@specialized(Fun2) O, That](z: O)(op: (O, V) => O)(implicit bf: CanBuildFrom[This, O, That]) =
		source.scanLeft(z)(op)(breakOut)

	@unspecialized
	override def scanRight[@specialized(Fun2) O, That](z: O)(op: (V, O) => O)(implicit bf: CanBuildFrom[This, O, That]) =
		source.scanRight(z)(op)(breakOut)


	override def +(elem: V) :This = clone() += elem //this ++ singleton(elem)
	//		if (trie.contains(elem)) clone()
	//		else new MutableTrieSet[K, V, T](trie.clone() + elem, size+1)

	override def -(elem: V) :This = clone() -= elem //this -- singleton(elem)
	//		if (trie.contains(elem)) new MutableTrieSet[K, V, T](trie.clone() - elem, size-1)
	//		else clone()

	@unspecialized
	override def ++(elems: FitTraversableOnce[V]) :This = elems match {
		case trie :TrieSetValues[K, V, T] =>
			fromSource(source.merge(trie.asTrie)(TrieSetValues.mutableUnion[K, V, T]))
		case mute :MutableTrieValueSetFoundation[K, V, T, _] =>
			fromSource(source.merge(mute.trie)(TrieSetValues.mutableUnion[K, V, T]))
		case _ => clone() ++= elems
	}

	@unspecialized
	override def --(elems: FitTraversableOnce[V]) :This = elems match {
		case trie :TrieSetValues[K, V, T] =>
			fromSource(source.merge(trie.asTrie)(TrieSetValues.mutableDifference[K, V, T]))
		case mute :MutableTrieValueSetFoundation[K, V, T, _] =>
			fromSource(source.merge(mute.trie)(TrieSetValues.mutableDifference[K, V, T]))
		case _ => clone() --= elems

	}



	override def +=(elem: V): this.type = {
		trie.update(this, elem, trie); this
	}

	override def -=(elem: V): this.type = {
		trie.update(this, elem, this); this
	}


	override def add(elem :V) :Boolean = {
		val sizeBefore = size
		trie.update(this, elem, trie)
		size != sizeBefore
	}

	override def remove(elem :V) :Boolean = {
		val sizeBefore = size
		trie.update(this, elem, this)
		size != sizeBefore
	}


	@unspecialized
	override def --=(xs: FitTraversableOnce[V]) :this.type = xs match {
		case _ if size==0 => this

		case trie :TrieSetValues[K, V, T] =>
			hang(source.merge(trie.asTrie)(new TrieCombinator[T]{
				private[this] final val Empty = (source :Trie[K, V, T]).empty

				override def emptyFirst(right: T): T = Empty
				override def emptySecond(left: T): T = left
				override def disjoint(left: T, right: T): T = left
				override def matched(left: T, right: T): T = {
					size = size - right.size
					Empty
				}
				override def reduced(res1: T, res2: T): T = SpecializedMutableTrieSet.this.reduced(res1, res2)
				override def reduced(original: T)(left: T, right: T) = SpecializedMutableTrieSet.this.reduced(original)(left, right)
			}))
			this

		case mute :MutableTrieValueSetFoundation[K, V, T, _] =>
			--=(mute.trie)

		case set :ValSet[V] =>
			filterNot(set); this //todo: verify if it's not better to iterate over set and remove elements by one

		case _ => removeAll(xs); this
	}

	protected[this] def removeAll(xs: FitTraversableOnce[V]): Unit = {
		val i = xs.toIterator
		while (size>0 && i.hasNext) trie.update(this, i.next(), this)
	}

	@unspecialized
	override def ++=(xs: FitTraversableOnce[V]) :this.type = xs match {
		case trie :TrieSetValues[K, V, T] =>
			hang(source.merge(trie.asTrie)(this)); this
		case mute :MutableTrieValueSetFoundation[K, V, T, _] =>
			hang(source.merge(mute.trie)(this)); this
		case _  => addAll(xs); this
	}


	protected[this] def addAll(xs: FitTraversableOnce[V]): Unit = {
		val i = xs.toIterator
		while (i.hasNext) trie.update(this, i.next(), trie)
	}


	protected override def trustedCopyTo(xs: Array[V], start: Int, total: Int) =
		ValSet.friendCopy(trie, xs, start, total)

	override def stringPrefix = trie.stringPrefix

	override def typeStringPrefix = "Mutable" + trie.typeStringPrefix

	@inline final override def count = size
}




class MutableTrieSet[K, @specialized(Elements) V, T <: StableSet[V] with TrieKeys[K, V, T] with TrieSetValues[K, V, T]] private[sets]
		(contents :T, leaves :Int)
	extends MutableTrieValueSetFoundation[K, V, T, MutableSet[V]](contents, leaves)
			with MutableSet[V] with SpecializedMutableTrieSet[K, V, T, MutableSet[V]]
{
	def this(contents :T) = this(contents, contents.size)



	override protected[this] def fromSource(other: T): MutableSet[V] = new MutableTrieSet[K, V, T](other)
	override protected[this] def fromSource(other: T, size: Int): MutableSet[V] = new MutableTrieSet[K, V, T](other, size)

//	protected[this] def singleton(value :V) :T = (trie :Trie[K, V, T]).empty + value

	override def clone() :MutableSet[V] = new MutableTrieSet[K, V, T](source.clone(), size)

	override def empty :MutableSet[V] = new MutableTrieSet[K, V, T]((trie :SetSpecialization[V, T]).empty :T, 0)


}



class MutableOrderedTrieSet[
		K, @specialized(Elements) V,
		T <: StableOrderedSet[V] with OrderedAs[V, T] with TrieKeys[K, V, T] with TrieSetValues[K, V, T]
	] private[sets]
		(contents :T, leaves :Int)
	extends MutableTrieValueSetFoundation[K, V, T, MutableSet[V]](contents, leaves)
			with MutableOrderedSet[V] with SpecializedMutableTrieSet[K, V, T, MutableOrderedSet[V]]//with SetSpecialization[V, MutableSet[V]]
{
	def this(contents: T) = this(contents, contents.size)

	override implicit def ordering: ValOrdering[V] = source.ordering

	override def keysIteratorFrom(start: V): FitIterator[V] = source.keysIteratorFrom(start)

	override def rangeImpl(from: ?[V], until: ?[V]): MutableOrderedSet[V] =
		new MutableOrderedTrieSet[K, V, T]((source :OrderedAs[V, T]).rangeImpl(from, until))

	override def from(from: V) = new MutableOrderedTrieSet[K, V, T](source.from(from))

	override def until(until: V) = new MutableOrderedTrieSet[K, V, T](source.until(until))

	override def range(from: V, until: V) = new MutableOrderedTrieSet[K, V, T](source.range(from, until))

	override def to(to: V) = new MutableOrderedTrieSet[K, V, T](source.to(to))


	override protected[this] def fromSource(other: T): MutableOrderedSet[V] = new MutableOrderedTrieSet[K, V, T](other)

	override protected[this] def fromSource(other: T, size: Int): MutableOrderedSet[V] = new MutableOrderedTrieSet[K, V, T](other, size)

	//	protected[this] def singleton(value :V) :T = (trie :Trie[K, V, T]).empty + value

	override def clone(): MutableOrderedSet[V] = new MutableOrderedTrieSet[K, V, T](source.clone(), size)

	override def empty: MutableOrderedSet[V] = new MutableOrderedTrieSet[K, V, T]((trie: SetSpecialization[V, T]).empty: T, 0)


}



*/


/*
class MutableTrieValueSet[K, @specialized(Elements) V, T <: StableSet[V] with TrieSetKeys[K, V, T] with TrieSetValues[K, V, T]] private[sets]
		(private[this] var root :T, private[this] var leaves :Int)
	extends IterableAdapter[T, V, MutableSet[V]] with MutableSet[V]
			with MutableTrieRoot[T] with AbstractTriePatch[K, V, T] with TrieCombinator[T] with OfKnownSize
{
	type This = MutableSet[V] //MutableTrieValueSet[K, V, T]

	def this(elems :T) = this(elems, elems.size)

	@inline final private[palimpsest] def trie :T = root

	@inline override final protected[this] def source: T = root


	override def hang(replacement: T): Unit = root = replacement
	override def size_++() :Unit = leaves += 1
	override def size_--() :Unit = leaves -= 1

	override protected[this] def fromSource(other: T): MutableSet[V] = new MutableTrieValueSet[K, V, T](other)
	protected[this] def fromSource(other :T, size :Int) :MutableSet[V] = new MutableTrieValueSet[K, V, T](other, size)

	protected[this] def singleton(value :V) :T = (root :Trie[K, V, T]).empty + value

	@inline final override def size = leaves
	@inline final protected[this] def size_=(newSize :Int) :Unit = size = newSize
	@inline final override def count = leaves



	@inline final override def head = root.head
	@inline final override def last = root.last

	//	override def rank(idx: Int) = root.rank(idx)

	override def iterator: FitIterator[V] = root.iterator
	//	override def reverseIterator :FitIterator[V] = root.reverseIterator

	@unspecialized
	override def tail =
		if (size==0) throw new UnsupportedOperationException(s"$typeStringPrefix().tail")
		else fromSource(root.tail.clone(), size-1)

	@unspecialized
	override def init =
		if (size==0) throw new UnsupportedOperationException(s"$typeStringPrefix().init")
		else fromSource(root.init.clone(), size-1)


	@unspecialized
	override protected[this] def dropTake(from: Int, until: Int) :This =
		if (until<=from || from>=size) empty
		else fromSource(root.slice(from, until).clone(), math.min(until, size) - from)

	@unspecialized
	override def drop(n :Int) = dropTake(math.max(0, n), size)
	@unspecialized
	override def take(n :Int) = dropTake(0, n)
	@unspecialized
	override def dropRight(n :Int) = dropTake(0, size - math.min(n, size))
	@unspecialized
	override def takeRight(n :Int) = dropTake(size - math.min(n, size), size)
	@unspecialized
	override def slice(from :Int, until :Int) = dropTake(math.max(from, 0), until)

	@unspecialized
	override def splitAt(n: Int) =
		if (n <= 0) (empty, clone())
		else if (n>=size) (clone(), empty)
		else {
			val split = root.splitAt(n)
			(fromSource(split._1.clone(), n), fromSource(split._2.clone(), size-n))
		}

	//todo: size of this
	@unspecialized
	override def filter(p: V => Boolean) :This = fromSource(root.filter(p))
	@unspecialized
	override def filterNot(p: V => Boolean) :This = fromSource(root.filterNot(p))





	override def +(elem: V) :This = this ++ singleton(elem)

	override def -(elem: V) :This = this -- singleton(elem)

	@unspecialized
	override def ++(elems: FitTraversableOnce[V]) :This = elems match {
		case trie :TrieSetValues[K, V, T] =>
			fromSource(root.merge(trie.asTrie)(TrieSetValues.mutableUnion[K, V, T]))
		case mute :MutableTrieValueSet[K, V, T] =>
			fromSource(root.merge(mute.trie)(TrieSetValues.mutableUnion[K, V, T]))
		case _ => clone() ++= elems
	}

	@unspecialized
	override def --(elems: FitTraversableOnce[V]) :This = elems match {
		case trie :TrieSetValues[K, V, T] =>
			fromSource(root.merge(trie.asTrie)(TrieSetValues.mutableDifference[K, V, T]))
		case mute :MutableTrieValueSet[K, V, T] =>
			fromSource(root.merge(mute.trie)(TrieSetValues.mutableDifference[K, V, T]))
		case _ => clone() --= elems

	}

	@unspecialized
	override def intersect(that: GenSet[V]) :This = that match {
		case trie :TrieSetValues[K, V, T] =>
			fromSource(root.merge(trie.asTrie)(TrieSetValues.mutableIntersection[K, V, T]))
		case mute :MutableTrieValueSet[K, V, T] =>
			fromSource(root.merge(mute.trie)(TrieSetValues.mutableIntersection[K, V, T]))
		case _ => filter(that)
	}

	@unspecialized
	override def subsetOf(that: GenSet[V]) = root subsetOf that



	override def contains(elem: V): Boolean = source.contains(elem)




	override def retain(p: V => Boolean) :Unit = {
		root = root.filter(p); size = root.size
	}

	override def clear() :Unit = { root = (root :Trie[K, V, T]).empty; size = 0 }


	/** Callback from [[remove]]/ [[-=]] when the key being deleted is not present in this trie. */
	override def patchMissing[S >: T](key: K, sibling: Trie[K, V, S]): S = sibling.empty

	/** Callback from [[remove]]/ [[-=]] deleting the leaf for the requested key/element. */
	override def reducePath[S >: T](oldLeaf: TrieLeaf[K, V, S]): S = oldLeaf.empty


	override def +=(elem: V): this.type = {
		root.update(this, elem, root); this
	}

	override def -=(elem: V): this.type = {
		root.update(this, elem, this); this
	}


	override def add(elem :V) :Boolean = {
		val sizeBefore = size
		root.update(this, elem, root)
		size != sizeBefore
	}

	override def remove(elem :V) :Boolean ={
		val sizeBefore = size
		root.update(this, elem, this)
		size != sizeBefore
	}

	override def --=(xs: FitTraversableOnce[V]) :this.type = xs match {
		case trie :TrieSetValues[K, V, T] =>
			root = root.merge(trie.asTrie)(new TrieCombinator[T]{
				private[this] final val Empty = (root :Trie[K, V, T]).empty

				override def emptyLeft(right: T): T = Empty
				override def emptyRight(left: T): T = left
				override def disjoint(left: T, right: T): T = left
				override def matched(left: T, right: T): T = {
					size = size - right.size
					Empty
				}
			})
			this

		case mute :MutableTrieValueSet[K, V, T] =>
			this --= mute.trie

		case set :ValSet[V] =>
			filterNot(set); this //todo: verify if it's not better to iterate over set and remove elements by one

		case _ => super.--=(xs)
	}

	override def ++=(xs: FitTraversableOnce[V]) :this.type = xs match {
		case trie :TrieSetValues[K, V, T] =>
			root = root.merge(trie.asTrie)(this); this
		case mute :MutableTrieValueSet[K, V, T] =>
			root = root.merge(mute.trie)(this); this
		case _  => super.++=(xs)
	}


	//callbacks from ++=
	override def emptyLeft(right: T): T = { size += right.size; right.editableCopy }
	override def emptyRight(left: T): T = left
	override def matched(left: T, right: T): T = left

	override def disjoint(first: T, second: T): T = {
		size += second.size
		root.disjoint(first, second.editableCopy)
		//		EditableLongTrieSet(first, second.clone().editable)
	}








	override def clone() :This = fromSource(source.clone(), size)

	override def empty :This = fromSource((root :SetSpecialization[V, T]).empty :T, 0)

	override def stable :StableSet[V] = root.stable

	override def stringPrefix = root.stringPrefix
	override def typeStringPrefix = "Mutable" + root.typeStringPrefix
}

*/


/*


trait OrderMutableTrieValueSet[K, @specialized(Elements) V, T <: StableOrderedSet[V] with OrderedAs[V, T] with TrieSetKeys[K, V, T] with TrieSetValues[K, V, T]]
	extends MutableOrderedSet[V]
{ this :MutableTrieValueSet[K, V, T] =>
	override implicit def ordering: Ordering[V] = trie.ordering

	@inline final protected[this] def ordered(other :T) :MutableOrderedSet[V] = new MutableOrderedTrieValueSet(other, other.size)

	override protected[this] def fromSource(other :T) :MutableOrderedSet[V] = ordered(other)

	override protected[this] def fromSource(other :T, size :Int) :MutableOrderedSet[V] = new MutableOrderedTrieValueSet(other, size)

	private[this] class MutableOrderedTrieValueSet(root :T, size :Int)
		extends MutableTrieValueSet[K, V, T](root, size) with OrderMutableTrieValueSet[K, V, T]

	override def keysIteratorFrom(start: V): FitIterator[V] = trie.keysIteratorFrom(start)

	override def rangeImpl(from: Option[V], until: Option[V]): MutableOrderedSet[V] =
		ordered((trie :OrderedAs[V, T]).rangeImpl(from, until))
}
*/
