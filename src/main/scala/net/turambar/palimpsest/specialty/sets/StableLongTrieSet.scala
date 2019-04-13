package net.turambar.palimpsest.specialty.sets
/*
import net.turambar.palimpsest.specialty.FitIterable.IterableAdapter
import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize
import net.turambar.palimpsest.specialty.{FitBuilder, FitIterator, FitTraversableOnce, IterableSpecialization, IterableTemplate}
import net.turambar.palimpsest.specialty.tries.LongTrieKeys.{commonPath, flipSign, EmptyLongKeys, LongKeyBranch, LongKeyLeaf, MutableLongTrie}
import net.turambar.palimpsest.specialty.tries.Trie.{MutableTrieRoot, ReplaceLeaf, TrieCombinator, AbstractTriePatch}
import net.turambar.palimpsest.specialty.iterables.{EmptyIterable, EmptyIterableTemplate, IterableFoundation, SingletonSpecialization}
import net.turambar.palimpsest.specialty.ordered.{OrderedAs, OrderedVals, ValOrdering}
import net.turambar.palimpsest.specialty.ordered.OrderedAs.EmptyOrderedTemplate
import net.turambar.palimpsest.specialty.sets.StableLongTrieSet.{EditableLongTrieSet, LongTrieSet1, StableLongTrieSetN}
import net.turambar.palimpsest.specialty.tries.TrieBranch.ValueTrieTemplate
import net.turambar.palimpsest.specialty.tries.EmptyTrie.AbstractEmptyTrie
import net.turambar.palimpsest.specialty.tries.TrieLeaf.ValueLeaf
import net.turambar.palimpsest.specialty.tries.{LongTrieKeys, OrderedLongKeyedTrie, Trie, TrieLeaf, TrieTemplate}
import net.turambar.palimpsest.specialty.tries.BinaryTrie.LeafValueIterator
import net.turambar.palimpsest.specialty.tries.OrderedLongKeyedTrie.{FlippedLongBranch, FlippedLongLeaf}

import scala.annotation.{tailrec, unspecialized}
import scala.collection.{GenIterable, GenSet, SortedSetLike}




/**
  * @author Marcin Mościcki
  */
trait StableLongTrieSet extends StableOrderedSet[Long] with OrderedSetTemplate[Long, StableLongTrieSet]
								with LongTrieKeys[Long, StableLongTrieSet] with TrieTemplate[Long, Long, StableLongTrieSet]
{
	override implicit def ordering: ValOrdering[Long] = ValOrdering.LongOrdering
	override def compare(e1: Long, e2: Long) :Int = java.lang.Long.compare(e1, e2)


	override def empty :StableLongTrieSet = StableLongTrieSet.Empty

	override def contains(elem: Long): Boolean = hasKey(elem ^ 0x8000000000000000L)


	override def +(elem: Long): StableLongTrieSet = patch(elem ^ 0x8000000000000000L, StableLongTrieSet.addElement)

	override def -(elem: Long): StableLongTrieSet = patch(elem ^ 0x8000000000000000L, StableLongTrieSet.removeElement)

	override def ++(elems: FitTraversableOnce[Long]) :StableLongTrieSet = elems match {
		case longs :StableLongTrieSet => unionTrie(longs)
		case mute :MutableLongTrieSet => combine(mute.trie, StableLongTrieSet.stableUnion)
		case _ => super.++(elems)
	}

	override def --(elems: FitTraversableOnce[Long]) :StableLongTrieSet = elems match {
		case longs :StableLongTrieSet => diffTrie(longs)
		case mute :MutableLongTrieSet => diffTrie(mute.trie)
		case _ => super.--(elems)
	}


	override def intersect(that: GenSet[Long]) :StableLongTrieSet = that match {
		case longs :StableLongTrieSet => intersectTrie(longs)
		case mute :MutableLongTrieSet => intersectTrie(mute.trie)
		case _ => intersect(that)
	}

	override def subsetOf(that: GenSet[Long]) = that match {
		case longs :StableLongTrieSet => subtrieOf(longs)
		case mute :MutableLongTrieSet => subtrieOf(mute.trie)
		case _ => super.subsetOf(that)
	}

	override def sameElements[U >: Long](that: GenIterable[U]) = that match {
		case longs :StableLongTrieSet =>
			if (hasFastSize && longs.hasFastSize) size==longs.size && subtrieOf(longs)
			else subtrieOf(longs) && size == longs.size

		case mute :MutableLongTrieSet =>
			if (hasFastSize && mute.trie.hasFastSize) size==mute.trie.size && subtrieOf(mute.trie)
			else subtrieOf(mute.trie) && size == mute.trie.size

		case _ => super.sameElements(that)
	}

	private[sets] def update(root :MutableTrieRoot[StableLongTrieSet], key :Long, mutant :AbstractTriePatch[Long, Long, StableLongTrieSet]) :Unit

	private[sets] def merge(other :StableLongTrieSet)(combinator :TrieCombinator[StableLongTrieSet]) :StableLongTrieSet =
		combine(other, combinator)



	override protected[this] def newBranch(rpath: Long, left: StableLongTrieSet, right: StableLongTrieSet): StableLongTrieSet =
		new StableLongTrieSetN(rpath, left, right)


//	override protected[this] def disjoint(other: StableLongTrieSet): StableLongTrieSet = {
//		val k = key
//		val path = commonPath(k, other.key)
//		if ((path & k) == path)
//			new StableLongTrieSetN(path, other, this)
//		else
//			new StableLongTrieSetN(path, this, other)
//	}

	override def stable = this
	override def mutable = new MutableLongTrieSet(editable, size)
	override def clone() :StableLongTrieSet = this
	private[sets] def editable :StableLongTrieSet = this

	override def typeStringPrefix = "LongTrieSet"
	override def stringPrefix = "Set[Long]"
}






/**
  * @author Marcin Mościcki
  */
class MutableLongTrieSet private[sets] (private[this] var root :StableLongTrieSet, private[this] var leaves :Int)
	extends IterableAdapter[StableLongTrieSet, Long, MutableOrderedSet[Long]] with IterableTemplate[Long, MutableOrderedSet[Long]]
			with MutableOrderedSet[Long]
			with MutableTrieRoot[StableLongTrieSet] with TrieCombinator[StableLongTrieSet]
{
	def this(elems :StableLongTrieSet) = this(elems, elems.size)

	override def hang(replacement: StableLongTrieSet): Unit = root = replacement
	override def size_++() :Unit = size += 1
	override def size_--() :Unit = size -= 1

	override implicit def ordering: ValOrdering[Long] = ValOrdering.LongOrdering

	@inline final private[sets] def trie = root
	@inline final protected[this] def source = root

	override protected[this] def fromSource(other: StableLongTrieSet): MutableOrderedSet[Long] =
		new MutableLongTrieSet(other.clone()) //obviously inefficient, should do it in one step, not three

	@inline final override def size = leaves
	@inline final private[this] def size_=(leafCount :Int) :Unit = leaves = leafCount
	@inline final override def isEmpty = size==0
	@inline final override def nonEmpty = size > 0
	override def hasFastSize = true
	override def hasDefiniteSize = true


	@inline final override def empty :MutableLongTrieSet = new MutableLongTrieSet(StableLongTrieSet.Empty, 0)

	@inline final override def head = root.head
	@inline final override def last = root.last


//	override def hasKey(key :Long) :Boolean = root.hasKey(key)

	override def keyAt(idx: Int) = root.keyAt(idx)

	override def iterator: FitIterator[Long] = root.iterator
	override def reverseIterator :FitIterator[Long] = root.reverseIterator


	override def tail =
		if (size==0) throw new UnsupportedOperationException(s"$typeStringPrefix().tail")
		else new MutableLongTrieSet(root.tail.clone(), size-1)

	override def init =
		if (size==0) throw new UnsupportedOperationException(s"$typeStringPrefix().init")
		else new MutableLongTrieSet(root.init.clone(), size-1)



	override protected[this] def dropTake(from: Int, until: Int) :MutableLongTrieSet =
		if (until<=from || from>=size) empty
		else new MutableLongTrieSet(root.slice(from, until).clone(), math.min(until, size) - from)

	override def drop(n :Int) = dropTake(math.max(0, n), size)
	override def take(n :Int) = dropTake(0, n)
	override def dropRight(n :Int) = dropTake(0, size - math.min(n, size))
	override def takeRight(n :Int) = dropTake(size - math.min(n, size), size)
	override def slice(from :Int, until :Int) = dropTake(math.max(from, 0), until)

	override def splitAt(n: Int) =
		if (n <= 0) (empty, clone())
		else if (n>=size) (clone(), empty)
		else {
			val split = root.splitAt(n)
			(new MutableLongTrieSet(split._1.clone(), n), new MutableLongTrieSet(split._2.clone(), size-n))
		}

	override def filter(p: (Long) => Boolean) :MutableLongTrieSet =
		new MutableLongTrieSet(root.filter(p))

	override def filterNot(p: (Long) => Boolean) :MutableLongTrieSet =
		new MutableLongTrieSet(root.filterNot(p))



	override def contains(elem: Long): Boolean = root.hasKey(flipSign(elem))




	override def +(elem: Long) :MutableLongTrieSet = this ++ new LongTrieSet1(flipSign(elem))

	override def -(elem: Long) :MutableLongTrieSet = this -- new LongTrieSet1(flipSign(elem))

	override def ++(elems: FitTraversableOnce[Long]) :MutableLongTrieSet = elems match {
		case trie :StableLongTrieSet =>
			new MutableLongTrieSet(root.merge(trie)(StableLongTrieSet.mutableUnion))
		case mute :MutableLongTrieSet =>
			new MutableLongTrieSet(root.merge(mute.trie)(StableLongTrieSet.mutableUnion))
		case _ => clone() ++= elems
	}

	override def --(elems: FitTraversableOnce[Long]) :MutableLongTrieSet = elems match {
		case trie :StableLongTrieSet =>
			new MutableLongTrieSet(root.merge(trie)(StableLongTrieSet.mutableDifference))
		case mute :MutableLongTrieSet =>
			new MutableLongTrieSet(root.merge(mute.trie)(StableLongTrieSet.mutableDifference))
		case _ => clone() --= elems

	}

	override def intersect(that: GenSet[Long]) :MutableOrderedSet[Long] = that match {
		case longs :StableLongTrieSet => new MutableLongTrieSet(root.merge(longs)(StableLongTrieSet.mutableIntersection))
		case mute :MutableLongTrieSet =>
			new MutableLongTrieSet(root.merge(mute.trie)(StableLongTrieSet.mutableIntersection))
		case _ => super.intersect(that)
	}

	override def subsetOf(that: GenSet[Long]) = root subsetOf that


	override def retain(p: (Long) => Boolean) :Unit = {
		root = root.filter(p); size = root.size
	}


	override def clear() :Unit = { root = StableLongTrieSet.Empty; size = 0 }

	override def +=(elem: Long): this.type = {
		root.update(this, flipSign(elem), StableLongTrieSet.addElement); this
	}

	override def -=(elem: Long): this.type = {
		root.update(this, flipSign(elem), StableLongTrieSet.removeElement); this
	}

	override def add(elem :Long) :Boolean = {
		val sizeBefore = size
		root.update(this, flipSign(elem), StableLongTrieSet.addElement)
		size != sizeBefore
	}

	override def remove(elem :Long) :Boolean ={
		val sizeBefore = size
		root.update(this, flipSign(elem), StableLongTrieSet.removeElement)
		size != sizeBefore
	}

	override def --=(xs: FitTraversableOnce[Long]) :this.type = xs match {
		case trie :StableLongTrieSet =>
			root = root.merge(trie)(new TrieCombinator[StableLongTrieSet]{
				override def emptyFirst(right: StableLongTrieSet): StableLongTrieSet = StableLongTrieSet.Empty
				override def emptySecond(left: StableLongTrieSet): StableLongTrieSet = left
				override def disjoint(left: StableLongTrieSet, right: StableLongTrieSet): StableLongTrieSet = left
				override def matched(left: StableLongTrieSet, right: StableLongTrieSet): StableLongTrieSet = {
					size = size - right.size
					StableLongTrieSet.Empty
				}

				override def reduced(res1: StableLongTrieSet, res2: StableLongTrieSet): StableLongTrieSet =
					MutableLongTrieSet.this.reduced(res1, res2)
			})
			this
		case mute :MutableLongTrieSet => this --= mute.trie
		case _ => super.--=(xs)
	}

	override def ++=(xs: FitTraversableOnce[Long]) :this.type = xs match {
		case trie :StableLongTrieSet =>
			root = root.merge(trie)(this); this
		case mute :MutableLongTrieSet =>
			root = root.merge(mute.trie)(this); this
		case _  => super.++=(xs)
	}


	//callbacks from ++=
	override def emptyFirst(right: StableLongTrieSet): StableLongTrieSet = { size += right.size; right.clone().editable }
	override def emptySecond(left: StableLongTrieSet): StableLongTrieSet = left
	override def matched(left: StableLongTrieSet, right: StableLongTrieSet): StableLongTrieSet = left

	override def disjoint(first: StableLongTrieSet, second: StableLongTrieSet): StableLongTrieSet = {
		size += second.size
		EditableLongTrieSet(first, second.clone().editable)
	}


	override def reduced(res1: StableLongTrieSet, res2: StableLongTrieSet): StableLongTrieSet =
		EditableLongTrieSet(res1, res2)

	override def reduced(original: StableLongTrieSet)(left: StableLongTrieSet, right: StableLongTrieSet) =
		new EditableLongTrieSet(original.key, left, right)

	override def keysIteratorFrom(start: Long): FitIterator[Long] = root.keysIteratorFrom(start)

	override def rangeImpl(from: ?[Long], until: ?[Long]): MutableOrderedSet[Long] =
		new MutableLongTrieSet(root.rangeImpl(from, until))

	override def count = size

	override def stable :StableLongTrieSet = root.stable

	override def clone() = new MutableLongTrieSet(root.clone(), size)

	override def typeStringPrefix = "LongTrieSet"
	override def stringPrefix = "Set[Long]"
}







object StableLongTrieSet {
	import java.lang.Long.{MIN_VALUE => SignBit}
	import LongTrieKeys.{flipSign, commonPath}

	@inline final def empty :StableLongTrieSet = Empty




	object Empty extends EmptyLongKeys[Long, Long, StableLongTrieSet]
						 with StableLongTrieSet with EmptyOrderedTemplate[Long, StableLongTrieSet] with EmptySetSpecialization[Long, StableLongTrieSet]
	{

//		override def lowerBound: Long = 0L
//		override def upperBound: Long = -1L //0xff...ff

		override private[sets] def update(root: MutableTrieRoot[StableLongTrieSet], key: Long, mutant: AbstractTriePatch[Long, Long, StableLongTrieSet]): Unit =
			mutant.patchMissing(key, this) match {
				case e if e.isEmpty => ()
				case newLeaf => root.size_++(); root.hang(newLeaf)
			}


//		override def editable = MutableLongTrieSet.BlackHole

//		override protected[this] def disjoint(other: StableLongTrieSet): StableLongTrieSet = other

//		override def clone() = this
	}


	class LongTrieSet1(flipped :Long) //extends TrieLeafFoundation[Long, Long, LongTrieSet] with LongTrieSet with SingletonSpecialization[Long, LongTrieSet]
		extends LongKeyLeaf[Long, Long, StableLongTrieSet](flipped) with ValueLeaf[Long, Long, StableLongTrieSet] with FlippedLongLeaf[Long, StableLongTrieSet]
				with StableLongTrieSet with SingletonSpecialization[Long, StableLongTrieSet] //with AbstractTriePatch[Long, Long, LongTrieSet]
	{
		@inline final override def value: Long = flipSign(key) // ^ 0x8000000000000000L
		@inline final override def head = flipSign(key) // ^ 0x8000000000000000L
//		@inline final override def lowerBound: Long = flipSign(key)
//		@inline final override def upperBound: Long = flipSign(key)

		override private[sets] def update(root: MutableTrieRoot[StableLongTrieSet], key: Long, mutant: AbstractTriePatch[Long, Long, StableLongTrieSet]): Unit =
			if (key==this.key) mutant.patchLeaf(this) match {
				case empty if empty.isEmpty => root.size_--(); root.hang(empty)
				case replacement => root.hang(replacement)
			} else mutant.patchMissing(key, this) match {
				case e if e.isEmpty => ()
				case other => root.size_++(); root.hang(branchWith(other))
			}

		override protected[this] def branchWith(other: StableLongTrieSet): StableLongTrieSet = {
			val k = key
			val path = commonPath(k, other.key)
			if ((path & k) == path)
				new StableLongTrieSetN(path, other, this)
			else
				new StableLongTrieSetN(path, this, other)
		}

//		override def editable = new MutableLongTrieSet.SetLeaf(key)
//		override def clone() = this
	}


	/** Base class for all long trie sets with at least two elements. In itself, it serves
	  * a niche function of adapting a stable (immutable) set for use with [[MutableLongTrieSet]].
	  * When adding an immutable set to a mutable one as an independent subtrie, sets of size above
	  * a predefined threshold do not clone their data into mutable/editable instances, but instead
	  * return an instance of this class, which marks any branches below it as immutable and prevents
	  * their modification. Only when an in-place update of this subtrie is requested, this instance
	  * is converted into an [[EditableLongTrieSet]] reflecting the modified contents.
	  *
	  * Grouping both immutable and mutable sets under single base class allows for rather seamless interoperability
	  * between them, resulting in more efficient handling of cases such as adding an immutable set to a mutable one.
	  * It also limits class file size for all classes by reusing common set methods and static forwarders to implemented interfaces.
	  * @see [[EditableLongTrieSet]]
	  * @see [[StableLongTrieSetN]]
	  */
	class LongTrieSetNode(delimitedPath :Long, zeros :StableLongTrieSet, ones :StableLongTrieSet)
		extends LongKeyBranch[Long, StableLongTrieSet](delimitedPath, zeros, ones)
				with ValueTrieTemplate[Long, Long, StableLongTrieSet]
				with FlippedLongBranch[Long, StableLongTrieSet] with StableLongTrieSet
				with TrieCombinator[StableLongTrieSet]
	{
//		@inline final override def asTrie :StableLongTrieSet = this

		@inline final def value = flipSign(key) // ^ 0x8000000000000000L

//		@inline final override def lowerBound: Long = flipSign(lowerBound)
//		@inline final override def upperBound: Long = flipSign(upperBound)

		@inline final override def head = left.head
		@inline final override def last = right.last

		override def iterator: FitIterator[Long] = new LeafValueIterator(this, 64)

		protected override def uncheckedCopyTo(xs: Array[Long], start: Int, total: Int) :Int = {
			def cpy(trie :StableLongTrieSet, pos :Int, left :Int) :Unit = trie match {
				case branch :LongTrieSetNode =>
					val lsize = branch.left.size
					cpy(branch.left, pos, left)
					if (lsize < left)
						cpy(branch.right, pos + lsize, left-lsize)
				case leaf :LongTrieSet1 => xs(pos) = leaf.value
				case _ => trie.copyToFitArray(xs, pos, left)
			}

			if (total>0) {
				cpy(this, start, total); math.min(size, total)
			} else 0
		}

		override private[sets] def update(root: MutableTrieRoot[StableLongTrieSet], key: Long, mutant: AbstractTriePatch[Long, Long, StableLongTrieSet]): Unit =
			root.hang(patchSubtrie(root, key, mutant)(this))

		override protected[this] def unionTrie(other: StableLongTrieSet) = combine(other, this)

		/** These methods are used to implement `++`/`union` via `combine`. */
		override def emptyFirst(right: StableLongTrieSet): StableLongTrieSet = right.editable
		override def emptySecond(left: StableLongTrieSet): StableLongTrieSet = left.editable
		override def matched(left: StableLongTrieSet, right: StableLongTrieSet): StableLongTrieSet = left.editable

		override def disjoint(first: StableLongTrieSet, second: StableLongTrieSet): StableLongTrieSet = {
			val key = first.key; val split = commonPath(key, second.key)
			if ((split & key) == split)
				new EditableLongTrieSet(split, second.editable, first.editable)
			else
				new EditableLongTrieSet(split, first.editable, second.editable)
		}


		override def reduced(res1: StableLongTrieSet, res2: StableLongTrieSet): StableLongTrieSet = join(res1, res2)

		override def reduced(original: StableLongTrieSet)(left: StableLongTrieSet, right: StableLongTrieSet) = trieOf(original.key, left, right)

		override protected[this] def copy(l: StableLongTrieSet, r: StableLongTrieSet) :StableLongTrieSet =
			new LongTrieSetNode(center, left, right)

		override protected[this] def newBranch(rpath: Long, left: StableLongTrieSet, right: StableLongTrieSet): StableLongTrieSet =
			new EditableLongTrieSet(rpath, left.editable, right.editable)


		override def clone() :StableLongTrieSet = new LongTrieSetNode(center, left.clone(), right.clone())

		override def stable = new StableLongTrieSetN(center, left, right)


	}



	@inline final def EditableLongTrieSet(s1 :StableLongTrieSet, s2 :StableLongTrieSet) :EditableLongTrieSet = {
		val k1 = s1.key
		val path = commonPath(k1, s2.key)
		if ((path & k1) == path)
			new EditableLongTrieSet(path, s2, s1)
		else
			new EditableLongTrieSet(path, s1, s2)
	}


	/** A non-immutable trie branch used internally by [[MutableLongTrieSet]]. By implementing [[MutableLongTrie]],
	  * it allows in-place modification of its subtries. Note that it is not a full implementation of a mutable
	  * set as in [[MutableSet]]/[[collection.mutable.Set]], and should be treated more as data than a
	  * first-class entity.
	  */
	class EditableLongTrieSet(delimitedPath :Long, zeros :StableLongTrieSet, ones :StableLongTrieSet)
		extends LongTrieSetNode(delimitedPath, zeros, ones) with MutableLongTrie[Long, StableLongTrieSet]
	{

		override protected[this] def newBranch(rpath: Long, left: StableLongTrieSet, right: StableLongTrieSet): EditableLongTrieSet =
			new EditableLongTrieSet(rpath, left.editable, right.editable)



		override private[sets] def update(root: MutableTrieRoot[StableLongTrieSet], key: Long, mutant: AbstractTriePatch[Long, Long, StableLongTrieSet]) = {
			mutate(root, key, mutant)
		}

		//		override def emptyLeft(right: StableLongTrieSet): StableLongTrieSet = right.editable
		override def emptySecond(left: StableLongTrieSet): StableLongTrieSet = left
		override def matched(left: StableLongTrieSet, right: StableLongTrieSet): StableLongTrieSet = left

		override def disjoint(first: StableLongTrieSet, second: StableLongTrieSet): StableLongTrieSet = {
			val key = first.key; val split = commonPath(key, second.key)
			if ((split & key) == split)
				new EditableLongTrieSet(split, second.editable, first)
			else
				new EditableLongTrieSet(split, first, second.editable)
		}


		override protected[this] def copy(l: StableLongTrieSet, r: StableLongTrieSet) :StableLongTrieSet =
			new EditableLongTrieSet(center, left, right)


		override def stable = new StableLongTrieSetN(center, left.stable, right.stable)
		override def clone() :StableLongTrieSet = new EditableLongTrieSet(center, left.clone(), right.clone())
	}


	@inline final def StableLongTrieSet(s1 :StableLongTrieSet, s2 :StableLongTrieSet) :StableLongTrieSet = {
		val k1 = s1.key
		val path = commonPath(k1, s2.key)
		if ((path & k1) == path)
			new StableLongTrieSetN(path, s2, s1)
		else
			new StableLongTrieSetN(path, s2, s1)
	}


	/** Proper implementation of really immutable long tries of size >= 2, used wherever the application
	  * requires a [[StableLongTrieSet]] (or [[StableOrderedSet[Long] ]] instance, unlike the other
	  * trie branch classes here, which are used only internalnly by the mutable set.
	  */
	class StableLongTrieSetN(delimitedPath :Long, zeros :StableLongTrieSet, ones :StableLongTrieSet, private[this] var _size :Int)
		extends LongTrieSetNode(delimitedPath, zeros, ones) with OfKnownSize
	{
		def this(delimitedPath :Long, zeros :StableLongTrieSet, ones :StableLongTrieSet) =
			this(delimitedPath, zeros, ones, zeros.size + ones.size)

		override def hasFastSize = true
		@inline final override def size = _size
		@inline final protected[this] def size_=(leaves :Int) :Unit = _size = leaves


		/** These methods are used to implement `++`/`union` via `combine`. */
		override def emptyFirst(right: StableLongTrieSet): StableLongTrieSet = right.stable
		override def emptySecond(left: StableLongTrieSet): StableLongTrieSet = left
		override def matched(left: StableLongTrieSet, right: StableLongTrieSet): StableLongTrieSet = left

		override def disjoint(first: StableLongTrieSet, second: StableLongTrieSet): StableLongTrieSet = {
			val key = first.key; val split = commonPath(key, second.key)
			if ((split & key) == split)
				new StableLongTrieSetN(split, second.stable, first)
			else
				new StableLongTrieSetN(split, first, second.stable)
		}


		override def keyAt(idx: Int) :Long =
			if (idx<0 || idx>=size)
				throw new IndexOutOfBoundsException(s"$stringPrefix<$size>.nth($idx)")
			else {
				@tailrec def skip(trie :StableLongTrieSet, n :Int) :Long = trie match {
					case branch :StableLongTrieSetN =>
						val lsize = branch.left.size
						if (n < lsize) skip(branch.left, n)
						else skip(branch.right, n-lsize)
					case leaf :LongTrieSet1 => leaf.value
				}
				if (idx < left.size)
					skip(left, idx)
				else
					skip(right, idx - left.size)
			}


		final override protected[this] def copy(l: StableLongTrieSet, r: StableLongTrieSet) = new StableLongTrieSetN(center, left, right)

		override protected[this] def newBranch(rpath: Long, left: StableLongTrieSet, right: StableLongTrieSet): StableLongTrieSet =
			new StableLongTrieSetN(rpath, left, right)


//		override def editable =
//			if (size <= 8) new MutableLongTrieSet.MutableLongSetBranch(center, left.editable, right.editable)
//			else new MutableLongTrieSet.StableSetAdapter(center, left, right, size)
		override def clone() :StableLongTrieSet = new StableLongTrieSetN(center, left.clone(), right.clone(), size)

		override def stable :StableLongTrieSetN = this

		override def editable :LongTrieSetNode =
			if (size <= 8) new EditableLongTrieSet(center, left.editable, right.editable)
			else new LongTrieSetNode(center, left, right)
//		override def editable
	}





	@inline final def addElement :AbstractTriePatch[Long, Long, StableLongTrieSet] = NewLeaf

	private[this] final val NewLeaf = new AbstractTriePatch[Long, Long, StableLongTrieSet] {
		override def patchMissing[S>:StableLongTrieSet](key: Long, sibling: Trie[Long, Long, S]): S = new LongTrieSet1(key)
		override def patchLeaf[S>:StableLongTrieSet](oldLeaf: TrieLeaf[Long, Long, S]): S = oldLeaf.asTrie
	}

	@inline final def removeElement :AbstractTriePatch[Long, Long, StableLongTrieSet] = RemoveLeaf

	private[this] final val RemoveLeaf = new ReplaceLeaf[Long, Long, StableLongTrieSet](Empty)



	@inline final def union :TrieCombinator[StableLongTrieSet] = SharedUnion

	private[this] final val SharedUnion = new TrieCombinator[StableLongTrieSet] {
		override def emptyFirst(right: StableLongTrieSet): StableLongTrieSet = right
		override def emptySecond(left: StableLongTrieSet): StableLongTrieSet = left
		override def disjoint(left: StableLongTrieSet, right: StableLongTrieSet): StableLongTrieSet = StableLongTrieSet(left, right)
		override def matched(left: StableLongTrieSet, right: StableLongTrieSet): StableLongTrieSet = right
		override def reduced(res1: StableLongTrieSet, res2: StableLongTrieSet): StableLongTrieSet = StableLongTrieSet(res1, res2)
	}

	@inline final def stableUnion :TrieCombinator[StableLongTrieSet] = StableUnion

	private[this] final val StableUnion = new TrieCombinator[StableLongTrieSet] {
		override def emptyFirst(right: StableLongTrieSet): StableLongTrieSet = right.stable
		override def emptySecond(left: StableLongTrieSet): StableLongTrieSet = left
		override def disjoint(left: StableLongTrieSet, right: StableLongTrieSet): StableLongTrieSet = StableLongTrieSet(left, right.stable)
		override def matched(left: StableLongTrieSet, right: StableLongTrieSet): StableLongTrieSet = left
		override def reduced(res1: StableLongTrieSet, res2: StableLongTrieSet): StableLongTrieSet = StableLongTrieSet(res1, res2)
	}

	@inline final def mutableUnion :TrieCombinator[StableLongTrieSet] = MutableUnion

	private[this] final val MutableUnion = new TrieCombinator[StableLongTrieSet] {
		override def emptyFirst(right: StableLongTrieSet): StableLongTrieSet = right.clone().editable
		override def emptySecond(left: StableLongTrieSet): StableLongTrieSet = left.clone().editable
		override def disjoint(left: StableLongTrieSet, right: StableLongTrieSet): StableLongTrieSet = EditableLongTrieSet(left.clone().editable, right.clone().editable)
		override def matched(left: StableLongTrieSet, right: StableLongTrieSet): StableLongTrieSet = left.clone().editable
		override def reduced(res1: StableLongTrieSet, res2: StableLongTrieSet): StableLongTrieSet = StableLongTrieSet(res1, res2)
	}



	@inline final def mutableDifference :TrieCombinator[StableLongTrieSet] = MutableDifference

	private[this] final val MutableDifference = new TrieCombinator[StableLongTrieSet] {
		override def emptyFirst(right: StableLongTrieSet): StableLongTrieSet = Empty
		override def emptySecond(left: StableLongTrieSet): StableLongTrieSet = left.clone()
		override def disjoint(left: StableLongTrieSet, right: StableLongTrieSet): StableLongTrieSet = left.clone()
		override def matched(left: StableLongTrieSet, right: StableLongTrieSet): StableLongTrieSet = Empty
		override def reduced(res1: StableLongTrieSet, res2: StableLongTrieSet): StableLongTrieSet = StableLongTrieSet(res1, res2)
	}


	@inline final def mutableIntersection :TrieCombinator[StableLongTrieSet] = MutableIntersection

	private[this] final val MutableIntersection = new TrieCombinator[StableLongTrieSet] {
		override def emptyFirst(right: StableLongTrieSet): StableLongTrieSet = Empty
		override def emptySecond(left: StableLongTrieSet): StableLongTrieSet = Empty
		override def disjoint(left: StableLongTrieSet, right: StableLongTrieSet): StableLongTrieSet = Empty
		override def matched(left: StableLongTrieSet, right: StableLongTrieSet): StableLongTrieSet = left.clone()
		override def reduced(res1: StableLongTrieSet, res2: StableLongTrieSet): StableLongTrieSet = StableLongTrieSet(res1, res2)
	}
}
*/
