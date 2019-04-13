package net.turambar.palimpsest.specialty.sets

/*

import net.turambar.palimpsest.specialty.FitIterable.IterableAdapter
import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize
import net.turambar.palimpsest.specialty.iterables.{EmptyIterable, EmptyIterableTemplate, SingletonSpecialization, SingletonTemplate}
import net.turambar.palimpsest.specialty.{Elements, FitIterator, FitTraversableOnce}
import net.turambar.palimpsest.specialty.tries.TrieBranch.{MutableTrieBranch, ValueTrieTemplate}
import net.turambar.palimpsest.specialty.tries.{TrieBranch, EmptyTrie, Trie, TrieLeaf, TrieKeys, TrieTemplate}
import net.turambar.palimpsest.specialty.tries.Trie.{MutableTrieRoot, TrieCombinator, TrieOperator, AbstractTriePatch, ValueTrie}

import scala.annotation.{tailrec, unspecialized}
import scala.collection.{GenIterable, GenSet, GenTraversableOnce, mutable}


/** Partial interface of trie sets, declaring front-end [[ValSet]] methods of extending classes.
  * Derived classes will likely mix-in an appropriate back-end trait of [[TrieKeys]], which in turn operates
  * in terms of the keys in this set, abstracting over values. This dichotomy exists so as to avoid
  * cartesian explosion of specialized variants when specializing on both key and value types.
  * Instead, this instance is specialized for the value (element) type, while [[TrieKeys]] is specialized for the key types.
  * They are paired only in concrete implementation classes according to actual needs.
  *
  * It is intended as a 'private' interface, not exposed to the clients of the library; in particular, it extends
  * [[TrieCombinator]], providing callbacks for creating unions of compatible tries. These methods, when called
  * directly, can result in broken instances of `T` violating the invariants!
  */
trait TrieSetValues[K, @specialized(Elements) V, T <: TrieKeys[K, V, T] with TrieSetValues[K, V, T] with StableSet[V]]
	extends ValueTrie[K, V, T] with SetSpecialization[V, T] with TrieCombinator[T]
{ this :T => //this :Trie[K, V, T] =>

	private[sets] def update(root :MutableTrieRoot[T], element :V, mutant :AbstractTriePatch[K, V, T]) :Unit


	private type Mutable = MutableTrieValueSetFoundation[K, V, T, _] //MutableTrieValueSet[K, V, T]

	override def ++(elems: FitTraversableOnce[V]) :T = elems match {
		//stable returns trie itself, but if we want to cast it to T we might as well safeguard against not-so-stable tries
		case trie :TrieSetValues[K, V, T] => unionTrie(trie.stable)
		case mute :Mutable => combine(mute.trie, TrieSetValues.stableUnion[K, V, T])
		case _ => super.++(elems)
	}

	override def --(elems: FitTraversableOnce[V]) :T = elems match {
		case trie :TrieSetValues[K, V, T] => diffTrie(trie.asTrie)
		case mute :Mutable => diffTrie(mute.trie)
		case _ => super.--(elems)
	}


	override def intersect(that: GenSet[V]) :T = that match {
		case trie :TrieSetValues[K, V, T] => intersectTrie(trie.asTrie)
		case mute :Mutable => intersectTrie(mute.trie)
		case _ => intersect(that)
	}

	override def subsetOf(that: GenSet[V]) = that match {
		case trie :TrieSetValues[K, V, T] => subtrieOf(trie.asTrie)
		case mute :Mutable => subtrieOf(mute.trie)
		case _ => super.subsetOf(that)
	}




	override def sameElements[U >: V](that: GenIterable[U]) = that match {
		case trie :TrieSetValues[K, V, T] => //todo: verify specialization
			if (hasFastSize && trie.hasFastSize) size == trie.size && subtrieOf(trie.asTrie)
			else subtrieOf(trie.asTrie) && size == trie.size

		case mute :Mutable =>
			if (hasFastSize && mute.trie.hasFastSize) size==mute.trie.size && subtrieOf(mute.trie)
			else subtrieOf(mute.trie) && size == mute.trie.size

		case _ => super.sameElements(that)
	}



	override protected[this] def unionTrie(other: T) :T = combine(other, this)

	override protected[this] def diffTrie(other :T) :T = combine(other, TrieSetValues.difference[K, V, T])

	override protected[this] def intersectTrie(other :T) :T = combine(other, TrieSetValues.intersection[K, V, T])

	override protected[this] def subtrieOf(other :T) :Boolean = combine(other, TrieSetValues.subtrieOf[K, V, T])




	/** Union of an empty subset of this instance and a compatible trie set, that is that set itself. */
	override def emptyFirst(right: T): T = right
	/** Union of a subset of this instance and an empty set, that is the subset itself. */
	override def emptySecond(left: T): T = left
	/** Union of a subset of this instance and an equal, compatible trie set - in other words, the subset (left argument) itself. */
	override def matched(left: T, right: T): T = left

	override def disjoint(res1: T, res2: T): T = reduced(res1, res2)

	protected def disjoint(other :T) :T = disjoint(asTrie, other)

	override def empty :T //=


	/** A fully mutable set initialized with the contents of this set. Created set will have compatible structure,
	  * allowing for optimized set operations between mutable and immutable trie sets.
	  */
	override def mutable :MutableSet[V] = new MutableTrieSet[K, V, T](editable, size)

	/** A really immutable version of this set, safe to expose to the outside world as [[StableSet[V] ]].
	  * While extending classes will generally implement [[StableSet]] for simplicity and interoperability,
	  * some of them will be private instances never exposed to the user, used internally to implement mutable variant of this set.
	  */
	override def stable :T //= asTrie //this //= this.asInstanceOf[T]



	/** A variant of this set which state can be mutated by implementations of [[MutableSet]].
	  * Proper immutable instances will create a (potentially lazy) copy of themselves using specific
	  * mutable nodes. If this instance is already a specialized mutable trie, it should return itself.
 	  *
	  * @return a trie set with the same elements and structure as this instance (possibly this very instance)
	  *         which can be used to store mutable contents by [[MutableTrieValueSetFoundation]].
	  * @see [[TrieKeys#editableCopy]]
	  */
	private[sets] def editable :T

	private[sets] def editableCopy :T

	override def typeStringPrefix = "TrieSet"
}










object TrieSetValues {

	abstract class EmptyTrieSetKeys[@specialized(Int, Long) K,/* @specialized(Elements)*/ V, T <: TrieKeys[K, V, T] with TrieSetValues[K, V, T] with StableSet[V]]
		extends EmptyTrie[K, V, T] with EmptyIterableTemplate[V, T] with StableSet[V] with SetSpecialization[V, T] with TrieKeys[K, V, T]
	{ this :T =>

//		override def --(elems: FitTraversableOnce[V]) :T = asTrie
//		override def --(xs: GenTraversableOnce[V]) :T = asTrie
//
//		override def intersect(that: GenSet[V]) :T = asTrie
//		override def subsetOf(that: GenSet[V]) = true


		override protected[this] def patch(root: MutableTrieRoot[T], key: K, mutant: AbstractTriePatch[K, V, T]): T =
			mutant.patchMissing(key, this) match {
				case e if e.isEmpty => e
				case leaf => root.size_++(); leaf
			}


		override protected[this] def mutate(root: MutableTrieRoot[T], key :K, mutant: AbstractTriePatch[K, V, T]) =
			mutant.patchMissing(key, this) match {
				case e if e.isEmpty => ()
				case newLeaf => root.size_++(); root.hang(newLeaf)
			}

		override protected[this] def mutate(root: MutableTrieRoot[T], parent :MutableTrieRoot[T], key :K, mutant: AbstractTriePatch[K, V, T]) =
			mutant.patchMissing(key, this) match {
				case e if e.isEmpty => ()
				case newLeaf => root.size_++(); parent.hang(newLeaf)
			}


//		override def empty :T = asTrie
		override final def stable :T = asTrie
		override final def editable :T = asTrie
		override final def clone() :T = asTrie

		@unspecialized
		override def copyToFitArray(xs: Array[V], start: Int, total: Int) = ()

//		override protected def disjoint(other: T): T = other
	}



	trait TrieSet1Key[@specialized(Int, Long) K, /*@specialized(Elements) */V, T <: TrieKeys[K, V, T] with TrieSetValues[K, V, T] with StableSet[V]]
		extends TrieLeaf[K, V, T] /*with StableSet[V] */ with TrieKeys[K, V, T] //with SingletonSpecialization[V, T] //with ValueLeaf[K, V, T] with SingletonSpecialization[V, T]
	{ this :T =>


		override final def stable :T = asTrie
		override final def editable :T = asTrie
		override final def clone() :T = asTrie

	}




	/** Base class for all binary trie sets with at least two elements. In itself, it serves
	  * a niche function of adapting a stable (immutable) set for use with [[MutableTrieValueSetFoundation]].
	  * When adding an immutable set to a mutable one as an independent subtrie, sets of size above
	  * a predefined threshold do not clone their data into mutable/editable instances, but instead
	  * return an instance of this class, which marks any branches below it as immutable and prevents
	  * their modification. Only when an in-place update of this subtrie is requested, this instance
	  * is converted into an [[EditableTrieSetKeys]] reflecting the modified contents.
	  *
	  * Grouping both immutable and mutable sets under single base class allows for rather seamless interoperability
	  * between them, resulting in more efficient handling of cases such as adding an immutable set to a mutable one.
	  * It also limits class file size for all classes by reusing common set methods and static forwarders to implemented interfaces.
	  *
	  * @see [[EditableTrieSetKeys]]
	  * @see [[StableTrieSetKeys]]
	  */
	trait TrieValueSetNode[@specialized(Int, Long) K, V, T <: TrieKeys[K, V, T] with TrieSetValues[K, V, T] with StableSet[V]]
		extends TrieBranch[K, V, T]
				with SetTemplate[V, T] with ValueTrieTemplate[K, V, T] //with MutableLongTrie[Long, StableLongTrieSet]
				with TrieKeys[K, V, T] with TrieCombinator[T]
	{ this :T =>
		override def head = left.head
		override def last = right.last



		protected override def uncheckedCopyTo(xs: Array[V], start: Int, total: Int) :Int = {
			def cpy(trie :T, pos :Int, left :Int) :Unit = trie match {
				case branch :TrieValueSetNode[K, V, T] =>
					val lsize = branch.left.size
					cpy(branch.left, pos, left)
					if (lsize < left)
						cpy(branch.right, pos + lsize, left-lsize)
//				case leaf :TrieValueSet1[K, V, T] => //xs(pos) = leaf.value
//					ValSet.friendCopy(trie, xs, pos, left)

				case _ => //trie.copyToFitArray(xs, pos, left)
					ValSet.friendCopy(trie, xs, pos, left)
			}

			if (total>0) {
				cpy(asTrie, start, total); math.min(size, total)
			} else 0
		}

		override protected[this] def mutate(root: MutableTrieRoot[T], parent :MutableTrieRoot[T], key: K, mutant: AbstractTriePatch[K, V, T]): Unit =
			parent.hang(patch(root, key, mutant))

		override protected[this] def mutate(root: MutableTrieRoot[T], key: K, mutant: AbstractTriePatch[K, V, T]): Unit =
			root.hang(patch(root, key, mutant))


		override protected[this] def unionTrie(other: T) = combine(other, this)

		/** These methods are used to implement `++`/`union` via `combine`. */
		override def emptyFirst(right: T): T = right.editable
		override def emptySecond(left: T): T = left.editable
		override def matched(left: T, right: T): T = left.editable

//		override def disjoint(first: T, second: T): T = editableCopy(first.editable, second.editable)



		/** Creates a new lazy mutable trie reusing the immutable contents of this node.
		  * In this class, it means both of the branches, but subclasses representing other node types
		  * will override this method. The intent is that wherever this is an effectively immutable or mutable
		  * trie, the returned trie will be independent of this instance, with any modifications (if possible)
		  * to any of the copies invisible to the other.
		  */
		override def clone() :T = asTrie //copy(left, right) //create a new lazy trie instance, reusing the immutable branches.


		override def editable :T = asTrie

//		protected[this] def editableCopy(l :T, r :T) :T
		//todo: all slice/drop methods from TrieBranch may return directly any of our children (or their subtries)
		//it is not sufficient to override copy
//		override def stable = new StableLongTrieSetN(center, left, right)


	}



	/** A non-immutable trie branch used internally by [[MutableTrieValueSetFoundation]]. By implementing [[MutableTrieBranch]],
	  * it allows in-place modification of its subtries. Note that it is not a full implementation of a mutable
	  * set as in [[MutableSet]]/[[collection.mutable.Set]], and should be treated more as data than a
	  * first-class entity.
	  */
	trait EditableTrieSetKeys[@specialized(Int, Long) K, V, T <: TrieKeys[K, V, T] with TrieSetValues[K, V, T] with StableSet[V]]
		extends TrieValueSetNode[K, V, T] with MutableTrieBranch[K, V, T]
	{ this :T =>


		override def emptySecond(left: T): T = left
		override def matched(left: T, right: T): T = left

//		override def disjoint(first: T, second: T): T


//		override def stable = new StableLongTrieSetN(center, left.stable, right.stable)
//		override def editable :T = this
		override def clone() :T = subTrie(left.clone(), right.clone())
	}




	/** Proper implementation of really immutable long tries of size >= 2, used wherever the application
	  * requires a [[TrieSetValues]] (or [[StableSet[V] ]] instance, unlike the other
	  * trie branch classes here, which are used only internalnly by the mutable set.
	  */
	trait StableTrieSetKeys[@specialized(Int, Long) K, V, T <: StableSet[V] with TrieKeys[K, V, T] with TrieSetValues[K, V, T]]
		extends TrieValueSetNode[K, V, T] with OfKnownSize
	{ this :T =>

		/** These methods are used to implement `++`/`union` via `combine`. */
		override def emptyFirst(right: T): T = right//.stable
		override def emptySecond(left: T): T = left
		override def matched(left: T, right: T): T = left

		override def disjoint(first: T, second: T): T


		override def elem(idx: Int) :T =
			if (idx<0 || idx>=size)
				(this :Trie[K, V, T]).empty //self type would make it pick implemented variant from SetSpecialization
			else {
				@tailrec def skip(trie :T, n :Int) :T = trie match {
					case branch :TrieBranch[K, V, T] =>
						val lsize = branch.left.size
						if (n < lsize) skip(branch.left, n)
						else skip(branch.right, n-lsize)
					case _ :TrieLeaf[K, V, T] => trie
				}
				if (idx < left.size)
					skip(left, idx)
				else
					skip(right, idx - left.size)
			}



		override def stable :T = asTrie //this

		//		override def editable =
		//			if (size <= 8) new EditableLongTrieSet(center, left.editable, right.editable)
		//			else new LongTrieSetNode(center, left, right)

	}





	private type ErasedType = TrieSetValues[Any, Any, Nothing] //TrieSetKeys[Any, Any, Nothing]// with TrieSetValues[Any, Any, Nothing]
	@inline final def union[K, V, T <: TrieKeys[K, V, T] with TrieSetValues[K, V, T] with StableSet[V]] :TrieCombinator[T] =
		SharedUnion.asInstanceOf[TrieCombinator[T]]

	private[this] final val SharedUnion = new TrieCombinator[ErasedType] {
		override def emptyFirst(right: ErasedType): ErasedType = right
		override def emptySecond(left: ErasedType): ErasedType = left
		override def disjoint(left: ErasedType, right: ErasedType): ErasedType = left.disjoint(right.asTrie)
		override def matched(left: ErasedType, right: ErasedType): ErasedType = right
		override def reduced(res1: ErasedType, res2: ErasedType): ErasedType = res1.reduced(res1.asTrie, res2.asTrie)
	}

	/** A combinator performing a union between a stable trie on the left, and not-stable on the  right, which requires converting to a stable instance to produce a really immutable result. */
	@inline final def stableUnion[K, V, T <: TrieKeys[K, V, T] with TrieSetValues[K, V, T] with StableSet[V]] :TrieCombinator[T] =
		StableUnion.asInstanceOf[TrieCombinator[T]]

	private[this] final val StableUnion = new TrieCombinator[ErasedType] {
		override def emptyFirst(right: ErasedType): ErasedType = right.stable
		override def emptySecond(left: ErasedType): ErasedType = left
		override def disjoint(left: ErasedType, right: ErasedType): ErasedType = left.disjoint(right.stable)
		override def matched(left: ErasedType, right: ErasedType): ErasedType = left
		override def reduced(res1: ErasedType, res2: ErasedType): ErasedType = res1.reduced(res1.asTrie, res2.asTrie)
	}

	@inline final def cloneUnion[K, V, T <: TrieKeys[K, V, T] with TrieSetValues[K, V, T] with StableSet[V]] :TrieCombinator[T] =
		CloneUnion.asInstanceOf[TrieCombinator[T]]

	private[this] final val CloneUnion = new TrieCombinator[ErasedType] {
		override def emptyFirst(right: ErasedType): ErasedType = right.clone()
		override def emptySecond(left: ErasedType): ErasedType = left.clone()
		override def disjoint(left: ErasedType, right: ErasedType): ErasedType = (left.clone() :ErasedType).disjoint(right.clone())
		override def matched(left: ErasedType, right: ErasedType): ErasedType = left.clone()
		override def reduced(res1: ErasedType, res2: ErasedType): ErasedType = res1.reduced(res1.asTrie, res2.asTrie)
	}

	/** A combinator performing a union which results in a fresh, mutable copy of the summands. Calls [[TrieKeys#editableCopy]] on each returned argument. */
	@inline final def mutableUnion[K, V, T <: TrieKeys[K, V, T] with TrieSetValues[K, V, T] with StableSet[V]] :TrieCombinator[T] =
		MutableUnion.asInstanceOf[TrieCombinator[T]]

	private[this] final val MutableUnion = new TrieCombinator[ErasedType] {
		override def emptyFirst(right: ErasedType): ErasedType = right.editableCopy
		override def emptySecond(left: ErasedType): ErasedType = left.editableCopy
		override def disjoint(left: ErasedType, right: ErasedType): ErasedType = left.reduced(left.editableCopy, right.editableCopy)
		override def matched(left: ErasedType, right: ErasedType): ErasedType = left.editableCopy
		override def reduced(res1: ErasedType, res2: ErasedType): ErasedType = res1.reduced(res1.asTrie, res2.asTrie)
	}

	@inline final def difference[K, V, T <: TrieKeys[K, V, T] with TrieSetValues[K, V, T] with StableSet[V]] :TrieCombinator[T] =
		Difference.asInstanceOf[TrieCombinator[T]]

	private[this] final val Difference = new TrieCombinator[ErasedType] {
		override def emptyFirst(right: ErasedType): ErasedType = right.empty
		override def emptySecond(left: ErasedType): ErasedType = left.clone()
		override def disjoint(left: ErasedType, right: ErasedType): ErasedType = left.clone()
		override def matched(left: ErasedType, right: ErasedType): ErasedType = left.empty
		override def reduced(res1: ErasedType, res2: ErasedType): ErasedType = res1.reduced(res1.asTrie, res2.asTrie)
	}



	@inline final def mutableDifference[K, V, T <: TrieKeys[K, V, T] with TrieSetValues[K, V, T] with StableSet[V]] :TrieCombinator[T] =
		MutableDifference.asInstanceOf[TrieCombinator[T]]

	private[this] final val MutableDifference = new TrieCombinator[ErasedType] {
		override def emptyFirst(right: ErasedType): ErasedType = (right.empty :ErasedType).editable
		override def emptySecond(left: ErasedType): ErasedType = left.editableCopy
		override def disjoint(left: ErasedType, right: ErasedType): ErasedType = left.editableCopy
		override def matched(left: ErasedType, right: ErasedType): ErasedType = (left.empty :ErasedType).editable
		override def reduced(res1: ErasedType, res2: ErasedType): ErasedType = res1.reduced(res1.asTrie, res2.asTrie)
	}



	@inline final def intersection[K, V, T <: TrieKeys[K, V, T] with TrieSetValues[K, V, T] with StableSet[V]] :TrieCombinator[T] =
		Intersection.asInstanceOf[TrieCombinator[T]]

	private[this] final val Intersection = new TrieCombinator[ErasedType] {
		override def emptyFirst(right: ErasedType): ErasedType = right.empty
		override def emptySecond(left: ErasedType): ErasedType = left.empty
		override def disjoint(left: ErasedType, right: ErasedType): ErasedType = left.empty
		override def matched(left: ErasedType, right: ErasedType): ErasedType = left.clone()
		override def reduced(res1: ErasedType, res2: ErasedType): ErasedType = res1.reduced(res1.asTrie, res2.asTrie)
	}



	@inline final def mutableIntersection[K, V, T <: TrieKeys[K, V, T] with TrieSetValues[K, V, T] with StableSet[V]] :TrieCombinator[T] =
		MutableIntersection.asInstanceOf[TrieCombinator[T]]

	private[this] final val MutableIntersection = new TrieCombinator[ErasedType] {
		override def emptyFirst(right: ErasedType): ErasedType = (right.empty :ErasedType).editable
		override def emptySecond(left: ErasedType): ErasedType = (left.empty :ErasedType).editable
		override def disjoint(left: ErasedType, right: ErasedType): ErasedType = (left.empty :ErasedType).editable
		override def matched(left: ErasedType, right: ErasedType): ErasedType = left.editableCopy
		override def reduced(res1: ErasedType, res2: ErasedType): ErasedType = res1.reduced(res1.asTrie, res2.asTrie)
	}

	@inline final def subtrieOf[K, V, T <: TrieKeys[K, V, T] with TrieSetValues[K, V, T] with StableSet[V]] :TrieOperator[T, Boolean] =
		MutableIntersection.asInstanceOf[TrieOperator[T, Boolean]]

	private[this] final val SubtrieOf = new TrieOperator[ErasedType, Boolean] {
		override def emptyFirst(second: ErasedType): Boolean = true
		override def emptySecond(first: ErasedType): Boolean = false //first.isEmpty
		override def disjoint(first: ErasedType, second: ErasedType): Boolean = false
		override def matched(first: ErasedType, second: ErasedType): Boolean = true
		override def reduced(res1: Boolean, res2: Boolean): Boolean = res1 && res2
	}
}

*/
