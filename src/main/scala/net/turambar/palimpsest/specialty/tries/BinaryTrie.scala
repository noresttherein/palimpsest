package net.turambar.palimpsest.specialty.tries

import net.turambar.palimpsest.specialty.{?, Elements, FitIterator, Var}
import net.turambar.palimpsest.specialty.RuntimeType.{Fun1, Fun2}
import net.turambar.palimpsest.specialty.tries.TrieElements.{ElementOf, EmptyElements, LeafElement}
import net.turambar.palimpsest.specialty.tries.GenericBinaryTrie._
import net.turambar.palimpsest.specialty.tries.Trie.{FoldPath, KeyTypes, MutableTrieParent}

import net.turambar.palimpsest.specialty.FitIterator.BaseIterator

import scala.annotation.{tailrec, unspecialized}


/** A common interface for tries consisting of three node types: empty, leaf and inner branch.
  * They are represented by corresponding classes
  * [[net.turambar.palimpsest.specialty.tries.BinaryTrie.GenericEmptyBinaryTrie]],
  * [[net.turambar.palimpsest.specialty.tries.BinaryTrie.GenericBinaryTrieLeaf]],
  * [[net.turambar.palimpsest.specialty.tries.BinaryTrie.GenericBinaryTrieBranch]].
  * It is assumed that the branch nodes have exactly two non-empty children and do not contain proper keys themselves,
  * but rather their [[Trie#label]] property serves as a dividing value between the left (lower keys) and right (higher keys)
  * nodes.
  *
  * As it happens, only concrete implementations are also binary in the sense that the keys are numbers interpreted
  * as binary sequences.
  *
  * This trait is the basis for implementation of both mutable and immutable tries, with high level of interoperability
  * between them. A mutable binary trie `T` will always be associated with a supertype `S &gt;: T` shared with 
  * immutable implementations. Both mutable and immutable tries can have nodes of the other type as descendants.
  * A mutable subtrie of an immutable trie is treated exactly the same as an immutable one, assuming it
  * is protected from outside modification and thus effectively immutable. On the other hand, an immutable node
  * present under a mutable trie indicates a fragment shared with other instances and neither it, nor any of nodes
  * underneath it may be modified in any way. As a consequence, it is possible for an immutable node to end up
  * indirectly included as a subtrie of other, non-compatible mutable trie (like a mutable map node under a mutable trie set).
  * For this reason node mutability cannot be inferred solely based on the node class and downcasting a
  * `BinaryTrie[K, S]` to `MutableBinaryTrie[K, M, S]` is unsafe regarding the type parameters. The only exception is
  * that if a mutable node is present directly under another mutable node, it ''must'' be of the same type (including
  * the type parameters) as long as both nodes are part of unbroken path of mutable nodes starting at the trie root.
  * The mutability or immutability of a trie is thus determined solely based on the class of its root node.
  * This utilises the concept of implementing mutable collections as wrappers for variables of its immutable counterparts
  * and generalizes it for arbitrary subtries. The effect is much improved data sharing between instances: transformation
  * of a mutable trie to an immutable one is now `O(1)`, as are slicing methods and immutable collection modifiers,
  * just as with standard immutable persistent implementations. This duality has no perceivable performance cost
  * for immutable tries, and while mutable operations on immutable fragments of mutable tries are slightly slower,
  * they do not change the computational complexity and happen only on demand, transforming gradually those fragments
  * to mutable nodes and potentially eliminating the need altogether.
  * 
  * Immutable nodes (as well as their common base trait with mutable variants) can be referenced by a simpler type aliases
  * equating `S` and `T` type parameters, with the base being [[BinaryTrie!]]. Similarly, [[BinaryTrie]] is a `val` reference
  * to this trait's companion object [[GenericBinaryTrie$]].
  * 
  * @tparam K key type stored in leaves of this trie.
  * @tparam T self type of this trie; `t &lt;:&lt; GenericBinaryTrie[K, T, S]` implies `t &lt;:&lt; T`.
  * @tparam S immutable supertype of `T`. For immutable tries, `S =:= T`; for mutable tries it is the type returned by its 
  *           [[GenericBinaryTrie#stable]] method.
  *           
  * @see [[MutableBinaryTrie]]
  * @see [[GenericBinaryTrie.GenericEmptyBinaryTrie]]    
  * @see [[GenericBinaryTrie.GenericBinaryTrieLeaf]]
  * @see [[GenericBinaryTrie.GenericBinaryTrieBranch]]    
  */
trait GenericBinaryTrie[@specialized(KeyTypes) K,
                        +S <: GenericBinaryTrie[K, S, S],
                        +T <: GenericBinaryTrie[K, S, T] with S]
//trait GenericBinaryTrie[@specialized(KeyTypes) K, +S, +T <: S]
	extends Trie[K, T] with TrieElements[K, S, T]
{ this :T with BinaryTrieNode =>

//	/** Returns `Nullable(this)` if this is an empty trie, or a safe null wrapper otherwise. */
//	@unspecialized
//	def asEmpty :Nullable[GenericEmptyBinaryTrie[K, S, T] with T] = Nullable.Empty
//
//	/** Returns `Nullable(this)` if this is a trie leaf, or a safe null wrapper otherwise. */
//	@unspecialized
//	def asLeaf :Nullable[GenericBinaryTrieLeaf[K, S, T] with T] = Nullable.Empty
//
//	/** Returns `Nullable(this)` if this is a trie branch, or a safe null wrapper otherwise. */
//	@unspecialized
//	def asBranch :Nullable[GenericBinaryTrieBranch[K, S, T] with T] = Nullable.Empty

	/** This is a version of [[Trie#headNode]] which returns a leaf directly from this trie as an immutable type.
	  * The difference is that future modifications to this trie may be visible in the returned leaf, but no
	  * adapter needs to be created if the leaf is indeed of type `S` rather than  `T`. For this reason this  is most
	  * suitable towards discard-after-use scenarios such as implementing iterable's `head` method.
	  */
	def viewHead :S

	/** This is a version of [[Trie#lastNode]] which returns a leaf directly from this trie as an immutable type.
	  * The difference is that future modifications to this trie may be visible in the returned leaf, but no
	  * adapter needs to be created if the leaf is indeed of type `S` rather than  `T`. For this reason this  is most
	  * suitable towards discard-after-use scenarios such as implementing iterable's `last` method.
	  */
	def viewLast :S

	/** This is a version of [[Trie#keyNode]] which returns a leaf directly from this trie as an immutable type.
	  * The difference is that future modifications to this trie may be visible in the returned leaf, but no
	  * adapter needs to be created if the leaf is indeed of type `S` rather than  `T`. For this reason this  is most
	  * suitable towards discard-after-use scenarios such as implementing iterable's `apply` method.
	  */
	def viewNode(idx :Var[Int]) :S

	/** This is a version of [[Trie#nodeFor]] which returns a leaf directly from this trie as an immutable type.
	  * The difference is that future modifications to this trie may be visible in the returned leaf, but no
	  * adapter needs to be created if the leaf is indeed of type `S` rather than  `T`. For this reason this  is most
	  * suitable towards discard-after-use scenarios such as implementing iterable's `apply` method.
	  */
	def viewNode(key :K) :S


	/** Verifies if the given key is present under this trie. Delegates by default (for inner nodes) to [[Trie#viewNode(key:K)]].
	  * Overriden here to make public.
	  */
	override def hasKey(key :K) :Boolean = viewNode(key).nonEmpty

	/* Overriden to make public. */
	override def nodeFor(key :K) :T



	/** An immutable modification of this trie concerning a single leaf, implemented by patching the location of the key
	  * with a replacement subtrie and then constructing the new trie by retreating up the path from the modified node
	  * and copying the ancestor nodes while substituting the modified child in place of the previous one.
	  *
	  * Used to implement insertion and deletion of keys and similar operations.
	  *
	  * This is essentially the same algorithm as [[TrieFriends.foldPath]], with the result of the fold being another binary trie.
	  * In fact, it should be equivalent to the result of `foldPath(path)(key)`, but the fold step can be optimized
	  * to invoke a direct factory method for a binary branch [[BinaryTriePatch#patchLeft]]/[[BinaryTriePatch#patchRight]].
	  * @param patch a modification of this trie specifying the contents of the new trie at the given key, depending on
	  *              whether it is present in this trie. Serves as an abstract factory for the nodes of the returned trie.
	  * @param key the modified key
	  * @tparam U type of the trie created by the given patch, being the super type of nodes in this trie.
	  * @return a new trie containing the modification of leaf with the given key returned by the patch, reusing the
	  *         unmodified contents of this trie. If this instance is immutable and the `patchLeft/patchRight`
	  *         modification turns out to be identity, `this` is returned.
	  */
//	def patchKey[U >: T <: Trie[K, U]](patch :BinaryTriePatch[K, S, U])(key :K) :U //= foldPath(patch)(key)

	def patchKey[F >: S <: BinaryTrie[K, F], U >: T <: Trie[K, U]](patch :BinaryTriePatch[K, F, U])(key :K) :U

	/* Overriden to narrow down result type. */
	override def filter[E](elements :ElementOf[E, S])(f :E => Boolean, where :Boolean) :T



	/** An immutable version of this trie. Nodes already effectively immutable will return themselves (which is the default
	  * implementation). Mutable nodes will freeze future in-place modifications as in [[BinaryTrie#freeze()]] and return
	  * an immutable version of themselves. The difference from `this.view` is that the trie returned here is guaranteed
	  * to not be externally modified.
	  * @return a trie equivalent to `{ this.freeze(); this.view }`.
	  * @see [[net.turambar.palimpsest.specialty.tries.GenericBinaryTrie#view]]
	  * @see [[net.turambar.palimpsest.specialty.tries.GenericBinaryTrie#copy]]
	  * @see [[net.turambar.palimpsest.specialty.tries.GenericBinaryTrie#freeze()]]
	  */
	def stable :S

	/** Create a shallow copy of this trie node to mark it as immutable. No modification to the returned trie will be
	  * possible, but any future modifications made through this reference may (but not always) be visible in the returned view.
	  * For a truly immutable version with the same contents see [[net.turambar.palimpsest.specialty.tries.BinaryTrie.BinaryTrieBranch#stable]].
	  *
	  * Implementations should take care to preserve the immutability of reused content and not expose (potentially mutable)
	  * subtries without propagating the immutability mark or copying the contents.
	  * @return `this` for immutable instances (including empty tries); an immutable trie node with the same contents
	  *        for mutable leaves and branches.
	  * @see [[net.turambar.palimpsest.specialty.tries.BinaryTrie.BinaryTrieBranch#stable]]
	  * @see [[net.turambar.palimpsest.specialty.tries.BinaryTrie.BinaryTrieBranch#copy]]
	  * @see [[net.turambar.palimpsest.specialty.tries.BinaryTrie.BinaryTrieBranch#freeze()]]
	  */
	def view :S


	/** Create a shallow copy of this trie by sharing the contents. No future modifications to either of the references
	  * will be seen in the other. Already immutable nodes will simply return themselves; mutable nodes will mark their
	  * subtries as immutable and return a shallow copy of themselves, allowing both copies to have their children
	  * replaced independently of each other. The difference between this method and
	  * [[net.turambar.palimpsest.specialty.tries.BinaryTrie.BinaryTrieBranch#stable]] is that the latter will always
	  * return an immutable instance, while this method will preserve the underlying type, returning a mutable instance
	  * with transparently frozen contents for mutable nodes.
	  *
	  * As the result, reusing officially mutable content when creating new collections becomes as cheap as with
	  * immutable collections and copying happens only on demand, when a modification of this subtrie is requested,
	  * and to the minimal extent required. The expense is that the contents will potentially be copied twice - by
	  * each trie independently. As the intended use of this method is aid in implementing collection methods which
	  * create new instances using elements contained in the source, in the most common scenario one of the copies
	  * will likely become swiftly discarded.
	  * @see [[net.turambar.palimpsest.specialty.tries.BinaryTrie.BinaryTrieBranch#view]]
	  * @see [[net.turambar.palimpsest.specialty.tries.BinaryTrie.BinaryTrieBranch#stable]]
	  * @see [[net.turambar.palimpsest.specialty.tries.BinaryTrie.BinaryTrieBranch#freeze()]]
	  */
	def copy :T


	/** Prevents any future modification to the subtries of this trie. Note that:
	  *   - leaf instances with associated additional values can still have them modified;
	  *   - branch nodes can have their children replaced with new instances.
	  *  In other words, the freezing doesn't make it an immutable instance and any information contained in this node
	  *  itself can still be modified.
	  */
	def freeze() :Unit

	/** Returns true if this instance can possibly change contents in the future. This is independent of the fact
	  * if this instance extends [[MutableBinaryTrie]], which simply provides interface more suited toward mutable
	  * implementations.
	  */
	def isMutable :Boolean

}





/** A mutable, or pseudo-mutable, version of [[GenericBinaryTrie]]. It is invariant regarding its self type `T`
  * and declares methods for in-place modification of the trie, assuming it is rooted in some external object.
  * Some extending classes may actually be immutable and provide the illusion of mutability by swapping their
  * reference for a new trie with each modification. In particular, it is possible for immutable subtries to be
  * present under (real) mutable branches.
  * @tparam K key type associated with this trie.
  * @tparam T self type of this trie, that is the type of all new tries returned by this trait.
  * @tparam S stable version of this trie.
  * @see [[GenericBinaryTrie.MutableEmptyBinaryTrie]]
  * @see [[GenericBinaryTrie.MutableBinaryTrieLeaf]]
  * @see [[GenericBinaryTrie.MutableBinaryTrieBranch]]
  */
trait MutableBinaryTrie[@specialized(KeyTypes) K, S <: GenericBinaryTrie[K, S, S], T <: GenericBinaryTrie[K, S, T] with S]
	extends GenericBinaryTrie[K, S, T]
{ outer :T with BinaryTrieNode =>


//	@unspecialized
//	override def asEmpty :Nullable[MutableEmptyBinaryTrie[K, S, T] with T] = Nullable.Empty
//
//	@unspecialized
//	override def asLeaf :Nullable[MutableBinaryTrieLeaf[K, S, T] with T] = Nullable.Empty
//
//	@unspecialized
//	override def asBranch :Nullable[MutableBinaryTrieBranch[K, S, T] with T] = Nullable.Empty


	/** A mutable modification of this trie concerning a single leaf. The implementation depends on whether this trie -
	  * or any subtrie - is in fact mutable. Mutable tries will simply descend down to the location of the key
	  * and substitute in place the lowest possible subtrie based on the replacement given by `patch`. However, if any
	  * of the nodes on the path is immutable, the algorithm switches to the immutable version of `patchKey` and
	  * replaces it with the new copy produced by recreating the path to the node with nodes produced by `patch`.
	  * Note that if you wish the resulting trie to be mutable, `patch` should return mutable
	  * [[net.turambar.palimpsest.specialty.tries.BinaryTrie.MutableBinaryTrieBranch]] from its [[BinaryTriePatch#patchBranch]]
	  * (and optionally lowest-level callbacks).
	  *
	  * It is possible that the patch operation cannot be performed under this node. Examples include immutable tries,
	  * deleting a key of a two-element trie, or inserting a key which doesn't belong to this subtrie. In that case
	  * a modified substitute is passed to its reference container `parent`.
	  *
	  * Default implementation provided here delegates simply to the immutable variant of this method, but is suitable
	  * only for immutable nodes - those for which the latter method will return `this` ''iff'' no change has been applied.
	  * @param patch a modification of this trie specifying the contents of the new trie at the given `key`, depending on
	  *              whether it is present in this trie. Serves as an abstract factory for any replacement nodes in the
	  *              resulting trie
	  * @param parent holder of the reference to this trie - either a parent node in a larger trie, or a wrapper such
	  *               as [[TriePot]]
	  * @param key the modified key
	  * @return `true` if and only if the set of keys contained in the subtrie rooted at `parent` changed as the result
	  *        of this operation. Note that patch implementations which change a node without replacing it (such as
	  *        modifying a value associated with the key in a `Map` implementation) will return `false`.
	  */
	def patchKey(patch :BinaryTriePatch[K, S, T], parent :MutableTrieParent[T])(key :K) :Boolean = {
		val replacement = patchKey[S, T](patch)(key)
		if (!(replacement eq this)){
			parent.setSubtrie(replacement)
			true
		}else
			false
	}


	/** Applies the given patch to every key in this trie, invoking its `whenKeyExists` method. The process modifies
	  * the trie in place while it is being traversed. Any new nodes introduced by the patch will not be included
	  * in the traversal. The process is equivalent to creating a copy collection with all keys from this trie
	  * and calling [[BinaryTrie#patchKey]] repeatedly with the same arguments.
	  *
	  * Default implementation treats this trie as if it were immutable, repeatedly creating new copies resulting from
	  * individual invocations of [[BinaryTrie#patchKey]]. It is however overridden by all three mutable node implementations.
	  *
	  * @param patch a modification applied to this trie on a key by key basis. Each leaf is replaced with the result
	  *              of its `whenKeyExists` method, providing it is not the the same leaf.
	  * @param parent holder of the reference to this trie - either a parent node in a larger trie, or a wrapper such
	  *               as [[TriePot]].
	  */
	def patchAll(patch :BinaryTriePatch[K, S, T], parent :MutableTrieParent[T]) :Unit = {
		var root :T = this
		foreach(new TrieElements.TrieKeys[K, S]) { key => root = root.patchKey(patch)(key) }
		parent.setSubtrie(root)
	}


	/** In place filtering of this trie following the semantics of [[scala.collection.mutable.Set.retain]].
	  * This algorithm happens in fact completely in place, without an intermediate collection being created.
	  * @param elements accessor function-like object returning the element for a given leaf.
	  * @param parent the wrapper of reference to this trie which is updated if the operation requires changing of the trie root
	  *               (such as deleting all keys).
	  * @param f predicate which must be satisfied by a leaf to remain in the trie
	  * @tparam E element type used for this operation.
	  */
	def retain[E](elements :ElementOf[E, S], parent :MutableTrieParent[T])(f :E => Boolean) :Unit =
		parent.setSubtrie(filter(elements)(f))



	override def isMutable :Boolean = true

}





object GenericBinaryTrie {

	/** An enforced self type for binary tries mandating their structure to consist solely of leaves carrying values
	  * and inner nodes with two children. [[BinaryTrie]] trait constrains its self type with a
	  * `this :BinaryTrieNode =>` preamble. This is a sealed trait that cannot be extended directly, so it makes sure
	  * that all concrete classes extend one of [[EmptyBinaryTrie]], [[BinaryTrieLeaf]], [[BinaryTrieBranch]] types
	  * and comply with their contracts. This knowledge allows for more efficient, dedicated implementations.
	  *
	  * @see [[EmptyBinaryTrie]]
	  * @see [[BinaryTrieLeaf]]
	  * @see [[BinaryTrieBranch]]
	  */
	sealed trait BinaryTrieNode


//	/** Base trait extended only by non empty tries, that is leaves and branches.
//	  * This interface '''is not specialized''' and must be mixed in '''after''' [[BinaryTrie]] in order to not sabotage
//	  * linearization of the generic and specialized versions of the latter.
//	  */
//	sealed trait NonEmptyTrie[K, +T <: BinaryTrie[K, T]] extends BinaryTrie[K, T] { this :T with BinaryTrieNode => }


	/** Base trait extended by implementations of empty binary tries */
	trait GenericEmptyBinaryTrie[@specialized(KeyTypes) K, +S <: GenericBinaryTrie[K, S, S], +T <: GenericBinaryTrie[K, S, T] with S]
		extends GenericBinaryTrie[K, S, T] with BinaryTrieNode with EmptyTrie[K, T] with EmptyElements[K, S, T]
	{ this :T =>

//		@unspecialized
//		override def asEmpty :Nullable[GenericEmptyBinaryTrie[K, S, T] with T] = new Nullable(this)

		override def viewHead :S = this

		override def viewLast :S = this

		override def viewNode(idx :Var[Int]) :S = this

		override def viewNode(key :K) :S = this

		/** Overriden to make public. */
		override def nodeFor(key :K) :T = this

		/** Overriden to make public. */
		override def hasKey(key :K) :Boolean = false

//		override def patchKey[U >: T <: Trie[K, U]](patch :BinaryTriePatch[K, S, U])(key :K) :U =
//			patch.whenTrieEmpty(key, this)

		override def patchKey[F >: S <: BinaryTrie[K, F], U >: T <: Trie[K, U]](patch :BinaryTriePatch[K, F, U])(key :K) :U =
			patch.whenTrieEmpty(key, this)


		override def isMutable :Boolean = false

		override def stable :S = this

		override def copy :T = this

		override def view :S = this

		override def freeze() :Unit = ()
	}



	/** The basic form of [[GenericEmptyBinaryTrie]] using the same type `S` as both the self type and immutable type.
	  * Used to indicate both really immutable instances and all instances which mutability does not matter in the
	  * context of reference.
	  * @tparam K key type of the trie
	  * @tparam S self, non-mutable type of this trie.
	  */
	type EmptyBinaryTrie[@specialized(KeyTypes) K, +S <: BinaryTrie[K, S]] = GenericEmptyBinaryTrie[K, S, S]



	/** An empty binary trie implementing the mutable interface. As empty tries have no possible contents, it is
	  * immutable in reality, but the [[MutableBinaryTrie]] trait is invariant regarding its self type, which would
	  * restrict the usage of this class as an immutable instance in a polymorphic context.
	  * @tparam K key type associated with this trie.
	  * @tparam T self type of this trie, that is the type of all new tries returned by this trait.
	  * @tparam S stable version of this trie.
	  */
	trait MutableEmptyBinaryTrie[@specialized(KeyTypes) K,  S <: GenericBinaryTrie[K, S, S], T <: GenericBinaryTrie[K, S, T] with S]
		extends MutableBinaryTrie[K, S, T] with GenericEmptyBinaryTrie[K, S, T]
	{ this :T =>

//		@unspecialized
//		override def asEmpty :Nullable[MutableEmptyBinaryTrie[K, S, T] with T] = new Nullable(this)

		override def patchKey(patch :BinaryTriePatch[K, S, T], parent :MutableTrieParent[T])(key :K) :Boolean =
			patch.whenTrieEmpty(key, this) match {
				case empty if empty.isEmpty => false
				case leaf => parent.setSubtrie(leaf); true
			}

		override def patchAll(patch :BinaryTriePatch[K, S, T], parent :MutableTrieParent[T]) :Unit = ()

		override def retain[E](elements :ElementOf[E, S], parent :MutableTrieParent[T])(f :E => Boolean) :Unit = ()
	}






	/** Base trait extended by concrete implementations of leaf nodes, serving also as singleton tries. */
	trait GenericBinaryTrieLeaf[@specialized(KeyTypes) K,  +S <: GenericBinaryTrie[K, S, S], +T <: GenericBinaryTrie[K, S, T] with S]
		extends GenericBinaryTrie[K, S, T] with TrieLeaf[K, T] with BinaryTrieNode with LeafElement[K, S, T]
	{ this :T =>

//		@unspecialized
//		override def asLeaf :Nullable[GenericBinaryTrieLeaf[K, S, T] with T] = new Nullable(this)

		override def viewHead :S = this

		override def viewLast :S = this

		override def viewNode(idx :Var[Int]) :S =
			if (idx.get == 0) { idx.--; this }
			else { idx.--; emptyTrie }

		override def viewNode(key :K) :S =
			if (key == this.key) this
			else emptyTrie

		/** Overriden to make public. */
		override def hasKey(k :K) :Boolean = key == k

		/** Overriden to make public. */
		override def nodeFor(key :K) :T =
			if (key == this.key) copy
			else emptyTrie



//		override def patchKey[U >: T <: Trie[K, U]](patch :BinaryTriePatch[K, S, U])(key :K) :U =
//			if (key == this.key) patch.whenKeyExists(key, this)
//			else patch.whenNoKey(key, this)


		override def patchKey[F >: S <: BinaryTrie[K, F], U >: T <: Trie[K, U]](patch :BinaryTriePatch[K, F, U])(key :K) :U =
			if (key == this.key) patch.whenKeyExists(key, this)
			else patch.whenNoKey(key, this)


		override def isMutable :Boolean = false

		override def stable :S = this //todo: when implementing mutable leaves, remove it

		override def copy :T = this //todo: when implementing mutable leaves, remove it

		override def view :S = this

		override def freeze() :Unit = ()

	}



	/** The basic form of [[GenericBinaryTrieLeaf]] using the same type `S` as both the self type and immutable type.
	  * Used to indicate both really immutable instances and all instances which mutability does not matter in the
	  * context of reference.
	  * @tparam K key type of the trie
	  * @tparam S self, non-mutable type of this trie.
	  */
	type BinaryTrieLeaf[@specialized(Int, Long) K, +S <: BinaryTrie[K, S]] = GenericBinaryTrieLeaf[K, S, S]



	/** Mutable interface of binary trie leaves. Note that unless subclasses introduce additional data, it is de facto
	  * immutable despite its mutable trait.
	  * @tparam K key type associated with this trie.
	  * @tparam T self type of this trie, that is the type of all new tries returned by this trait.
	  * @tparam S stable version of this trie.
	  */
	trait MutableBinaryTrieLeaf[@specialized(KeyTypes) K,  S <: GenericBinaryTrie[K, S, S], T <: GenericBinaryTrie[K, S, T] with S]
		extends MutableBinaryTrie[K, S, T] with GenericBinaryTrieLeaf[K, S, T]
	{ this :T =>

//		@unspecialized
//		override def asLeaf :Nullable[MutableBinaryTrieLeaf[K, S, T] with T] = new Nullable(this)

		override def keyNode(idx :Var[Int]) :T =
			if (idx.get == 0) { idx.--; copy }
			else { idx.--; emptyTrie }


		override def patchKey(patch :BinaryTriePatch[K, S, T], root :MutableTrieParent[T])(key :K) :Boolean =
			if (key == this.key) {
				val replacement = patch.whenKeyExists(key, this)
				if (!(replacement eq this)) {
					root.setSubtrie(replacement); true
				} else false
			} else {
				val replacement = patch.whenNoKey(key, this)
				if (!(replacement eq this)) {
					root.setSubtrie(replacement); true
				} else false
			}

		override def patchAll(patch :BinaryTriePatch[K, S, T], parent :MutableTrieParent[T]) :Unit = {
			val substitute = patch.whenKeyExists(key, this)
			if (!(substitute eq this))
				parent.setSubtrie(substitute)
		}

		override def retain[E](elements :ElementOf[E, S], parent :MutableTrieParent[T])(f :E => Boolean) :Unit =
			if (!elements.satisfies(this, f))
				parent.setSubtrie(emptyTrie)
	}






	/** Base class for inner nodes in binary tries, containing two subtries below it. Note that this is a class rather
	  * than a trait and '''is not specialized''' regarding the key, meaning that any derived classes '''must''' mix in
	  * [[BinaryTrie]] separately. For this reason this class does '''not''' extend [[GenericBinaryTrie]] and requires
	  * it to be mixed in by extending classes. By making it a class, all traversing methods can reference
	  * directly member fields of this instance rather than through a virtual getter.
	  *
	  * This skeleton implementation has no knowledge about locating a node for a given key, but can implement
	  * all collection-like methods returning a new trie with elements from this trie thanks to a virtual copy constructor.
	  * @param _left 'left' (first) child of this node
	  * @param _right 'right' (second) child of this node
	  * @tparam K type of keys in this trie
	  * @tparam T self type of this trie (and its children)
	  */
	abstract class GenericBinaryTrieBranch[K, +S <: GenericBinaryTrie[K, S, S], +T <: GenericBinaryTrie[K, S, T] with S]
	                                      (private[this] var _left :S, private[this] var _right :S)
		extends TrieBranchTemplate[K, S, T] with BinaryTrieNode with GenericBinaryTrie[K, S, T]
	{ this :T => //with GenericBinaryTrie[K, S, T] =>

		/** Supertype of all branches in this trie. */
		protected[this] type SuperBranch = BinaryTrieBranch[K, S]

//		override def asBranch :Nullable[GenericBinaryTrieBranch[K, S, T] with T] = Nullable(this)
//
//		@inline final override def asTrie :T = this


		@inline final override def left :S = _left

		@inline final protected[this] def left_=(subtrie :S) :Unit = _left = subtrie

		@inline final override def right :S = _right

		@inline final protected[this] def right_=(subtrie :S) :Unit = _right = subtrie

		/** Returns `left.view`, setting to it the left child at the same time. This makes the returned copy immutable. */
		@inline final def leftView :S = { val v = _left.view; _left = v; v }

		/** Returns `right.view`, setting to it the right child at the same time. This makes the returned copy immutable. */
		@inline final def rightView :S = { val v = _right.view; _right = v; v }



		override def stable :S = this

		override def freeze() :Unit = ()

		override def copy :T = this

		override def clone() :T = {
			def rec(node :S) :T = node match {
				case branch :SuperBranch => branchLike(branch.asTrie)(rec(branch.left), rec(branch.right))
				case _ => cloneLeaf(node)
			}
			rec(this)
		}

		protected[this] def cloneLeaf(leaf :S) :T //= leaf

		protected[this] def leafLike(leaf :S) :T //= leaf


		/* The following abundance of factory/copy methods is motivated by the desire to avoid copying,
		 * or even calls to potentially no-op methods like leafLike if possible. Methods like take/drop and others
		 * defined here implemented so that if there is a need for copying, it will be done by the subclass.
		 * This achieves almost zero performance impact associated with sharing mutable and immutable nodes for
		 * the immutable tries.
		 */

		/** Given a subtrie being the result of this instances ''patch'' or ''swap'' methods, adapt it to be of the same
		  * class as this instance. Mutable branches need do nothing here, but immutable nodes which share their
		  * subtries directly need to call `view` to make sure the result is a valid trie root.
		  * @param trie a trie being the result of one of this instance's protected `take`/`drop` methods.
		  * @return a trie of the same mutability as this instance.
		  */
		protected[this] def export(trie :T) :T = trie


		/** Adapt a node being a descendant of this branch to the public self type `T` for the purpose of becoming
		  * part of another trie constructed by modifying this one. It should have the same semantics as `subtrie.copy`
		  * in that no cross-modification should be possible, with the difference being that the dynamic
		  * type of the returned node is determined by this object rather than the argument.
		  * @param subtrie a descendant node of this trie
		  * @return a new node with equal children as `subtrie` but of the same class as this instance
		  *         and protected from outside modification.
		  */
		protected[this] def share(subtrie :S) :T



		/** Performs the exact same purpose as [[BinaryTriePatch#patchLeft]]: creates a new copy of the given trie after
		  * possible modification to its left child. Returned node must be safe from outside modification, but
		  * immutable instances can simply return `branch` if `left` and `branch.left` reference the same object.
		  * The difference between this method and [[BinaryTrieBranch#swapLeft]] is that the latter always creates
		  * a new object and is generally called when `left` is already known to be changed or a new copy is explicitly
		  * required to preserve mutability.
		  * @param branch a node located under this trie.
		  * @param left a possibly modified version of `branch.left`, already deemed safe to share.
		  */
		protected[this] def patchLeft(branch :SuperBranch, left :S) :T =
			branchLike(branch.asTrie)(left, branch.right)


		/** Performs the exact same purpose as [[BinaryTriePatch#patchRight]]: creates a new copy of the given trie after
		  * possible modification to its right child. Returned node must be safe from outside modification, but
		  * immutable instances can simply return `branch` if `right` and `branch.right` reference the same object.
		  * The difference between this method and [[BinaryTrieBranch#swapRight]] is that the latter always creates
		  * a new object and is generally called when `right` is already known to be changed or a new copy is explicitly
		  * required to preserve mutability.
		  * @param branch a node located under this trie.
		  * @param right a possibly modified version of `branch.right`, already deemed safe to share.
		  */
		protected[this] def patchRight(branch :SuperBranch, right :S) :T =
			branchLike(branch.asTrie)(branch.left, right)


		/** Returns a branch based on the given prototype instance with its children replaced. Returned node
		  * should share all information with `branch` except for its subtries; it is caller's responsibility to
		  * ensure that `left` and `right` are valid children and any other invariants of `T` will hold after
		  * creating the copy.
		  * This method is in general called as the result of filtering a node in this trie, 
		  * @param branch a node in this trie
		  * @param left a replacement for the left node of `branch`, with the same longest common key prefix as `branch.left`.
		  * @param right a replacement for the right node of `branch`, with the same longest common key prefix as `branch.right`.
		  * @return a new branch with `left` and `right` as its children of the same class as this instance, or simply `branch` 
		  *         if this trie is immutable and `left == branch.left && right == branch.right`.
		  */
		protected[this] def patchBranch(branch :SuperBranch)(left :S, right :S) :T =
			branchLike(branch.asTrie)(left, right)



		/** Make a copy of the given subtrie of this trie by substituting its left branch with a new subtrie.
		  * The returned node should be safe from outside modification and its right subtrie should equal `branch.right`.
		  * The new child given as the argument is already deemed safe to share.
		  * @param branch a branch located under this trie
		  * @param left a trie created with this instances factory methods from elements under `branch.left`.
		  * @return a new branch node of the same mutability as `this` with everything copied from `branch` aside of its
		  *         left child.
		  */
		protected[this] def swapLeft(branch :SuperBranch, left :S) :T =
			branchLike(branch.asTrie)(left, branch.right) //new immutable branch (swapLeft is overridden in mutable branch)


		/** Make a copy of the given subtrie of this trie by substituting its right branch with a new subtrie.
		  * The returned node should be safe from outside modification and its left subtrie should equal `branch.left`.
		  * The new child given as the argument is already deemed safe to share.
		  * @param branch a branch located under this trie
		  * @param right a trie created with this instances factory methods from elements under `branch.right`.
		  * @return a new branch node of the same mutability as `this` with everything copied from `branch` aside of its
		  *         right child.
		  */
		protected[this] def swapRight(branch :SuperBranch, right :S) :T =
			branchLike(branch.asTrie)(branch.left, right)

		/** Make a copy of the given branch with the same children (in terms of equality) but of the same class as this instance. */
		protected[this] def branchLike(branch :SuperBranch) :T

		/** A copy constructor creating a new branch of this trie type based on the given template branch and new children.
		  * @param branch a branch being modified - any information outside the actual subtries should be carried over.
		  * @param left a non-empty trie resulting from filtering `branch.left` (possibly equal).
		  * @param right a non-empty trie resulting from filtering `branch.right` (possibly equal).
		  * @return a copy of `branch` with its children replaced by `left` and `right`.
		  */
		protected[this] def branchLike(branch :S)(left :S, right :S) :T





		override def isEmpty = false

		override def nonEmpty = true

		override def size :Int = {
			def rec(node :S) :Int = node match {
				case branch :SuperBranch =>
					rec(branch.left) + rec(branch.right)
				case _ => 1
			}
			rec(left) + rec(right)
		}

		override def ofAtLeast(items :Int) :Boolean = {
			def remainder(node :S, amount :Int) :Int = node match {
				case branch :SuperBranch =>
					val rem = remainder(branch.left, amount)
					if (rem <= 0) rem
					else remainder(branch.right, rem)
				case _ =>
					amount - 1
			}
			remainder(this, items) <= 0
		}



		override def headNode :T = leafLike(viewHead(left))

		override def viewHead :S = viewHead(left)

		@tailrec final protected[this] def viewHead(node :S) :S = node match {
			case branch :SuperBranch => viewHead(branch.left)
			case _ => node
		}


		override def lastNode :T = leafLike(viewLast)

		override def viewLast :S = {
			@tailrec def rec(node :S) :S = node match {
				case branch :SuperBranch => rec(branch.right)
				case _ => node
			}
			rec(right)
		}


		override def keyNode(n :Var[Int]) :T = leafLike(viewNode(n)) //todo: handle empty trie if n out of bounds

		override def viewNode(n :Var[Int]) :S = {
			def rec(node :S, i :Var[Int]) :S = node match {
				case branch :SuperBranch =>
					val t = rec(branch.left, i)
					if (i.get < 0) t
					else rec(branch.right, i)
				case _ => i.--; node
			}
			val res = rec(this, n)
			if (n.get < 0) res
			else emptyTrie
		}


//		override def nodeFor(key :K) :T = leafLike(viewNode(key))


		override def tail :T = left match { //manually inlined to guarantee correct mutability of the root node
			case branch :SuperBranch => swapLeft(this, tail(branch))
			case _ => leafLike(right)
		}

		protected[this] final def tail(subtrie :SuperBranch) :S = subtrie.left match {
			case branch :SuperBranch => swapLeft(subtrie, tail(branch))
			case _ => subtrie.rightView
		}


		override def init :T = right match { //inlined to guarantee returned root node has correct mutability
			case branch :SuperBranch => swapRight(this, init(branch))
			case _ => leafLike(left)
		}

		protected[this] final def init(subtrie :SuperBranch) :S = subtrie.right match {
			case branch :SuperBranch =>
				swapRight(subtrie, init(branch))
			case _ => subtrie.leftView
		}



		override def dropTrie(count :Var[Int]) :T =
			if (count.get <= 0) copy //this
			else dropTrie(this, count)(emptyTrie).asInstanceOf[T]

		protected[this] final def dropTrie(node :S, count :Var[Int], root :Boolean = true)(implicit Empty :T) :S = node match {
			case branch :SuperBranch => //already safe for mutable leaves
				val l = dropTrie(branch.left, count, false)
				if (count.get > 0)
					dropTrie(branch.right, count, root)
				else if (l eq Empty)
					if (root) //branch.right is the final result and must be adapted to T
						share(branch.right) //requires creation of 3 new nodes for mutable tries
					else //will be suspended under another branch, we need only assure it won't be externally modified
						branch.rightView
				else
					swapLeft(branch, l)
			case _ => count.--; Empty
		}



		override def takeTrie(count :Var[Int]) :T =
			if (count.get <= 0) emptyTrie
			else takeTrie(this, count) //result is a branch created by patchRight/patchBranch

		protected[this] final def takeTrie(branch :SuperBranch, count :Var[Int]) :T = branch.left match {
			//longer than necessary in order to avoid calling leafLike for every leaf
			case lbranch :SuperBranch =>
				val l = takeTrie(lbranch, count)
				if (count.get <= 0)
					l
				else branch.right match {
					case rbranch :SuperBranch =>
						patchBranch(branch)(l, takeTrie(branch, count))
					case _ =>
						count.--; patchLeft(branch, l)
				}
			case lleaf if count.get == 1 =>
				count := 0; leafLike(lleaf)

			case _ => branch.right match {
				case rbranch :SuperBranch =>
					count.--; patchRight(branch, takeTrie(rbranch, count))
				case _ =>
					count -= 2; branchLike(branch)

			}
		}



		override def dropRightTrie(count :Var[Int]) :T =
			if (count.get <= 0) copy
			else dropRightTrie(this, count)(emptyTrie).asInstanceOf[T] //returns only results of swapRight or leftView, so is immutable

		protected[this] final def dropRightTrie(node :S, count :Var[Int], root :Boolean = true)(implicit Empty :T) :S = node match {
			case branch :SuperBranch => //already safe for mutable leaves
				val r = dropRightTrie(branch.right, count, false)
				if (count.get > 0)
					dropRightTrie(branch.left, count, root)
				else if (r eq Empty)
					if (root) share(branch.left) //final result, so we need to prevent modification here
					else branch.leftView //will be suspended from a branch, no need to preserve this object's mutability
				else
					swapRight(branch, r)
			case _ => count.--; Empty
		}



		override def takeRightTrie(count :Var[Int]) :T =
			if (count.get <= 0) emptyTrie
			else takeRightTrie(this, count)

		protected[this] final def takeRightTrie(branch :SuperBranch, count :Var[Int]) :T = branch.right match {
			//as with other methods, written so that (almost) all calls to .view and similar are left to the subclass' patch methods
			case rbranch :SuperBranch =>
				val r = takeRightTrie(rbranch, count)
				if (count.get <= 0)
					r
				else branch.left match {
					case lbranch :SuperBranch =>
						patchBranch(branch)(takeRightTrie(branch, count), r)
					case lleaf =>
						count.--; patchRight(branch, r)
				}
			case rleaf if count.get == 1 =>
				count := 0; leafLike(rleaf)

			case _ => branch.left match {
				case lbranch :SuperBranch =>
					count.--; patchLeft(branch, takeRightTrie(lbranch, count))
				case _ =>
					count -= 2; branchLike(branch)

			}
		}




		override def sliceTrie(dropKeys :Var[Int], takeKeys :Var[Int]) :T =
			if (takeKeys.get <= 0)
				emptyTrie
			else
				dropTake(this, dropKeys, takeKeys)(emptyTrie)

		protected[this] final def dropTake(subtrie :S, dropKeys :Var[Int], takeKeys :Var[Int])(implicit Empty :T) :T = subtrie match {
			case branch :SuperBranch =>
				val l = dropTake(branch.left, dropKeys, takeKeys)
				if (takeKeys.get <= 0)
					l //dropped and took all we needed
				else if (dropKeys.get > 0)
					dropTake(branch.right, dropKeys, takeKeys) //dropped  whole left, more to drop
				else if (l eq Empty) branch.right match { //dropping ends now, start taking
					case rbranch :SuperBranch => takeTrie(rbranch, takeKeys)
					case rleaf => leafLike(rleaf)
				} else branch.right match {//started taking already somewhere in branch.left
					case rbranch :SuperBranch => patchBranch(branch)(l, takeTrie(rbranch, takeKeys))
					case _ => takeKeys.--; patchLeft(branch, l)
				}
			case _ => //we only drop leaves - all taking done by takeTrie
				dropKeys.--; Empty
		}



		override def splitTrie(idx :Var[Int]) :(T, T) =
			if (idx.get <= 0) (emptyTrie, copy)
			else
				splitTrie(this, idx)(emptyTrie)

		protected[this] final def splitTrie(subtrie :S, idx :Var[Int])(implicit Empty :T) :(T, T) = subtrie match {
			case branch :SuperBranch =>
				val lsplit = splitTrie(branch.left, idx)
				if (idx.get > 0) { //lsplit._1 == branch.left
					val (l, r) = splitTrie(branch.right, idx)
					(patchBranch(branch)(lsplit._1, l), r)
				} else if (lsplit._2 eq Empty) { //branch is sitting on the fence
					(lsplit._1, share(branch.right))
				} else //idx falls inside branch.left
					(lsplit._1, swapLeft(branch, lsplit._2))

			case _ => idx.--; (leafLike(subtrie), Empty) //leaf
		}




		override def dropWhile[E](elements :ElementOf[E, S])(f :E => Boolean) :T =
			export(dropWhile(this)(elements, f, emptyTrie))

		protected[this] final def dropWhile[E](node :S)(implicit elements :ElementOf[E, S], f :E => Boolean, Empty :T) :T =
			node match {
				case branch :SuperBranch =>
					val l = dropWhile(branch.left)
					if (l eq Empty) dropWhile(branch.right)
					else patchLeft(branch, l)
				case _ if elements.satisfies(node, f) => Empty
				case _ => leafLike(node) //this case is executed at most once
			}



		override def takeWhile[E](elements :ElementOf[E, S])(f :E => Boolean) :T =
			export(takeWhile(this)(elements, f, emptyTrie))


		protected[this] final def takeWhile[E](node :S)(implicit elements :ElementOf[E, S], f :E => Boolean, Empty :T) :T = node match {
			case branch :SuperBranch =>
				val l = takeWhile(branch.left)
				if (l eq branch.left) {
					val r = takeWhile(branch.right)
					if (r eq Empty) l
					else patchRight(branch, r)
				} else l
			case _ if elements.satisfies(node, f) => leafLike(node) //todo:
			case _ => Empty
		}



		override def span[E](elements :ElementOf[E, S])(f :E => Boolean) :(T, T) = {
			val res = span(this)(elements, f, emptyTrie)
			val l = export(res._1)
			val r = export(res._2)
			if ((l eq res._1) & (r eq res._2)) res
			else (export(res._1), export(res._2))
		}

		protected[this] final def span[E](node :S)(implicit elements :ElementOf[E, S], f :E=>Boolean, Empty :T) :(T, T) =
			node match {
				case branch :SuperBranch =>
					val (l1, l2) = span(branch.left)
					if (l2 eq Empty) {
						val (r1, r2) = span(branch.right)
						if (r1 eq Empty) (l1, r2)
						else (patchBranch(branch)(l1, r1), r2)
					} else
						(l1, patchLeft(branch, l2))
				case _ if elements.satisfies(node, f) =>
					(leafLike(node), Empty)
				case _ =>
					(Empty, leafLike(node))
			}



		override def partition[E](elements :ElementOf[E, S])(f :E => Boolean) :(T, T) = {
			val Empty = emptyTrie
			def rec(node :S) :(T, T) = node match { //good also for mutable tries
				case branch :SuperBranch =>
					val (lt, lf) = rec(branch.left)
					val (rt, rf) = rec(branch.right)
					if (lf eq Empty)
						if (rt eq Empty) (lt, rf)
						else (branchLike(node)(lt, rt), rf)
					else if (lt eq Empty)
						if (rf eq Empty) (rt, lf)
						else (rt, branchLike(node)(lf, rf))
					else
						if (rt eq Empty) (lt, branchLike(node)(lf, rf))
						else if (rf eq Empty) (branchLike(node)(lt, rt), lf)
						else (branchLike(node)(lt, rt), branchLike(node)(lf, rf))
				//leaf cases
				case _ if elements.satisfies(node, f) => (leafLike(node), Empty) //todo: copy() required only for mutable maps
				case _ => (Empty, leafLike(node))
			}
			rec(this)
		}



		override def filter[E](elements :ElementOf[E, S])(f :E => Boolean, where :Boolean) :T =
			filter(this)(elements, emptyTrie, f, where)

		final protected[this] def filter[E](node :S)(implicit elements :ElementOf[E, S], Empty :T, f :E=>Boolean, where :Boolean) :T = node match { //implementation suitable also for mutable tries
			case branch :SuperBranch =>
				val l = filter(branch.left); val r = filter(branch.right)
				if (l eq Empty) r
				else if (r eq Empty) l
				else branchLike(node)(l, r) //patchBranch(branch)(l, r)
			case _ if elements.satisfies(node, f) == where => leafLike(node) //todo: copy is only needed for mutable maps
			case _ => Empty
		}



		override def find_?[E](elements :ElementOf[E, S])(f :E => Boolean, where :Boolean): ?[E] = {
			def rec(node :S): ?[E] = node match {
				case branch :SuperBranch =>
					val res = rec(branch.left)
					if (res.isEmpty) rec(branch.right)
					else res
				case _ =>
					elements.satisfying(node, f, where)
			}
			val res = rec(left)
			if (res.isEmpty) rec(right)
			else res
		}


		override def count[E](elements :ElementOf[E, S])(f :E => Boolean) :Int = {
			def rec(node :S) :Int = node match {
				case branch :SuperBranch => rec(branch.left) + rec(branch.right)
				case _ if elements.satisfies(node, f) => 1
				case _ => 0
			}
			rec(left) + rec(right)
		}





		override def foreach[@specialized(Fun1) E, @specialized(Unit) O](elements :ElementOf[E, S])(f :E => O) :Unit = {
			def rec(node :S) :Unit = node match {
				case branch :SuperBranch =>
					rec(branch.left); rec(branch.right)
				case _ => f(elements.elementOf(node))
			}
			rec(left); rec(right)
		}


		override def reverseForeach[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Unit) :Unit = {
			def rec(node :S) :Unit = node match {
				case branch :SuperBranch =>
					rec(branch.right); rec(branch.right)
				case _ => f(elements.elementOf(node))
			}
			rec(right); rec(left)
		}


		override def foldLeft[@specialized(Fun2) E, @specialized(Fun2) O](elements :ElementOf[E, S])(acc :O)(f :(O, E) => O) :O = {
			def rec(node :S, res :O) :O = node match {
				case branch :SuperBranch => rec(branch.right, rec(branch.left, res))
				case _ => f(res, elements.elementOf(node))
			}
			rec(right, rec(left, acc))
		}


		override def foldRight[@specialized(Fun2) E, @specialized(Fun2) O](elements :ElementOf[E, S])(acc :O)(f :(E, O) => O) :O = {
			def rec(node :S, res :O) :O = node match {
				case branch :SuperBranch => rec(branch.left, rec(branch.right, res))
				case _ => f(elements.elementOf(node), res)
			}
			rec(left, rec(right, acc))
		}



		override def copyToArray[@specialized(Elements) E](elements :ElementOf[E, S])(array :Array[E], from :Int, max :Int) :Int =
			if (from < 0)
				throw new IndexOutOfBoundsException(s"BinaryTrieBranch.copyToArray([], $from, $max)")
			else if (from >= array.length || max <= 0)
				0
			else {
				def rec(node :S, start :Int, count :Int) :Int = node match {
					case branch :SuperBranch =>
						val copied = rec(branch.left, start, count)
						if (copied < count)
							copied + rec(branch.right, start + copied, count - copied)
						else
							count
					case _ =>
						array(start) = elements.elementOf(node); 1
				}
				rec(this, from, math.min(max, array.length-from))
			}


		override def iterator[@specialized(Elements) E](elements :ElementOf[E, S]) :FitIterator[E] =
			new LeafElementIterator[E, K, S](elements, this, maxDepth)

		override def reverseIterator[@specialized(Elements) E](elements :ElementOf[E, S]) :FitIterator[E] =
			new ReverseLeafElementIterator[E, K, S](elements, this, maxDepth)


		protected def maxDepth :Int

	}



	/** The basic form of [[GenericBinaryTrieBranch]] representing its immutable interface. Tries of this type can
	  * be both mutable and immutable in reality.
	  * @tparam K trie key type
	  * @tparam T trie self type; `trie :T` for any `trie :BinaryTrieBranch[K, T]`.
	  */
	type BinaryTrieBranch[K, +T <: BinaryTrie[K, T]] = GenericBinaryTrieBranch[K, T, T]



	/** Immutable trie branch. Note that this trait is not specialized, so just as with its base class [[GenericBinaryTrieBranch]],
	  * derived classes must mix in [[GenericBinaryTrie]] independently to assure specialization.
	  * @tparam K type of keys in this trie.
	  * @tparam S the immutable self type of this trie.
	  */
	trait StableBinaryTrieBranch[K, +S <: BinaryTrie[K, S]]
		extends GenericBinaryTrieBranch[K, S, S] with GenericBinaryTrie[K, S, S]
	{ this :S =>

		override def isMutable = false

		override def view :S = this


		override protected[this] def export(subtrie :S) :S = subtrie.view

		override protected[this] def share(subtrie :S) :S = subtrie //return as-is and call export for the final root instead


		override protected[this] def branchLike(branch :SuperBranch) :S = branch.asTrie


		override protected[this] def patchLeft(branch :SuperBranch, left :S) :S =
			if (left eq branch.left) branch.asTrie
			else branchLike(branch.asTrie)(left, branch.right)

		override protected[this] def patchRight(branch :SuperBranch, right :S) :S =
			if (right eq branch.right) branch.asTrie
			else branchLike(branch.asTrie)(branch.left, right)

		override protected[this] def patchBranch(branch :SuperBranch)(left :S, right :S) :S =
			if ((left eq branch.left) & (right eq branch.right)) branch.asTrie
			else branchLike(branch.asTrie)(left, right)


		
		override def dropTrie(count :Var[Int]) :S =
			if (count.get <= 0) this
			else dropTrie(this, count)(emptyTrie).view

		override def takeTrie(count :Var[Int]) :S =
			if (count.get <= 0) emptyTrie
			else takeTrie(this, count).view

		override def dropRightTrie(count :Var[Int]) :S =
			if (count.get <= 0) this
			else dropRightTrie(this, count)(emptyTrie).view
		
		override def takeRightTrie(count :Var[Int]) :S =
			if (count.get <= 0) emptyTrie
			else takeRightTrie(this, count).view


		override def sliceTrie(dropKeys :Var[Int], takeKeys :Var[Int]) :S =
			if (takeKeys.get <= 0) emptyTrie
			else dropTake(this, dropKeys, takeKeys)(emptyTrie).view


		override def splitTrie(idx :Var[Int]) :(S, S) =
			if (idx.get <= 0) (emptyTrie, this)
			else {
				val res = splitTrie(this, idx)(emptyTrie)
				val l = res._1.view
				val r = res._2.view
				if ((l eq res._1) & (r eq res._2)) res
				else (res._1.view, res._2.view)
			}


	}




	/** A mutable inner branch node of binary tries, which can have its children replaced. Note that just like
	  * [[BinaryTrieBranch]], this trait is '''not specialized''' and [[MutableBinaryTrie]] must be mixed in individually
	  * by any extending classes. Overrides the `drop/take` family of methods to safely reuse contents from under this trie.
	  */
	trait MutableBinaryTrieBranch[K, S <: GenericBinaryTrie[K, S, S], T <: GenericBinaryTrie[K, S, T] with S]
		extends GenericBinaryTrieBranch[K, S, T] with MutableBinaryTrie[K, S, T] with MutableTrieParent[S]
	{ this :T =>

//		@inline final override def asBranch :Nullable[MutableBinaryTrieBranch[K, S, T] with T] = Nullable(this)

		@inline final private[tries] def setLeft(trie :S) :Unit = left = trie

		@inline final private[tries] def setRight(trie :S) :Unit = right = trie

		@inline final private[tries] def replace(child :S, replacement :S) :Unit =
			if (left eq child) left = replacement
			else right = replacement


		
		override protected[this] def branchLike(branch :S)(left :S, right :S) :T //with MutableBinaryTrieBranch[K, T]

		override protected[this] def branchLike(branch :SuperBranch) :T =
			branchLike(branch.asTrie)(branch.leftView, branch.rightView)

		/** Overriden to prevent shared `branch.right` from modification. */
		override protected[this] def swapLeft(branch :SuperBranch, left :S) :T =
			branchLike(branch.asTrie)(left, branch.rightView)

		/** Overriden to prevent shared `branch.left` from modification. */
		override protected[this] def swapRight(branch :SuperBranch, right :S) :T =
			branchLike(branch.asTrie)(branch.leftView, right)

		/** Overriden to prevent shared `branch.right` from modification. Equivalent to `swapLeft` for mutable branches. */
		override protected[this] def patchLeft(branch :SuperBranch, left :S) :T =
			branchLike(branch.asTrie)(left, branch.rightView)

		/** Overriden to prevent shared `branch.left` from modification. Equivalent to `swapRight` for mutable branches. */
		override protected[this] def patchRight(branch :SuperBranch, right :S) :T =
			branchLike(branch.asTrie)(branch.leftView, right)

		/** Same as `branchLike` for mutable branches. */
		override protected[this] def patchBranch(branch :SuperBranch)(left :S, right :S) :T =
			branchLike(branch.asTrie)(left, right)



		override protected[this] def share(subtrie :S) :T = subtrie match {
			case branch :SuperBranch =>
				branch.freeze()
				branchLike(subtrie)(branch.left, branch.right)
			case _ =>
				leafLike(subtrie)
		}



		/** An immutable branch with immutable copies of this node's children. The copying is one level deep only:
		  * any subtries of the children remain intact as any subtrie rooted in an immutable node is considered immutable.
		  */
		override def stable :S = {
			left = left.view
			right = right.view
			view
		}


		/** Substitutes both children with `_.view` to mark them as immutable. */
		override def freeze() :Unit = {
			left = left.view
			right = right.view
		}


		/** Shallow freezing of this trie for modifications. Substitutes both children with `_.view` to prevent
		  * modification by either of the copies and returns a clone.
		  */
		override def copy :T = {
			val l = left.view; left = l
			val r = right.view; right = r
			branchLike(this :S)(l, r)
		}



		override protected[this] def leafLike(leaf :S) :T

		protected[this] def trieLike(subtrie :S) :T = subtrie match {
			case mutable :MutableBinaryTrieBranch[K, S, T] => mutable.asTrie
			case branch :SuperBranch => branchLike(subtrie)(branch.left, branch.right)
			case _ => leafLike(subtrie)
		}


		override def patchAll(patch :BinaryTriePatch[K, S, T], root :MutableTrieParent[T]) :Unit = {
			//fixme all wrong - rewrite based on retain, but maybe better after it's tested...
		}


		override def retain[E](elements :ElementOf[E, S], root :MutableTrieParent[T])(f :E => Boolean) :Unit =
		{   //yes, this method length is unholy, but all cases are that slightly different from each other
			//and the alternative is a couple of functions with lengthy argument lists and complex contracts. Sorry.
			val Empty = emptyTrie
			val stack = new Array[MutableBinaryTrieBranch[K, S, T]](maxDepth)
			var top = 0 //index of the top element of the stack
			stack(0) = this //stack holds the whole path in the trie to child
			var child = left //always one of the children of the top stack element
			do {
				child match {
					case branch :MutableBinaryTrieBranch[K, S, T] =>
						top += 1 //keep going left until the next leaf is reached
						stack(top) = branch
						child = branch.left //ready for the next loop

					case _ :GenericBinaryTrieLeaf[K, S, T] => //todo: mutable leaves
						if (elements.satisfies(child, f)) { //the easy case: we are keeping the leaf.
							while (top >= 0 && (child eq stack(top).right)) { //retreat until we are the left sibling
								child = stack(top).asTrie
								top -= 1
							}
							if (top < 0)
								return //caution: function return
							child = stack(top).right //child becomes its next sibling
						} else { //deleting the child
							if (top < 0) { //we have just deleted the only/last leaf of the trie
								root.setSubtrie(Empty)
								return //caution: function return
							} else {
								val parent = stack(top)
								top -= 1
								if (parent.left eq child) {
									child = parent.right //next node to consider
									if (top < 0) //substitute parent with the sibling of the deleted leaf
										root.setSubtrie(trieLike(child))
									else
										stack(top).replace(parent.asTrie, child)
								} else {
									child = parent.left //already processed sibling
									if (top < 0) {
										root.setSubtrie(trieLike(child))
										return //caution: function return
									} else {
										val granny = stack(top)
										if (granny.left eq parent) { //granny.right hasn't been visited yet
											granny.setLeft(child)
											child = granny.right //ready for the next loop
										} else {
											granny.setRight(child)
											while (top > 0 && (stack(top) eq stack(top - 1).right))
												top -= 1
											if (top == 0) //nowhere to retreat: we were the right-most path in the trie
												return //caution: function return
											top -= 1
											child = stack(top).right //last not visited right turn
										}
									}
								}
							}
						}
					case _ => //immutable branch: switch to the ground-up filter
						if (top < 0) {
							root.setSubtrie(filter(child)(elements, Empty, f, true)) //todo: specialize this
							return //caution: function return
						} else {
							val substitute = filter(child)(elements, Empty, f, true) //todo: specialize this!
							val parent = stack(top)
							if (parent.left eq child) { //parent.right is going to be next
								child = parent.right
								if (substitute.nonEmpty)
									parent.setLeft(substitute)
								else { //delete parent.left from the trie
									top -= 1
									if (top < 0)
										root.setSubtrie(trieLike(child))
									else
										stack(top).replace(parent.asTrie, child)
								}
							} else { //child eq parent.right
								if (substitute.nonEmpty) {
									parent.setRight(substitute)
									while (top > 0 && (stack(top) eq stack(top - 1).right))
										top -= 1
									top -= 1
									if (top < 0)
										return //caution: function return
									child = stack(top).right
								} else { //already visited parent.left will replace parent
									top -= 1
									if (top < 0) {
										root.setSubtrie(trieLike(parent.left))
										return //caution: function return
									} else if (stack(top).left eq parent) {
										stack(top).setLeft(parent.left)
										child = stack(top).right
									} else {
										stack(top).setRight(parent.left)
										while (top > 0 && (stack(top) eq stack(top - 1).right))
											top -= 1
										top -= 1
										if (top < 0)
											return //caution: function return
										child = stack(top).right //stack(top+1) eq stack(top).left
									}
								}
							}
						}
				}
			} while(true)
		}



	}




	/** Modification of a binary trie concerning a single leaf. After the initial callback for a leaf returns a replacement,
	  * the [[BinaryTriePatch#foldUp]] method successively replaces the proper child of each node on the way back up with previously
	  * computed subtrie. While its purpose could be also satisfied with its base trait [[FoldPath]], trie implementations
	  * can make optimizations and implement the backtracking directly.
	  *
	  * The type `T` of argument tries and type `U` of produced tries are separated primarily for the former to be
	  * a contravariant argument, making this trait a valid argument type for methods of `BinaryTrie`. In practice,
	  * implementations will generally have `T =:= U`.
	  *
	  * @tparam K key type in the tries this operation works for
	  * @tparam T the type of the input trie
	  * @tparam U type of the trie resulting from this modification
	  */
	trait BinaryTriePatch[@specialized(KeyTypes) K, T <: BinaryTrie[K, T], U]
		extends FoldPath[K, T, U]
	{

		/** Replace the child `child` of `parent` (which must be a [[BinaryTrieBranch]] with `res`. After determining
		  * which (left, right or none in case of no modification) should be replaced, delegates to
		  * [[BinaryTriePatch#patchLeft]] or [[BinaryTriePatch#patchRight]] to create a new version of `parent`.
		  */
		override final def foldUp(key :K, parent :T, child :T, res :U) :U = parent match {
			case branch :BinaryTrieBranch[K, T] =>
				if (branch.left eq child)
					patchLeft(branch, res)
				else
					patchRight(branch, res)
			case _ => throw new IllegalArgumentException(s"$this.foldUp: parent is not a BinaryTrieBranch: ${parent.getClass}")
		}



		/** Create a new trie branch using the first argument as the template and the provided left child.
		  * This method is invoked when the modification process backtracks up the trie and is used to obtain the
		  * replacement for the node `template` on the path. The left child given as arguments is guaranteed to belong
		  * under the branch `template` in its position and is a sharable node returned by previous steps of this process.
		  * The right child should be preserved the same regarding the contents, but possibly updated to allow sharing
		  * with a new instance. Any other information present in the branch and introduced by subclasses should also
		  * remain the same.
		  * @param branch a branch in the patched trie to update with the new child
		  * @param left a non-empty replacement for the left child of `branch`
		  * @return a replacement for `branch` with the new left child to use as a valid new trie.
		  */
		def patchLeft(branch :BinaryTrieBranch[K, T], left :U) :U


		/** Create a new trie branch using the first argument as the template and the provided right child.
		  * This method is invoked when the modification process backtracks up the trie and is used to obtain the
		  * replacement for the node `template` on the path. The right child given as arguments is guaranteed to belong
		  * under the branch `template` in its position and is a sharable node returned by previous steps of this process.
		  * The left child should be preserved the same regarding the contents, but possibly updated to allow sharing
		  * with a new instance. Any other information present in the branch and introduced by subclasses should also
		  * remain the same.
		  * @param branch a branch in the patched trie to update with the new child
		  * @param right a non-empty replacement for the right child of `branch`
		  * @return a replacement for `branch` with the new right child to use as a valid new trie.
		  */
		def patchRight(branch :BinaryTrieBranch[K, T], right :U) :U

 	}



	/** Creates an initial stack for use by a [[BinaryTrieIterator]] iterating over all leaves in the given trie.
	  * Simply creates the array of the size corresponding to maximal potential depth of the stack given as `depth`,
	  * with the given trie in its first cell. Resulting array can be passed to the iterator constructor, specifying
	  * `0` as the index of the top stack element. The iterator itself will fill the stack by descending to the first leaf
	  * in iteration order.
	  */
	@inline final private[tries] def seedStack[K, T <: BinaryTrie[K, T]](trie :BinaryTrie[K, T], depth :Int) :Array[BinaryTrie[K, T]] = {
		val res = new Array[BinaryTrie[K, T]](depth)
		res(0) = trie
		res
	}



	/** An iterator traversing a trie using a manual stack implemented with an array. Implements the traversal
	  * and all relevant iterator methods except for `head` and `next`. Subclasses need only to implement accessor
	  * retrieving the element type out of current node given as [[BinaryTrieIterator#topNode]].
	  * @param top   index of the top element on the stack. Negative index means an empty iterator.
	  * @param stack path to the current node splitting the trie in two, with the 'right-hand' side containing remaining elements.
	  *              The top of the stack - `stack(top)`, providing `this` is not empty - is a singleton leaf node
	  *              in the trie, being the `head` of this iterator. Below it, are all inner nodes of type [[BinaryTrieBranch]]
	  *              from which we descended to the left child. Thus, all right children of all inner nodes on the path
	  *              remain to be iterated. Whenever we descend right from an inner node, we remove it from the stack
	  *              (replacing it with its child), as there will be nothing more to do with that node.
	  */
	abstract class BinaryTrieIterator[+E, K, +T <: BinaryTrie[K, T]](stack :Array[BinaryTrie[K, T]], private[this] var top :Int)
		extends BaseIterator[E]
	{ this :FitIterator[E] =>

		/** Initializes this iterator to traverse the whole trie given as its first argument.
 		  * @param trie trie to traverse
		  * @param depth reserved stack size (length of maximum path in the trie)
		  */ 
		def this(trie: T, depth :Int) = {
			this(seedStack(trie, depth), 0)
			descend()
		}

		@tailrec final protected def descend(): Unit = stack(top) match {
			case branch :BinaryTrieBranch[K, T] =>
				top += 1
				stack(top) = branch.left
				descend()
			case _ => //found the 'leftmost' singleton - minimum in the tree.
		}

		@inline protected[this] final def topNode :T = stack(top).asInstanceOf[T]


		override def hasDefiniteSize = true

		override def hasNext :Boolean = top >= 0

		override def size :Int = {
			@tailrec def count(i :Int, sum :Int) :Int =
				if (i<0) sum
				else count(i-1, sum + stack(i).asInstanceOf[BinaryTrieBranch[K, T]].right.size)
			if (top<0) 0
			else count(top-1, topNode.size)
		}

		override def ofAtLeast(n :Int) :Boolean = {
			@tailrec def count(i :Int, left :Int) :Boolean =
				left <= 0 || i>=0 && count(i-1, left - stack(i).asInstanceOf[BinaryTrieBranch[K, T]].right.size)
			n <= 0 || top >= 0 && count(top-1, n - topNode.size)
		}


		final def skip(): Unit = {
			stack(top) = null
			top -= 1
			if (top >= 0 && stack(top).plurality > 1) {
				stack(top) = stack(top).asInstanceOf[BinaryTrieBranch[K, T]].right
				descend()
			}
		}




		//todo: is this really any better than looping skip() ? trie.size is O(n)
		override final def drop(n: Int) :FitIterator[E] = {
			if (n > 0 && top >= 0) {
				var left = n - 1
				stack(top) = null
				top -= 1
				while (top >= 0 && left > 0) {
					//the outer loop retreats up the tree dropping right subtrees of size smaller than remaining elements to drop
					val nextTree = stack(top).asInstanceOf[BinaryTrieBranch[K, T]].right
					val leafCount = nextTree.size
					if (leafCount <= left) {
						//covers singletons and empty sets
						left -= leafCount
						stack(top) = null
						top -= 1
					} else {
						//now the next right child up is larger than we need; go down left as long as the tree contains non-dropped elements
						stack(top) = nextTree//.asInstanceOf[Branch].left
						var trieSize = nextTree.size
						while (trieSize > left) {
							//left is still >=1, so stack(top) has to be an inner node to enter loop
							val next = stack(top).asInstanceOf[BinaryTrieBranch[K, T]].left
							stack(top + 1) = next
							top += 1; trieSize = next.size
						} //now we are at a node in the tree which has to be dropped in its entirety, so we return to the main retreating loop
						left -= trieSize
						stack(top) = null
						top -= 1
					}
				} //stack(top) - if it exists - is a Branch
				if (top>=0) {
					stack(top) = stack(top).asInstanceOf[BinaryTrieBranch[K, T]].right
					descend()
				}
			}
			this
		}


	}


	/** A binary trie iterator iterating over the leaves themselves.
	  * @param path path to the current node splitting the trie in two, with the 'right-hand' side containing remaining elements.
	  *             The top of the stack - `stack(top)`, providing `this` is not empty - is a singleton leaf node
	  *             in the trie, being the `head` of this iterator. Below it, are all inner nodes of type [[BinaryTrieBranch]]
	  *             from which we descended to the left child. Thus, all right children of all inner nodes on the path
	  *             remain to be iterated. Whenever we descend right from an inner node, we remove it from the stack
	  *             (replacing it with its child), as there will be nothing more to do with that node.
	  * @param top  index of the top element on the stack. Negative index means an empty iterator.
	  * @tparam K trie key type
	  * @tparam T trie node type
	  */
	class LeafIterator[K, +T <: BinaryTrie[K, T]](path :Array[BinaryTrie[K, T]], top :Int)
		extends BinaryTrieIterator[T, K, T](path, top) with FitIterator[T]
	{
		def this(trie :T, depth :Int) = {
			this(seedStack(trie, depth), 0)
			descend()
		}

		override def head: T = topNode

		override def next(): T = { val leaf = topNode; skip(); leaf }
	}


	/** A binary trie iterator iterating over keys in the leaves.
	  * @param path path to the current node splitting the trie in two, with the 'right-hand' side containing remaining elements.
	  *             The top of the stack - `stack(top)`, providing `this` is not empty - is a singleton leaf node
	  *             in the trie, being the `head` of this iterator. Below it, are all inner nodes of type [[BinaryTrieBranch]]
	  *             from which we descended to the left child. Thus, all right children of all inner nodes on the path
	  *             remain to be iterated. Whenever we descend right from an inner node, we remove it from the stack
	  *             (replacing it with its child), as there will be nothing more to do with that node.
	  * @param top  index of the top element on the stack. Negative index means an empty iterator.
	  * @tparam K trie key type
	  * @tparam T trie node type
	  */
	class LeafKeyIterator[@specialized(Int, Long) K, +T<:BinaryTrie[K, T]](path :Array[BinaryTrie[K, T]], top :Int)
		extends BinaryTrieIterator[K, K, T](path, top) with FitIterator[K]
	{
		/** Initializes this iterator to traverse the whole trie given as its first argument.
		  * @param trie trie to traverse
		  * @param depth reserved stack size (length of maximum path in the trie)
		  */
		def this(trie :T, depth :Int) = {
			this(seedStack(trie, depth), 0)
			descend()
		}

		override def head: K = topNode.key

		override def next(): K = { val k = topNode.key; skip(); k }
	}


	

	/** A binary trie iterator enumerating values located (or computed) in its leaves as provided by the trie node `value` method.
	  *
	  * @param path path to the current node splitting the trie in two, with the 'right-hand' side containing remaining elements.
	  *             The top of the stack - `stack(top)`, providing `this` is not empty - is a singleton leaf node
	  *             in the trie, being the `head` of this iterator. Below it, are all inner nodes of type [[BinaryTrieBranch]]
	  *             from which we descended to the left child. Thus, all right children of all inner nodes on the path
	  *             remain to be iterated. Whenever we descend right from an inner node, we remove it from the stack
	  *             (replacing it with its child), as there will be nothing more to do with that node.
	  * @param top  index of the top element on the stack. Negative index means an empty iterator.
	  * @tparam K trie key type
	  * @tparam V enumerated value type stored in the leaves.             
	  * @tparam T trie node type
	  */
	class LeafValueIterator[K, @specialized(Elements) +V, +T <: BinaryTrie[K, T] with ValueTrie[K, V, T]]
	                       (path :Array[BinaryTrie[K, T]], top :Int)
		extends BinaryTrieIterator[V, K, T](path, top) with FitIterator[V]
	{
		/** Initializes this iterator to traverse the whole trie given as its first argument.
		  * @param trie trie to traverse
		  * @param depth reserved stack size (length of maximum path in the trie)
		  */
		def this(trie :T, depth :Int) = {
			this(seedStack(trie, depth), 0)
			descend()
		}

		override def head: V = topNode.value

		override def next(): V = { val hd = topNode.value; skip(); hd }
	}


	/** A binary trie iterator enumerating elements located (or computed) in its leaves as provided by the accessor `getter`.
	  * 
	  * @param getter a function-like object returning an element for every trie leaf provided as its argument.
	  * @param path path to the current node splitting the trie in two, with the 'right-hand' side containing remaining elements.
	  *             The top of the stack - `stack(top)`, providing `this` is not empty - is a singleton leaf node
	  *             in the trie, being the `head` of this iterator. Below it, are all inner nodes of type [[BinaryTrieBranch]]
	  *             from which we descended to the left child. Thus, all right children of all inner nodes on the path
	  *             remain to be iterated. Whenever we descend right from an inner node, we remove it from the stack
	  *             (replacing it with its child), as there will be nothing more to do with that node.
	  * @param top  index of the top element on the stack. Negative index means an empty iterator.
	  * @tparam E element type of the trie for the purpose of this iterator.              
	  * @tparam K trie key type
	  * @tparam T trie node type
	  */
	class LeafElementIterator[@specialized(Elements) +E, K, +T <: BinaryTrie[K, T]]
	                         (getter :ElementOf[E, T], path :Array[BinaryTrie[K, T]], top :Int)
		extends BinaryTrieIterator[E, K, T](path, top) with FitIterator[E]
	{
		/** Initializes this iterator to traverse the whole trie given as its first argument.
		  * @param getter a function-like object returning an element for every trie leaf provided as its argument.
		  * @param trie trie to traverse
		  * @param depth reserved stack size (length of maximum path in the trie)
		  */
		def this(getter :ElementOf[E, T], trie :T, depth :Int) = {
			this(getter, seedStack(trie, depth), 0)
			descend()
		}

		override def head :E = getter.elementOf(topNode)

		override def next() :E = { val res = getter.elementOf(topNode); skip(); res }
	}







	/** An iterator traversing a trie in the reverse order using a manual stack implemented with an array.
	  * Implements the traversal and all relevant iterator methods except for `head` and `next`. Subclasses need only
	  * to implement accessor retrieving the element type out of current node given as [[BinaryTrieIterator#topNode]].
	  * @param top   index of the top element on the stack. Negative index means an empty iterator.
	  * @param stack path to the current node splitting the trie in two, with the 'left-hand' side containing remaining elements.
	  *              The top of the stack - `stack(top)`, providing `this` is not empty - is a singleton leaf node
	  *              in the trie, being the `head` of this iterator. Below it, are all inner nodes of type [[BinaryTrieBranch]]
	  *              from which we descended to the right child. Thus, all left children of all inner nodes on the path
	  *              remain to be iterated over. Whenever we descend left from an inner node, we remove it from the stack
	  *              (replacing it with its child), as there will be nothing more to do with that node.
	  */
	abstract class ReverseBinaryTrieIterator[+E, K, +T <: BinaryTrie[K, T]]
	                                        (stack :Array[BinaryTrie[K, T]], private[this] var top :Int)
		extends BaseIterator[E]
	{ this :FitIterator[E] =>

		/** Initializes this iterator to traverse the whole trie given as its first argument.
		  * @param trie trie to traverse
		  * @param depth reserved stack size (length of maximum path in the trie)
		  */
		def this(trie: T, depth :Int) = {
			this(seedStack(trie, depth), 0)
			descend()
		}

		@tailrec final protected def descend(): Unit = stack(top) match {
			case branch :BinaryTrieBranch[K, T] =>
				top += 1
				stack(top) = branch.left
				descend()
			case _ => //found the 'leftmost' singleton - minimum in the tree.
		}

		@inline protected[this] final def topNode :T = stack(top).asInstanceOf[T]


		override def hasDefiniteSize = true

		override def hasNext :Boolean = top >= 0

		override def size :Int = {
			@tailrec def count(i :Int, sum :Int) :Int =
				if (i<0) sum
				else count(i-1, sum + stack(i).asInstanceOf[BinaryTrieBranch[K, T]].left.size)
			if (top<0) 0
			else count(top-1, topNode.size)
		}

		override def ofAtLeast(n :Int) :Boolean = {
			@tailrec def count(i :Int, left :Int) :Boolean =
				left <= 0 || i>=0 && count(i-1, left - stack(i).asInstanceOf[BinaryTrieBranch[K, T]].left.size)
			n <= 0 || top >= 0 && count(top-1, n - topNode.size)
		}


		final def skip(): Unit = {
			stack(top) = null.asInstanceOf[T]
			top -= 1
			if (top >= 0 && stack(top).plurality > 1) {
				stack(top) = stack(top).asInstanceOf[BinaryTrieBranch[K, T]].left
				descend()
			}
		}



	}






	/** A binary trie iterator enumerating in the reverse order elements located (or computed) in its leaves as provided
	  * by the accessor `getter`.
	  *
	  * @param getter a function-like object returning an element for every trie leaf provided as its argument.
	  * @param top  index of the top element on the stack. Negative index means an empty iterator.
	  * @param path path to the current node splitting the trie in two, with the 'left-hand' side containing remaining elements.
	  *             The top of the stack - `stack(top)`, providing `this` is not empty - is a singleton leaf node
	  *             in the trie, being the `head` of this iterator. Below it, are all inner nodes of type [[BinaryTrieBranch]]
	  *             from which we descended to the right child. Thus, all left children of all inner nodes on the path
	  *             remain to be iterated over. Whenever we descend left from an inner node, we remove it from the stack
	  *             (replacing it with its child), as there will be nothing more to do with that node.
	  * @tparam E element type of the trie for the purpose of this iterator.
	  * @tparam K trie key type
	  * @tparam T trie node type
	  */
	class ReverseLeafElementIterator[@specialized(Elements) +E, K, +T <: BinaryTrie[K, T]]
	                                (getter :ElementOf[E, T], path :Array[BinaryTrie[K, T]], top :Int)
		extends ReverseBinaryTrieIterator[E, K, T](path, top) with FitIterator[E]
	{
		/** Initializes this iterator to traverse the whole trie given as its first argument.
		  * @param getter a function-like object returning an element for every trie leaf provided as its argument.
		  * @param trie trie to traverse
		  * @param depth reserved stack size (length of maximum path in the trie)
		  */
		def this(getter :ElementOf[E, T], trie :T, depth :Int) = {
			this(getter, seedStack(trie, depth), 0)
			descend()
		}

		override def head :E = getter.elementOf(topNode)

		override def next() :E = { val res = getter.elementOf(topNode); skip(); res }
	}




}


