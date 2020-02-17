package net.noresttherein.palimpsest.tries

import net.noresttherein.palimpsest.tries.BinaryTrie.{BinaryTrieBranch, BinaryTriePatch}
import net.noresttherein.palimpsest.tries.Trie.KeyTypes
import net.noresttherein.palimpsest.tries.TrieFriends.TrieOp
import net.noresttherein.palimpsest.tries.BinaryTrieKeySetFactory.{SharingTrieOp, TrackingTrieKeyPatch, TrieKeyPatch}
import net.noresttherein.palimpsest.tries.GenericBinaryTrie.{BranchPatch, GenericBinaryTrieBranch}

import scala.annotation.unspecialized



trait TrieKeySetOps[K, F <: BinaryTrie[K, F], T <: GenericBinaryTrie[K, F, T] with F] {
	def DeleteKey :TrieKeyPatch[K, F, T]
	def InsertKey :TrieKeyPatch[K, F, T]
	def FlipKey :TrieKeyPatch[K, F, T]

	def Intersection :SharingTrieOp[F, F]
	def SelfIntersection :SharingTrieOp[F, F]

	def newRoot(result :F) :T
}



/** A grouping of [[TrieOp]] and [[BinaryTriePatch]] implementations realising set operations on tries' key sets.
  * Declares abstract factory methods for trie nodes needed to provide that functionality.
  * Extended by companion objects of concrete set or trie classes.
  * @tparam K key type of the trie
  * @tparam T the type of trie to which these operations are dedicated
  * @tparam F 'friend' trie type - a super type of `T` being the type of arguments for binary set operators.
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait BinaryTrieKeySetFactory[@specialized(KeyTypes) K, F <: BinaryTrie[K, F], T <: GenericBinaryTrie[K, F, T] with F]
	extends TrieKeySetOps[K, F, T] with BranchPatch[K, F, T, T]
{ ops =>

	protected type Branch = GenericBinaryTrieBranch[K, F, T]

	/** Empty trie factory/accessor delegated to from patches and operators. */
	def emptyTrie :T

	/** Method for adapting a friendly trie to serve as a new trie root of the associated type.
	  * Called after trie patching or juxtaposition is performed.
	  * @param trie a legal subtrie of `T` returned by the patches and operators declared here.
	  */
	def newRoot(trie :F) :T

	/** Adapts the most generic compatible leaf type to the leaf of the associated trie type `T`. */
	protected def leafLike(leaf :F) :T

	/** Factory for leaves/singleton tries used by patch implementations. */
	protected def newLeaf(key :K) :T

	/** Target method of [[BinaryTrieKeySetFactory#InsertKey]]'s `whenNoKey` - accepts verbatim a new key to insert and the future
	  * sibling of the new leaf and creates a branch with the new leaf holding the given key and the `closest` node
	  * as its children.
	  * @param key key which is being inserted into the trie.
	  * @param closest subtrie of the patched trie, passed verbatim. The caller must make sure it can be shared with
	  *                the constructed trie.
	  */
	protected def newLeaf(key :K, closest :F) :T = joinTries(newLeaf(key), closest)

	/** Create a new trie branch having the given two tries as its children in the correct order. The order of arguments
	  * here does not correspond `left` and `right` branch children and must be determined by the implementation.
	  */
	protected def joinTries(t1 :F, t2 :F) :T

	/** Create a new branch like the `parent` one using provided `left` and `right` children. */
	protected def reduce(parent :F)(left :F, right :F) :T

	/** Target method invoked from the eponymous method of all patches defined here. */
	protected override def patchLeft(template :BinaryTrieBranch[K, F], left :T) :T

	/** Target method invoked from the eponymous method of all patches defined here. */
	protected override def patchRight(template :BinaryTrieBranch[K, F], right :T) :T



	/** Base class for patches of associated tries which delegates the `patchLeft`/`patchRight` methods to the
	  * corresponding method of this object.
	  */
	abstract class AbstractTriePatch extends TrieKeyPatch[K, F, T] {

		override def patchLeft(branch :BinaryTrieBranch[K, F], left :T) :T = ops.patchLeft(branch, left)

		override def patchRight(branch :BinaryTrieBranch[K, F], right :T) :T = ops.patchRight(branch, right)
	}




	class DeleteKeyFoundation extends AbstractTriePatch {

		override def whenTrieEmpty(key :K, empty :F) :T = emptyTrie

		override def whenNoKey(key :K, closest :F) :T =
			closest.asInstanceOf[T] //this cast does not hold, but is erased and returning the same node results in no change

		override def whenKeyExists(key :K, leaf :F) :T = emptyTrie

		override def trackSize :TrackingTrieKeyPatch[K, F, T] = new TrackingDelete

		override def collective :SharingTrieOp[F, F] = Difference

		override def toString = "DeleteKey"
	}
//todo: inner classes are not specialized!
	/** Full implementation of key deleting patches, delegating to this object's factory methods for new nodes. */
	class DeleteKey extends DeleteKeyFoundation {
		private[this] val empty = emptyTrie

		override def whenTrieEmpty(key :K, empty :F) :T = this.empty

		override def whenKeyExists(key :K, leaf :F) :T = empty

	}

	/** Tracks the number of deleted keys (as a negative number) performed by this patch. */
	class TrackingDelete extends DeleteKeyFoundation with TrackingTrieKeyPatch[K, F, T] {
		private[this] val empty = emptyTrie

		private[this] var delta = 0

		def deltaSize :Int = delta

		override def whenTrieEmpty(key :K, empty :F) :T = this.empty

		override def whenKeyExists(key :K, leaf :F) :T = { delta -= 1; empty }

	}

	/** Default patch for deleting keys from tries backing the set companion classes of this object. */
	val DeleteKey :TrieKeyPatch[K, F, T] = new DeleteKey

	/** Factory method of element-deleting patches which track the change to the set size. */
	def TrackingDelete :TrackingTrieKeyPatch[K, F, T] = new TrackingDelete




	/** Full implementation of key inserting patches, delegating to this object's factory methods for new nodes. */
	trait InsertKey extends AbstractTriePatch {

		override def whenTrieEmpty(key :K, empty :F) :T = newLeaf(key)

		override def whenNoKey(key :K, closest :F) :T = newLeaf(key, closest)

		override def whenKeyExists(key :K, leaf :F) :T =
			leaf.asInstanceOf[T] //this cast does not hold, but is erased and returning the same node results in no change

		override def trackSize :TrackingTrieKeyPatch[K, F, T] = new TrackingInsert

		override def collective :SharingTrieOp[F, F] = Union

		override def toString = "InsertKey"
	}

	/** Tracks the number of inserted keys (as a negative number) performed by this patch. */
	class TrackingInsert extends InsertKey with TrackingTrieKeyPatch[K, F, T] {
		private[this] var delta = 0
		def deltaSize :Int = delta

		override def whenTrieEmpty(key :K, empty :F) :T = { delta += 1; newLeaf(key) }

		override def whenNoKey(key :K, closest :F) :T = { delta += 1; newLeaf(key, closest) }
	}

	/** Default patch for inserting keys to tries backing the set companion classes of this object. */
	val InsertKey :TrieKeyPatch[K, F, T] = new InsertKey {}

	/** Factory method of element inserting patches which track the change to the set size. */
	def TrackingInsert :TrackingTrieKeyPatch[K, F, T] = new TrackingInsert





	/** Default patch for changing key membereship in tries backing the set companion classes of this object.
	  * Inserts the key if it is not present in the trie and removes it if it is.
	  */
	class FlipKeyFoundation extends AbstractTriePatch {

		override def whenTrieEmpty(key :K, empty :F) :T = newLeaf(key)

		override def whenNoKey(key :K, closest :F) :T = newLeaf(key, closest)

		override def whenKeyExists(key :K, leaf :F) :T = emptyTrie

		override def trackSize :TrackingTrieKeyPatch[K, F, T] = new TrackingFlip

		override def collective :SharingTrieOp[F, F] = SymmetricDifference
	}

	class FlipKey extends FlipKeyFoundation {
		private[this] val empty = emptyTrie

		override def whenKeyExists(key :K, leaf :F) :T = empty
	}

	/** Factory method of patches implementing operation `set xor element`, which track the change to the set size. */
	class TrackingFlip extends FlipKeyFoundation with TrackingTrieKeyPatch[K, F, T] {
		private[this] val empty = emptyTrie

		private[this] var delta = 0

		override def deltaSize :Int = delta

		override def whenTrieEmpty(key :K, empty :F) :T = { delta += 1; newLeaf(key) }

		override def whenNoKey(key :K, closest :F) :T = { delta += 1; newLeaf(key, closest) }

		override def whenKeyExists(key :K, leaf :F) :T = { delta -= 1; empty }

	}

	/** Default patch implementing operation `set xor element`. */
	val FlipKey :TrieKeyPatch[K, F, T] = new FlipKey

	/** Factory method of patches implementing operation `set xor element`, which track the change to the set size. */
	def TrackingFlip :TrackingTrieKeyPatch[K, F, T] = new TrackingFlip




	abstract class SharingTrieOpFoundation extends SharingTrieOp[F, F] {

		override def reduce(path :F)(res1 :F, res2 :F) :F = ops.reduce(path)(res1, res2)

		protected def opName :String

		override def toString :String = opName + "[" + trieTypeName + "]"
	}

	/** A base class for set difference operators implementing methods which do not return any of the argument tries. */
	@unspecialized
	abstract class DifferenceFoundation extends SharingTrieOpFoundation {
		private[this] val empty = ops.emptyTrie

		override def mapEmpty(emptyFirst :F, emptySecond :F) :F = empty

		override def mapSecond(first :F, second :F) :F = empty

		override def mapMatch(firstLeaf :F, secondLeaf :F) :F = empty

		override def selfOp :SharingTrieOp[F, F] = SelfDifference

		override def opName :String = "--"
	}

	@unspecialized
	class SelfDifference extends DifferenceFoundation {
		override def mapFirst(first :F, second :F) :F = first

		override def mapMismatch(first :F, second :F) :F = first

		override def opName :String = "--="
	}


	/** Default key set difference implementation pointed to from [[BinaryTrieKeySetFactory#DeleteKey]]. */
	val Difference :SharingTrieOp[F, F]

	/** Difference of key sets of two tries taking the fragments from the  first argument as-is.
	  * @see [[net.noresttherein.palimpsest.tries.BinaryTrieKeySetFactory.SharingTrieOp#selfOp]]
	  */
	val SelfDifference :SharingTrieOp[F, F] = new SelfDifference




	@unspecialized
	abstract class UnionFoundation extends SharingTrieOpFoundation {
		override def mapEmpty(emptyFirst :F, emptySecond :F) :F = emptyFirst

		override def selfOp :SharingTrieOp[F, F] = SelfUnion

		override def opName :String = "++"
	}

	@unspecialized
	class SelfUnion extends UnionFoundation {

		override def mapFirst(first :F, second :F) :F = first

		override def mapSecond(first :F, second :F) :F = second.stable

		override def mapMatch(first :F, second :F) :F = first

		override def mapMismatch(first :F, second :F) :F = joinTries(first, second.stable)

		override def opName :String = "++="
	}

	/** Default key set union implementation pointed to from [[BinaryTrieKeySetFactory#InsertKey]]. */
	val Union :SharingTrieOp[F, F]

	val SelfUnion :SharingTrieOp[F, F] = new SelfUnion


	@unspecialized
	abstract class SymmetricDifferenceFoundation extends SharingTrieOpFoundation {
		private[this] val empty = ops.emptyTrie

		override def mapEmpty(emptyFirst :F, emptySecond :F) :F = emptyFirst

		override def mapMatch(firstLeaf :F, secondLeaf :F) :F = empty

		override def selfOp :SharingTrieOp[F, F] = SelfSymmetricDifference

		override def opName = "^"
	}

	@unspecialized
	class SelfSymmetricDifference extends SymmetricDifferenceFoundation {

		override def mapFirst(first :F, second :F) :F = first

		override def mapSecond(first :F, second :F) :F = second.stable

		override def mapMismatch(first :F, second :F) :F = joinTries(first, second.stable)

		override def opName = "^="
	}


	/** Default key set symmetric difference operator pointed to from [[SharingTrieOp#FllpKey]]. */
	val SymmetricDifference :SharingTrieOp[F, F]

	val SelfSymmetricDifference :SharingTrieOp[F, F] = new SelfSymmetricDifference




	/** A base class for set intersection operators implementing methods which do not return any of the argument tries. */
	@unspecialized
	abstract class IntersectionFoundation extends SharingTrieOpFoundation {
		private[this] val empty = ops.emptyTrie

		override def mapEmpty(emptyFirst :F, emptySecond :F) :T = empty

		override def mapFirst(first :F, second :F) :T = empty

		override def mapSecond(first :F, second :F) :T = empty

		override def mapMismatch(first :F, second :F) :T = empty

		override def selfOp :SharingTrieOp[F, F] = SelfIntersection

		override def opName = "&"
	}

	@unspecialized
	class SelfIntersection extends IntersectionFoundation {
		override def mapMatch(firstLeaf :F, secondLeaf :F) :F = firstLeaf

		override def opName = "&="
	}

	/** Default set intersection operator for the set companion class of this object. */
	val Intersection :SharingTrieOp[F, F] //= new Intersection

	val SelfIntersection :SharingTrieOp[F, F] = new SelfIntersection


	/** Type name of the trie backing the associated collection type, used solely in `toString` implementations. */
	protected def trieTypeName :String
}








/** An extension of [[BinaryTrieKeySetFactory]] providing trie key set operations for immutable sets. While immutability is neither
  * enforced nor required, all operators listed here assume that subtries of argument tries can be used as-is
  * in construction of new tries.
  * @tparam K key type stored in the trie `T`
  * @tparam T argument trie type of all patches and trie operators.
  */
trait StableBinaryTrieKeySetFactory[K, T <: BinaryTrie[K, T]] extends BinaryTrieKeySetFactory[K, T, T] { ops =>

	override protected def leafLike(leaf :T) :T = leaf


	protected def patchLeft(template :BinaryTrieBranch[K, T], left :T) :T =
		if (template.left eq left) template.asTrie
		else reduce(template.asTrie)(left, template.right)

	protected def patchRight(template :BinaryTrieBranch[K, T], right :T) :T =
		if (template.right eq right) template.asTrie
		else reduce(template.asTrie)(template.left, right)




	/** A trie keywise set difference subtracting the second argument from the first, working for a two of immutable tries. */
	@unspecialized
	class StableDifference extends DifferenceFoundation {

		override def mapFirst(first :T, second :T) :T = first

		override def mapMismatch(first :T, second :T) :T = first

	}

	/** Default set difference operator for the set companion class of this object. */
	override val Difference :SharingTrieOp[T, T] = new StableDifference




	/** A trie keywise set union with left preference, working for a two of immutable tries. */
	@unspecialized
	class StableUnion extends UnionFoundation {

		override def mapFirst(first :T, second :T) :T = first

		override def mapSecond(first :T, second :T) :T = second

		override def mapMismatch(first :T, second :T) :T = joinTries(first, second)

		override def mapMatch(firstLeaf :T, secondLeaf :T) :T = firstLeaf //secondLeaf

	}

	/** Default set union operator for the set companion class of this object. */
	override val Union :SharingTrieOp[T, T] = new StableUnion




	/** A trie keywise set symmetric difference, working for a two of immutable tries. */
	@unspecialized
	class StableSymmetricDifference extends SymmetricDifferenceFoundation {

		override def mapEmpty(emptyFirst :T, emptySecond :T) :T = emptyFirst

		override def mapFirst(first :T, second :T) :T = first

		override def mapSecond(first :T, second :T) :T = second

		override def mapMismatch(first :T, second :T) :T = joinTries(first, second)

	}

	/** Default set symmetric difference operator for the set companion class of this object. */
	override val SymmetricDifference :SharingTrieOp[T, T] = new StableSymmetricDifference




	/** A trie keywise set intersection with left preference, working for a two of immutable tries. */
	@unspecialized
	class StableIntersection extends IntersectionFoundation {

		override def mapEmpty(emptyFirst :T, emptySecond :T) :T = emptyFirst

		override def mapMatch(firstLeaf :T, secondLeaf :T) :T = firstLeaf

	}


	/** Default set intersection operator for the set companion class of this object. */
	override val Intersection :SharingTrieOp[T, T] = new StableIntersection


}









/** A variant of [[TrieKeySetOps]] for mutable sets/maps/tries. Changes behaviour in the following way:
  *   - implemented trie factory methods call [[BinaryTrie#copy]] before reusing subtries of arguments in the result
  *   - default union, intersection, difference and symmetric difference implement in-place mutations of the first argument.
  *     Therefore, portions of the first trie are reused as-is in the result, without preventing cross-modification.
  *     The mutable variants remain as before, creating a new, separate trie.
  * @tparam K type of keys in trie `T`
  * @tparam M associated mutable trie type
  * @tparam S associated stable trie type and the common base type of tries to which operators defined here are applicable.
  */
trait MutableBinaryTrieKeySetFactory[@specialized(KeyTypes) K, S <: BinaryTrie[K, S], M <: GenericBinaryTrie[K, S, M] with S]
	extends BinaryTrieKeySetFactory[K, S, M]
{ ops =>


	override protected def newLeaf(key :K, closest :S) :M = joinTries(newLeaf(key), closest.copy)

	override protected def patchLeft(template :BinaryTrieBranch[K, S], left :M) :M =
		reduce(template.asTrie)(left, template.rightView)

	override protected def patchRight(template :BinaryTrieBranch[K, S], right :M) :M =
		reduce(template.asTrie)(template.leftView, right)



	@unspecialized
	class MutableDifference extends DifferenceFoundation {

		override def mapFirst(first :S, second :S) :S = first.stable

		override def mapMismatch(first :S, second :S) :S = first.stable
	}

	override val Difference :SharingTrieOp[S, S] = new MutableDifference



	@unspecialized
	class MutableUnion extends UnionFoundation {

		override def mapFirst(first :S, second :S) :S = first.stable

		override def mapSecond(first :S, second :S) :S = second.stable

		override def mapMismatch(first :S, second :S) :M = joinTries(first.stable, second.stable)

		override def mapMatch(firstLeaf :S, secondLeaf :S) :S = firstLeaf.stable

	}

	override val Union :SharingTrieOp[S, S] = new MutableUnion




	@unspecialized
	class MutableSymmetricDifference extends SymmetricDifferenceFoundation {

		override def mapFirst(first :S, second :S) :S = first.stable

		override def mapSecond(first :S, second :S) :S = second.stable

		override def mapMismatch(first :S, second :S) :S = joinTries(first.stable, second.stable)
	}

	override val SymmetricDifference :SharingTrieOp[S, S] = new MutableSymmetricDifference



	@unspecialized
	class MutableIntersection extends IntersectionFoundation {

		override def mapMatch(firstLeaf :S, secondLeaf :S) :S = leafLike(firstLeaf)

	}


	override val Intersection :SharingTrieOp[S, S] = new MutableIntersection



}









object BinaryTrieKeySetFactory {

	/** An extension of [[BinaryTrie.BinaryTriePatch]] for tries with no additional values associated with the keys.
	  * These patches therefore can be reusable as no additional information aside from the inserted key is generally needed
	  * to implement the desired operation. It introduces two new features: first, a factory method [[TrieKeyPatch#trackSize]] returning
	  * the version of this patch which tracks the total of changes to the trie size performed by the patch.
	  * Second, it provides an accessor to a [[TrieOp]] being the generalization of this operation on a single key to
	  * working on whole tries.
	  * @tparam K key type in the tries this operation works for
	  * @tparam T the type of the input trie
	  * @tparam F 'friend' trie type of `T`, that is an upper type of `T` with the same internal structure, which will be
	  *           accepted by the `juxtapose` method of `T` (assuming it implements [[TrieFriends TrieFriends[K, T, F]]]
	  *           The [[TrieKeyPatch#collective]] method of this patch returns a [[TrieOp]] working on tries of this type.
	  */
	trait TrieKeyPatch[@specialized(KeyTypes) K, F <: BinaryTrie[K, F], T <: GenericBinaryTrie[K, F, T] with F]
		extends BinaryTriePatch[K, F, T]
	{
		/** Generalization of this operation from a single key to a trie key set. So, where this instance might
		  * implement key insertion, this method would return a `TrieOp` implementing trie union, and so on.
		  */
		def collective :SharingTrieOp[F, F]

		/** A patch performing the exact same operation as this one, but additionally tracking the total of changes made.
		  * A change constitutes of returning something else than the argument by the ''when'' methods. This is particularly
		  * useful when the patch is used repeatedly for several keys.
		  */
		def trackSize :TrackingTrieKeyPatch[K, F, T] = new TrackPatch(this)

	}


	/** A trie patch which can be queried for the amount by which argument trie size have been changed by this instance.
	  * Assuming it is applied sequentially to the 'same' trie (that is, each application works on the trie being
	  * the result of the previous one), the size of the final trie will change by [[TrackingTrieKeyPatch#deltaSize]].
	  */
	trait TrackingTrieKeyPatch[@specialized(KeyTypes) K, F <: BinaryTrie[K, F], T <: GenericBinaryTrie[K, F, T] with F]
		extends TrieKeyPatch[K, F, T]
	{
		/** Total number of keys added (or removed, if negative) by this instance. Note that removing keys decreases this value,
		  * and if the patch both inserts and removes keys, the absolute value won't reflect the number of changes made.
		  */
		def deltaSize :Int

		override def toString :String = super.toString + '#' + deltaSize
	}




	private class TrackPatch[@specialized(KeyTypes) K, F <: BinaryTrie[K, F], T <: GenericBinaryTrie[K, F, T] with F]
	                        (patch :TrieKeyPatch[K, F, T])
		extends TrackingTrieKeyPatch[K, F, T]
	{
		private[this] var delta :Int = 0

		override def deltaSize :Int = delta

		override def trackSize :TrackingTrieKeyPatch[K, F, T] = new TrackPatch(patch)

		override def collective :SharingTrieOp[F, F] = patch.collective

		override def patchLeft(template :BinaryTrieBranch[K, F], left :T) :T = patch.patchLeft(template, left)

		override def patchRight(template :BinaryTrieBranch[K, F], right :T) :T = patch.patchRight(template, right)


		override def whenTrieEmpty(key :K, empty :F) :T = {
			val res = patch.whenTrieEmpty(key, empty)
			if (res.nonEmpty)
				delta += 1
			res
		}

		override def whenNoKey(key :K, closest :F) :T = {
			val res = patch.whenNoKey(key, closest)
			if (!(res eq closest))
				delta += 1
			res
		}

		override def whenKeyExists(key :K, leaf :F) :T = {
			val res = patch.whenKeyExists(key, leaf)
			if (res.isEmpty)
				delta -= 1
			res
		}
	}



	trait SharingTrieOp[-F, T] extends TrieOp[F, T] {

		/** A variant of this operator which takes subtries from the left (original this) argument as-is but converts
		  * any subtries from the second argument to their stable counterpart. If the associated trie class is immutable
		  * - and thus all left arguments are ''effectively'' immutable - this implements the operation working
		  * on the original 'self' collection and a mutable 'friend' (for example, ''StableSet'' `++` ''MutableSet'').
		  * On the other hand, if the associated trie class is mutable, than this operator would implement an in place
		  * modification of the self argument and any friend (mutable or not) trie (for example ''StableSet'' `++=` ''MutableSet''.
		  */
		def selfOp :TrieOp[F, T]

	}

}