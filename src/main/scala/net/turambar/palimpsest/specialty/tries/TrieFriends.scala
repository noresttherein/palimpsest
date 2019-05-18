package net.turambar.palimpsest.specialty.tries

import net.turambar.palimpsest.specialty.tries.Trie.{FoldPath, KeyTypes, TrieOpRes}
import net.turambar.palimpsest.specialty.tries.TrieFriends.{TrieOp, TriePredicate}


/** A mixin trait for tries which would like to provide generic implementations for binary trie operators
  * working on related types sharing internal structure. Self type `This` parameter is covariant in order
  * to preserve the property that subtries (and return type of many methods) share that type with this instance.
  * This makes it impossible to accept a parameter of this type by any method accessible from the outside
  * of this instance.  On the other hand, there is obviously a need for some upper bound in order for any
  * meaningful comparison to be possible. Hence this trait introduces a second self type `Friend`, being
  * a super type of `This`, which can be compared structurally with any other `Friend` instance, regardless
  * of their proper self types.
  *
  * @tparam K type of keys stored in this trie
  * @tparam This self type of this trie, that is the trie type returned by methods creating tries based solely on this
  *              instance.
  * @tparam Friend super type of `This`, instances of which are interchangeable from its point of view and
  *                which can be compared with this instance.
  *
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait TrieFriends[/*@specialized(KeyTypes) */K, Friend <: Trie[K, Friend], +This <: Trie[K, This] with Friend] extends TrieTemplate[K, This] {
	this :This =>


	/** Generic implementation of any binary operator working on `Friend` tries which can be defined by collating
	  * keys from both tries. It separates particular trie implementations, with knowledge about traversal procedure,
	  * from the abstract recursive definition of an operator. Concrete implementations leverage knowledge
	  * of common internal structure in order to traverse both tries simultaneously, single out keys unique to any
	  * of the tries as well as those shared by both and delegate to appropriate methods of the given operator
	  * definition. For example, set operations such as union, intersection, difference and others, can be
	  * implemented solely based on knowledge if any individual element should be included based on its presence
	  * in both tries, regardless of the order - it is the only meaningful detail differing between these operations,
	  * which all can realised by this method.
	  * @param other a compatible trie to compare/combine with this instance.
	  * @param collation an operator declaring rules of recursive computation of the final result by
	  *                  first obtaining primary results for individual keys and then combining them together.
	  * @tparam T a type shared by this trie and the given argument in terms of which the operator is defined.
	  * @tparam O result of the operator
	  * @return result of applying the operator to individual key/key set pairs and reducing obtained values by
	  *         its `reduce` method.
	  * @see [[TrieOp]] for more precise definition
	  */
	def juxtapose[T >: This <: Trie[K, T] with Friend, @specialized(TrieOpRes) O](other :T)(collation :TrieOp[T, O]) :O

	/** A variant of [[net.turambar.palimpsest.specialty.tries.TrieFriends#juxtapose]] which implements a relation
	  * (such as equality or being a subset of) and reduces individual results via logical conjunction. This knowledge
	  * makes it possible to end the comparison early, when the first `false` result is found.
	  * @param other a compatible trie to compare with this instance.
	  * @param predicate a predicate to apply to matching/missing keys.
	  * @tparam T a type shared by this trie and the given argument, in terms of which the operator is defined.
	  * @return logical conjunction of results of individual key comparison.
	  */
	def correlated[T >: This <: Trie[K, T] with Friend](other :T)(predicate :TriePredicate[T]) :Boolean



	/** A folding function working on subtries of this trie rooted in nodes on the path to the leaf containing the given key,
	  * in the reverse order of their positions on that path.
	  * It descends recursively down this trie in search for a leaf holding the key `key`. When recursion stops,
	  * either due to finding the leaf for the key or when it is determined that no such key exists in this trie,
	  * an appropriate callback method of the folding operator `op` is called to obtain the seed for the folding accumulator.
	  * Subsequently, recursion is rewound, at each step invoking the operator with the most recent accumulator value
	  * and the subtrie at the given recursion level.
	  */
	def foldPath[U >: This <: Trie[K, U] with Friend, @specialized(TrieOpRes) O](op :FoldPath[K, U, O])(key :K) :O

}

trait Op[-T, F, O]


object TrieFriends {






	/** An operator returning values of `O` for two tries of type `T` which can be implemented by a divide and conquer
	  * algorithm. It is used as a callback argument to method  [[Trie!.juxtapose juxtapose]], which simultaneously traverses
	  * both tries and passes their corresponding subtries to this instance. In this manner, the traversal, aware of
	  * exact details of internal structure of `T` and common to many possible operators such as trie union,
	  * intersection or equality, is separated from the recursive definition of an operator. This results in both
	  * minimization of code repetition and extra flexibility for creating custom trie operations. In particular,
	  * can be used to implement trie union, intersection, difference, symmetric difference, as well as relations
	  * of being a subtrie, equality both in terms of key sets and actual nodes (for `Map` implementations).
	  *
	  * In essence, it can be thought of as a recursive map/reduce algorithm. The [[Trie!.juxtapose]] method
	  * is responsible for splitting each of the two tries into a collection of its subtries and pairing them with
	  * corresponding subtries from the other source trie in the following manner:
	  *
	  *   -  key nodes (usually leaves) for keys which a present in both subtries are paired together.
	  *   -  subtries containing disjoint key sets, but for which common longest prefix is longer than the prefix
	  *      associated with the parent of either subtrie, are paired together. In other words, these are two key sets
	  *      such that adding all keys from one to the other source trie would result in 'replanting' of the associated
	  *      subtrie as a sibling of the second subtrie in the two. This represents the case when recursion down the tries
	  *      can be cut short in knowledge that the key sets under each of those subtries must be disjoint and that they
	  *      can be safely joint together under a common parent, which would exist as part of a trie union implementation.
	  *   -  subtries containing keys sharing a prefix which does not exist in the other trie are left by themselves,
	  *      symbolically paired with an empty trie.
	  *
	  *  Each such two marks the termination of recursion and a map step involving invoking an appropriate method of
	  *  this class returning a partial result for that two. These results are then subsequently reduced by pairing
	  *  a result for a given two/subtrie with another of the longest common prefix. More procedurally, recursion
	  *  handles at each step one non-empty node from each trie, starting with whole tries. If the path to one of these
	  *  nodes form a strict prefix of the other, the 'shorter' node is checked for a child which has a longer common
	  *  prefix with the other node. If such a child exists, recursion substitutes it for its parent and continues.
	  *  Additionally, all siblings of the substitute represent prefixes unique to that trie and are paired with empty
	  *  tries for passing to [[TrieOp!.mapFirst]]/[[TrieOp!.mapSecond]], depending if the non-empty child is first or second.
	  *  If the key prefixes (paths in the tries) associated with these nodes differ, recursion stops as their key sets
	  *  are certain to be disjoint and returns result of [[TrieOp!.mapMismatch]]. If both of the nodes are leaves,
	  *  their key must at this step be equal and [[TrieOp!.mapMatch]] is invoked to likewise terminate the recursion.
	  *  In the other case, the children of each node are then examined in search for prefixes which are present in both tries.
	  *  If a child of one node extends the prefix so that the longest common prefix with any child of the other trie
	  *  equals the path of the two parents, it is matched with an empty trie and one of the methods
	  *  [[TrieOp!.mapFirst]]/[[TrieOp!.mapSecond]] are called as before. Otherwise, there is such a child in the other trie
	  *  (there can be at most one), that their common prefix is longer than the prefix to their parents and recursion
	  *  continues for this two. At the end of each step, results for such children pairs are subsequently reduced by
	  *  [[TrieOp!.reduce]]. In addition to partial results, that of the two nodes which has shorter prefix is given as an
	  *  argument. As the reduced two was computed for a two of subtries which have longer prefix with each other than
	  *  with any other key from their corresponding tries, it conceptually represents a prefix node which would be the
	  *  parent of the two in the union of the two tries. In case of binary tries, `reduce` is called exactly once
	  *  at each step; for tries with many potential children it may be called more than once for each such parent argument.
	  *
	  *  For the purpose of simplification, algorithm behaves as if each key ended with a 'end of word' symbol which cannot
	  *  occur anywhere else (that is, keys are stored only in leaves). Moreover, a special case is when one of the
	  *  initial trie arguments to the [[TrieFriends.juxtapose]] is empty. If both are empty, the result of a special method
	  *  [[TrieOp!.mapEmpty]] is returned directly; if only one, than either [[TrieOp!.mapFirst]] or [[TrieOp!.mapSecond]]
	  *  is called for the non empty node.
	  *
	  *  @tparam T a trie type on which this operator works
	  *  @tparam O result type of the operator
	  *  @define Trie net.turambar.specialty.tries.Trie
	  */
	trait TrieOp[-T, @specialized(TrieOpRes) O] {

		/** Callback for the case when both of the argument tries is empty. It is never called as a result of recursion itself.
		  * @param emptyFirst first/left/this (empty) trie passed to [[Trie!.juxtapose]].
		  * @param emptySecond second/right/argument (empty) trie passed to [[Trie!.juxtapose]]
		  * @return desired result of the operation.
		  */
		def mapEmpty(emptyFirst :T, emptySecond :T) :O

		/** Callback called either when the second/right (only) of the operator arguments is empty, or when recursion
		  * finds a node in the first/left trie with a key set disjoint from the key set of the other trie.
		  * @param first the non empty node from the first trie with no counterpart in the right trie.
		  * @param second some node from the second trie, used solely as a virtual constructor, when the dynamic type
		  *               of the arguments may differ and affect the exact type of the result. In particular, may
		  *               serve as a trie factory and provide [[Trie!.emptyTrie]].
		  */
		def mapFirst(first :T, second :T) :O

		/** Callback called either when the first/left (only) of the operator arguments is empty, or when recursion
		  * finds a node in the second/right trie with a key set disjoint from the key set of the other trie.
		  * @param first some node from the first trie, used solely as a virtual constructor, when the dynamic type
		  *              of the arguments may differ and affect the exact type of the result. In particular, may
		  *              serve as a trie factory and provide [[Trie!.emptyTrie]].
		  * @param second the non empty node from the second trie with no counterpart in the left trie.
		  */
		def mapSecond(first :T, second :T) :O

		/** Callback called when recursion encounters a trie two with different prefixes and thus representing disjoint
		  * key sets.
		  * @param first a subtrie of the first/left/this of the operator arguments.
		  * @param second a subtrie of the second/right of the operator arguments.
		  */
		def mapMismatch(first :T, second :T) :O

		/** Callback involved when leaves for the same key are found in both argument tries.
		  * @param firstLeaf a leaf (or at least a key node) from the first/left operator argument
		  * @param secondLeaf a leaf (or at least a key node) from the seoond/right operator argument
		  */
		def mapMatch(firstLeaf :T, secondLeaf :T) :O

		/** Reduce (or 'conquer' in 'divide & conquer') step combining two earlier results of deeper recursion into
		  * a single result. For binary tries, it is called exactly once for every non-terminal recursion step and
		  * `res1` and `res2` will be the results of recursion down the left and right subtries of `path`, respectively.
		  * For tries with more possible children, it can be called multiple times for the same `path` argument and
		  * any of the arguments may be results of earlier reduction. In that case `path` will be the longest common
		  * prefix of any two of key sets included in computation of the partial results given. For example, in
		  * implementation of trie union, it would correspond to the prefix associated with the new parent of `res1`
		  * and `res2`.
		  * @param path a prefix node defining the level of recursion; both partial results where computed for key sets
		  *             starting with that prefix and it is the longest common prefix for subtries from which res1 and
		  *             `res2` where computed. It comes directly from one of the two tries and was the parent
		  *             of one of the nodes which produced the following arguments.
		  * @param res1 a partial result computed based on a subtrie of one of the argument tries; for binary tries,
		  *             always the left one.
		  * @param res2 a partial result computed based on a subtrie of one of the argument tries; for binary tries,
		  *             always the right one.
		  * @return a combined result from `res1` and `res2` corresponding to the set of keys starting with the prefix
		  *         associated with the `path` node.
		  */
		def reduce(path :T)(res1 :O, res2 :O) :O

		/** Creates an operator which swaps the order of the initial trie arguments. */
		def inverse :TrieOp[T, O] = new TrieOp.InverseOp(this)
	}



	object TrieOp {

		private[TrieFriends] class InverseOp[-T, @specialized(TrieOpRes) O](target :TrieOp[T, O]) extends TrieOp[T, O] {

			override def mapEmpty(first :T, second :T) :O = target.mapEmpty(second, first)

			override def mapFirst(first :T, emptySecond :T) :O = target.mapSecond(emptySecond, first)

			override def mapSecond(emptyFirst :T, second :T) :O = target.mapFirst(second, emptyFirst)

			override def mapMismatch(first :T, second :T) :O = target.mapMismatch(second, first)

			override def mapMatch(firstLeaf :T, secondLeaf :T) :O = target.mapMatch(secondLeaf, firstLeaf)


			override def reduce(path :T)(res1 :O, res2 :O) :O = target.reduce(path)(res2 :O, res1 :O) :O


			override def inverse :TrieOp[T, O] = target

			protected[this] def op :TrieOp[T, O] = target

			override def toString = s"inversed($target)"
		}


		trait KeyUnion[T] extends TrieOp[T, T] {
			override def mapEmpty(emptyFirst :T, emptySecond :T) :T = emptyFirst

			override def mapFirst(first :T, second :T) :T = first

			override def mapSecond(first :T, second :T) :T = second

			override def mapMatch(firstLeaf :T, secondLeaf :T) :T = firstLeaf
		}

		trait KeyReplace[T] extends KeyUnion[T] {
			override def mapMatch(firstLeaf :T, secondLeaf :T) :T = secondLeaf
		}


		trait KeyIntersection[T <: Trie[_, T]] extends TrieOp[T, T] {
			override def mapEmpty(emptyFirst :T, emptySecond :T) :T = emptyFirst

			override def mapFirst(first :T, second :T) :T = first.emptyTrie

			override def mapSecond(first :T, second :T) :T = first.emptyTrie

			override def mapMismatch(first :T, second :T) :T = first.emptyTrie

			override def mapMatch(firstLeaf :T, secondLeaf :T) :T = firstLeaf
		}


		trait KeyDifference[T <: Trie[_, T]] extends TrieOp[T, T] {
			override def mapEmpty(emptyFirst :T, emptySecond :T) :T = emptyFirst

			override def mapFirst(first :T, second :T) :T = first

			override def mapSecond(first :T, second :T) :T = first.emptyTrie

			override def mapMatch(firstLeaf :T, secondLeaf :T) :T = firstLeaf.emptyTrie

			override def mapMismatch(first :T, second :T) :T = first
		}


		trait KeyXor[T <: Trie[_, T]] extends TrieOp[T, T] {
			override def mapEmpty(emptyFirst :T, emptySecond :T) :T = emptyFirst

			override def mapFirst(first :T, second :T) :T = first

			override def mapSecond(first :T, second :T) :T = second

			override def mapMatch(firstLeaf :T, secondLeaf :T) :T = firstLeaf.emptyTrie

		}
	}


	/** A trie operator which traverses both tries simultaneously, comparing corresponding sub-tries to verify if they
	  * satisfy a certain relation. Its `reduce` method is final and defined to return logical conjunction.
	  * While the same purpose could be served with just [[TrieOp]], fixing reduce semantics allows the tries
	  * to optimize the process and stop comparison with the first false result. Note that any algorithm requiring logical
	  * disjunction instead can be easily transformed to work on the negated predicate instead.
	  */
	trait TriePredicate[-T] extends TrieOp[T, Boolean] {
		final override def reduce(parent :T)(leftRes: Boolean, rightRes: Boolean): Boolean = leftRes && rightRes
	}

	trait Subtrie[-T] extends TriePredicate[T] {
		override def mapEmpty(emptyFirst :T, emptySecond :T) :Boolean = true
		override def mapFirst(first :T, second :T) :Boolean = false
		override def mapSecond(first :T, second :T) :Boolean = true
		override def mapMismatch(first :T, second :T) :Boolean = false
		override def mapMatch(firstLeaf :T, secondLeaf :T) :Boolean = firstLeaf == secondLeaf
	}
	object Subtrie extends Subtrie[Any]


	trait KeySubset[-T] extends TriePredicate[T] {
		override def mapEmpty(emptyFirst :T, emptySecond :T) :Boolean = true
		override def mapFirst(first :T, second :T) :Boolean = false
		override def mapSecond(first :T, second :T) :Boolean = true
		override def mapMismatch(first :T, second :T) :Boolean = false
		override def mapMatch(firstLeaf :T, secondLeaf :T) :Boolean = true
	}
	object KeySubset extends KeySubset[Any]


	trait Equal[-T] extends TriePredicate[T] {
		override def mapEmpty(emptyFirst :T, emptySecond :T) :Boolean = true
		override def mapFirst(first :T, second :T) :Boolean = false
		override def mapSecond(first :T, second :T) :Boolean = false
		override def mapMismatch(first :T, second :T) :Boolean = false
		override def mapMatch(firstLeaf :T, secondLeaf :T) :Boolean = firstLeaf == secondLeaf
	}
	object Equal extends Equal[Any]


	trait SameKeys[-T] extends TriePredicate[T] {
		override def mapEmpty(emptyFirst :T, emptySecond :T) :Boolean = true
		override def mapFirst(first :T, second :T) :Boolean = false
		override def mapSecond(first :T, second :T) :Boolean = false
		override def mapMismatch(first :T, second :T) :Boolean = false
		override def mapMatch(firstLeaf :T, secondLeaf :T) :Boolean = true
	}
	object SameKeys extends SameKeys[Any]




	trait EmptyFriend[@specialized(KeyTypes) K, Friend <: Trie[K, Friend], +This <: Trie[K, This] with Friend] extends TrieFriends[K, Friend, This] {
		this :This =>

		override def juxtapose[T >: This <: Trie[K, T] with Friend, @specialized(TrieOpRes) O](other :T)(collation :TrieOp[T, O]) :O =
			if (other.isEmpty) collation.mapEmpty(this, other)
			else collation.mapSecond(this, other)

		override def correlated[T >: This <: Trie[K, T] with Friend](other :T)(predicate :TriePredicate[T]) :Boolean =
			if (other.isEmpty) predicate.mapEmpty(this, other)
			else predicate.mapSecond(this, other)

		def foldPath[U >: This <: Trie[K, U] with Friend, @specialized(TrieOpRes) O](op :FoldPath[K, U, O])(key :K) :O =
			op.whenTrieEmpty(key, this)

	}



	trait LeafFriend[@specialized(KeyTypes) K, Friend <: Trie[K, Friend], +This <: Trie[K, This] with Friend] extends TrieFriends[K, Friend, This] {
		this :This =>

		def foldPath[U >: This <: Trie[K, U] with Friend, @specialized(TrieOpRes) O](op :FoldPath[K, U, O])(key :K) :O =
			if (key==this.key) op.whenKeyExists(key, this)
			else op.whenNoKey(key, this)
	}
}