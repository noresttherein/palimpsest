package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.RuntimeType.Fun1
import net.turambar.palimpsest.specialty.tries.BinaryTrie.{BinaryTrieBranch, BinaryTriePatch}
import net.turambar.palimpsest.specialty.tries._
import net.turambar.palimpsest.specialty.tries.TrieElements.{ElementCounter, ElementOf}
import net.turambar.palimpsest.specialty.tries.TrieFriends.{KeySubset, SameKeys, TrieOp}
import net.turambar.palimpsest.specialty.{FitIterable, FitIterator, FitTraversableOnce}
import net.turambar.palimpsest.specialty.tries.BinaryTrieOps.{BinaryTrieOp, TrackingTrieKeyPatch, TrieKeyPatch}

import scala.annotation.{tailrec, unspecialized}
import scala.collection.{GenIterable, GenSet, GenTraversableOnce, LinearSeq}


/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait TrieKeySetSpecialization[K, F <: BinaryTrie[K, F],
                               T <: GenericBinaryTrie[K, F, T] with TrieFriends[K, F, F] with TrieElements[K, F, T] with F,
                               @specialized(TrieElements.Types) E,
                               +S <: ValSet[E] with SetSpecialization[E, S]]
	extends IterableTriePotFoundation[K, F, T, E, S] with SetSpecialization[E, S] with IterableTriePotSpecialization[K, F, T, E, S]
	   with ElementOf[E, F]
{ //this :S =>

	//overriden to narrow down result type - elements now must accept tries of supertype F
	override protected[this] def elements :ElementOf[E, F] = this

	//overriden to narrow down result type - elements now must accept tries of supertype F
	override protected[this] def countingElements :ElementCounter[E, F]

	/** Check if the given collection is of a compatible type as this instance and, if so, retrieve its backing trie. */
	protected[this] def friendTrie(elems :GenTraversableOnce[_]) :Option[F]


	protected[this] def ops :TrieKeySetOps[K, F, T]


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



	protected[this] def juxtaposition(other :F)(op :BinaryTrieOp[F, F]) :S


//	protected[this] def newRoot(trie :F) :T = ops.newRoot(trie)

	protected[this] def newSet(trie :F, size :Int = -1) :S //= plant(trie.stable, size)


	override def empty :S = plant(trie.emptyTrie, 0)

//	override protected[this] def copy :S //= this.asInstanceOf[S]
//
//	override protected[this] def copy(contents :T, unsureSize :Int) :S //= this.asInstanceOf[S]


	override def head :E = elementOf(trie.viewHead)

	override def last :E = elementOf(trie.viewLast)

	//todo: head_?/last_?

	override def +(elem :E) :S = {
		val t = patchTrie(trie, elem)(ops.InsertKey)
		if (t eq trie) copy
		else replant(t, 1)
	}


	override def -(elem :E) :S = {
		val t = patchTrie(trie, elem)(ops.DeleteKey)
		if (t eq trie) copy
		else replant(t, -1)
	}


	override def ^(elem :E) :S =
		if (hasFastSize) {
			val patch = ops.FlipKey.trackSize //ops.TrackingFlip
			replant(patchTrie(trie, elem)(patch), patch.deltaSize)
		} else
			plant(patchTrie(trie, elem)(ops.FlipKey))


	override def +(elem1 :E, elem2 :E, elems :E*) :S = +/-(ops.InsertKey)(elem1, elem2, elems : _*)

	override def -(elem1 :E, elem2 :E, elems :E*) :S = +/-(ops.DeleteKey)(elem1, elem2, elems : _*)

	final protected[this] def +/-(patch :TrieKeyPatch[K, F, T])(elem1 :E, elem2 :E, elems :E*) :S =
		if (hasFastSize) {
			val mod = patch.trackSize
			var t = patchTrie(trie, elem1)(mod)
			t = patchTrie(trie, elem2)(mod)
			t = patchTrie(trie, elems)(mod)
			plant(t, size + mod.deltaSize)
		} else {
			var t = patchTrie(trie, elem1)(patch)
			t = patchTrie(trie, elem2)(patch)
			t = patchTrie(trie, elems)(patch)
			plant(t)
		}



	@unspecialized
	override def ++(elems :GenTraversableOnce[E]) :S = ++/--(elems)(ops.InsertKey)

	@unspecialized
	override def --(elems :GenTraversableOnce[E]) :S = ++/--(elems)(ops.DeleteKey)

	@unspecialized
	protected[this] def ++/--(elems :GenTraversableOnce[E])(patch :TrieKeyPatch[K, F, T]) :S =
		if (elems.isEmpty)
			copy //clone()
		else friendTrie(elems) match {
			case Some(other) => juxtaposition(other)(patch.collective)

			case _ if hasFastSize =>
				val feedback = patch.trackSize
				elems match {
					case fit :FitTraversableOnce[E] =>
						plant(patchTrie(trie, fit)(feedback), size + feedback.deltaSize)
					case _ =>
						plant(patchTrie(trie, elems)(feedback), size + feedback.deltaSize)
				}
			case _ => elems match {
				case fit :FitTraversableOnce[E] =>
					plant(patchTrie(trie, fit)(patch))
				case _ =>
					plant(patchTrie(trie, elems)(patch))
			}
		}



	@unspecialized
	override def ++(elems :FitTraversableOnce[E]) :S = ++/--(elems)(ops.InsertKey)

	@unspecialized
	override def --(elems :FitTraversableOnce[E]) :S = ++/--(elems)(ops.DeleteKey)

	@unspecialized
	protected[this] def ++/--(elems :FitTraversableOnce[E])(patch :TrieKeyPatch[K, F, T]) :S =
		if (elems.isEmpty)
			copy
		else friendTrie(elems) match {
			case Some(other) =>
				juxtaposition(other)(patch.collective)
			case None if hasFastSize =>
				val feedback = patch.trackSize
				plant(patchTrie(trie, elems)(feedback), size + feedback.deltaSize)
			case None =>
				plant(patchTrie(trie, elems)(patch))
		}


	@unspecialized
	override def ^(that :GenSet[E]) :S = ++/--(that)(ops.FlipKey)

	@unspecialized
	override def ^(that :ValSet[E]) :S = ++/--(that)(ops.FlipKey)



	override def &(that :ValSet[E]) :S =
		if (isEmpty || that.isEmpty)
			(this :SetSpecialization[E, S]).empty
		else friendTrie(that) match {
			case Some(other) =>
				juxtaposition(other)(ops.Intersection)
			case _ if { val count = unsureSize; count >= 0 && that.hasFastSize && that.size < count } =>
				val it = that.iterator
				val insert = ops.InsertKey
				var t = trie.emptyTrie
				var count = 0
				while (it.hasNext) {
					val e = it.next()
					if (contains(e)) {
						t = patchTrie(t, e)(insert)
						count += 1
					}
				}
				plant(t, count)
			case _ if hasFastSize =>
				newSet(trie.filter(this)(that))
			case _ =>
				val counter = countingElements
				val filtered = trie.filter(counter)(that)  //todo: this possibly is not specialized for some element types
				size = counter.count
				newSet(filtered)
		}


	override def intersect(that :GenSet[E]) :S =
		if (isEmpty || that.isEmpty)
			(this :SetSpecialization[E, S]).empty
		else friendTrie(that) match {
			case Some(other) =>
				juxtaposition(other)(ops.Intersection)
			case _ if hasFastSize =>
				newSet(trie.filter(this)(that)) //todo: this is not specialized, bad!
			case _ =>
				val counter = countingElements
				val filtered = trie.filter(counter)(that)
				size = counter.count
				newSet(filtered)
		}


	override def subsetOf(that :GenSet[E]) :Boolean =
		friendTrie(that) match {
			case Some(other) =>
				(trie correlated other)(KeySubset)
			case None =>
				forall(that)
		}



	override def sameElements[U >: E](that :GenIterable[U]) :Boolean =
		friendTrie(that) match {
			case Some(other) =>
				(trie correlated other)(SameKeys)
			case None => that match {
				case fit :FitIterable[U] =>
					val count = unsureSize
					(count < 0 || !fit.hasFastSize || count == fit.size) && iterator.sameElements(fit.iterator)
				case _ =>
					iterator sameElements that.iterator
			}
		}



	override def equals(that :Any) :Boolean = that match {
		case set :ValSet[_] => friendTrie(set) match {
			case _ if set eq this => true

			case Some(other) =>
				val elems = unsureSize
				(elems < 0 || !set.hasFastSize || elems == set.size) && (trie correlated other)(SameKeys)

			case _ if size != set.size => false
//
//			case None if set.specialization == specialization =>
//				set.canEqual(this) && forall(set.asInstanceOf[ValSet[E]])
			case _ => try {
					set.canEqual(this) && forall(set.asInstanceOf[ValSet[E]])
				} catch {
					case _ :ClassCastException => false
				}
		}
		case _ => super.equals(that)
	}


	override def copyToFitArray(xs :Array[E], start :Int, total :Int) :Unit =
		trie.copyToArray(this)(xs, start, total)


}




trait StableTrieKeySetTemplate[K, T <: GenericBinaryTrie[K, T, T] with TrieFriends[K, T, T] with TrieElements[K, T, T],
                               E, +S <: ValSet[E] with SetSpecialization[E, S]]
	extends IterableTriePot[K, T, T, E, S]
{ this :TrieKeySetSpecialization[K, T, T, E, S] with S =>

	override protected[this] def juxtaposition(other :T)(op :BinaryTrieOp[T, T]) :S = {
		val res =
			if (trie.isMutable)
				(trie juxtapose other)(op.selfOp)
			else
				(trie juxtapose other)(op)
		if (res eq trie) this
		else newSet(res)
	}


	override protected[this] def copy :S = this

	override protected[this] def copy(contents :T, unsureSize :Int) :S = this
}




trait TraversableTrieKeySet[K, F <: BinaryTrie[K, F],
                            T <: GenericBinaryTrie[K, F, T] with TrieFriends[K, F, F] with TrieElements[K, F, F] with F,
                            @specialized(Fun1) E, +S <: ValSet[E] with SetSpecialization[E, S]]
	extends TrieKeySetSpecialization[K, F, T, E, S]
{

	override protected[this] def patchTrie(t :T, elems :FitTraversableOnce[E])(patch :BinaryTriePatch[K, F, T]) :T = {
		var res = t
		elems foreach { e => res = patchTrie(res, e)(patch) }
		res
	}

}




trait MutableTrieKeySetSpecialization[K, F <: BinaryTrie[K, F],
                                      T <: MutableBinaryTrie[K, F, T] with TrieFriends[K, F, F] with TrieElements[K, F, T] with F,
                                      @specialized(TrieElements.Types) E,
                                      +S <: MutableSet[E] with MutableSetSpecialization[E, S]]
	extends IterableTriePotFoundation[K, F, T, E, S] with MutableTriePot[T]
	   with MutableSetSpecialization[E, S] with TrieKeySetSpecialization[K, F, T, E, S] //override default mutable set ops like ++
{ //this :S =>


	override protected[this] def juxtaposition(other :F)(op :BinaryTrieOp[F, F]) :S =
		newSet((trie juxtapose other)(op))

	protected def patchTrie(elem :E)(patch :BinaryTriePatch[K, F, T]) :Boolean

	protected[this] def patchTrie(elems :GenTraversableOnce[E])(patch :BinaryTriePatch[K, F, T]) :Unit = elems match {
		//		case fit :FitTraversableOnce[E] =>
		//			patchTrie(fit)(patch)
		case list :LinearSeq[E] =>
			var l = list
			while (l.nonEmpty) {
				patchTrie(l.head)(patch)
				l = l.tail
			}
		case it :Iterator[E] =>
			while (it.hasNext)
				patchTrie(it.next())(patch)
		case _ =>
			elems foreach { e => patchTrie(e)(patch) }
	}

	protected[this] def patchTrie(elems :FitTraversableOnce[E])(patch :BinaryTriePatch[K, F, T]) :Unit = {
		val it = elems.toIterator
		while (it.hasNext)
			patchTrie(it.next())(patch)
	}


	protected[this] def newRoot(trie :F) :T = ops.newRoot(trie)

	override protected[this] def newSet(trie :F, size :Int) :S = plant(newRoot(trie), size)

	override protected[this] def copy :S = plant(trie.copy, unsureSize)

	override protected[this] def copy(contents :T, unsureSize :Int) :S = plant(trie, size)





	override def &(that :ValSet[E]) :S =
		if (isEmpty || that.isEmpty)
			(this :SetSpecialization[E, S]).empty
		else friendTrie(that) match {
			case Some(other) =>
				newSet((trie juxtapose other)(ops.Intersection))
			case _ if hasFastSize && that.hasFastSize && that.size < size =>
				val it = that.iterator
				val res = plant(trie.emptyTrie, 0)
				while (it.hasNext) {
					val e = it.next()
					if (contains(e))
						res += e
				}
				res
			case _ if hasFastSize =>
				newSet(trie.filter(this)(that))
			case _ => //todo: not specialized for non Fun1 element types
				val counter = countingElements
				val filtered = trie.filter(counter)(that)
				size = counter.count
				newSet(filtered)
		}


	override def add(elem :E) :Boolean = {
		val res = patchTrie(elem)(ops.InsertKey)
		if (res && hasFastSize)
			size_++
		res
	}

	override def remove(elem :E) :Boolean = {
		val res = patchTrie(elem)(ops.DeleteKey)
		if (res && hasFastSize)
			size_--
		res
	}


	override def +=(elem :E) :this.type = {
		if (patchTrie(elem)(ops.InsertKey) && hasFastSize)
			size_++
		this
	}

	override def -=(elem :E) :this.type = {
		if (patchTrie(elem)(ops.DeleteKey) && hasFastSize)
			size_--
		this
	}

	override def ^=(elem :E) :this.type = {
		if (hasFastSize) {
			val feedback = ops.FlipKey.trackSize
			patchTrie(elem)(feedback)
			size_+=(feedback.deltaSize)
		} else
			  patchTrie(elem)(ops.FlipKey)
		this
	}


	@unspecialized
	override def ++=(xs :FitTraversableOnce[E]) :this.type = ++/--=(xs)(ops.InsertKey)

	@unspecialized
	override def --=(xs :FitTraversableOnce[E]) :this.type = ++/--=(xs)(ops.DeleteKey)

	@unspecialized
	override def ^=(xs :ValSet[E]) :this.type = ++/--=(xs)(ops.FlipKey)

	@unspecialized
	protected[this] def ++/--=(xs :FitTraversableOnce[E])(patch :TrieKeyPatch[K, F, T]) :this.type =
		if (xs.isEmpty)
			this
		else friendTrie(xs) match {
			case Some(other) =>
				trie = newRoot((trie juxtapose other)(patch.collective.selfOp))
				size = -1
				this
			case _ if hasFastSize =>
				val count = patch.trackSize
				patchTrie(xs)(count)
				size_+=(count.deltaSize)
				this
			case _ =>
				patchTrie(xs)(patch)
				this
		}


	@unspecialized
	override def &=(that :GenSet[E]) :this.type =
		if (isEmpty)
			this
		else if (that.isEmpty) {
			clear(); this
		} else friendTrie(that) match {
			case Some(other) =>
				trie = newRoot((trie juxtapose other)(ops.SelfIntersection))
				size = -1
				this
			case _ =>
				retain(that)
				this
		}

	@unspecialized
	override def ++=(xs :TraversableOnce[E]) :this.type = ++/--=(xs)(ops.InsertKey)

	@unspecialized
	override def --=(xs :TraversableOnce[E]) :this.type = ++/--=(xs)(ops.DeleteKey)

	@unspecialized
	override def ^=(xs :GenSet[E]) :this.type = ++/--=(xs)(ops.FlipKey)

	@unspecialized
	protected[this] def ++/--=(xs :GenTraversableOnce[E])(patch :TrieKeyPatch[K, F, T]) :this.type =
		if (xs.isEmpty)
			this
		else friendTrie(xs) match {
			case Some(other) =>
				trie = newRoot((trie juxtapose other)(patch.collective.selfOp))
				size = -1
				this
			case _ if hasFastSize =>
				val count = patch.trackSize
				xs match {
					case fit :FitTraversableOnce[E] => patchTrie(fit)(count)
					case _ => patchTrie(xs)(count)
				}
				size_+=(count.deltaSize)
				this
			case _ =>
				xs match {
					case fit :FitTraversableOnce[E] => patchTrie(fit)(patch)
					case _ => patchTrie(xs)(patch)
				}
				this
		}


	@unspecialized
	override def &=(that :ValSet[E]) :this.type =
		if (isEmpty)
			this
		else if (that.isEmpty) {
			clear(); this
		} else friendTrie(that) match {
			case Some(other) =>
				trie = newRoot((trie juxtapose other)(ops.SelfIntersection))
				size = -1
				this
			case _ if { val elems = unsureSize; elems >= 0 && that.hasFastSize && that.size < elems } =>
				val insert = ops.InsertKey
				val it = that.iterator
				val copy = plant(trie, -1)
				trie = trie.emptyTrie
				while (it.hasNext) {
					val e = it.next()
					if (copy.contains(e))
						patchTrie(e)(insert)
				}
				this
			case _ =>
				retain(that); this
		}


	@unspecialized
	override def retain(f :E => Boolean) :Unit = {
		trie.retain(this, this)(f)
		size = -1
	}


	override def clear() :Unit = {
		trie = trie.emptyTrie
		size = 0
	}

	override def clone() :S = plant(trie.copy, unsureSize)

}




