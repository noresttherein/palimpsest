package net.turambar.palimpsest.specialty

import scala.annotation.unspecialized
import scala.collection.generic.{CanBuildFrom, FilterMonadic}
import scala.collection.{GenIterable, GenTraversableOnce, IterableLike, mutable}

import net.turambar.palimpsest.specialty.FitIterable.{FilterIterable, SpecializedFilter}
import net.turambar.palimpsest.specialty.Specialized.{Fun1Vals, Fun2, Fun2Vals}
import net.turambar.palimpsest.specialty.seqs.{ArrayView, FitBuffer, FitList, FitSeq, ReverseSeq}


/** Partial specialization of the `IterableLike` trait containing methods which can be implemented
  * without specialization for `E` - usually by delegating to another specialized method either
  * of this trait or its iterator. It is a partial interface only and should be considered as a group
  * of methods implemented and inherited by [[FitIterable]] and not a refererrable interface in itself,
  * or even implemented directly in general. All instances of it must also descend from [[FitIterableLike]],
  * which is the fully specialized version of scala [[IterableLike]], providing similar implementations
  * It exists solely to avoid unnecessary explosion of specialized variants of methods like
  * [[IterableTemplate#foldLeft]], which don't feature `E` as part os their signature and can be implemented efficiently
  * by delegation to another specialized method.
  * @tparam Repr specific type of the collection described by this interface, usually the self type of implementors,
  *              but it is not enforced here.
  * @see [[FitIterableLike]] full version of this interface
  * @see [[net.turambar.palimpsest.specialty.FitIterable.IterableFoundation]] - unspecialized base class implementing most methods
  *     via its iterator and builder which should be extended wherever possible to minimize static forwarders
  */
sealed trait IterableTemplate[+E, +Repr] extends IterableLike[E, Repr] { iterable :FitIterableLike[E, Repr] =>
	def specialization :Specialized[_<:E] //= mySpecialization
	
	/** Traverses this collection applying `f` to every element. Use [[FitIterableLike#traverse]] instead to avoid
	  * unnecessary boxing resulting from non-unit return types of `f`.
	  * @tparam U return type of the computation, which should be `Unit` for `f` to be executed without boxing the argument.
	  */
	override def foreach[@specialized(Unit) U](f: (E) => U): Unit = iterator.foreach(f)
	
	
	
	/** Traverses the whole collection in an order opposite to [[foreach]]. */
	protected def reverseForeach(f :E=>Unit) :Unit
	
	/** Equivalent - and usually implemented as - [[FitIterableLike#foreach(f)]], but enforces `Unit` as
	  * the return type of the traversing function, which makes the compiler call the specialized `f.apply`
	  * instead of boxing the argument as it would with an arbitrary return type of `f`.
	  * For this reason this function should be preferred over `foreach`
	  * @param f computation to be applied to every element of this collection.
	  */
	def traverse(f :E=>Unit) :Unit = foreach(f)
	
	/** A specialized version of `Iterator` allowing for efficient traversing of this collection
	  * without boxing and unboxing. Most methods declared here or in [[FitIterableLike]] delegate directly
	  * to it, so should provide sensible implementations of most methods in its interface.
	  */
	def iterator :FitIterator[E]
	
	
	/** Delegates to iterator's specialized [[FitIterator#foldLeft]]. */
	override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, E) => O): O = iterator.foldLeft(z)(op)
	
	
	/** Delegates to specialized [[foldLeft]]. */
	override def /:[@specialized(Fun2) O](z: O)(op: (O, E) => O): O = foldLeft(z)(op)
	
	
	/** Delegates to specialized specialized [[foldRight]]. */
	override def :\[@specialized(Fun2) O](z: O)(op: (E, O) => O): O = foldRight(z)(op)
	
	
	/** Delegates to the corresponding, specialized method of the iterator. */
	override def foldRight[@specialized(Fun2) O](z: O)(op: (E, O) => O): O = iterator.foldRight(z)(op)
	
	
	//todo: should non-specializable 'trap' methods like fold/reduce have their deprecated overrides here?
	
//	/** Delegates to specialized [[foldLeft]]. */
//	override def fold[U >: E](z: U)(op: (U, U) => U): U = foldLeft(z)(op)
	
//	override def withFilter(p: (E) => Boolean): FilterIterable[E, Repr] = new SpecializedFilter(this, p)
	
	
	protected[this] def filter(p :E=>Boolean, ourTruth :Boolean) :Repr
	
	override def map[@specialized(Fun1Vals) O, That](f: (E) => O)(implicit bf: CanBuildFrom[Repr, O, That]): That = {
		val b = FitBuilder(bf(repr)).mapInput(f)
		b.sizeHint(this)
		b ++= this
		b.result()
	}
	
	
	
	override def flatMap[U, That](f: E => GenTraversableOnce[U])(implicit bf: CanBuildFrom[Repr, U, That]): That = {
		val b = FitBuilder(bf(repr)).flatMapInput(f)
		b ++= this
		b.result()
	}
	
	
	
	override def ++[B >: E, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
		val b = FitBuilder(bf(repr))
		b ++= thisCollection ++= that.seq
		b.result()
	}
	
	override def ++:[B >: E, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
		val b = FitBuilder(bf(repr))
		if (ofKnownSize(that))
			b.sizeHint(this, that.size)
		b ++= thisCollection ++= that.seq
		b.result()
	}
	
	//	override def ++:[B >: E, That](that: Traversable[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = super.++:(that)
	
	
	
	
	/** Subclasses should implement as efficiently as possible, taking advantage of specialization to boost [[ArrayView]] performance. */
	override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int) :Unit =
		iterator.copyToArray(xs, start, len)
	
	
	
	
	def toFitSeq :FitSeq[E]
	
	override def toBuffer[B >: E]: mutable.Buffer[B] = toFitBuffer //todo: optimistic buffer for primitive collections
	
	def toFitBuffer[U >: E :Specialized] :FitBuffer[U]
	
	
	
	/** Compares both collections by delegating directly to [[FitIterator#sameElements]]. */
	override def sameElements[U >: E](that: GenIterable[U]): Boolean = {
		val these = this.iterator
		val those = that.iterator
		these sameElements those
	}
	
	
	override def stringPrefix :String =
		typeStringPrefix+"["+mySpecialization.classTag+"]"
	
	
	protected[this] def typeStringPrefix :String = super.stringPrefix
	
	
}





/** Default implementations of methods of specialized collections which actually require specialization.
  * In general, a hands-off approach is taken and wherever possible, the method delegates to the corresponding
  * method of this instance's iterator, or the builder - received from `CanBuildFrom` or [[FitIterableLike#newBuilder]].
  * In particular, when possible, implementations here try to avoid explicit traverse of this collection and rather
  * pass it as the whole argument to the builder/iterator, which makes it easier to implement various optimizations.
  * Note that iterator-based implementations can be more efficient than those based on [[FitIterableLike#foreach]],
  * as the latter requires passing the computation as a function, which is specialized for fewer argument types
  * than the elements of this collection.
  * @author Marcin Mościcki
  */
trait FitIterableLike[@specialized(Elements) +E, +Repr] extends IterableTemplate[E, Repr] { iterable =>
	
	/*   ************* General purpose methods **************    */
	
	/** Specialization of this iterable. Double call necessary to enforce specialized call and covariance type clash. */
	protected[this] def mySpecialization :Specialized[E] = Specialized[E]
	
	/** Underlying representation of element type and preferred version of `@specialized` methods to use.
	  * It will generally reflect the type used to store the values in runtime. If this instance is an erased,
	  * unspecialized variant, returned specialization will be equal to `Specialized[Any]`. Note however that this
	  * only reflects the most generic interface without any constraints. Implementations,
	  * especially dedicated subclasses for `AnyRef` subtypes, may internally store their contents as strict subclasses
	  * of `AnyRef`, making casts risky! Infer as little as possible - you have been warned.
	  */
	def specialization :Specialized[_<:E] = mySpecialization
	
	override def head: E = iterator.head
	
	override def last :E
	
	

	
	
	
	/** Default implementation traverses this collection and makes a copy of its prefix and suffix, as per spec.
	  * Subclasses should generally override it with a more efficient implementation sharing the contents of this collection instead.
	  * @param p predicate splitting this collection into two continuous parts
	  * @return longest prefix satisfying the predicate and the remaining suffix of this collection
	  */
	override def span(p: (E) => Boolean): (Repr, Repr) = {
		val prefix = newBuilder; val suffix = newBuilder
		var inPrefix = true; val it = iterator
		while (it.hasNext) {
			val e = it.next()
			if (inPrefix && p(e)) prefix += e
			else suffix += e
		}
		(prefix.result(), suffix.result())
	}
	
	

	
	/** Implemented using [[reverseForeach]]. */
	override def foldRight[@specialized(Fun2) O](z: O)(op: (E, O) => O): O = {
		var acc = z
		reverseForeach { e => acc = op(e, acc) }
		acc
	}


	
	
	override def withFilter(p: (E) => Boolean): FilterIterable[E, Repr] = new SpecializedFilter(this, p)

	/** Implemented as explicit traversing of this collection using its iterator.
	  * Both of the returned collections are completely new instances built using `this.newBuilder`.
	  * @param p predicate dividing the elements of this collection into two groups
	  * @return a collection containing all elements from this collection for which `p(e)` is true,
	  *         and a collection containing all elements from this collection for which `p(e)` is false - in that order.
	  */
	override def partition(p: (E) => Boolean): (Repr, Repr) = {
		val yes = newBuilder; val no = newBuilder
		val i = iterator
		while (i.hasNext) {
			val e = i.next()
			if (p(e)) yes += e
			else no += e
		}
		(yes.result(), no.result())
	}
	
	


	override def scanLeft[@specialized(Fun2Vals) O, That](z: O)(op: (O, E) => O)(implicit bf: CanBuildFrom[Repr, O, That]): That = {
		var acc = z
		val b = bf(repr)
		b.sizeHint(this, 1)
		b += z
		val s = FitBuilder(b).mapInput { e :E => acc = op(acc, e); acc }
		s ++= this
		s.result()
	}
	
	
	/** Implemented using [[reverseForeach]]. */
	override def scanRight[@specialized(Fun2Vals) O, That](z: O)(op: (E, O) => O)(implicit bf: CanBuildFrom[Repr, O, That]): That = {
		val b = bf(repr)
		val buff = FitList.newReverseBuilder[O]
		buff += z
		var acc = z
		reverseForeach { e :E => acc = op(e, acc); buff += acc }
		val res = buff.result()
		b.sizeHint(res.length)
		b ++= res
		b.result()
	}
	
	

	
	/** Specialized iterator, assumed to provide sensible implementation of all methods.
	  * Most methods defined in [[FitIterableLike]] directly delegate to the corresponding method of this iterator.
	  */
	override def iterator: FitIterator[E]
	
	@unspecialized
	override def toIterator :FitIterator[E] = iterator
	
	def inverse :FitIterable[E] = new ReverseSeq(toFitSeq)
	
	
	
	
	/** Builder for `this` type of collection (`'Repr'`) specialized for the same element type as this collection. */
	override protected[this] def newBuilder: FitBuilder[E, Repr]
	

}
