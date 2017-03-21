package net.turambar.palimpsest.specialty

import scala.annotation.unspecialized
import scala.collection.generic.{CanBuildFrom, FilterMonadic}
import scala.collection.{breakOut, GenIterable, GenTraversableOnce, IterableLike, mutable}

import net.turambar.palimpsest.specialty.FitIterable.{FilterIterable, SpecializedFilter}
import net.turambar.palimpsest.specialty.Specialized.{Fun1Vals, Fun1Res, Fun2, Fun2Vals}
import net.turambar.palimpsest.specialty.seqs.{ArrayView, FitBuffer, FitList, FitSeq}


/** Partial specialization of the `IterableLike` trait containing methods which can be implemented
  * without specialization for `E` - usually by delegating to another specialized method either
  * of this trait or its iterator. It is a partial interface only and should be considered as a group
  * of methods implemented and inherited by [[FitIterable]] and not a refererrable interface in itself,
  * or even implemented directly in general. All instances of it must also descend from [[IterableSpecialization]],
  * which is the fully specialized version of scala [[IterableLike]], providing similar implementations
  * It exists solely to avoid unnecessary explosion of specialized variants of methods like
  * [[IterableTemplate#foldLeft]], which don't feature `E` as part os their signature and can be implemented efficiently
  * by delegation to another specialized method.
 *
  * @tparam Repr specific type of the collection described by this interface, usually the self type of implementors,
  *              but it is not enforced here.
  * @see [[IterableSpecialization]] full version of this interface
  * @see [[net.turambar.palimpsest.specialty.iterables.IterableFoundation]] - unspecialized base class implementing most methods
  *      via its iterator and builder which should be extended wherever possible to minimize static forwarders
  */
//we are not sealing this trait in order to allow similar templates for other collections to extend it without extending the 'specialized' interfaces.
trait IterableTemplate[+E, +Repr] extends FitTraversableOnce[E] with IterableLike[E, Repr] {
//	this :IterableSpecialization[E, Repr] =>

//	override protected[this] def thisCollection :FitIterable[E] = this.asInstanceOf[FitIterable[E]]

	/** Specialization of this iterable. Double call necessary to enforce specialized call and covariance type clash. */
	protected[this] def mySpecialization :Specialized[E]

	/** Underlying representation of element type and preferred version of `@specialized` methods to use.
	  * It will generally reflect the type used to store the values in runtime. If this instance is an erased,
	  * unspecialized variant, returned specialization will be equal to `Specialized[Any]`. Note however that this
	  * only reflects the most generic interface without any constraints. Implementations,
	  * especially dedicated subclasses for `AnyRef` subtypes, may internally store their contents as strict subclasses
	  * of `AnyRef`, making casts risky! Infer as little as possible - you have been warned.
	  */
	override def specialization :Specialized[_<:E] = mySpecialization



	/** Similarly to `hasDefiniteSize`, this collection can can declare that `size`
	  * is implemented efficiently and doesn't affect the state of the collection (for example, doesn't move an iterator).
	  * Used to determine optional size hint for builders processing elements from this collection.
	  */
	def hasFastSize = isEmpty



	override def foreach[@specialized(Unit) U](f: (E) => U): Unit = iterator.foreach(f)

	/** Applies the given function to the first element in this collection and returns the result.
	  * While generally useful, it is defined here to resolve conflicts between independent definition of this
	  * method by several mixins. Subclasses which actually use this method - in particular singleton implementations -
	  * would do well to provide specialized implementations.
	  * @return value returned by `f` for `head`
	  * @throws `NoSuchElementException` if this collection is empty
	  * @see [[net.turambar.palimpsest.specialty.iterables.SingletonFoundation]]
	  * @see [[net.turambar.palimpsest.specialty.iterables.SingletonSpecialization]]
	  */
	protected[this] def forHead[O](f :E=>O) :O = f(head)
	protected[this] def forLast[O](f :E=>O) :O = f(last)


	protected def reverseForeach(f :E=>Unit) :Unit

	/** Equivalent - and usually implemented as - [[IterableSpecialization#foreach(f)]], but enforces `Unit` as
	  * the return type of the traversing function, which makes the compiler call the specialized `f.apply`
	  * instead of boxing the argument as it would with an arbitrary return type of `f`.
	  * For this reason this function should be preferred over `foreach`
 *
	  * @param f computation to be applied to every element of this collection.
	  */
	override def traverse(f :E=>Unit) :Unit = foreach(f)

	@inline final private[palimpsest] def reverseTraverse(f :E=>Unit) :Unit = reverseForeach(f)

	override def iterator :FitIterator[E] //= fitIterator

	def fitIterator :FitIterator[E] = iterator


	/** Delegates to iterator's specialized [[FitIterator#foldLeft]]. */
	override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, E) => O): O = iterator.foldLeft(z)(op)


	/** Delegates to specialized [[foldLeft]]. */
	override def /:[@specialized(Fun2) O](z: O)(op: (O, E) => O): O = foldLeft(z)(op)


	/** Delegates to specialized specialized [[foldRight]]. */
	override def :\[@specialized(Fun2) O](z: O)(op: (E, O) => O): O = foldRight(z)(op)


	/** Delegates to the corresponding, specialized method of the iterator. */
	override def foldRight[@specialized(Fun2) O](z: O)(op: (E, O) => O): O = iterator.foldRight(z)(op)


//	override def scanLeft[@specialized(Fun2) O, That](z: O)(op: (O, E) => O)(implicit bf: CanBuildFrom[Repr, O, That]) = iterator.scanLeft(z)(op)
//
//	override def scanRight[@specialized(Fun2) O, That](z: O)(op: (E, O) => O)(implicit bf: CanBuildFrom[Repr, O, That]) = super.scanRight(z)(op)

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
		if (hasFastSize && ofKnownSize(that))
			b.sizeHint(size + that.size)
		b ++= thisCollection ++= that.seq
		b.result()
	}

	override def ++:[B >: E, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
		val b = FitBuilder(bf(repr))
		if (hasFastSize && ofKnownSize(that))
			b.sizeHint(size + that.size)
		b ++= thisCollection ++= that.seq
		b.result()
	}

	override def ++:[B >: E, That](that: Traversable[B])(implicit bf: CanBuildFrom[Repr, B, That]): That =
		(that ++ seq)(breakOut)




	/** Subclasses should implement as efficiently as possible, taking advantage of specialization to boost [[ArrayView]] performance. */
	override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int): Unit =
		if (mySpecialization.runType isAssignableFrom xs.getClass.getComponentType )
			if (start<0)
				throw new IllegalArgumentException(s"$stringPrefix.copyToArray([], $start, $len)")
			else {
				val count = math.min(xs.length-start, len)
				if (count>0)
					verifiedCopyTo(xs.asInstanceOf[Array[E]], start, count)
			}
		else iterator.copyToArray(xs, start, len)

	protected[this] def verifiedCopyTo(xs: Array[E], start: Int, total: Int): Int =
		if (isEmpty) 0
		else { iterator.copyToArray(xs, start, total); math.min(total, size) }



	//todo: make toSeq the real implementaion delegating to toFitSeq?
	override def toSeq :Seq[E] = toFitSeq
	def toFitSeq :FitSeq[E] = (FitSeq.fitBuilder(mySpecialization) ++= this).result()

	/** Creates a buffer with the contents of this collection.
	  * If this collection is mutable, returned buffer may share its contents,
	  * with mutually visible modifications.
	  * Note that while returned buffer is of type [[FitBuffer]], this method can't be specialized,
	  * and as a result, returned instance will likely be an erased, not a specialized version.
	  * If you want to preserve element type and possibly share the contents,
	  * use [[IterableTemplate#toFitBuffer]] instead.
	  */
	override def toBuffer[B >: E]: FitBuffer[B] = toFitBuffer //todo: optimistic buffer for primitive collections

	def toFitBuffer[U >: E :Specialized] :FitBuffer[U] = FitBuffer.like[U] ++= this

	def inverse :FitIterable[E] = (FitList.reverseBuilder(mySpecialization) ++= this).result()



	/** This method is a specialized variant of [[Iterable#newBuilder]] and the returned builder,
	  * unless explicitly stated to the contrary, should be optimised towards creating sequences
	  * with the same specialization and underlying structure.
	  */
	override protected[this] def newBuilder: FitBuilder[E, Repr]



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
  * method of this instance's iterator, or the builder - received from `CanBuildFrom` or [[IterableSpecialization#newBuilder]].
  * In particular, when possible, implementations here try to avoid explicit traverse of this collection and rather
  * pass it as the whole argument to the builder/iterator, which makes it easier to implement various optimizations.
  * Note that iterator-based implementations can be more efficient than those based on [[IterableSpecialization#foreach]],
  * as the latter requires passing the computation as a function, which is specialized for fewer argument types
  * than the elements of this collection.
  *
  * @author Marcin Mościcki
  */
trait IterableSpecialization[@specialized(Elements) +E, +Repr] extends IterableTemplate[E, Repr] /*with mutable.Cloneable[Repr]*/ {

	
	/** Specialization of this iterable. Double call necessary to enforce specialized call and covariance type clash. */
	protected[this] def mySpecialization :Specialized[E] = Specialized[E]
	

	override def head: E = iterator.head
	
	override def last :E
	
	override protected[this] def forHead[@specialized(Fun1Res) O](f: (E) => O) = f(head)

	override protected[this] def forLast[@specialized(Fun1Res) O](f: (E) => O) = f(last)


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
	
	

	
	override def foldRight[@specialized(Fun2) O](z: O)(op: (E, O) => O): O = {
		var acc = z
		reverseForeach { e => acc = op(e, acc) }
		acc
	}


	
	
	override def withFilter(p: (E) => Boolean): FilterIterable[E, Repr] = new SpecializedFilter(this, p)

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
	
	


	override def scanLeft[@specialized(Fun2) O, That](z: O)(op: (O, E) => O)(implicit bf: CanBuildFrom[Repr, O, That]): That = {
		var acc = z
		val b = bf(repr)
		b.sizeHint(this, 1)
		b += z
		val s = FitBuilder(b).mapInput { e :E => acc = op(acc, e); acc }
		s ++= this
		s.result()
	}
	
	
	override def scanRight[@specialized(Fun2) O, That](z: O)(op: (E, O) => O)(implicit bf: CanBuildFrom[Repr, O, That]): That = {
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
	
	

	

//	/** The intent of this method is twofold: if this collection is mutable, than the
//	  * returned collection should be an exact but independent copy, and thus any future
//	  * changes to any of the instances will not affect the other. If the underlying collection
//	  * is immutable, then it can just return itself (and thus it is possible that `x.clone() eq x`).
//	  * However, if this instance is a slice of a larger collection, sharing representation with other
//	  * instances and possibly preventing garbage collection of a larger structure,
//	  * then it generally should create a new instance with 'minimised' representation.
//	  * Default implementation uses the builder associated with this collection to create a new instance.
//	  */
//	override def clone() :Repr = (newBuilder ++= this).result()
}
