package net.turambar.palimpsest.specialty

import java.lang.Math

import scala.annotation.unspecialized
import scala.collection.generic.{CanBuildFrom, FilterMonadic}
import scala.collection.{breakOut, GenIterable, GenTraversableOnce, IterableLike, mutable}

import net.turambar.palimpsest.specialty.FitIterable.{FilterIterable, SpecializedFilter}
import net.turambar.palimpsest.specialty.Specialized.{Fun1Vals, Fun1Res, Fun2, Fun2Vals}
import net.turambar.palimpsest.specialty.seqs.{ArrayView, FitBuffer, FitList, FitSeq}


/** Partial specialization of the `IterableLike` trait containing methods which can be implemented
  * without specialization for `E` - usually by delegating to another specialized method either
  * of this trait or its iterator. It is a partial interface only and should be considered as a group
  * of methods implemented and inherited by [[FitIterable]] and not a referable interface in itself,
  * or even implemented directly in general. All instances of it must also descend from [[IterableSpecialization]],
  * which is the fully specialized version of scala [[IterableLike]], providing similar implementations
  * It exists solely to avoid unnecessary explosion of specialized variants of methods like
  * [[IterableTemplate#foldLeft]], which don't feature `E` as part of their signature and can be implemented efficiently
  * by delegation to another specialized method.
  *
  *
  * This interface exists in part to minimize the number of method signatures in exploded synthetic classes of the
  * specialized interfaces, but primarily to circumvent linearization issues inherent to specialization.
  * When a non-specialized class or trait mixes in a specialized interface, only the generic, non-specialized version of
  * the latter is used. This leaves any extending specialized classes/traits with generic, erased method implementations
  * unless they mix in the interface again with a specialized argument. In the latter case the generic and specialized
  * synthetic classes of the interface could become separated in class linearization, breaking internals of the scala
  * specialization implementaion. Hence '''all non-specialized classes/traits should mix-in only non-specialized traits'''
  * like this one, instead of the public specialized interface.
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
	def hasFastSize :Boolean = isEmpty

	def head_? : ?[E]

	override def headOption :Option[E] = if (isEmpty) None else Some(head)

	def last_? : ?[E]

	override def lastOption :Option[E] = if (isEmpty) None else Some(last)

	/** Applies the given function to the first element in this collection and returns the result.
	  * Useful especially in non-specialized caller context, where `f(head)` would result in boxing, while
	  * the implementation will likely be specialized. While benefit in case of a single element will
	  * likely be minimal, the alternative of invoking `head` directly would still need a virtual method call.
	  * While generally useful, it is defined here to resolve conflicts between independent definition of this
	  * method by several mixins. Subclasses which actually use this method - in particular singleton implementations -
	  * would do well to provide specialized implementations.
	  * @return value returned by `f` for `head`
	  * @throws `NoSuchElementException` if this collection is empty
	  * @see [[net.turambar.palimpsest.specialty.iterables.SingletonFoundation]]
	  * @see [[net.turambar.palimpsest.specialty.iterables.SingletonSpecialization]]
	  */
	protected[this] def forHead[@specialized(Fun1Res) O](f :E=>O) :O //= f(head)


	/** Applies the given function to the last element in this collection and returns the result.
	  * Useful especially in non-specialized context, where `f(last)` would result in boxing, while
	  * the implementation will likely be specialized.
	  * It is defined here to resolve conflicts between independent definition of this
	  * method by several mixins. Subclasses which actually use this method - in particular singleton implementations -
	  * would do well to provide specialized implementations.
	  * @return value returned by `f` for `last`
	  * @throws `NoSuchElementException` if this collection is empty
	  * @see [[net.turambar.palimpsest.specialty.iterables.SingletonFoundation]]
	  * @see [[net.turambar.palimpsest.specialty.iterables.SingletonSpecialization]]
	  */
	protected[this] def forLast[@specialized(Fun1Res) O](f :E=>O) :O //= f(last)



	override def filter(p :E => Boolean) :Repr = filter(p, true)

	override def filterNot(p :E => Boolean) :Repr = filter(p, false)

	def filter(p :E=>Boolean, where :Boolean) :Repr


	//todo: traverse(f.asInstanceOf[E=>Unit])
	override def foreach[@specialized(Unit) U](f: E => U): Unit = iterator.foreach(f)

	//todo: consider renaming inverseForeach/foreachReversed. why is it protected? its useful
	protected def reverseForeach(f :E=>Unit) :Unit

	/** Equals - and usually implemented as - [[IterableSpecialization#foreach(f)]], but enforces `Unit` as
	  * the return type of the traversing function, which makes the compiler call the specialized `f.apply`
	  * instead of boxing the argument as it would with an arbitrary return type of `f`.
	  * For this reason this function should be preferred over `foreach`
	  *
	  * @param f computation to be applied to every element of this collection.
	  */
	override def traverse(f :E=>Unit) :Unit = foreach(f)

	@inline final private[palimpsest] def reverseTraverse(f :E=>Unit) :Unit = reverseForeach(f)


	override def map[@specialized(Fun1Vals) O, That](f: E => O)(implicit bf: CanBuildFrom[Repr, O, That]): That = {
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





	override def find(p :E=>Boolean) :Option[E] = find_?(p, true).toOption

	def find_?(p :E=>Boolean): ?[E] = find_?(p, true)
	//todo: consider any other type, not only boolean maybe? and maybe use default argument to avoid
	def find_?(p :E=>Boolean, where :Boolean): ?[E] = iterator.find_?(p, where)

	override def exists(p :E=>Boolean) :Boolean = find_?(p).isDefined
	override def forall(p :E=>Boolean) :Boolean = find_?(p, false).isEmpty


	/** Delegates to iterator's specialized [[FitIterator#foldLeft]]. */
	override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, E) => O): O = iterator.foldLeft(z)(op)


	/** Delegates to the corresponding, specialized method of the iterator. */
	override def foldRight[@specialized(Fun2) O](z: O)(op: (E, O) => O): O = iterator.foldRight(z)(op)


	/** Delegates to specialized [[foldLeft]]. */
	override def /:[@specialized(Fun2) O](z: O)(op: (O, E) => O): O = foldLeft(z)(op)


	/** Delegates to specialized specialized [[foldRight]]. */
	override def :\[@specialized(Fun2) O](z: O)(op: (E, O) => O): O = foldRight(z)(op)

	//	override def scanLeft[@specialized(Fun2) O, That](z: O)(op: (O, E) => O)(implicit bf: CanBuildFrom[Repr, O, That]) = iterator.scanLeft(z)(op)
	//
	//	override def scanRight[@specialized(Fun2) O, That](z: O)(op: (E, O) => O)(implicit bf: CanBuildFrom[Repr, O, That]) = super.scanRight(z)(op)

	//todo: should non-specializable 'trap' methods like fold/reduce have their deprecated overrides here?

	//	override def fold[U >: E](z: U)(op: (U, U) => U): U = foldLeft(z)(op)

	//	override def withFilter(p: (E) => Boolean): FilterIterable[E, Repr] = new SpecializedFilter(this, p)


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
		if (mySpecialization.runType isAssignableFrom xs.getClass.getComponentType)
			if (start<0)
				throw new IndexOutOfBoundsException(s"$stringPrefix.copyToArray([], $start, $len)")
			else {
				val count = Math.min(xs.length - start, len)
				if (count>0)
					uncheckedCopyTo(xs.asInstanceOf[Array[E]], start, count)
			}
		else manualCopyToArray(xs, start, len) //iterator.copyToArray(xs, start, len)


	/** Implementation of [[IterableTemplate.copyToArray]] which assumes that parameters have been verified.
	  * Default implementation delegates to the appropriate method of this collection's iterator, but subclasses are
	  * encouraged to override it with more efficient implementations.
	  * @param xs target array of element type verified to be of the same specialization as this collection.
	  * @param start index of the array cell at which write should begin. Must have been verified to be non-negative.
	  * @param total number of elements of this collection to write. Must have been verified to be positive and fit in the array.
	  * @return number of elements written, which will be `min(this.size, total)`.
	  */
	protected[this] def uncheckedCopyTo(xs: Array[E], start: Int, total: Int): Int =
		if (isEmpty) 0
		else {
			iterator.copyToArray(xs, start, total)
			Math.min(total, size)
		}


	protected[this] def manualCopyToArray[U >: E](xs :Array[U], start :Int, len :Int) :Unit =
		iterator.copyToArray(xs, start, len)


	override def iterator :FitIterator[E] //= toIterator

	override def toIterator :FitIterator[E] = iterator


	//todo:
//	override def toList :List[E] = toFitList

//	def toFitList :LinkedList[E]

	//todo: make toSeq the real implementation delegating to toFitSeq?
	override def toSeq :FitSeq[E] = (FitSeq.fitBuilder(mySpecialization) ++= this).result()

	@deprecated("use toSeq", "")
	def toFitSeq :FitSeq[E] = toSeq

	/** Creates a buffer with the contents of this collection.
	  * If this collection is mutable, returned buffer may share its contents,
	  * with mutually visible modifications.
	  * Note that while returned buffer is of type [[FitBuffer]], this method can't be specialized,
	  * and as a result, returned instance will likely be not a specialized version.
	  * If you want to preserve element type and possibly share the contents,
	  * use [[IterableTemplate#toFitBuffer]] instead.
	  */
	override def toBuffer[B >: E]: FitBuffer[B] = toFitBuffer //todo: optimistic buffer for primitive collections

	def toFitBuffer[U >: E :Specialized] :FitBuffer[U] = FitBuffer.like[U] ++= this

//	@deprecated("what does it even mean to inverse a set...", "")
	def inverse :FitIterable[E] = (FitList.reverseBuilder(mySpecialization) ++= this).result()



	/** This method is a specialized variant of [[Iterable#newBuilder]] and the returned builder,
	  * unless explicitly stated to the contrary, should be optimised towards creating sequences
	  * with the same specialization and underlying structure.
	  */
	override protected[this] def newBuilder: FitBuilder[E, Repr]



	/** Compares both collections by delegating directly to [[FitIterator#sameElements]]. */
	override def sameElements[U >: E](that: GenIterable[U]): Boolean = that match {
		case fit :FitIterable[U] =>
			(!hasFastSize || !fit.hasFastSize || size == fit.size) && (iterator sameElements fit.iterator)
		case _ =>
			iterator sameElements that.iterator
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

	override def head_? : ?[E] = if (isEmpty) Blank else Sure(head)

	override def last_? : ?[E] = if (isEmpty) Blank else Sure(last)

	override protected[this] def forHead[@specialized(Fun1Res) O](f: E => O) :O = f(head)

	override protected[this] def forLast[@specialized(Fun1Res) O](f: E => O) :O = f(last)


	override def span(p: E => Boolean): (Repr, Repr) = {
		val prefix = newBuilder; val suffix = newBuilder
		val i = iterator
		while (i.hasNext && p(i.head)) prefix += i.next()
		while (i.hasNext) suffix += i.next()
		(prefix.result(), suffix.result())
	}

	override def filter(p :E => Boolean, where :Boolean) :Repr = {
		val builder = newBuilder
		val it = iterator
		while (it.hasNext) {
			val e = it.next()
			if (p(e) == where)
				builder += e
		}
		builder.result()
	}
	
	override def withFilter(p: E => Boolean): FilterIterable[E, Repr] = new SpecializedFilter(this, p)

	override def partition(p: E => Boolean): (Repr, Repr) = {
		val yes = newBuilder; val no = newBuilder
		val i = iterator
		while (i.hasNext) {
			val e = i.next()
			if (p(e)) yes += e
			else no += e
		}
		(yes.result(), no.result())
	}




	override def count(p :E => Boolean) :Int = {
		var res = 0
		foreach { e => if (p(e)) res += 1 }
		res
	}


	override def foldLeft[@specialized(Fun2) O](z :O)(op: (O, E) => O): O = {
		var acc = z
		foreach { e => acc = op(acc, e) }
		acc
	}

	override def foldRight[@specialized(Fun2) O](z: O)(op: (E, O) => O): O = {
		var acc = z
		reverseForeach { e => acc = op(e, acc) }
		acc
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
	
	

	
//todo: clone

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

