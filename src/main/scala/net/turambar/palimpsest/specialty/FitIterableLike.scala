package net.turambar.palimpsest.specialty

import scala.annotation.unspecialized
import scala.collection.generic.{CanBuildFrom, FilterMonadic}
import scala.collection.{GenIterable, GenTraversableOnce, IterableLike, mutable}

import net.turambar.palimpsest.specialty.Specialized.{Fun1Vals, Fun2, Fun2Vals}




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
trait FitIterableLike[@specialized(Elements) +E, +Repr] extends IterableLike[E, Repr] { iterable =>
	
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
	
	
	
	/*   ************* Slicing methods **************  */
	
	
	
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
	
	
	
	/*  ********** Traversing & predicate testing methods delegating to the iterator *************  */
	
	/** Traverses this collection applying `f` to every element. Use [[FitIterableLike#traverse]] instead to avoid
	  * unnecessary boxing resulting from non-unit return types of `f`.
	  * @tparam U return type of the computation, which should be `Unit` for `f` to be executed without boxing the argument.
	  */
//	@unspecialized
//	override def foreach[@specialized(Unit) U](f: (E) => U): Unit = iterator.foreach(f) //traverse(f.asInstanceOf[E=>Unit]) //iterator.foreach(f)
	
	/** Equivalent - and usually implemented as - [[FitIterableLike#foreach(f)]], but enforces `Unit` as
	  * the return type of the traversing function, which makes the compiler call the specialized `f.apply`
	  * instead of boxing the argument as it would with an arbitrary return type of `f`.
	  * For this reason this function should be preferred over `foreach`
	  * @param f computation to be applied to every element of this collection.
	  */
	@unspecialized
	def traverse(f :E=>Unit) :Unit
	
	/** Traverses the whole collection in an order opposite to [[foreach]]. */
	@unspecialized
	def reverseForeach(f :E=>Unit) :Unit
	
	
	/** Delegates to iterator's specialized [[FitIterator#foldLeft]]. */
	override def foldLeft[@specialized(Fun2) O](z: O)(@unspecialized op: (O, E) => O): O = iterator.foldLeft(z)(op)
	
	
	/** Delegates to specialized [[foldLeft]]. */
	@inline override def /:[@specialized(Fun2) O](z: O)(op: (O, E) => O): O = foldLeft(z)(op)
	
	
	/** Delegates to specialized specialized [[foldRight]]. */
	@inline override def :\[@specialized(Fun2) O](z: O)(op: (E, O) => O): O = foldRight(z)(op)
	
	
/** Delegates to specialized [[foldLeft]]. */
//	@unspecialized //todo: determine how specialization works here
//	@inline override def fold[U >: E](z: U)(op: (U, U) => U): U = foldLeft(z)(op)
	
	
	
	/** Implemented using [[reverseForeach]]. */
	override def foldRight[@specialized(Fun2) O](z: O)(op: (E, O) => O): O = {
		var acc = z
		reverseForeach { e => acc = op(e, acc) }
		acc
	}

	
	
	
	/** Implemented using [[inverse]] and its [[tail]] and [[foreach]] methods. */
	//	@unspecialized //this is unspecialized as U is erased to Object and op will be called with two objects
	override def reduceRight[U >: E](op: (E, U) => U): U = {
		val elems = inverse
		val rest = inverse.tail //throws UnsupportedOperationException for us
		var acc :U = inverse.head
		for (e <- rest) { acc = op(e, acc) }
		acc
	}
	
	
	/** Implemented using [[tail]] and [[foreach]]. Assumes efficient slicing. */
	override def reduceLeft[U >: E](op: (U, E) => U): U = {
		val rest = tail.asInstanceOf[FitIterable[E]] //first to take advantage of UnsupportedOperationException
		var acc :U = head
		for (e <- rest) { acc = op(acc, e) }
		acc
	}
	
	
	/** Implemented using [[tail]] and [[fold]]. Assumes efficient slicing and that `this :FitIterable[E]`. */
	override def reduce[U >: E](op: (U, U) => U): U =
		tail.asInstanceOf[FitIterable[U]].fold(head)(op)
	
	
	
	
	/************ filtering and other self-typed collections ***************/
	
	override def withFilter(p: (E) => Boolean): FitFilter = new FitFilter(p)

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
	
	
	class FitFilter(p :E=>Boolean) extends FilterMonadic[E, Repr] {
		
		override def map[@specialized(Fun1Vals) O, That](f: (E) => O)(implicit bf: CanBuildFrom[Repr, O, That]): That = {
			val b = FitBuilder(bf(repr)).mapInput(f).filterInput(p)
			b ++= iterable
			b.result()
		}
		
		override def flatMap[O, That](f: (E) => GenTraversableOnce[O])(implicit bf: CanBuildFrom[Repr, O, That]): That = {
			val b = FitBuilder(bf(repr)).flatMapInput(f).filterInput(p)
			b ++= iterable
			b.result()
		}
		
		override def foreach[@specialized(Unit) U](f: (E) => U): Unit =
			iterator.filter(p).foreach(f) //.traverse { e => if (p(e)) f(e) }
		
		override def withFilter(f: (E) => Boolean): FitFilter =
			new FitFilter(e => p(e) && f(e))
	}
	
	
	
	
	/********************** mapping and related *********************************/
	
	
	@unspecialized
	override def map[@specialized(Fun1Vals) O, That](f: (E) => O)(implicit bf: CanBuildFrom[Repr, O, That]): That = {
		val b = FitBuilder(bf(repr)).mapInput(f)
		b.sizeHint(this)
		b ++= this
		b.result()
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
	
	
	/** Specialized iterator, assumed to provide sensible implementation of all methods.
	  * Most methods defined in [[FitIterableLike]] directly delegate to the corresponding method of this iterator.
	  */
	override def iterator: FitIterator[E]
	
	@unspecialized
	override def toIterator :FitIterator[E] = iterator
	
	def inverse :FitIterable[E] = new ReverseSeq(toFitSeq)
	
	override def toSeq: Seq[E]
	
	def toFitSeq :FitSeq[E]
	
	override def toBuffer[B >: E]: mutable.Buffer[B] =
		toFitBuffer //todo: optimistic buffer for primitive collections
	
	def toFitBuffer[U >: E :Specialized] :FitBuffer[U]
	
	
	/** Builder for `this` type of collection (`'Repr'`) specialized for the same element type as this collection. */
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
