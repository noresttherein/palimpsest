package net.turambar.palimpsest.specialty

import scala.annotation.unspecialized
import scala.collection.generic.{CanBuildFrom, FilterMonadic, GenericCompanion}
import scala.collection.{GenIterable, GenTraversableOnce, IterableLike, mutable}

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.FitIterable.IterableFoundation
import net.turambar.palimpsest.specialty.Specialized.{Fun1, Fun1Vals, Fun2, Fun2Vals}
import net.turambar.palimpsest.specialty.seqs.{FitList, SharedArray}


/** Root trait for specialized collections and iterators.
  * It isn't specialized itself, so avoid using it to actually traverse the elements,
  * preferring [[FitIterable]] and [[FitIterator]] interfaces where specialization is possible.
  * It exists mainly to check
  * @tparam E
  */
trait FitItems[+E] extends TraversableOnce[E] { //with FilterMonadic[E, FitTraversableOnce[E]] {
//	protected[this] def mySpecialization :Specialized[E]
	def specialization :Specialized[_<:E] //= mySpecialization
	
	def traverse(f :E=>Unit) :Unit
	
	def hasFastSize :Boolean
	
	override def toIterator :FitIterator[E]

}

object FitItems {
	def unapply[E](col :GenTraversableOnce[E]) :Option[FitItems[E]] =
		col match {
			case it :FitItems[E] => Some(it)
			case arr :mutable.WrappedArray[E] => Some(SharedArray(arr.array))
			case _ => None
		}

	
}



/** Base trait of specialized collections mirroring scala [[Iterable]]. Overrides methods which can benefit
  * from specialization and delegates even more of them directly to their iterator counterparts.
  * @author Marcin Mościcki
  */
trait FitIterable[@specialized(Elements) +E]
	extends FitItems[E] with Iterable[E] with FitIterableLike[E, FitIterable[E]]
	        with SpecializedTraversableTemplate[E, FitIterable]
{
	
	override def companion: FitCompanion[FitIterable] = FitIterable
}



object FitIterable extends InterfaceIterableFactory[FitIterable] {
	override protected[this] type RealType[@specialized(Elements) X] = FitList[X]
	
	override protected[this] def default = FitList
	
	
	def unapply[E](col :GenTraversableOnce[E]) :Option[FitIterable[E]] = col match {
		case it :FitIterable[E] => Some(it)
		case arr :mutable.WrappedArray[E] => Some(SharedArray(arr.array))
		case _ => None
	}
	
	
	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[FitIterable[_], E, FitIterable[E]] ): CanBuildFrom[FitIterable[_], E, FitIterable[E]] =
		fit.cbf
	
	
	abstract class FilterIterable[+E, +Repr] extends FilterMonadic[E, Repr] {
		
		override def map[@specialized(Fun1Vals) O, That](f: (E) => O)(implicit bf: CanBuildFrom[Repr, O, That]): That =  {
			val b = FitBuilder(bf(from)).mapInput(f).filterInput(predicate)
			b ++= iterable
			b.result()
		}
		
		override def flatMap[O, That](f: (E) => GenTraversableOnce[O])(implicit bf: CanBuildFrom[Repr, O, That]): That = {
			val b = FitBuilder(bf(from)).flatMapInput(f).filterInput(predicate)
			b ++= iterable
			b.result()
		}
		
		override def foreach[@specialized(Unit) U](f: (E) => U): Unit =
			iterable.iterator.filter(predicate).foreach(f)
		
		override def withFilter(f: (E) => Boolean): FilterIterable[E, Repr]
		
		protected[this] def iterable :FitIterableLike[E, Repr]
		protected[this] def from :Repr = iterable.repr
		protected[this] def predicate :E=>Boolean
	}
	
	
	class SpecializedFilter[@specialized(Elements) +E, +Repr](
			protected[this] val iterable :FitIterableLike[E, Repr],
			protected[this] val predicate :E=>Boolean)
		extends FilterIterable[E, Repr]
	{
		override def withFilter(f: (E) => Boolean): SpecializedFilter[E, Repr] =
			new SpecializedFilter(iterable, (e :E) => predicate(e) && f(e))
	}
	
	
	
	
	
	/** Base class for [[FitIterable]] hierarchy containing default implementations of all methods
	  * which don't require specialization (or can be implemented by delegating to a smaller subset of specialized methods).
	  * Exists to decrease the class sizes of concrete types as well as static forwarding to trait-implemented methods.
	  * All implementations provided here, unless stated to the contrary, delegate to the corresponding iterator methods and,
	  * in case of collection results, append the iterator as a whole to `newBuilder`.
	  */
	abstract class IterableFoundation[+E, +Repr] extends FitItems[E] with FitIterableLike[E, Repr] {
		override def specialization :Specialized[_<:E] = mySpecialization
		
//		@inline override final def items :ForEach[E] = new ForEach(this)
		
		override def isEmpty = iterator.isEmpty
		
		override def nonEmpty = iterator.hasNext
		
		override def hasFastSize = isEmpty
		
		
		/** Generic default target of all slicing methods defined in `FitIterable` extracted to be specializable.
		  * @param from index of the first element of the slice, validated to be >=0.
		  * @param until not validated index after the last element of the slice
		  */
		protected[this] def dropTake(from :Int, until :Int) :Repr = {
			val builder = newBuilder
			val taken = until-from
			if (taken<=0)
				builder.result()
			else {
				builder.sizeHintBounded(taken, this)
				builder ++= iterator.drop(from).take(taken)
//				iterator.drop(from).take(taken).foreach(builder.addOne) //{ builder += _ }
				builder.result()
			}
		}
		
		override def slice(from: Int, until: Int): Repr = dropTake(math.max(from, 0), until)
		
		override def take(n: Int): Repr = dropTake(0, n)
		
		override def drop(n: Int): Repr = dropTake(math.max(n, 0), Int.MaxValue)
		
		/** Creates a new collection with [[newBuilder]] and iterator's specialized [[FitIterator#takeWhile]]. */
		override def takeWhile(p: (E) => Boolean): Repr = {
			val b = newBuilder
			b ++= iterator.takeWhile(p)
			b.result()
		}
		
		/** Creates a new collection with [[newBuilder]] and iterator's specialized [[FitIterator#dropWhile]]. */
		override def dropWhile(p: (E) => Boolean): Repr = {
			val b = newBuilder
			b ++= iterator.dropWhile(p)
			b.result()
		}
		
		
		
		
		/*  ********** Traversing & predicate testing methods delegating to the iterator *************  */
		
		//	overriden for specialization
		
		/** Delegates to iterator's specialized [[FitIterator#foreach]]. */
		override def foreach[@specialized(Unit) O](f: (E) => O): Unit = iterator.foreach(f)
		
		/** Traverses the whole collection in an order opposite to [[foreach]]. */
		override def reverseForeach(f :E=>Unit) :Unit = inverse.foreach(f)
		
		override def traverse(f :E=>Unit) :Unit = foreach(f) //iterator.foreach(f)
		
		/** Delegates to iterator's specialized [[FitIterator#forall]]. */
		override def forall(p: E => Boolean): Boolean = iterator.forall(p)
		
		
		/** Delegates to iterator's specialized [[FitIterator#exists]]. */
		override def exists(p: E => Boolean): Boolean = iterator.exists(p)
		
		
		/** Delegates to iterator's specialized [[FitIterator#find]]. */
		override def find(p: E => Boolean): Option[E] = iterator.find(p)
		
		
		/** Delegates to iterator's specialized [[FitIterator#count]]. */
		override def count(p: (E) => Boolean): Int = iterator.count(p)
		
		
		/** Delegates to iterator's specialized [[FitIterator#foldLeft]]. */
		override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, E) => O): O = iterator.foldLeft(z)(op)
		
		
		/** Delegates to [[foldLeft]]. */
		@inline final override def /:[@specialized(Fun2) O](z: O)(op: (O, E) => O): O = foldLeft(z)(op)
		
		
		/** Delegates to specialized [[foldRight]]. */
		@inline final override def :\[@specialized(Fun2) O](z: O)(op: (E, O) => O): O = foldRight(z)(op)
		
		
		/** Delegates to [[foldLeft]]. */
		@inline override def fold[U >: E](z: U)(op: (U, U) => U): U = foldLeft(z)(op)
		
		
		
		/** Delegates to the corresponding method of our iterator and adds the whole iterator to a new builder from `newBuilder`. */
		protected[this] def filter(p :(E) => Boolean, ourTruth :Boolean) :Repr =
			(newBuilder ++= iterator.filter(p, ourTruth)).result()
		
		
		override def filter(p: (E) => Boolean): Repr = filter(p, ourTruth = true)
		
		override def filterNot(p: (E) => Boolean): Repr = filter(p, ourTruth = false)
		

		
		
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
		
		
		
		override def toIterator :FitIterator[E] = iterator
		
		override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int): Unit =
			iterator.copyToArray(xs, start, len)

		
		override def sameElements[U >: E](that: GenIterable[U]): Boolean = {
			val these = this.iterator
			val those = that.iterator
			these sameElements those
		}
		
	}
	
	
	
} //object FitIterable



