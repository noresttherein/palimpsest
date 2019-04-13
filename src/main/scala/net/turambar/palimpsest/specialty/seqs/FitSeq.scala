package net.turambar.palimpsest.specialty.seqs

import scala.annotation.unspecialized
import scala.collection.generic.CanBuildFrom
import scala.collection.{immutable, mutable, GenTraversableOnce, SeqLike}
import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.iterables.{DoubletonFoundation, DoubletonSpecialization, IterableFoundation, SingletonFoundation, SingletonSpecialization}
import net.turambar.palimpsest.specialty.seqs.StableSeq.{MakeStableIndexed, Seq1, Seq2}
import net.turambar.palimpsest.specialty.{Elements, FitCompanion, FitIterable, FitIterableFactory, FitIterator, InterfaceIterableFactory, IterableSpecialization, SpecializableIterable, Specialized}


/** A scala `Seq` (either mutable or immutable underneath) which is specialized on its element type.
  * In order not to pollute the API with a counterpart of every scala collection interface, it rolls `IndexedSeq` and `Seq`
  * into one. Subclasses are expected to provide fast [[FitSeq#length]] operation to benefit from proper buffer allocation
  * on copying, but not all provide fast random indexing. This breaks the contract of [[IndexedSeq]] which promises it,
  * but fortunately default implementation in the collections library do not depend on it, and you shouldn't either, if possible.
  * Prefer iterating over it with an `Iterator` or, better, appropriate traversing operations, rather than indexing it directly,
  * as it can be generally implemented more efficiently anyway.
  *
  * Another departure from the contract is that current implementations by default return views of themselves with all slicing
  * operations (`drop`/`take` family) and sometimes concatenation. This means that they will be generally O(1), lending themselves
  * well to recursive algorithms and, in case of mutable collections, modifications to any are visibile to the rest.
  * This departure is considered beneficial as it allows easier 'overwriting' of contents in a particular place of a buffer,
  * but use [[FitSeq#copy]], [[FitSeq#immutable]] or [[FitSeq#toIndexedCollection]] where you want to guarantee immutable content.
  * The latter method has an additional guarantee of returning a 'true' [[IndexedSeq]] with O(1) indexing.
  *
  * For above reason, when storing a sequence of unknown origin for longer periods of time, consider also using
  * [[FitSeq#copy]] or [[FitSeq#immutable]] to drop references to any originating collection, which would prevent it from
  * being garbage collected (such as taking a short slice from a much longer sequence).
  */
trait FitSeq[@specialized(Elements) +E]
	extends Seq[E] with SeqLike[E, FitSeq[E]]
			with FitIterable[E] with SeqTemplate[E, FitSeq[E]] with IterableSpecialization[E, FitSeq[E]]
			with SpecializableIterable[E, FitSeq]
{
	//Dope Vector, slice

	/** Target of `apply` for internal use, assuming the index is valid (faster). */
	protected[this] def at(idx :Int) :E

	/** Quick access to the element at the given index for other collections. Directly calls [[at]] */
	private[seqs] def get(idx :Int) :E = at(idx)

	override def apply(idx: Int): E =
		if (idx<0 || idx>=length)
			throw new IndexOutOfBoundsException(idx.toString)
		else at(idx)



	override def companion: FitCompanion[FitSeq] = FitSeq


}








/** Factory for sequences specialized on their element type. */
object FitSeq extends InterfaceIterableFactory[FitSeq] {
	import Specialized.Fun1Vals

	type Stable[@specialized(Elements) +E] = StableSeq[E]
	type Mutable[@specialized(Elements) E] = MutableSeq[E]
	type Indexed[@specialized(Elements) +E] = FitIndexedSeq[E]

	final val Empty :FitSeq[Nothing] = ArrayPlus.Empty

	@inline def Acc[E :Specialized] :StableSeq[E] = ArrayPlus.Acc[E]
	
	protected[this] type RealType[@specialized(Elements) X] = StableArray[X]

	override protected[this] final def default: FitIterableFactory[StableArray] = StableArray

	/** Highest precedence implicit `CanBuildFrom` for the whole `FitSeq[_]` hierarchy - without this level of indirection
	  * we can't override implicits from `IndexedSeq`.
	  */
	implicit override def canBuildFrom[E](implicit fit :CanFitFrom[FitSeq[_], E, FitSeq[E]]) :CanBuildFrom[FitSeq[_], E, FitSeq[E]] =
		fit.cbf
//	implicit def canBuildFrom[F[X]<:FitSeq[X], E](implicit fitCBF :FitCanBuildFrom[F[_], E, F[E]]) :CanBuildFrom[F[_], E, F[E]] =
//		fitCBF.cbf
	
	

	def single[@specialized(Elements) E](elem :E) :StableSeq[E] = new Seq1[E](elem)
	
	def pair[@specialized(Elements) E](first :E, second :E) :StableSeq[E] = new Seq2(first, second)

//	/** Adapt any sequence to a specialized interface. This is an unspecialized method creating
//	  * an unspecialized (erased) instance.
//	  */
//	def adapt[E](seq :Seq[E]) :FitSeq[E] = seq match {
//		case s :FitSeq[_] => s.asInstanceOf[FitSeq[E]]
//		case _ => new ErasedFitSeq[E](seq)
//	}

	

	
/*
	class ErasedFitSeq[E](private val backing :Seq[E], private val offset :Int, val length :Int)
		extends SeqFoundation[E, FitSeq[E]] with FitSeq[E] with SliceLike[E, FitSeq[E]]
	{
		def this(backing :Seq[E]) = this(backing, 0, backing.length)
		
		override protected def section(from: Int, until: Int): FitSeq[E] =
			new ErasedFitSeq[E](backing, offset+from, until-from)
		
		override def apply(idx: Int): E = backing(offset+idx)
		
		override def iterator: FitIterator[E] = FitIterator.adapt(backing.iterator.slice(offset, offset + length))
	}
*/


	abstract class SeqFoundation[+E, +Repr<:FitSeq[E]] extends IterableFoundation[E, Repr] with SeqTemplate[E, Repr] {

		override def indexOf[U >: E](elem: U, from: Int): Int =
			if (mySpecialization.boxType isAssignableFrom elem.getClass)
				positionOf(elem.asInstanceOf[E], from)
			else
				iterator.indexOf(elem, from)

		override protected[this] def positionOf(elem :E, from :Int) :Int = iterator.indexOf(elem, from)

		override def lastIndexOf[U >: E](elem: U, end: Int): Int =
			if (mySpecialization.boxType isAssignableFrom elem.getClass)
				lastPositionOf(elem.asInstanceOf[E], end)
			else
				iterator.indexOf(elem, end)

		override protected[this] def lastPositionOf(elem :E, from :Int) :Int = iterator.indexOf(elem, from)




	}
	
}

