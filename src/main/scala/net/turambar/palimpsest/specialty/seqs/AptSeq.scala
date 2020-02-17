package net.turambar.palimpsest.specialty.seqs

import scala.collection.generic.CanBuildFrom
import scala.collection.SeqLike
import net.turambar.palimpsest.specialty.iterables.AptCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.iterables._
import net.turambar.palimpsest.specialty.seqs.StableSeq.{Seq1, Seq2}
import net.turambar.palimpsest.specialty.{ItemTypes, RuntimeType}
import net.turambar.palimpsest.specialty.RuntimeType.Specialized

/** A scala `Seq` (either mutable or immutable underneath) which is specialized on its element type.
  * In order not to pollute the API with a counterpart of every scala collection interface, it rolls `IndexedSeq` and `Seq`
  * into one. Subclasses are expected to provide fast [[AptSeq#length]] operation to benefit from proper buffer allocation
  * on copying, but not all provide fast random indexing. This breaks the contract of [[IndexedSeq]] which promises it,
  * but fortunately default implementation in the collections library do not depend on it, and you shouldn't either, if possible.
  * Prefer iterating over it with an `Iterator` or, better, appropriate traversing operations, rather than indexing it directly,
  * as it can be generally implemented more efficiently anyway.
  *
  * Another departure from the contract is that current implementations by default return views of themselves with all slicing
  * operations (`drop`/`take` family) and sometimes concatenation. This means that they will be generally O(1), lending themselves
  * well to recursive algorithms and, in case of mutable collections, modifications to any are visibile to the rest.
  * This departure is considered beneficial as it allows easier 'overwriting' of contents in a particular place of a buffer,
  * but use [[AptSeq#copy]], [[AptSeq#immutable]] or [[AptSeq#toIndexedCollection]] where you want to guarantee immutable content.
  * The latter method has an additional guarantee of returning a 'true' [[IndexedSeq]] with O(1) indexing.
  *
  * For above reason, when storing a sequence of unknown origin for longer periods of time, consider also using
  * [[AptSeq#copy]] or [[AptSeq#immutable]] to drop references to any originating collection, which would prevent it from
  * being garbage collected (such as taking a short slice from a much longer sequence).
  */
trait AptSeq[@specialized(ItemTypes) +E]
	extends Seq[E] with SeqLike[E, AptSeq[E]]
			with AptIterable[E] with SeqTemplate[E, AptSeq[E]] with IterableSpecialization[E, AptSeq[E]]
			with SpecializableIterable[E, AptSeq] with CloneableIterable[E, AptSeq[E]]
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



	override def companion: AptCompanion[AptSeq] = AptSeq


}








/** Factory for sequences specialized on their element type. */
object AptSeq extends InterfaceIterableFactory[AptSeq] {
	import Specialized.Fun1Vals

	type Stable[@specialized(ItemTypes) +E] = StableSeq[E]
	type Mutable[@specialized(ItemTypes) E] = MutableSeq[E]
	type Indexed[@specialized(ItemTypes) +E] = AptIndexedSeq[E]

	final val Empty :AptSeq[Nothing] = ArrayPlus.Empty

	@inline def Acc[E :RuntimeType] :StableSeq[E] = ArrayPlus.of[E]

	protected[this] type RealType[@specialized(ItemTypes) X] = StableArray[X]

	override protected[this] final def default: AptIterableFactory[StableArray] = StableArray

	/** Highest precedence implicit `CanBuildFrom` for the whole `AptSeq[_]` hierarchy - without this level of indirection
	  * we can't override implicits from `IndexedSeq`.
	  */
	@inline implicit override def canBuildFrom[E](implicit fit :CanFitFrom[AptSeq[_], E, AptSeq[E]]) :CanBuildFrom[AptSeq[_], E, AptSeq[E]] =
		fit.cbf
//	implicit def canBuildFrom[F[X]<:AptSeq[X], E](implicit fitCBF :FitCanBuildFrom[F[_], E, F[E]]) :CanBuildFrom[F[_], E, F[E]] =
//		fitCBF.cbf



	override def one[@specialized(ItemTypes) E](elem :E) :StableSeq[E] = new Seq1[E](elem)

	def two[@specialized(ItemTypes) E](first :E, second :E) :StableSeq[E] = new Seq2(first, second)

//	/** Adapt any sequence to a specialized interface. This is an unspecialized method creating
//	  * an unspecialized (erased) instance.
//	  */
//	def adapt[E](seq :Seq[E]) :AptSeq[E] = seq match {
//		case s :AptSeq[_] => s.asInstanceOf[AptSeq[E]]
//		case _ => new ErasedFitSeq[E](seq)
//	}




/*
	class ErasedFitSeq[E](private val backing :Seq[E], private val offset :Int, val length :Int)
		extends SeqFoundation[E, AptSeq[E]] with AptSeq[E] with SliceLike[E, AptSeq[E]]
	{
		def this(backing :Seq[E]) = this(backing, 0, backing.length)

		override protected def section(from: Int, until: Int): AptSeq[E] =
			new ErasedFitSeq[E](backing, offset+from, until-from)

		override def apply(idx: Int): E = backing(offset+idx)

		override def iterator: FitIterator[E] = FitIterator.adapt(backing.iterator.slice(offset, offset + length))
	}
*/


	abstract class SeqFoundation[+E, +Repr<:AptSeq[E]] extends IterableFoundation[E, Repr] with SeqTemplate[E, Repr]
	
}

