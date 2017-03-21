package net.turambar.palimpsest.specialty.seqs

import scala.annotation.unspecialized
import scala.collection.generic.CanBuildFrom
import scala.collection.{GenTraversableOnce, SeqLike, immutable, mutable}
import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.iterables.{DoubletonFoundation, DoubletonSpecialization, IterableFoundation, SingletonFoundation, SingletonSpecialization}
import net.turambar.palimpsest.specialty.seqs.StableSeq.MakeStableIndexed
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
	
	

	def single[@specialized(Elements) E](elem :E) :FitSeq[E] = new Seq1[E](elem)
	
	def pair[@specialized(Elements) E](first :E, second :E) :FitSeq[E] = new Seq2(first, second)

//	/** Adapt any sequence to a specialized interface. This is an unspecialized method creating
//	  * an unspecialized (erased) instance.
//	  */
//	def adapt[E](seq :Seq[E]) :FitSeq[E] = seq match {
//		case s :FitSeq[_] => s.asInstanceOf[FitSeq[E]]
//		case _ => new ErasedFitSeq[E](seq)
//	}

	
	
	/** Specialized singleton sequence (small wrapper over a single element). */
	private[seqs] class Seq1[@specialized(Elements) +E](override val head :E)
		extends SeqFoundation[E, MakeStableIndexed[E]] //with immutable.IndexedSeq[E] with FitIndexedSeq[E, ConstSeq[E]] with ConstSeq[E]
				with MakeStableIndexed[E] with FitIndexedSeq[E] with StableSeq[E] with SingletonSpecialization[E, MakeStableIndexed[E]]
	{
		override def empty = MakeStableIndexed.Empty
		
		override final def length: Int = 1


		override def apply(idx: Int): E =
			if (idx!=0) throw new IndexOutOfBoundsException(s"$this($idx)")
			else head
		
		override protected[this] def at(idx: Int): E = head
		
		@unspecialized
		override protected def section(from: Int, until: Int): MakeStableIndexed[E] =
			if (from==0 && until==1) this
			else MakeStableIndexed.Empty
		
		
		override def segmentLength(p: (E) => Boolean, from: Int): Int =
			if (from==0 && p(head)) 1 else 0
		
		override def indexWhere(p: (E) => Boolean, from: Int): Int =
			if (from==0 && !p(head)) 1 else 0

		@unspecialized
		override def lastIndexWhere(p: (E) => Boolean, from: Int): Int = indexWhere(p, from)
		
		override def indexOf[U >: E](elem: U, from: Int): Int =
			if (from==0 && elem==head) 0 else -1
		
		override def lastIndexOf[U>:E](elem :U, from :Int) :Int = indexOf(elem, from)
		
//		override def find(p: (E) => Boolean): Option[E] = if (p(head)) Some(head) else None
		
		
		
/*
		override def foreach[@specialized(Unit) U](f: (E) => U): Unit = f(head)

		override def filter(f :E=>Boolean) :MakeStableIndexed[E] =
			if (f(head)) this
			else MakeStableIndexed.Empty
		
		override def filterNot(f :E=>Boolean) :MakeStableIndexed[E] =
			if (f(head)) MakeStableIndexed.Empty
			else this
		
		override def map[@specialized(Fun1Vals) U, That](f: (E) => U)(implicit bf: CanBuildFrom[MakeStableIndexed[E], U, That]): That = bf match {
			case fit :CanFitFrom[_, _, _] if fit.companion == FitSeq || fit.companion == StableSeq =>
				new Seq1(head).asInstanceOf[That]
			case _ => (bf(this) += f(head)).result()
		}
		
		//todo: check if the builder can't just return mapped sequence
		override def flatMap[U, That](f: (E) => GenTraversableOnce[U])(implicit bf: CanBuildFrom[MakeStableIndexed[E], U, That]): That =
			(bf(this) ++= f(head).seq).result()
		
		
		override def iterator: FitIterator[E] = FitIterator(head)
*/
		@unspecialized
		final override def reverseIterator :FitIterator[E] = iterator

		@unspecialized override def toFitSeq = this
		@unspecialized override def reverse = this
		@unspecialized override def inverse = this
		
//		override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int): Unit =
//			if (len>0 && start<xs.length)
//				xs(start) = head
//
//		override def copyToBuffer[B >: E](dest: mutable.Buffer[B]): Unit = dest += head
		
		override def typeStringPrefix = "Seq1"
	}
	
	
	
	
	
	/** Specialized two-element sequence. */
	private[seqs] class Seq2[@specialized(Elements) +E](override val head :E, override val last :E)
		extends SeqFoundation[E, MakeStableIndexed[E]] //with immutable.IndexedSeq[E]
				with MakeStableIndexed[E] with FitIndexedSeq[E] with DoubletonSpecialization[E, MakeStableIndexed[E]]
	{
		override def headOption = Some(head)
		override def lastOption = Some(last)
		
		override def length: Int = 2

		override def empty = MakeStableIndexed.Empty
		override def tail = new Seq1(last)
		override def init = new Seq1(head)
		
		
		override def apply(idx: Int): E = idx match {
			case 0 => head
			case 1 => last
			case n => throw new IndexOutOfBoundsException(s"$this($idx)")
		}
			
		
		override protected[this] def at(idx: Int): E = idx match {
			case 0 => head
			case 1 => last
			case _ => throw new IndexOutOfBoundsException(s"$this")
		}
		
		
		override protected def section(from: Int, until: Int): MakeStableIndexed[E] =
			until - from match {
				case 0 => MakeStableIndexed.Empty
				case 2 => this
				case 1 if from==0 => new Seq1(head)
				case 1 => new Seq1(last)
			}
				
		
		override def segmentLength(p: (E) => Boolean, from: Int): Int =
			if (from<=0)
				if (p(head))
					if (p(last)) 2 else 1
				else 0
			else if (from==1 && p(last)) 1
			else 0
		
		override def indexWhere(p: (E) => Boolean, from: Int): Int =
			if (from<=0)
				if (p(head)) 0
				else if (p(last)) 1 else 2
			else if (from==1 && p(head)) 1
			else 0



		override def reverseIterator = FitIterator(last, head)
		override def reverse = new Seq2(last, head)
		@unspecialized override def inverse = reverse
		@unspecialized override def toFitSeq = this
		
/*		override def foreach[@specialized(Unit) U](f: (E) => U): Unit = { f(head); f(last) }
		
		override def filter(p: (E) => Boolean): MakeStableIndexed[E] =
			if (p(head))
				if (p(last)) this
				else new Seq1(head)
			else if (p(last)) new Seq1(last)
			else MakeStableIndexed.Empty
		
		
		override def filterNot(p: (E) => Boolean): MakeStableIndexed[E] =
			if (p(head))
				if (p(last)) MakeStableIndexed.Empty
				else new Seq1(last)
			else if (p(last)) new Seq1(head)
			else this
		
		
		
		override def map[@specialized(Fun1Vals) O, That](f: (E) => O)(implicit bf: CanBuildFrom[MakeStableIndexed[E], O, That]): That = bf match {
			case fit :CanFitFrom[_, _, _] if fit.companion==FitSeq || fit.companion == StableSeq =>
				new Seq2(f(head), f(last)).asInstanceOf[That]
			case _ =>
				(bf(this) += f(head) += f(last)).result()
		}
		
		override def flatMap[U, That](f: (E) => GenTraversableOnce[U])(implicit bf: CanBuildFrom[MakeStableIndexed[E], U, That]): That =
			(bf(this) ++= f(head).seq ++= f(last).seq).result()
		
		
		override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int): Unit = {
			val space = xs.length-start
			if (len>0 && space > 0) {
				xs(start) = head
				if (len>1 && space > 1)
					xs(start+1) = last
			}
		}
			
		
		override def copyToBuffer[B >: E](dest: mutable.Buffer[B]): Unit = {
			dest += head
			dest += last
		}
		*/
		override def typeStringPrefix = "Seq2"
	}
	
	
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

