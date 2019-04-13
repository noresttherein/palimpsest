package net.turambar.palimpsest.specialty.seqs

import scala.annotation.unspecialized
import scala.collection.generic.CanBuildFrom
import scala.collection.{immutable, IndexedSeqLike}
import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.iterables.{DoubletonSpecialization, IterableFoundation, SingletonSpecialization}
import net.turambar.palimpsest.specialty.{Elements, FitCompanion, FitIterableFactory, FitIterator, InterfaceIterableFactory, IterableSpecialization, SpecializableIterable, Specialized}
import net.turambar.palimpsest.specialty.seqs.FitSeq.SeqFoundation






/** An immutable indexed sequence specialized on its element type.
  * @author Marcin Mościcki
  */
trait StableSeq[@specialized(Elements) +E]
	extends immutable.Seq[E] with FitSeq[E] with SeqTemplate[E, StableSeq[E]] with IterableSpecialization[E, StableSeq[E]] //with SliceLike[E, StableSeq[E]]
			with SpecializableIterable[E, StableSeq]
{

	@unspecialized
	override def immutable[U >: E](implicit cbf: CanFitFrom[_, E, StableSeq[U]]): StableSeq[U] =
		if (cbf.honorsBuilderFrom) this
		else (cbf() ++= this).result()

	@unspecialized
	override def toSeq: StableSeq[E] = this

	@unspecialized
	override def toFitSeq :StableSeq[E] = this


	override def companion: FitCompanion[StableSeq] = StableSeq


}



/** Factory of immutable, specialized indexed sequences. */
object StableSeq extends InterfaceIterableFactory[StableSeq] {
	@inline def Acc[E :Specialized] :StableSeq[E] = ArrayPlus.Acc[E]

	def single[@specialized(Elements) E](elem :E) :StableSeq[E] = new Seq1[E](elem)

	def pair[@specialized(Elements) E](first :E, second :E) :StableSeq[E] = new Seq2(first, second)



	protected[this] type RealType[@specialized(Elements) X] = StableArray[X]
	protected[this] final def default = StableArray


	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[StableSeq[_], E, StableSeq[E]]): CanBuildFrom[StableSeq[_], E, StableSeq[E]] =
		fit.cbf

	//todo: IsStableIndexed?
	trait MakeStableIndexed[+E] extends immutable.IndexedSeq[E] with IndexedSeqLike[E, MakeStableIndexed[E]]
										with FitIndexedSeq[E] with IterableSpecialization[E, MakeStableIndexed[E]]
										with StableSeq[E] with SliceLike[E, MakeStableIndexed[E]]
										with SpecializableIterable[E, MakeStableIndexed]
	{
		override def companion :FitCompanion[MakeStableIndexed] = MakeStableIndexed

		@unspecialized override def toSeq :MakeStableIndexed[E] = this
	}

	object MakeStableIndexed extends InterfaceIterableFactory[MakeStableIndexed] {
		final val Empty :MakeStableIndexed[Nothing] = ArrayPlus.Empty
		override protected[this] type RealType[@specialized(Elements) X] = ArrayPlus[X]

		override protected[this] def default: FitIterableFactory[ArrayPlus] = ArrayPlus

		@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[MakeStableIndexed[_], E, MakeStableIndexed[E]]): CanBuildFrom[MakeStableIndexed[_], E, MakeStableIndexed[E]] =
			fit.cbf
	}





	/** Specialized singleton sequence (small wrapper over a single element). */
	private[seqs] class Seq1[@specialized(Elements) +E](override val head :E)
		extends SeqFoundation[E, MakeStableIndexed[E]] //with immutable.IndexedSeq[E] with FitIndexedSeq[E, ConstSeq[E]] with ConstSeq[E]
			with MakeStableIndexed[E] with FitIndexedSeq[E] with StableSeq[E] with SingletonSpecialization[E, MakeStableIndexed[E]]
	{
		override def empty :MakeStableIndexed[Nothing] = MakeStableIndexed.Empty

		override final def length: Int = 1


		override def apply(idx: Int): E =
			if (idx!=0) throw new IndexOutOfBoundsException(s"$this($idx)")
			else head

		override protected[this] def at(idx: Int): E = head

		@unspecialized
		override protected def section(from: Int, until: Int): MakeStableIndexed[E] =
			if (from==0 && until==1) this
			else MakeStableIndexed.Empty


		override def segmentLength(p: E => Boolean, from: Int): Int =
			if (from==0 && p(head)) 1 else 0

		override def indexWhere(p: E => Boolean, from: Int): Int =
			if (from==0 && !p(head)) 1 else 0

		@unspecialized
		override def lastIndexWhere(p: E => Boolean, from: Int): Int = indexWhere(p, from)

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

		@unspecialized override def toSeq :MakeStableIndexed[E] = this
		@unspecialized override def reverse :MakeStableIndexed[E] = this
		@unspecialized override def inverse :MakeStableIndexed[E] = this

		//		override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int): Unit =
		//			if (len>0 && start<xs.length)
		//				xs(start) = head
		//
		//		override def copyToBuffer[B >: E](dest: mutable.Buffer[B]): Unit = dest += head

		override def stringPrefix = s"Seq[$specialization]"
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

		override def empty :MakeStableIndexed[Nothing] = MakeStableIndexed.Empty
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
		@unspecialized override def inverse :MakeStableIndexed[E] = reverse
		@unspecialized override def toSeq :MakeStableIndexed[E] = this

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
		override def stringPrefix = s"Seq[$specialization]"
		override def typeStringPrefix = "Seq2"
	}

}
