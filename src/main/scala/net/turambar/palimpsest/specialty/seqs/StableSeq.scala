package net.turambar.palimpsest.specialty.seqs

import scala.annotation.unspecialized
import scala.collection.generic.{CanBuildFrom, GenericTraversableTemplate}
import scala.collection.{immutable, IndexedSeqLike, LinearSeq, LinearSeqLike}
import net.turambar.palimpsest.specialty.iterables.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.iterables._
import net.turambar.palimpsest.specialty.{ItemTypes, FitBuilder, RuntimeType}
import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.seqs.FitSeq.SeqFoundation






/** An immutable indexed sequence specialized on its element type.
  * @author Marcin Mościcki
  */
trait StableSeq[@specialized(ItemTypes) +E]
	extends immutable.Seq[E] //with IsStable[E] with StableIterableTemplate[E, StableSeq[E]]
	   with FitSeq[E] with SeqTemplate[E, StableSeq[E]] with IterableSpecialization[E, StableSeq[E]]
	   with SpecializableIterable[E, StableSeq]
	   with StableIterable[E] with StableIterableTemplate[E, StableSeq[E]]
{

//	@unspecialized
//	override def immutable[U >: E](implicit cbf: CanFitFrom[_, E, StableSeq[U]]): StableSeq[U] =
//		if (cbf.honorsBuilderFrom) this
//		else (cbf() ++= this).result()

	@unspecialized
	override def toSeq: StableSeq[E] = this

	@unspecialized
	override def toFitSeq :StableSeq[E] = this


	override def companion: FitCompanion[StableSeq] = StableSeq


}



/** Factory of immutable, specialized indexed sequences. */
object StableSeq extends InterfaceIterableFactory[StableSeq] {
	final val Empty :StableSeq[Nothing] = new EmptySeq[Nothing]
	private[this] final val EmptyIndexed = new EmptySeq

	@inline def acc[E :RuntimeType] :StableSeq[E] = ArrayPlus.of[E]

	override def one[@specialized(ItemTypes) E](elem :E) :StableSeq[E] = new Seq1[E](elem)

	def two[@specialized(ItemTypes) E](first :E, second :E) :StableSeq[E] = new Seq2(first, second)



	protected[this] type RealType[@specialized(ItemTypes) X] = StableArray[X]
	protected[this] final def default :FitIterableFactory[StableArray] = StableArray


	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[StableSeq[_], E, StableSeq[E]]): CanBuildFrom[StableSeq[_], E, StableSeq[E]] =
		fit.cbf





	private[seqs] class EmptySeq[@specialized(ItemTypes) +E]
		extends SeqFoundation[E, StableIndexedSeq[E]]
		   with FitIndexedSeq[E] with FitIndexedSeqTemplate[E, StableIndexedSeq[E]]
		   with StableSeq[E] with StableIndexedSeq[E] with EmptySeqTemplate[E, StableIndexedSeq[E]]
	{
		override protected def at(idx :Int) :E = throw new NoSuchElementException(debugString + "(" + idx + ")")
		protected[this] override def newBuilder :FitBuilder[E, StableIndexedSeq[E]] = StableIndexedSeq.newBuilder[E]
	}




	/** Specialized singleton sequence (small wrapper over a single element). */
	private[seqs] class Seq1[@specialized(ItemTypes) +E](override val head :E)
		extends SeqFoundation[E, StableIndexedSeq[E]]
		   with FitIndexedSeq[E] with FitIndexedSeqTemplate[E, StableIndexedSeq[E]]
		   with StableSeq[E] with StableIndexedSeq[E] with SingletonSpecialization[E, StableIndexedSeq[E]]
	{
		override def empty :StableIndexedSeq[E] = EmptyIndexed

		override final def length: Int = 1


		override def apply(idx: Int): E =
			if (idx!=0) throw new IndexOutOfBoundsException(s"$this($idx)")
			else head

		override protected[this] def at(idx: Int): E = head

		@unspecialized
		override protected def section(from: Int, until: Int): StableIndexedSeq[E] =
			if (from==0 && until==1) this
			else EmptyIndexed


		override def segmentLength(p: E => Boolean, from: Int): Int =
			if (from==0 && p(head)) 1 else 0

		override def indexWhere(p: E => Boolean, from: Int): Int =
			if (from==0 && !p(head)) 1 else 0

		@unspecialized
		override def lastIndexWhere(p: E => Boolean, from: Int): Int = indexWhere(p, from)

		override def indexOf[U >: E](elem: U, from: Int): Int =
			if (from==0 && elem==head) 0 else -1

		override def lastIndexOf[U>:E](elem :U, from :Int) :Int = indexOf(elem, from)



		@unspecialized
		final override def reverseIterator :FitIterator[E] = iterator

		@unspecialized override def toSeq :StableIndexedSeq[E] = this
		@unspecialized override def reverse :StableIndexedSeq[E] = this
		@unspecialized override def inverse :StableIndexedSeq[E] = this


		override def debugPrefix = "Seq1"
	}





	/** Specialized two-element sequence. */
	private[seqs] class Seq2[@specialized(ItemTypes) +E](override val head :E, override val last :E)
		extends SeqFoundation[E, StableIndexedSeq[E]]
			with FitIndexedSeq[E] with FitIndexedSeqTemplate[E, StableIndexedSeq[E]]
			with StableSeq[E] with StableIndexedSeq[E] with DoubletonSpecialization[E, StableIndexedSeq[E]]
	{

		override def length: Int = 2

		override def empty :StableIndexedSeq[E] = EmptyIndexed
		override def tail = new Seq1(last)
		override def init = new Seq1(head)


		override def apply(idx: Int): E = idx match {
			case 0 => head
			case 1 => last
			case _ => throw new IndexOutOfBoundsException(s"$this($idx)")
		}


		override protected[this] def at(idx: Int): E = apply(idx)


		override protected def section(from: Int, until: Int): StableIndexedSeq[E] =
			until - from match {
				case 0 => EmptyIndexed
				case 2 => this
				case 1 if from==0 => new Seq1(head)
				case 1 => new Seq1(last)
			}


		override def segmentLength(p: E => Boolean, from: Int): Int =
			if (from <= 0)
				if (p(head))
					if (p(last)) 2 else 1
				else 0
			else if (from == 1 && p(last)) 1
			else 0

		override def indexWhere(p: E => Boolean, from: Int): Int =
			if (from <= 0 && p(head)) 0
			else if (from == 1 && p(last)) 1
			else -1


		override def indexOf[U >: E](elem :U, from :Int) :Int =
			if (from <= 0 && head == elem) 0
			else if (from == 1 && last == elem) 1
			else -1


		override def lastIndexOf[U >: E](elem :U, end :Int) :Int =
			if (end >= 1 && last == elem) 1
			else if (end == 0 && head == elem) 0
			else -1




		//todo: inverse, reverse, reversedIterator and scala reversed: a bit much...
		override def reverseIterator :FitIterator[E] = FitIterator.two(last, head)
		override def reverse = new Seq2(last, head)
		@unspecialized override def inverse :StableIndexedSeq[E] = reverse
		@unspecialized override def toSeq :StableIndexedSeq[E] = this

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
		override def stringPrefix = s"Seq[$runtimeType]"
		override def debugPrefix = "Seq2"
	}

}
