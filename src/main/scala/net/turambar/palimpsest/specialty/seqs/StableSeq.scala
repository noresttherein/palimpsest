package net.turambar.palimpsest.specialty.seqs

import scala.annotation.unspecialized
import scala.collection.generic.CanBuildFrom
import scala.collection.{IndexedSeqLike, immutable}
import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.FitIterable.IterableFoundation
import net.turambar.palimpsest.specialty.{Elements, FitCompanion, FitIterableFactory, InterfaceIterableFactory, IterableSpecialization, SpecializableIterable, Specialized}






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

	protected[this] type RealType[@specialized(Elements) X] = StableArray[X]
	protected[this] final def default = StableArray


	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[StableSeq[_], E, StableSeq[E]]): CanBuildFrom[StableSeq[_], E, StableSeq[E]] =
		fit.cbf

	trait MakeStableIndexed[+E] extends immutable.IndexedSeq[E] with IndexedSeqLike[E, MakeStableIndexed[E]]
										with FitIndexedSeq[E] with IterableSpecialization[E, MakeStableIndexed[E]]
										with StableSeq[E] with SliceLike[E, MakeStableIndexed[E]]
										with SpecializableIterable[E, MakeStableIndexed]
	{
		override def companion :FitCompanion[MakeStableIndexed] = MakeStableIndexed

		@unspecialized override def toFitSeq :MakeStableIndexed[E] = this
	}

	object MakeStableIndexed extends InterfaceIterableFactory[MakeStableIndexed] {
		override protected[this] type RealType[@specialized(Elements) X] = ArrayPlus[X]

		override protected[this] def default: FitIterableFactory[ArrayPlus] = ArrayPlus

		@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[MakeStableIndexed[_], E, MakeStableIndexed[E]]): CanBuildFrom[MakeStableIndexed[_], E, MakeStableIndexed[E]] =
			fit.cbf
	}
	

	 
}
