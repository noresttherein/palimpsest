package net.turambar.palimpsest.specialty.seqs

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.{Elements, FitCompanion, InterfaceIterableFactory, Specialized, SpecializedTraversableTemplate}



/** An immutable indexed sequence specialized on its element type.
  * @author Marcin Mościcki
  */
trait ConstSeq[@specialized(Elements) +E] //todo: unspecialize it
	extends immutable.IndexedSeq[E] with SpecializedTraversableTemplate[E, ConstSeq]
	        with FitSeq[E] with FitSeqLike[E, ConstSeq[E]]
{

	override def copy :ConstSeq[E] = this

	override def immutable[U >: E](implicit cbf: CanFitFrom[_, E, ConstSeq[U]]): ConstSeq[U] =
		if (cbf.honorsBuilderFrom) this
		else (cbf() ++= this).result()

	override def toSeq: ConstSeq[E] = this

	override def toIndexedSeq :ConstSeq[E] =  this

//	override def seq: ConstSeq[E] = this

	override def companion: FitCompanion[ConstSeq] = ConstSeq
	
	

}

/** Factory of immutable, specialized indexed sequences. */
object ConstSeq extends InterfaceIterableFactory[ConstSeq] {
	@inline def Acc[E :Specialized] :ConstSeq[E] = ArrayPlus.Acc[E]
	
	protected[this] type RealType[@specialized(Elements) X] = ConstArray[X]
	protected[this] final def default = ConstArray
	
	/** Performs identity conversion from specialized [[CanFitFrom]] to a generic `CanBuildFrom`.
	  * Unfortunately, in order to avoid conflits with implicit resolution of `CanBuildFrom`,
	  * concrete descendant companion objects must implement it directly.
	  *
	  * @param fit specialized builder factory for this collection type
	  * @tparam E desired element type of built collection
	  * @return [[CanFitFrom#cbf]]
	  */
	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[ConstSeq[_], E, ConstSeq[E]]): CanBuildFrom[ConstSeq[_], E, ConstSeq[E]] =
		fit.cbf
}