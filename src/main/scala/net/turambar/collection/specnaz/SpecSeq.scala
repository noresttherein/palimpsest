package net.turambar.collection.specnaz

import net.turambar.collection.specnaz.SpecCompanion.{SpecCanBuildFrom, SpecBuilder}

import scala.collection.generic.{CanBuildFrom, GenericCompanion, GenericTraversableTemplate}
import scala.collection.mutable



trait SpecSeq[@specialized(Reified) +E]
	extends IndexedSeq[E] with GenericSpecializedTraversable[E, SpecSeq] with SpecSeqLike[E, SpecSeq[E]]
{
	override def toSeq :Seq[E] = this

	override def companion: SpecCompanion[SpecSeq] = SpecSeq

}





object SpecSeq extends CopycatSeqFactory[SpecSeq] {
	@inline def Acc[E :Specialized] :ConstSeq[E] = GrowableArray.Acc[E]
	
	protected[this] type RealType[@specialized(Reified) X] = ConstArray[X]

	override protected[this] final def impl: SpecSeqFactory[ConstArray] = ConstArray

	/** Highest precedence implicit `CanBuildFrom` for the whole `SpecSeq[_]` hierarchy - without this level of indirection
	  * we can't override implicits from `IndexedSeq`.
	  */
	implicit def canBuildFrom[F[X]<:SpecSeq[X], E](implicit specCBF :SpecCanBuildFrom[F[_], E, F[E]]) :CanBuildFrom[F[_], E, F[E]] = specCBF


	def adapt[E](seq :Seq[E]) :SpecSeq[E] = seq match {
		case s :SpecSeq[_] => s.asInstanceOf[SpecSeq[E]]
		case _ => new GenericSpecSeq[E](seq)
	}

}

