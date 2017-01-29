package net.turambar.collection.specnaz

import net.turambar.collection.specnaz.SpecCompanion.{SpecCanBuildFrom, SpecBuilder}

import scala.collection.generic.{GenericCompanion, GenericTraversableTemplate}
import scala.collection.immutable


import scala.reflect.ClassTag

/**
  * @author Marcin Mościcki
  */
trait ConstSeq[@specialized(Reified) +E]
	extends immutable.IndexedSeq[E] with GenericSpecializedTraversable[E, ConstSeq]
	with SpecSeq[E] with SpecSeqLike[E, ConstSeq[E]]
{

	override def copy :ConstSeq[E] = this

	override def immutable[U >: E](implicit cbf: SpecCanBuildFrom[_, E, ConstSeq[U]]): ConstSeq[U] =
		if (cbf.companion==companion) this
		else (cbf() ++= this).result()

	override def toSeq: ConstSeq[E] = this

	override def toIndexedSeq :ConstSeq[E] =  this

	override def seq: ConstSeq[E] = this

	override def companion: SpecCompanion[ConstSeq] = ConstSeq

}


object ConstSeq extends CopycatSeqFactory[ConstSeq] {
	@inline def Acc[E :Specialized] :ConstSeq[E] = GrowableArray.Acc[E]
	
	protected[this] type RealType[@specialized(Reified) X] = ConstArray[X]
	protected[this] final def impl = ConstArray



}