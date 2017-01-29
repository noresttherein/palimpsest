package net.turambar.collection.specnaz

import net.turambar.collection.specnaz.SpecCompanion.SpecBuilder

import scala.collection.generic.{GenericCompanion, GenericTraversableTemplate}
import scala.collection.mutable



/**
  * @author Marcin Mościcki
  */
trait MutableSeq[@specialized(Reified) E]
	extends SpecSeq[E] with MutableSeqLike[E, MutableSeq[E]]
	with mutable.IndexedSeq[E] //with mutable.IndexedSeqOptimized[E, MutableSeq[E]]
	with GenericSpecializedTraversable[E, MutableSeq]
{

	override def seq: MutableSeq[E] = this

	override def companion: SpecCompanion[MutableSeq] = MutableSeq

}


object MutableSeq extends CopycatSeqFactory[MutableSeq] {
	protected[this] type RealType[@specialized(Reified) X] = ArraySlice[X]

	override protected[this] final def impl: SpecSeqFactory[ArraySlice] = ArraySlice

	def of[E <: AnyVal :Specialized](size :Int) :MutableSeq[E] =
		ArraySlice(Specialized.erasedArray[E](size))

	def of[E :Specialized](size :Int, value :E) :MutableSeq[E] =
		ArraySlice(arrayFill(Specialized.erasedArray[E](size), value))
}
