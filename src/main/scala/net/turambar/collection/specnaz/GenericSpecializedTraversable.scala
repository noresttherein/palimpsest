package net.turambar.collection.specnaz

import scala.collection.{mutable, GenTraversable}
import scala.collection.generic.{GenericCompanion, GenericTraversableTemplate}

import net.turambar.collection.specnaz.SpecCompanion.{SpecBuilder}



/**
  * @author Marcin Mościcki
  */
trait GenericSpecializedTraversable[@specialized(Reified) +E, +S[@specialized(Reified) X] <: SpecSeq[X] with GenericSpecializedTraversable[X, S]]
	extends GenericTraversableTemplate[E, S] //with GenTraversable[E]
{ //this :GenTraversable[E] =>
	override def foreach[@specialized(Unit) U](f: (E) => U): Unit

	override def head: E

	override def companion: SpecCompanion[S]

	override protected[this] def newBuilder: SpecBuilder[E, S[E]] = companion.newBuilder[E]

	//todo: maybe this should be companion.specializedBuilder so that erased invocations still return a really specialized instance?
	override def genericBuilder[@specialized(Reified) T]: SpecBuilder[T, S[T]] = companion.newBuilder[T]

	def specBuilder[T :Specialized] :SpecBuilder[T, S[T]] = companion.specBuilder[T]

}
