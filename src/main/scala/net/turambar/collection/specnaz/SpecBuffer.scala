package net.turambar.collection.specnaz

import net.turambar.collection.specnaz.SpecCompanion.{SpecCanBuildFrom, SpecBuilder}

import scala.collection.generic.{Growable, GenericCompanion, GenericTraversableTemplate}
import scala.collection.mutable



/**
  * @author Marcin Mościcki
  */
trait SpecBuffer[@specialized(Reified) E]
	extends mutable.Buffer[E] with mutable.BufferLike[E, SpecBuffer[E]] with GenericSpecializedTraversable[E, SpecBuffer]
	with MutableSeq[E] with MutableSeqLike[E, SpecBuffer[E]]
{

	def appender :SpecBuffer[E] = new BufferTail(this)


	override def +=(elem: E): this.type

	override def +=(elem1: E, elem2: E, elems: E*): this.type =
		this += elem1 += elem2 ++= elems


	override def +=:(elem: E): this.type



	override def -=(x: E): this.type = {
		val i = indexOf(x)
		if (i != -1) remove(i)
		this
	}

	override def -=(elem1: E, elem2: E, elems: E*): this.type = {
		this -= elem1
		this -= elem2
		this --= elems
	}

		
	

	override def remove(n: Int): E


	override def companion: SpecCompanion[SpecBuffer] = SpecBuffer

	override protected[this] def typeStringPrefix = "SpecBuffer"
}





object SpecBuffer extends SpecSeqFactory[SpecBuffer] {

	def of[E <: AnyVal :Specialized](size :Int) :SpecBuffer[E] =
		SharedArrayBuffer(Specialized.erasedArray[E](size))

	def of[E :Specialized](size :Int, value :E) :SpecBuffer[E] =
		SharedArrayBuffer(arrayFill(Specialized.erasedArray[E](size), value))


	@inline
	final override def newBuilder[@specialized(Reified) E]: SpecBuilder[E, SpecBuffer[E]] =
		SharedArrayBuffer.newBuilder[E]

	@inline
	final override def specBuilder[E: Specialized]: SpecBuilder[E, SpecBuffer[E]] = SharedArrayBuffer.specBuilder[E]
}
