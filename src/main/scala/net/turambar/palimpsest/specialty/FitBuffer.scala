package net.turambar.palimpsest.specialty

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom



/** A `@specialized` version of [[mutable.Buffer]] extending also [[FitSeq]].
  *
  * @author Marcin Mościcki
  */
trait FitBuffer[@specialized(Elements) E]
	extends mutable.Buffer[E] with mutable.BufferLike[E, FitBuffer[E]] with SpecializedTraversableTemplate[E, FitBuffer]
	        with MutableSeq[E] with MutableSeqLike[E, FitBuffer[E]]
{
//	import Specialized.Fun1
	
	def appender :FitBuffer[E] = new TailBuffer(this)
	

	override def +=(elem: E): this.type

	override def +=(elem1: E, elem2: E, elems: E*): this.type =
		this += elem1 += elem2 ++= elems

	def ++=(elems :FitItems[E]) :this.type

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


	override def companion: FitCompanion[FitBuffer] = FitBuffer

	override protected[this] def typeStringPrefix = "FitBuffer"
}




/** Factory for [[mutable.Buffer]]s specialized on their element type.
  * Default implementation is that of a [[SharedArrayBuffer]].
  */
object FitBuffer extends InterfaceIterableFactory[FitBuffer] { //SpecializedSeqFactory[FitBuffer] {

	override protected[this] type RealType[@specialized(Elements) X] = SharedArrayBuffer[X]
	override protected[this] def default: FitIterableFactory[SharedArrayBuffer] = SharedArrayBuffer
	
	def of[E <: AnyVal :Specialized](size :Int) :FitBuffer[E] =
		SharedArrayBuffer(Specialized.erasedArray[E](size))

	def of[E :Specialized](size :Int, value :E) :FitBuffer[E] =
		SharedArrayBuffer(arrayFill(Specialized.erasedArray[E](size), value))
	
	
	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[FitBuffer[_], E, FitBuffer[E]]): CanBuildFrom[FitBuffer[_], E, FitBuffer[E]] =
		fit.cbf

	
	
/*
	trait ProxyFitBuffer[@specialized(Elements) E] extends FitBuffer[E] {
		protected[this] def target :FitBuffer[E]
		protected def proxy(buf :FitBuffer[E]) :FitBuffer[E] = buf
		
		override def length: Int = target.length
		
		override protected def section(from: Int, until: Int): FitBuffer[E] = proxy(target.section(from, until))
		
		override protected[this] def at(idx: Int): E = target.get(idx)
		
		override protected[this] def set(idx: Int, elem: E): Unit = target.uncheckedUpdate(idx, elem)
		
		override def update(idx: Int, elems: TraversableOnce[E]): Unit = target.update(idx, elems)
		
		override def update(fromIndex: Int, value: E, count: Int): Unit = target.update(fromIndex, value, count)
		
		override def +=(elem: E): this.type = { target += elem; this }
		
		override def +=:(elem: E): this.type = { elem +=: target; this }
		
		override def ++=(xs: TraversableOnce[E]): this.type = { target ++= xs; this }
		
		override def ++=:(xs: TraversableOnce[E]): this.type = { xs ++=: target; this }
		
		override def --=(xs: TraversableOnce[E]): this.type = { target --= xs; this }
		
		
		override def insertAll(n: Int, elems: Traversable[E]): Unit = target.insertAll(n, elems)
		
		override def remove(n: Int): E = target.remove(n)
		
		override def overwrite(start: Int, length: Int): FitBuffer[E] = proxy(target.overwrite(start, length))

		override def clear(): Unit = target.clear()
		
		
	}
	
	
	class OptimisticFitBuffer[@specialized(Elements) E](fit :FitBuffer[E], unfit : =>FitBuffer[E]) extends ProxyFitBuffer[E] {
		protected[this] final var target = fit
		private[this] var optimistic = true
		
		override protected def proxy(buf: FitBuffer[E]): FitBuffer[E] =
			if (optimistic) new OptimisticFitBuffer(fit, unfit)
			else buf
		
		
	}
*/
}
