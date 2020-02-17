package net.turambar.palimpsest.specialty.seqs

import scala.collection.generic.CanBuildFrom
import scala.collection.{mutable, GenTraversableOnce, IndexedSeqLike}
import net.turambar.palimpsest.specialty.iterables.AptCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.{arrayFill, ItemTypes, Vals, RuntimeType}
import net.turambar.palimpsest.specialty.iterables.{CloneableIterable, AptCompanion, AptIterableFactory, InterfaceIterableFactory, SpecializableIterable}

import scala.annotation.unspecialized
import scala.reflect.ClassTag


/*

trait FitBufferLike[E, +Repr<:FitBufferLike[E, Repr]]
	extends mutable.BufferLike[E, Repr] with MutableSliceLike[E, Repr]
{ self :Repr =>

}
*/



/** A `@specialized` version of [[mutable.Buffer]] extending also [[AptSeq]].
  *
  * @author Marcin Mościcki
  */
trait AptBuffer[@specialized(ItemTypes) E]
	extends mutable.Buffer[E] with mutable.BufferLike[E, AptBuffer[E]]
	   with MutableSeq[E] with ValSeqLike[E, AptBuffer[E]] with CloneableIterable[E, AptBuffer[E]]
	   with SpecializableIterable[E, AptBuffer]
{
//	import Specialized.Fun1

/*
	//todo: this is here till we find a better place for Subtractable implementatoin
	protected[seqs] def indicesOf(elems1 :Traversable[E], elems2 :GenTraversableOnce[E]) :mutable.Set[Int] = {
		var result = mutable.Set[Int]()
		var searchOffsets = mutable.Map[E, Int]().withDefaultValue(0)
		def collect(e :E) :Unit = { //todo this is not specialized!
		val i = indexOf(e, searchOffsets(e))
			if (i>=0) {
				result += i
				searchOffsets += e -> (i+1)
			}
		}
		elems1.foreach(collect); elems2.foreach(collect)
		result
	}
*/




	def appender :AptBuffer[E] = new TailBuffer(this)

//	def overwrite(offset :Int) :AptBuffer[E]

	override def +=(elem: E): this.type

	override def +=(elem1: E, elem2: E, elems: E*): this.type =
		this += elem1 += elem2 ++= elems


	override def ++=(xs: TraversableOnce[E]) :this.type = xs match {
		case fit :Vals[E] => this ++= fit
		case _ => xs.foreach (this += _); this

	}

	def ++=(elems :Vals[E]) :this.type = {
		val it = elems.toIterator
		while(it.hasNext) this += it.next()
		this
	}

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

//	def cutdown(toSize :Int) :Unit =
//		if (toSize <= 0) clear() //overflow conscious
//		else trimEnd(length - toSize)


	override def companion: AptCompanion[AptBuffer] = AptBuffer

	override protected[this] def typeStringPrefix = "Buffer"
}




/** Factory for [[mutable.Buffer]]s specialized on their element type.
  * Default implementation is that of a [[SharedArrayBuffer]].
  */
object AptBuffer extends InterfaceIterableFactory[AptBuffer] { //SpecializedSeqFactory[AptBuffer] {

	override protected[this] type RealType[@specialized(ItemTypes) X] = SharedArrayBuffer[X]
	override protected[this] def default: AptIterableFactory[SharedArrayBuffer] = SharedArrayBuffer

	@inline def emptyOf[E :RuntimeType](sizeHint :Int) :AptBuffer[E] = SharedArrayBuffer.ofCapacity[E](sizeHint)


	@inline def sized[E <: AnyVal :RuntimeType](size :Int) :AptBuffer[E] = SharedArrayBuffer.ofSize(size)

	@inline def fill[E](size :Int, value :E)(implicit elements :ClassTag[E] = ClassTag(RuntimeType.UnboxedClass(value.getClass))) :AptBuffer[E] =
		SharedArrayBuffer.fill(size, value)



	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[AptBuffer[_], E, AptBuffer[E]]): CanBuildFrom[AptBuffer[_], E, AptBuffer[E]] =
		fit.cbf

	
	
/*
	trait ProxyFitBuffer[@specialized(Elements) E] extends AptBuffer[E] {
		protected[this] def target :AptBuffer[E]
		protected def proxy(buf :AptBuffer[E]) :AptBuffer[E] = buf
		
		override def length: Int = target.length
		
		override protected def section(from: Int, until: Int): AptBuffer[E] = proxy(target.section(from, until))
		
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
		
		override def overwrite(start: Int, length: Int): AptBuffer[E] = proxy(target.overwrite(start, length))

		override def clear(): Unit = target.clear()
		
		
	}
	
	
	class OptimisticAptBuffer[@specialized(Elements) E](fit :AptBuffer[E], unfit : =>AptBuffer[E]) extends ProxyFitBuffer[E] {
		protected[this] final var target = fit
		private[this] var optimistic = true
		
		override protected def proxy(buf: AptBuffer[E]): AptBuffer[E] =
			if (optimistic) new OptimisticAptBuffer(fit, unfit)
			else buf
		
		
	}
*/
}
