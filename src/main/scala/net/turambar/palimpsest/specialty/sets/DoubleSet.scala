package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.{FitBuilder, FitIterator, FitTraversableOnce}
import net.turambar.palimpsest.specialty.FitIterable.IterableMapping
import net.turambar.palimpsest.specialty.FitIterator.MappedIterator
import net.turambar.palimpsest.specialty.Specialized.{Fun1Res, Fun2}
import net.turambar.palimpsest.specialty.tries.{BinaryTrie, IterableTriePotFoundation, LongTrie, MutableLongTrie}
import net.turambar.palimpsest.specialty.tries.BinaryTrie.BinaryTriePatch

import scala.collection.GenTraversableOnce
import java.lang.Double.{doubleToRawLongBits => doubleToKey, longBitsToDouble => keyToDouble}



//todo: alias 0.0 and -0.0 to 0; consider semantics of NaN

/*
class DoubleSet(contents :LongTrie, keyCount :Int)
	extends IterableTriePotFoundation[Long, LongTrie, Double, DoubleSet](contents, keyCount)
	   with StableSet[Double] with SetSpecialization[Double, DoubleSet]
	   with TrieKeySetSpecialization[Long, LongTrie, LongTrie, Double, DoubleSet]
//	   with TraversableTrieKeySet[Long, LongTrie, LongTrie, Double, DoubleSet]
	   with LongTrieSet[Double, DoubleSet]
	   with ElementOf[Double, LongTrie]
{
	override protected def plant(trie :LongTrie, trieSize :Int) :DoubleSet = new DoubleSet(trie, trieSize)

	override protected def elements :ElementOf[Double, LongTrie] = this

	override protected def countingElements :ElementCounter[Double, LongTrie] = new DoubleElementCounter

	@inline final override def elementOf(leaf :LongTrie) :Double = keyToDouble(leaf.key)

	override protected def friendTrie(elems :GenTraversableOnce[_]) :Option[LongTrie] = elems match {
		case set :DoubleSet => Some(set.trie)
		case _ => None
	}

	override protected def patchTrie(t :LongTrie, element :Double)(patch :BinaryTriePatch[Long, LongTrie, LongTrie]) :LongTrie =
		t.patchKey(patch)(doubleToKey(element))

	override protected def patchTrie(t :LongTrie, elems :FitTraversableOnce[Double])(patch :BinaryTriePatch[Long, LongTrie, LongTrie]) :LongTrie = {
		var res = t
		elems foreach { elem => res = t.patchKey(patch)(doubleToKey(elem)) }
		res
	}

	override def empty :DoubleSet = DoubleSet.Empty


	override def apply(elem :Double) :Boolean = trie.hasKey(doubleToKey(elem))

	override def contains(elem :Double) :Boolean = trie.hasKey(doubleToKey(elem))

	//todo
	override def mutable :ValSet.Mutable[Double] = ???


	override def stringPrefix = "Set[Double]"
	override def typeStringPrefix = "DoubleSet"
}




/**
  * @author Marcin Mościcki
  */
object DoubleSet {

	final val Empty :DoubleSet = new DoubleSet(MutableLongTrie.EmptyLongTrie, 0)

	class DoubleElementCounter extends ElementOf[Double, LongTrie] with ElementCounter[Double, LongTrie] {
		private[this] var invocations = 0

		override def elementOf(keyNode :LongTrie) :Double = { invocations+= 1; keyToDouble(keyNode.key) }

		override def count :Int = invocations
	}
/*
	import java.lang.Double.{doubleToLongBits, longBitsToDouble}

	private[this] final val DoubleToLong :Double => Long = doubleToLongBits
	private[this] final val LongToDouble :Long=>Double = longBitsToDouble

	final val Empty :StableSet[Double] = new LongView(DirectLongSet.Empty)
	
	def empty :StableSet[Double] = Empty
	
	def newBuilder :FitBuilder[Double, StableSet[Double]] =
		DirectLongSet.newBuilder.mapInput(DoubleToLong).mapResult(ints => new LongView(ints))

	def singleton(value :Double) :StableSet[Double] = new LongView(DirectLongSet.singleton(doubleToLongBits(value)))

	def mutable :MutableSet[Double] = new MutableLongView(DirectLongSet.mutable)


//	type Sorted = FitSet.Sorted[Double]
//
//	object Sorted {
//		final val Empty :SortedFitSet[Double] = new DirectLongSet.SortedViewAs[Double](LongToDouble, DoubleToLong)(DirectLongSet.Sorted.Empty)
//		def newBuilder :FitBuilder[Double, SortedFitSet[Double]] = DirectLongSet.Sorted.newBuilder.mapInput(DoubleToLong).mapResult(
//			ints => new DirectLongSet.SortedViewAs[Double](LongToDouble, DoubleToLong)(ints)
//		)
//		def Singleton(value :Double) :SortedFitSet[Double] = new DirectLongSet.SortedViewAs[Double](LongToDouble, DoubleToLong)(DirectLongSet.Sorted.Singleton(value))
//	}

	object Mutable {
		def empty :MutableSet[Double] = new MutableLongView(DirectLongSet.Mutable.empty)
		def newBuilder :FitBuilder[Double, MutableSet[Double]] = new MutableLongView(DirectLongSet.Mutable.empty)
		def Singleton(value :Double) :MutableSet[Double] = new MutableLongView(DirectLongSet.Mutable.singleton(doubleToLongBits(value)))
	}

	private trait DoubleAsLongSet[+S<:ValSet[Long] with SetSpecialization[Long, S], +Repr <: ValSet[Double] with SetSpecialization[Double, Repr]]
		extends IterableMapping[Long, S, Double, Repr] with ValSet[Double] with SetSpecialization[Double, Repr]
	{
		@inline override protected def forSource[@specialized(Fun1Res) O](f :Double=>O) = { x :Long => f(longBitsToDouble(x)) }

		override protected[this] def fromSource(col: S): Repr
		override protected[this] val source :S

		protected def my(x :Long) :Double = longBitsToDouble(x)
		@inline override final def from = LongToDouble

		override def empty :Repr = fromSource((source :SetSpecialization[Long, S]).empty)
		override def mutable :MutableSet[Double] = new MutableLongView(source.mutable)


		override def head :Double = from(source.head)
		override def last :Double = from(source.last)

		override def contains(elem: Double): Boolean = source.contains(DoubleToLong(elem))


		override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, Double) => O) =
			source.foldLeft(z)( (o :O, x :Long) => op(o, longBitsToDouble(x)))

		override def foldRight[@specialized(Fun2) O](z :O)(op :(Double, O)=>O) =
			source.foldRight(z)( (x :Long, o :O) => op(longBitsToDouble(x), o) )


		override def +(elem: Double): Repr = fromSource(source + doubleToLongBits(elem))

		override def -(elem: Double): Repr = fromSource(source - doubleToLongBits(elem))

		override def iterator: FitIterator[Double] = new MappedIterator(from)(source.iterator)

		override def newBuilder =
			source.newBuilder.mapInput(DoubleToLong).mapResult(fromSource)

	}



	private trait DoubleAsLongMutableSet[
			+S<:MutableSet[Long] with SetSpecialization[Long, S],
			+Repr<:MutableSet[Double] with MutableSetSpecialization[Double, Repr] with SetSpecialization[Double, Repr]
		] extends IterableMapping[Long, S, Double, Repr] with MutableSet[Double] with MutableSetSpecialization[Double, Repr] with DoubleAsLongSet[S, Repr]
	{
		override def stable :ValSet.Stable[Double] = new LongView(source.stable)

		override def add(elem: Double) = source.add(doubleToLongBits(elem))
		override def remove(elem: Double) = source.remove(doubleToLongBits(elem))

		override def +=(elem: Double): this.type = { source -= doubleToLongBits(elem); this }
		override def -=(elem: Double): this.type = { source -= doubleToLongBits(elem); this }

		override def +=(elem1: Double, elem2: Double, elems: Double*) :this.type = {
			source += (doubleToLongBits(elem1), doubleToLongBits(elem2), elems.map(DoubleToLong):_*); this
		}
		override def -=(elem1: Double, elem2: Double, elems: Double*) = {
			source.-=(doubleToLongBits(elem1), doubleToLongBits(elem2), elems.map(DoubleToLong):_*); this
		}

		override def sizeHint(expect: Int) = source.sizeHint(expect)

		override def count = size


	}



	private class LongView(protected val source :StableSet[Long])
		extends DoubleAsLongSet[StableSet[Long], StableSet[Double]] with StableSet[Double]
	{
		override protected[this] def fromSource(col: StableSet[Long]): StableSet[Double] = new LongView(col)
	}


	private class MutableLongView(protected val source :MutableSet[Long])
		extends DoubleAsLongMutableSet[MutableSet[Long], MutableSet[Double]]
	{
		override protected[this] def fromSource(col: MutableSet[Long]): MutableSet[Double] = new MutableLongView(col)
	}
*/

}
*/
