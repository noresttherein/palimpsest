package net.turambar.palimpsest.specialty.seqs

import scala.annotation.unspecialized
import scala.collection.generic.CanBuildFrom
import scala.collection.{GenIterable, GenSeq, GenTraversableOnce, IndexedSeqLike, IndexedSeqOptimized, SeqLike, mutable}

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.FitIterator.{IndexedIterator, ReverseIndexedIterator}
import net.turambar.palimpsest.specialty.Specialized.Fun1Vals
import net.turambar.palimpsest.specialty.{Elements, FitBuilder, FitIterableLike, FitIterator, Specialized, ofKnownSize}

/** A specialized version of standard scala `SeqLike`, overriding all methods which can
  * be specialized on the element type of the `Repr` collection.
  * Provides a couple additional methods.
 *
  * @tparam E specialized element type
  * @tparam Repr a specialized collection, assumed to be a subclass of [[FitSeq]]
  * @see [[FitSeq]]
  * @author Marcin Mościcki
  */
trait FitSeqLike[@specialized(Elements) +E, +Repr] extends IndexedSeqLike[E, Repr] with FitIterableLike[E, Repr] {

	/** Target of `apply` for internal use, assuming the index is valid (faster). */
	protected[this] def at(idx :Int) :E
	
	/** Quick access to the element at the given index for sibling collections. Directly calls [[at]] */
	@inline
	final private[seqs] def get(idx :Int) :E = at(idx)
	
	/** Create a slice of this instance assuming the indices are already validated. */
	protected def section(from :Int, until :Int) :Repr

	/** Access to protected `section` method of sibling collections. */
	@inline final protected[this] def sectionOf(seq :FitSeqLike[_, Repr], from :Int, until :Int) :Repr = seq.section(from, until)
	
	/** Empty collection returned when requested for slices of zero length; implemented as `section(0, 0)`. */
	@unspecialized
	protected[this] def emptyCollection :Repr = section(0, 0)
	
	
	
	/************** General methods ***************/
	
	@inline
	final override def size: Int = length

	@inline
	final override def hasDefiniteSize: Boolean = true


	override def apply(idx: Int): E =
		if (idx<0 || idx>=length)
			throw new IndexOutOfBoundsException(idx.toString)
		else at(idx)


	override def isEmpty: Boolean = length==0

	override def nonEmpty: Boolean = length!=0
	
	
	
	override def head: E =
		if (length>0) at(0)
		else throw new NoSuchElementException(s"empty.head")
	
	override def headOption: Option[E] =
		if (length>0) Some(at(0))
		else None
	
	
	override def last: E =
		if (length>0) at(length-1)
		else throw new NoSuchElementException("empty.last")
	
	override def lastOption: Option[E] =
		if (length>0) Some(at(length-1))
		else None
	
	
	/************** Slicing methods ***************/
	

	override def tail: Repr =
		if (length>0) section(1, length)
		else throw new UnsupportedOperationException("empty.tail")

	

	final override def init: Repr =
		if (length>0) section(0, length-1)
		else throw new UnsupportedOperationException("empty.init")

	
	
	override def take(n: Int): Repr =
		if (n>=length) repr
		else if (n<=0) emptyCollection //section(0, 0)
		else section(0, n)

	override def drop(n: Int): Repr =
		if (n>=length) emptyCollection //section(length, length)
		else if (n<=0) repr
		else section(n, length)

	override def takeRight(n: Int): Repr =
		if (n>=length) repr
		else if (n<=0) emptyCollection //section(length, length)
		else section(length-n, length)

	override def dropRight(n: Int): Repr =
		if (n>=length) emptyCollection //section(0, 0)
		else if (n<=0) repr
		else section(0, length-n)

	override def splitAt(n: Int): (Repr, Repr) =
		if (n>=length) repr -> emptyCollection //(repr, section(length, length))
		else if (n<=0) emptyCollection -> repr //(section(0, 0), repr)
		else (section(0, n), section(n, length))

	override def slice(from: Int, until: Int): Repr = {
		val start = from max 0 min length
		section(start, until max start min length)
	}

	
	
	@inline @unspecialized
	final override def takeWhile(p: (E) => Boolean): Repr = section(0, prefixLength(p))
	
	@unspecialized
	override def dropWhile(p: (E) => Boolean): Repr = section(prefixLength(p), length)
	
	
	@unspecialized
	override def span(p: (E) => Boolean): (Repr, Repr) = {
		val suffix = dropWhile(p)
		(section(0, length - toCollection(suffix).length), suffix)
	}
	
	
	
	
	
	/************** Searching for an element methods  ***************/
	
	@inline @unspecialized
	final override def prefixLength(p: E => Boolean): Int = segmentLength(p, 0)
	
	
	override def segmentLength(p: E => Boolean, from: Int): Int = {
		var i=math.max(from, 0); val l=length
		while(i<l && p(at(i))) i += 1
		i
	}

	
	override def indexWhere(p: E => Boolean, from :Int): Int = {
		var i = math.max(from, 0); val l = length
		while(i<l && !p(at(i))) i+=1
		i
	}
	
	@inline @unspecialized
	final override def indexWhere(p :E => Boolean) :Int = indexWhere(p, 0)
	
	
	@unspecialized
	override def find(p: E => Boolean): Option[E] = {
		val i = indexWhere(p, 0)
		if (i<length) Some(at(i)) else None
	}
	

	override def lastIndexWhere(p: E => Boolean, from :Int): Int =
		if (from<0) -1
		else {
			var i = from min length
			do { i -=1 } while(i>=0 && !p(at(i)))
			i
		}

	@inline @unspecialized
	final override def lastIndexWhere(p: E => Boolean): Int = lastIndexWhere(p, length)
	


	@inline
	final override def indexOf[U >: E](elem: U): Int = indexOf(elem, 0)
	
	
	
	override def indexOf[U >: E](elem: U, from: Int): Int =
		if (mySpecialization.boxType.isAssignableFrom(elem.getClass))
			fitIndexOf(elem.asInstanceOf[E], from)
		else indexWhere(elem == _, from)
	
	protected[this] def fitIndexOf(elem :E, from :Int) :Int = {
		var i = math.max(from, 0); val l = length
		while (i<l && at(i)!=elem) i+=1
		if (i<l
		) i else -1
	}



	@inline
	final override def lastIndexOf[U >: E](elem: U): Int = lastIndexOf(elem, length)
	
	override def lastIndexOf[U >: E](elem: U, end: Int): Int =
		if (mySpecialization.boxType.isAssignableFrom(elem.getClass))
			fitLastIndexOf(elem.asInstanceOf[E], end)
		else lastIndexWhere(elem == _, end)
	
	protected[this] def fitLastIndexOf(elem :E, end :Int) :Int = {
		var i = (length-1) min end
		while (i>=0 && at(i)!=elem) i-=1
		i
	}
	
	
	
	
	/************** Searching & matching for sequences ***************/
	
	
	
	override def indexOfSlice[U >: E](that: GenSeq[U], from: Int): Int = that match {
		case s :FitSeqLike[_, _] if mySpecialization==s.specialization && s.length <= 8 => //todo: KMP
			val spec = s.asInstanceOf[FitSeqLike[E, _]]
			var i = from max 0
			val limit = length - spec.length
			while (i<=limit && !startsWith(spec, i)) i += 1
			if (i>limit) -1 else i
		
		case _ =>
			defaultImpl.indexOfSlice(that, from)
	}
	
	@inline
	final override def lastIndexOfSlice[U >: E](that: GenSeq[U]): Int = lastIndexOfSlice(that, 0)
	
	override def lastIndexOfSlice[U >: E](that: GenSeq[U], end: Int): Int = that match {
		case s :FitSeqLike[_, _] if mySpecialization==s.specialization && s.length <= 8 => //todo: KMP
			val spec = s.asInstanceOf[FitSeqLike[E, _]]
			var i = end min (length - spec.length)
			while(i<=0 && !startsWith(spec, i)) i -=1
			if (i<0) -1 else i
		case _ =>
			defaultImpl.lastIndexOfSlice(that, end)
	}



	@inline
	final override def containsSlice[U](that: GenSeq[U]): Boolean = indexOfSlice(that, 0) >= 0

	@inline
	final override def contains[U >: E](elem: U): Boolean = indexOf(elem) >= 0





	@inline
	final override def startsWith[U](that :GenSeq[U]) :Boolean = startsWith(that, 0)


	override def startsWith[U](that: GenSeq[U], offset: Int): Boolean = that match {
		case s :FitSeqLike[_, _] if mySpecialization==s.specialization =>
			startsWith(s.asInstanceOf[FitSeqLike[E, _]], offset)
		case _ =>
			defaultImpl.startsWith(that, offset)
	}


	@inline @unspecialized
	final protected[this] def startsWith(that :FitSeqLike[E, _], offset :Int) :Boolean =
		!(offset>length || offset<0 || length-offset <= that.length) &&
			startsWithUnchecked(that, offset)
	
	@unspecialized
	protected[this] def startsWithUnchecked(that: FitSeqLike[E, _], offset: Int): Boolean = {
		iterator.drop(offset).take(that.length) sameElements that.iterator
	}
	
	
	
	final override def endsWith[U](that: GenSeq[U]): Boolean = that match {
		case s :FitSeqLike[_, _] if mySpecialization==s.specialization =>
			startsWith(s.asInstanceOf[FitSeqLike[E, _]], length-s.length)
		case _ =>
			defaultImpl.endsWith(that)
	}
	
	
	
	
	/************** Predicate testing and traversing methods ***************/
	
//	override def foreach[@specialized(Unit) U](f: E => U): Unit = {
//		var i=0; val l=length
//		while(i<l) { f(at(i)); i+=1 }
//	}
	
	override def reverseForeach(f: (E) => Unit): Unit = {
		var i = length-1
		while(i>=0) { f(at(i)); i-=1 }
	}
	
	@inline @unspecialized
	final override def forall(p: E => Boolean): Boolean = prefixLength(p) == length

	@inline @unspecialized
	final override def exists(p: E => Boolean): Boolean = indexWhere(p, 0) >= 0


//todo: sorting
//	override def sortWith(lt: (E, E) => Boolean): Repr = super.sortWith(lt)
//
//	override def sortBy[U](f: (E) => U)(implicit ord: Ordering[U]): Repr = super.sortBy(f)
//
//	override def sorted[U >: E](implicit ord: Ordering[U]): Repr = super.sorted


	/************ filtering and other self-typed collections ***************/
	

	
	override def reverse: Repr = {
		val b = newBuilder
		reverseForeach(b.addOne)
		b.result()
	}
		
	
	
	
	def copy :Repr = (newBuilder ++= this).result()
	
	
	
	/***************** mapping *******************************************/
	
	
/*
	override def map[@specialized(Fun1Vals) O, That](f: (E) => O)(implicit bf: CanBuildFrom[Repr, O, That]): That =
		bf(repr) match {
			case fit :FitBuilder[O, That] =>
				fit.sizeHint(length)
				var i = 0; val e = length
//				while(i<e) {
//					val x = at(i) :E
//					val o = f(x)
//					fit += o
//					i += 1
//				}
				val it = iterator
				while(it.hasNext) {
					val el = it.next()
					val o = f(el)
					fit += o
				}
				fit.result()
			case unfit =>
				var i = 0; val e = length
				while(i<e) { unfit += f(at(i)); i+=1 }
				unfit.result()
		}
*/
	


	override def reverseMap[@specialized(Fun1Vals) U, That](f: (E) => U)(implicit bf: CanBuildFrom[Repr, U, That]): That = {
		bf(repr) match {
			case b :FitBuilder[U, That] =>
				b.sizeHint(length)
				reverseForeach { e => b += f(e) }
				b.result()
			case b =>
				b.sizeHint(length)
				reverseForeach { e => b += f(e) }
				b.result()
		}
	}

	
	
	
	
	
	override def +:[U >: E, That](elem: U)(implicit bf: CanBuildFrom[Repr, U, That]): That = {
		def builderFrom = bf(repr) match {
			case b :FitBuilder[U, That] => val fit = b.typeHint[U]; fit.sizeHint(length+1); fit
			case b => b.sizeHint(length+1); b
		}
		val b = builderFrom
		b += elem; b ++= this
		b.result()
	}


	override def :+[U >: E, That](elem: U)(implicit bf: CanBuildFrom[Repr, U, That]): That = {
		def builderFrom = bf(repr) match {
			case b :FitBuilder[U, That] => val fit = b.typeHint[U]; fit.sizeHint(length+1); fit
			case b => b.sizeHint(length+1); b
		}
		val b = builderFrom
		b ++= this; b += elem
		b.result()
	}

	override def ++[U >: E, That](that: GenTraversableOnce[U])(implicit bf: CanBuildFrom[Repr, U, That]): That = {
		def builderFrom = bf(repr) match {
			case b :FitBuilder[U, That] => b.typeHint[U]
			case b => b
		}
		val b = builderFrom
		if (ofKnownSize(that))
			b sizeHint length + that.seq.size
		b ++= this; b ++= that.seq
		b.result()
	}

	override def ++:[U >: E, That](that: TraversableOnce[U])(implicit bf: CanBuildFrom[Repr, U, That]): That = {
		def builderFrom = bf(repr) match {
			case b :FitBuilder[U, That] => b.typeHint[U]
			case b => b
		}
		val b = builderFrom
		if (ofKnownSize(that))
			b.sizeHint(length + that.seq.size)
		b ++= that.seq; b ++= this
		b.result()
	}
	
	



	override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int): Unit =
		if (mySpecialization.runType.isAssignableFrom(xs.getClass.getComponentType)) { //E >: U
			val realLength = len min length min (xs.length - start)
			specializedCopy(xs.asInstanceOf[Array[E]], start, len)
		} else super.copyToArray(xs, start, len)
	
	protected[this] def specializedCopy(target :Array[E], start :Int, len :Int) :Unit = {
		var i = 0
		while (i < len) {target(start + i) = at(i); i += 1 }
	}
	
	
	/** Creates a buffer with the contents of this collection.
	  * If this collection is mutable, returned buffer may share its contents,
	  * with mutually visible modifications.
	  * Note that while returned buffer is of type [[FitBuffer]], this method can't be specialized,
	  * and as a result, returned instance will likely be an erased, not a specialized version.
	  * If you want to preserve element type and possibly share the contents,
	  * use [[FitSeqLike#toFitBuffer]] instead.
	  */
	override def toBuffer[U >: E]: FitBuffer[U] = //todo: maybe a proxy class which starts with elements as E but reverts to AnyRef on ClassCastException?
		new GrowingArrayBuffer[U] ++= this
	
	/** 'Specialized' variant of standard `toBuffer` which will honor implicit type information
	  * and preserve storage of this instance if `targetType` is compatible with this instances specialization/element type.
	  */
	def toFitBuffer[U >: E](implicit targetType :Specialized[U]) :FitBuffer[U] = {
		val tpe = mySpecialization.runType
		if (tpe.isPrimitive && tpe.isAssignableFrom(targetType.runType))
			(FitBuffer.empty[E] ++= this).asInstanceOf[FitBuffer[U]]
		else FitBuffer.empty[U] ++= this
	}
	
	override def inverse :FitSeq[E] = new ReverseSeq[E](toFitSeq)
//	override def toSeq: FitSeq[E] = this.asInstanceOf[FitSeq[E]]
	
	def immutable[U>:E](implicit cbf :CanFitFrom[_, E, ConstSeq[U]]) :ConstSeq[U] =
		(cbf() ++= this).result()
	
	override def toSeq :Seq[E] = this.asInstanceOf[FitSeq[E]]
	
	override def toFitSeq :FitSeq[E] = this.asInstanceOf[FitSeq[E]]
	
	override def toIndexedSeq :ConstSeq[E] =
		(ConstSeq.newBuilder[E] ++= this).result()
	





	override def iterator: FitIterator[E] = new ForwardIterator

	override def reverseIterator: FitIterator[E] = new ReverseIterator
	
	
	
	
	/** This method is a specialized variant of [[Iterable#newBuilder]] and the returned builder,
	  * unless explicitly stated to the contrary, should be optimised towards creating sequences
	  * with the same specialization and underlying structure.
	  */
	override protected[this] def newBuilder: FitBuilder[E, Repr]
	
	@inline final protected[this] def newBuilder(sizeHint :Int) :FitBuilder[E, Repr] = {
		val b = newBuilder
		b.sizeHint(sizeHint)
		b
	}

	
	
	
	@inline
	final override def lengthCompare(len: Int): Int = length-len

	override def sameElements[U >: E](that: GenIterable[U]): Boolean = that match {
		case s :FitSeqLike[_, _] if mySpecialization==s.specialization =>
			(s.length==length) && startsWithUnchecked(s.asInstanceOf[FitSeqLike[E, _]], 0)

		case w :mutable.WrappedArray[_] if w.array.getClass.getComponentType==mySpecialization.runType =>
			val arr = w.array.asInstanceOf[Array[E]]
			val l = length
			(arr.length==l) && {
				var i = 0
				while(i<l && at(i)==arr(i)) i+= 1
				i==l
			}
		case _ => defaultImpl.sameElements(that :GenIterable[Any])
	}




	protected class ForwardIterator extends IndexedIterator[E](0, length) with FitIterator[E]
	{
		override def head :E = at(index)
		override def next() = { val res :E = at(index); index+=1; res }
		
		override def foreach[@specialized(Unit) U](f: (E) => U): Unit = FitSeqLike.this.foreach(f)
		
		override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int): Unit =
			FitSeqLike.this.seq.copyToArray(xs, start, len max size)
		
		override def toIndexedSeq = toSeq.toIndexedSeq
		
		override def toSeq :FitSeq[E] = FitSeqLike.this.section(index, end).asInstanceOf[FitSeq[E]]

	}

	protected class ReverseIterator extends ReverseIndexedIterator[E](length-1, 0) with FitIterator[E] {
		override def head: E = at(index)
		override def next() :E = { val res :E = at(index); index-=1; res }
		
		override def toIndexedSeq = toSeq.toIndexedSeq

		override def toSeq :FitSeq[E] = new ReverseSeq[E](FitSeqLike.this.section(end, index+1).asInstanceOf[FitSeq[E]])
	}
	
	protected[this] def defaultImpl :SeqLike[E, Repr] = new IndexedSeqOptimized[E, Repr]  {

		override def repr: Repr = FitSeqLike.this.repr

		override def seq: IndexedSeq[E] = FitSeqLike.this.thisCollection

		override def length: Int = FitSeqLike.this.length

		override def apply(idx: Int): E = at(idx)
		
		override protected[this] def newBuilder: mutable.Builder[E, Repr] = FitSeqLike.this.newBuilder
	}


}
