package net.turambar.collection.specnaz


import net.turambar.collection.specnaz.SpecCompanion.{SpecBuilder, SpecCanBuildFrom}
import net.turambar.collection.specnaz.Specialized.Fun2
import scala.collection.generic.CanBuildFrom
import scala.collection.{GenIterable, GenSeq, GenTraversableOnce, IndexedSeqOptimized, mutable}

import net.turambar.collection.specnaz.SpecIterator.{SpecSeqIterator, ReverseSpecSeqIterator}




/**
  * @author Marcin Mościcki
  */
trait SpecSeqLike[@specialized(Reified) +E, +Repr] extends IndexedSeqOptimized[E, Repr] {

	override def length: Int

	/** Access to siblings' protected `at` method. */
	@inline
	final private[specnaz] def get(idx :Int) :E = at(idx)

	/** Retrieve the value at the given index without explicitly checking its range. */
	protected[this] def at(idx :Int) :E

	/** Create a slice of this instance without checking whether the given indices are in range. */
	protected def subseq(from :Int, until :Int) :Repr

	/** Access to protected `subseq` method of sibling instances. */
	@inline final protected[this] def subseqOf(seq :SpecSeqLike[_, Repr], from :Int, until :Int) = seq.subseq(from, until)

	@inline final protected[this] def mySpecialization :Specialized[E] = Specialized[E]

	/** Underlying representation of element type and preferred version of `@specialized` methods to use.
	  * It will generally reflect the type used to store the values in runtime. If this instance is an erased,
	  * unspecialized variant, returned specialization will be equal to `Specialized[Any]`. Note however that this
	  * only reflects the most generic interface without any constraints. Implementations,
	  * especially dedicated subclasses for `AnyRef` subtypes, may internally store their contents as strict subclasses
	  * of `AnyRef`, making casts risky! Infer as little as possible - you have been warned.
	  */
	def specialization :Specialized[_] = mySpecialization //double step necessary to enforce a specialized call

	/** This method is a specialized variant of [[Iterable#newBuilder]] and the returned builder,
	  * unless explicitly stated to the contrary, should be optimised towards creating sequences
	  * with the same specialization and underlying structure.
	  */
	override protected[this] def newBuilder: SpecBuilder[E, Repr]




	@inline
	final override def size: Int = length

	@inline
	final override def hasDefiniteSize: Boolean = true


	@inline
	final override def apply(idx: Int): E =
		if (idx<0 || idx>=length)
			throw new IndexOutOfBoundsException(idx.toString)
		else at(idx)


	@inline
	final override def isEmpty: Boolean = length==0

	@inline
	final override def nonEmpty: Boolean = length>0



	final override def head: E =
		if (length>0) at(0)
		else throw new NoSuchElementException(s"empty.head")

	final override def headOption: Option[E] =
		if (length>0) Some(at(0))
		else None

	final override def tail: Repr =
		if (length>0) subseq(1, length)
		else throw new UnsupportedOperationException("empty.tail")



	final override def last: E =
		if (length>0) at(length-1)
		else throw new NoSuchElementException("empty.last")

	final override def lastOption: Option[E] =
		if (length>0) Some(at(length-1))
		else None

	final override def init: Repr =
		if (length>0) subseq(0, length-1)
		else throw new UnsupportedOperationException("empty.init")

	@inline
	final override def take(n: Int): Repr =
		if (n>=length) repr
		else if (n<=0) subseq(0, 0)
		else subseq(0, n)

	@inline
	final override def drop(n: Int): Repr =
		if (n>=length) subseq(length, length)
		else if (n<=0) repr
		else subseq(n, length)

	@inline
	final override def takeRight(n: Int): Repr =
		if (n>=length) repr
		else if (n<=0) subseq(length, length)
		else subseq(length-n, length)

	@inline
	final override def dropRight(n: Int): Repr =
		if (n>=length) subseq(0, 0)
		else if (n<=0) repr
		else subseq(0, length-n)

	@inline
	final override def splitAt(n: Int): (Repr, Repr) =
		if (n>=length) (repr, subseq(length, length))
		else if (n<=0) (subseq(0, 0), repr)
		else (subseq(0, n), subseq(n, length))

	@inline
	final override def slice(from: Int, until: Int): Repr = {
		val start = from max 0 min length
		subseq(start, until max start min length)
	}

	//todo: Function1 is not specialized with regard to Byte, Short, Char, Boolean :(
	@inline
	final override def takeWhile(p: (E) => Boolean): Repr = subseq(0, prefixLength(p))

	@inline
	final override def dropWhile(p: (E) => Boolean): Repr = subseq(prefixLength(p), length)



	override def span(p: (E) => Boolean): (Repr, Repr) = {
		val i = prefixLength(p)
		(subseq(0, i), subseq(i, length))
	}


	override def indexWhere(p: E => Boolean, from :Int): Int = {
		var i = from max 0; val l=length
		while(i<l && !p(at(i))) i+=1
		if (i<l) i else -1
	}

	@inline
	final override def indexWhere(p :E => Boolean) :Int = indexWhere(p, 0)

	override def lastIndexWhere(p: E => Boolean, from :Int): Int =
		if (from<0) -1
		else {
			var i = from min length
			do { i -=1 } while(i>=0 && !p(at(i)))
			i
		}

	@inline
	final override def lastIndexWhere(p: E => Boolean): Int = lastIndexWhere(p, length)

	final override def segmentLength(p: E => Boolean, from: Int): Int = {
		var i=from; val l=length
		while(i<l && p(at(i))) i += 1
		i
	}

	@inline
	final override def prefixLength(p: E => Boolean): Int = segmentLength(p, 0)


	override def find(p: E => Boolean): Option[E] = {
		val i = prefixLength(p)
		if (i<length) Some(at(i)) else None
	}



	override def indexOfSlice[U >: E](that: GenSeq[U], from: Int): Int = that match {
		case s :SpecSeqLike[_, _] if mySpecialization==s.specialization && s.length <= 8 => //todo: KMP
			val spec = s.asInstanceOf[SpecSeqLike[E, _]]
			var i = from max 0
			val limit = length - spec.length
			while (i<=limit && !startsWith(spec, i)) i += 1
			if (i>limit) -1 else i

		case _ =>
			AsIndexedSeq.indexOfSlice(that, from)
	}

	@inline
	final override def lastIndexOfSlice[U >: E](that: GenSeq[U]): Int = lastIndexOfSlice(that, 0)

	override def lastIndexOfSlice[U >: E](that: GenSeq[U], end: Int): Int = that match {
		case s :SpecSeqLike[_, _] if mySpecialization==s.specialization && s.length <= 8 => //todo: KMP
			val spec = s.asInstanceOf[SpecSeqLike[E, _]]
			var i = end min (length - spec.length)
			while(i<=0 && !startsWith(spec, i)) i -=1
			if (i<0) -1 else i
		case _ =>
			AsIndexedSeq.lastIndexOfSlice(that, end)
	}

	@inline
	final override def indexOf[U >: E](elem: U): Int = indexOf(elem, 0)

	override def indexOf[U >: E](elem: U, from: Int): Int = {
		var i = from max 0; val l = length
		while (i<l && at(i)!=elem) i+=1
		if (i<length) i else -1
	}


	final protected[this] def specIndexOf(elem :E, from :Int) :Int = {
		var i = from max 0; val l = length
		while (i<l && at(i)!=elem) i+=1
		if (i<length) i else -1
	}

	@inline
	final override def lastIndexOf[U >: E](elem: U): Int = lastIndexOf(elem, length)

	override def lastIndexOf[U >: E](elem: U, end: Int): Int = {
		var i = (length-1) min end
		while (i>=0 && at(i)!=elem) i-=1
		i
	}

	final protected[this] def specLastIndexOf(elem :E, end :Int) :Int = {
		var i = (length-1) min end
		while (i>=0 && at(i)!=elem) i-=1
		i
	}






	@inline
	final override def containsSlice[U](that: GenSeq[U]): Boolean = indexOfSlice(that, 0) >= 0

	@inline
	final override def contains[U >: E](elem: U): Boolean = indexOf(elem) >= 0





	@inline
	final override def startsWith[U](that :GenSeq[U]) :Boolean = startsWith(that, 0)


	override def startsWith[U](that: GenSeq[U], offset: Int): Boolean = that match {
		case s :SpecSeqLike[_, _] if mySpecialization==s.specialization =>
			startsWith(s.asInstanceOf[SpecSeqLike[E, _]], offset)
		case _ =>
			AsIndexedSeq.startsWith(that, offset)
	}


	@inline
	final protected[this] def startsWith(that :SpecSeqLike[E, _], offset :Int) :Boolean =
		!(offset>length || offset<0 || length-offset <= that.length) &&
			startsWithUnchecked(that, offset)

	final protected[this] def startsWithUnchecked(that :SpecSeqLike[E, _], offset :Int) :Boolean = {
		var i=0; val l=that.length
		while(i<l && at(offset + i)==that.get(i)) i += 1
		i==l
	}


	final override def endsWith[U](that: GenSeq[U]): Boolean = that match {
		case s :SpecSeqLike[_, _] if mySpecialization==s.specialization =>
			startsWith(s.asInstanceOf[SpecSeqLike[E, _]], length-s.length)
		case _ =>
			AsIndexedSeq.endsWith(that)
	}





	@inline
	final override def forall(p: E => Boolean): Boolean = prefixLength(p) == length

	@inline
	final override def exists(p: E => Boolean): Boolean = indexWhere(p) >= 0


	final override def count(p: E => Boolean): Int = {
		var n=0; var i=0
		while(i<length) {
			if (p(at(i))) n+=1
			i+=1
		}
		n
	}










	override def foldLeft[@specialized(Fun2) U](z: U)(op: (U, E) => U): U = {
		var i=0; var res = z
		while(i<length) { res = op(res, at(i)); i+=1 }
		res
	}


	override def foldRight[@specialized(Fun2) U](z: U)(op: (E, U) => U): U = {
		var i=length; var res = z
		while(i>0) { i -= 1; res = op(at(i), res) }
		res
	}


//todo: sorting
//	override def sortWith(lt: (E, E) => Boolean): Repr = super.sortWith(lt)
//
//	override def sortBy[U](f: (E) => U)(implicit ord: Ordering[U]): Repr = super.sortBy(f)
//
//	override def sorted[U >: E](implicit ord: Ordering[U]): Repr = super.sorted

	@inline
	override def /:[@specialized(Fun2) B](z: B)(op: (B, E) => B): B = foldLeft(z)(op)

	@inline
	override def :\[@specialized(Fun2) B](z: B)(op: (E, B) => B): B = foldRight(z)(op)

	@inline
	override def fold[@specialized(Fun2) U >: E](z: U)(op: (U, U) => U): U = foldLeft(z)(op)


	@inline
	final override def reduceLeft[U >: E](op: (U, E) => U): U =
		if (length==0)
			throw new UnsupportedOperationException("empty.reduceLeft")
		else reduceLeftUnchecked(op)

	@inline
	final override def reduceRight[U >: E](op: (E, U) => U): U =
		if (length==0)
			throw new UnsupportedOperationException("empty.reduceRight")
		else reduceRightUnchecked(op)

	@inline
	final override def reduceLeftOption[U >: E](op: (U, E) => U): Option[U] =
		if (length==0) None
		else Some(reduceLeftUnchecked(op))

	@inline
	final override def reduceRightOption[U >: E](op: (E, U) => U): Option[U] =
		if (length==0) None
		else Some(reduceRightUnchecked(op))

	@inline
	final override def reduce[A1 >: E](op: (A1, A1) => A1): A1 = reduceLeft(op)

	@inline
	final override def reduceOption[A1 >: E](op: (A1, A1) => A1): Option[A1] = reduceLeftOption(op)


	private[this] final def reduceLeftUnchecked[U >: E](op :(U, E) => U) :U = {
		var acc = at(0) :U; var i=1; val last = length
		while(i < last) {
			acc = op(acc, at(i))
			i+=1
		}
		acc
	}

	private[this] final def reduceRightUnchecked[U >: E](op :(E, U) => U) :U = {
		var i = length-1; var acc = at(i) :U
		while(i<0) {
			i -= 1
			acc = op(at(i), acc)
		}
		acc
	}


//
//	override def zip[A1 >: E, U, That](that: GenIterable[U])(implicit bf: CanBuildFrom[Repr, (A1, U), That]): That = super.zip(that)
//
//	override def zipWithIndex[A1 >: E, That](implicit bf: CanBuildFrom[Repr, (A1, Int), That]): That = super.zipWithIndex
//



	override def reverse: Repr = {
		val b = newBuilder
		b sizeHint length
		var i=length-1
		while(i>=0) { b += at(i); i-=1 }
		b.result()
	}




	@inline
	final override def foreach[@specialized(Unit) U](f: E => U): Unit = {
		var i=0; val l=length
		while(i<l) { f(at(i)); i+=1 }
	}



	@inline
	final protected[this] def mapBuilder[U, That](bf :CanBuildFrom[Repr, U, That]) :mutable.Builder[U, That] = {
		val b = bf(repr); b.sizeHint(length); b
	}
	@inline
	final protected[this] def mapBuilder[U, That](bf :SpecCanBuildFrom[Repr, U, That]) :SpecBuilder[U, That] = {
		val b = bf(repr); b.sizeHint(length); b
	}


	@inline
	final override def map[@specialized(Reified) U, That](f: (E) => U)(implicit bf: CanBuildFrom[Repr, U, That]): That = {
		bf match {
			case s :SpecCanBuildFrom[_, _, _] => specMap(f)(s.asInstanceOf[SpecCanBuildFrom[Repr, U, That]])
			case _ =>
				AsIndexedSeq.map(f)
		}
	}

	protected[this] final def specMap[@specialized(Reified) U, That](f :E => U)(implicit cbf :SpecCanBuildFrom[Repr, U, That]) :That = {
		val builder = mapBuilder(cbf)
		var i=0; val l=length
		while (i<l) { builder+=f(at(i)); i+=1 }
		builder.result()
	}


	@inline
	final override def flatMap[U, That](f: E => GenTraversableOnce[U])(implicit bf: CanBuildFrom[Repr, U, That]): That = {
		val builder = bf(repr)
		var i=0; val l=length
		while (i<l) { builder ++= f(at(i)).seq; i+=1 }
		builder.result()
	}




	@inline
	final override def reverseMap[@specialized(Reified) U, That](f: (E) => U)(implicit bf: CanBuildFrom[Repr, U, That]): That = bf match {
		case s :SpecCanBuildFrom[_, _, _] => specReverseMap(f)(s.asInstanceOf[SpecCanBuildFrom[Repr, U, That]])
		case _ => erasedReverseMap(f)
	}

	final protected[this] def erasedReverseMap[U, That](f :E=>U)(implicit bf :CanBuildFrom[Repr, U, That]) :That = {
		val builder = mapBuilder(bf)
		var i = length-1
		while(i>=0) { builder += f(at(i)); i-=1 }
		builder.result()
	}

	final protected[this] def specReverseMap[@specialized(Reified) U, That](f :E=>U)(implicit cbf :SpecCanBuildFrom[Repr, U, That]) :That = {
		val builder = mapBuilder(cbf)
		var i = length-1
		while(i>=0) { builder += f(at(i)); i-=1 }
		builder.result()
	}





	@inline
	final override def filter(p: (E) => Boolean): Repr = pick(p, value = true)

	@inline
	final override def filterNot(p: (E) => Boolean): Repr = pick(p, value = false)

	protected[this] def pick(p :(E) => Boolean, value :Boolean) :Repr = {
		val builder = newBuilder
		var i = 0; val l = length
		while (i < l) {
			val e = at(i)
			if (p(e)==value) builder += e
			i += 1
		}
		builder.result()
	}

	override def partition(p: (E) => Boolean): (Repr, Repr) = {
		val yes = newBuilder; val no = newBuilder
		var i = 0; val l = length
		while (i < l) {
			val e = at(i)
			if (p(e)) yes += e
			else no += e
			i += 1
		}
		(yes.result(), no.result())
	}




	override def +:[B >: E, That](elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
		val b = bf(repr); b sizeHint length+1
		b += elem; b ++= this
		toSeq
		b.result()
	}


	override def :+[B >: E, That](elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
		val b = bf(repr); b sizeHint length + 1
		b ++= this; b += elem
		b.result()
	}

	override def ++[B >: E, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
		val b = bf(repr)
		if (hasFastSize(that)) b sizeHint length + that.seq.size
		b ++= this; b ++= that.seq
		b.result()
	}

	override def ++:[B >: E, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
		val b = bf(repr)
		if (hasFastSize(that)) b.sizeHint(length + that.seq.size)
		b ++= that.seq; b ++= this
		b.result()
	}
	
	



	override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int): Unit =
		if (mySpecialization.runType.isAssignableFrom(xs.getClass.getComponentType)) { //E >: U
			val arr = xs.asInstanceOf[Array[E]]
			val end = len min length min (xs.length-start)
			var i = 0
			while (i<end) { arr(start+i) = at(i); i+= 1 }
		} else {
			var i = 0
			val end = len min length min (xs.length-start)
			while(i<end) { xs(start+i) = at(i); i+=1 }
		}


	/** This method can't be specialized - if you want to preserve element type and possibly share the contents,
	  * use [[SpecSeqLike#toSpecBuffer]] instead.
	  */
	override def toBuffer[U >: E]: SpecBuffer[U] = //todo: maybe a proxy class which starts with elements as E but reverts to AnyRef on ClassCastException?
		new ResizableArrayBuffer[U] ++= this

	def toSpecBuffer[U >: E](implicit targetType :Specialized[U]) :SpecBuffer[U] = {
		val tpe = mySpecialization.runType
		if (tpe.isPrimitive && tpe.isAssignableFrom(targetType.runType))
			(SpecBuffer.empty[E] ++= this).asInstanceOf[SpecBuffer[U]]
		else SpecBuffer.empty[U] ++= this
	}



	def immutable[U>:E](implicit cbf :SpecCanBuildFrom[_, E, ConstSeq[U]]) :ConstSeq[U] =
		(cbf() ++= this).result()

	def copy :Repr = (newBuilder ++= this).result()



	override def iterator: SpecIterator[E] = new ForwardIterator

	override def reverseIterator: SpecIterator[E] = new ReverseIterator




	@inline
	final override def lengthCompare(len: Int): Int = length-len

	override def sameElements[U >: E](that: GenIterable[U]): Boolean = that match {
		case s :SpecSeqLike[_, _] if mySpecialization==s.specialization =>
			(s.length==length) && startsWithUnchecked(s.asInstanceOf[SpecSeqLike[E, _]], 0)

		case w :mutable.WrappedArray[_] if w.array.getClass.getComponentType==mySpecialization.runType =>
			val arr = w.array.asInstanceOf[Array[E]]
			val l = length
			(arr.length==l) && {
				var i = 0
				while(i<l && at(i)==arr(i)) i+= 1
				i==l
			}
		case _ => AsIndexedSeq.sameElements(that :GenIterable[Any])
	}


	override def stringPrefix =
		typeStringPrefix+"["+mySpecialization.classTag+"]"


	protected[this] def typeStringPrefix :String = super.stringPrefix


	protected class ForwardIterator(protected[this] final var index :Int = 0) extends SpecSeqIterator[E]() {
		protected[this] final var end = SpecSeqLike.this.length
		
		@inline final override def head :E = at(index)

		@inline final override def next(): E = { index+=1; at(index-1) }

		@inline final override def copyToArray[B >: E](xs: Array[B], start: Int, len: Int): Unit =
			SpecSeqLike.this.seq.copyToArray(xs, start, len max size)
		
		override def toIndexedSeq = toSeq.toIndexedSeq
		
		override def toSeq :SpecSeq[E] = SpecSeqLike.this.subseq(index, end).asInstanceOf[SpecSeq[E]]

		override def toString = s"iter#$index>>($size)"
	}

	protected class ReverseIterator(protected[this] final var index :Int=length-1) extends ReverseSpecSeqIterator[E] {
		protected[this] final var end = 0
		
		@inline final override def head: E = at(index)

		@inline final override def next(): E = { val res = at(index); index -= 1; res }
		
		override def toIndexedSeq = toSeq.toIndexedSeq

		override def toSeq :SpecSeq[E] = new ReversedSeq[E](SpecSeqLike.this.subseq(end, index+1).asInstanceOf[SpecSeq[E]])

		override def toString = s"iter<<#$index"
	}

	protected[this] object AsIndexedSeq extends IndexedSeqOptimized[E, Repr]  {

		override def repr: Repr = SpecSeqLike.this.repr

		override def seq: IndexedSeq[E] = SpecSeqLike.this.seq

		override def length: Int = SpecSeqLike.this.length

		override def apply(idx: Int): E = at(idx)

		override protected[this] def newBuilder: mutable.Builder[E, Repr] = SpecSeqLike.this.newBuilder
	}


}



