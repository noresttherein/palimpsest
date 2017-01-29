package net.turambar.collection.specnaz


import java.util

import net.turambar.collection.specnaz.SpecCompanion.{ArraySection$, SpecBuilder, SpecCanBuildFrom}

import scala.Iterator
import scala.annotation.{tailrec, unspecialized}
import scala.collection.generic.{CanBuildFrom, GenTraversableFactory, GenericCompanion}
import scala.collection._
import scala.reflect.{ClassTag, classTag}




/**
  * @author Marcin Mościcki
  */
trait SpecCompanion[+S[@specialized(Reified) X] <: SpecSeq[X] with GenericSpecializedTraversable[X, S]]
		extends GenericCompanion[S]
{ factory =>

	/** An empty, unspecialized instance of S[_] and a valid instance of `S[T]` for all immutable (variant) collections `S`. */
	val Empty :S[Nothing] = empty[Nothing]
//	def Empty[E :Specialized] =

	/** An empty collection `S[E]` instance specialized in regard to `E`. */
	override def empty[@specialized(Reified) E]: S[E] =
		newBuilder[E].result()


	/** Create a new instance containing the given elements.
	  * '''Do not use it with empty argument list''' - not only [[net.turambar.collection.specnaz.SpecCompanion#empty empty[E]] will be more efficient, but due to
	  * a bug in scala up to 2.11.8 such call won't be specialized if `apply` method is overloaded.
	  *
	  * @return a specialized subclass of `S[E]`
	  */
	override def apply[@specialized(Reified) E](elems: E*): S[E] = (newBuilder[E] ++= elems).result()


	def apply[E :Specialized](elems :TraversableOnce[E]) :S[E] = (specBuilder[E] ++= elems).result()


	override def newBuilder[@specialized(Reified) E]: SpecBuilder[E, S[E]] //= specBuilder[E]

	def specBuilder[E :Specialized] :SpecBuilder[E, S[E]]



//	def CanBuildSelf[@specialized(Reified) E] :SpecCanBuildFrom[_, E, S[E]]




}







trait SpecSeqFactory[S[@specialized(Reified) X] <: GenericSpecializedTraversable[X, S] with SpecSeq[X]]
	extends GenTraversableFactory[S] with SpecCompanion[S]
{ factory =>

//	def apply[E :Specialized](elements :TraversableOnce[E]) :S[E]

	@inline
	final protected def trim(arrayLength :Int, offset :Int, length :Int) :(Int, Int) = {
		val start = offset max 0 min arrayLength
		val len = length max 0 min (arrayLength-start)
		(start, len)
	}


	override def fill[@specialized(Reified) E](n: Int)(elem: => E): S[E] = {
		var i=0
		val builder = newBuilder[E]
		while (i<n) { builder += elem; i +=1 }
		builder.result()
	}


	override def tabulate[@specialized(Reified) E](n: Int)(f: (Int) => E): S[E] = {
		var i=0
		val builder = newBuilder[E]
		while (i<n) { builder += f(i); i += 1 }
		builder.result()
	}


	override def iterate[@specialized(Reified) E](start: E, len: Int)(f: (E) => E): S[E] =
		if (len<=0) empty[E]
		else {
			var i=1
			val builder = newBuilder[E]
			builder sizeHint len
			builder += start
			var last = start
			while(i<len) {
				i += 1
				last = { val next = f(last); builder += next; next }
			}
			builder.result()
		}


	class ErasedCanBuildFrom[E] extends GenericCanBuildFrom[E] with SpecCanBuildFrom[S[_], E, S[E]] {

		final override def apply(from: S[_]): SpecBuilder[E, S[E]] = from.genericBuilder[E]

		final override def apply(): SpecBuilder[E, S[E]] = newBuilder[E]


		override private[specnaz] def companion: Any = factory

		override def canEqual(that :Any) = that.isInstanceOf[ErasedCanBuildFrom[_]]

		override def equals(that :Any) = that match {
			case cbf :ErasedCanBuildFrom[_] => (cbf eq this) ||
			cbf.canEqual(this) && cbf.companion == companion
		}

		override def hashCode = companion.hashCode

		override def toString = companion+".ReusableCBF"
	}

	override val ReusableCBF = new ErasedCanBuildFrom[Nothing]




	class CanBuildSpecialized[@specialized(Reified) E] extends SpecCanBuildFrom[S[_], E, S[E]] {
		override def apply(from: S[_]): SpecBuilder[E, S[E]] = from.genericBuilder[E]

		override def apply(): SpecBuilder[E, S[E]] = newBuilder[E]


		override def specialization: Specialized[E] = Specialized[E]


		override private[specnaz] def companion: Any = factory

		override def canEqual(that :Any) = that.isInstanceOf[CanBuildSpecialized[_]]

		override def toString = s"$factory.CBF[$specialization]"
	}




	type CBF[E] = SpecCanBuildFrom[S[_], E, S[E]]

	protected[this] val CBF = new Specialize[CBF] {
		override def specialized[@specialized E: Specialized]: CBF[E] = canBuildSpecialized[E] //new CanBuildSpecialized[E]
	}
	protected def canBuildSpecialized[@specialized E :Specialized] = new CanBuildSpecialized[E]


	implicit def CanBuildSpecialized[E :Specialized] :CBF[E] = CBF() //SpecCanBuildFrom[S[_], E, S[E]] = CBF()



	implicit val CanBuildBytes = CanBuildSpecialized[Byte]
	implicit val CanBuildShorts = CanBuildSpecialized[Short]
	implicit val CanBuildInts = CanBuildSpecialized[Int]
	implicit val CanBuildLongs = CanBuildSpecialized[Long]
	implicit val CanBuildFloats = CanBuildSpecialized[Float]
	implicit val CanBuildDoubles = CanBuildSpecialized[Double]
	implicit val CanBuildChars = CanBuildSpecialized[Char]
	implicit val CanBuildBooleans = CanBuildSpecialized[Boolean]


	override val toString = {
		val className = getClass.getName
		val start = className.lastIndexOf('.')
		val end = className.indexOf('$', start+1)
		if (end<0) className.substring(start+1)
		else className.substring(start+1, end)
	}
}


abstract class CopycatSeqFactory[S[@specialized(Reified) X] <: GenericSpecializedTraversable[X, S] with SpecSeq[X]]
	extends SpecSeqFactory[S]
{
	protected[this] type RealType[@specialized(Reified) X] <: S[X] with GenericSpecializedTraversable[X, RealType]
	protected[this] def impl :SpecSeqFactory[RealType]

	@inline final override def empty[@specialized(Reified) E]: S[E] = impl.empty[E]

	@inline final override def newBuilder[@specialized(Reified) E]: SpecBuilder[E, S[E]] = impl.newBuilder[E]

	@inline final override def specBuilder[E: Specialized]: SpecBuilder[E, S[E]] = impl.specBuilder[E]


}



/**
  * @author Marcin Moscicki
  */
object SpecCompanion {


	trait SpecBuilder[@specialized(Reified) -E, +To] extends mutable.Builder[E, To] /*with Accumulator[E]*/ {

		override def +=(elem1: E, elem2: E, elems: E*): this.type =
			this += elem1 += elem2 ++= elems

		override def +=(elem: E): this.type

		override def result(): To

		override def sizeHint(expect: Int): Unit = ()

	}


	trait SpecCanBuildFrom[-From, @specialized(Reified) -E, +To] extends CanBuildFrom[From, E, To] {
		override def apply(from: From): SpecBuilder[E, To]

		override def apply(): SpecBuilder[E, To]

		def copy(from :SpecSeq[E]) :To = (apply() ++= from).result()
		
		/** If `true`, `this(from)` takes its builder as is customary from [[SpecSeq#genericBuilder]]. */
		def honorsBuilderFrom = true
		
		def elementType :Class[_] = mySpecialization.runType
		
		//intermediate method necessary, as `def specialization` is not specialized
		private[this] def mySpecialization = Specialized[E]
		def specialization :Specialized[_] = mySpecialization

		def canEqual(that :Any) = that.isInstanceOf[SpecCanBuildFrom[_, _, _]]

		override def equals(that :Any) = that match {
			case cbf :SpecCanBuildFrom[_, _, _] =>
				(cbf eq this) || cbf.canEqual(this) && canEqual(cbf) && companion==cbf.companion && mySpecialization == cbf.specialization
			case _ => false
		}

		override def hashCode = companion.hashCode * 31 + mySpecialization.hashCode

		override def toString = s"SpecCBF[$mySpecialization]"


		private[specnaz] def companion :Any = this
	}




	private[specnaz] class ArraySection[E] private[specnaz](val array :Array[E], val start :Int, val length :Int) {
		def this(array :Array[E]) = this(array, 0, array.length)

//		def this(array :Array[E]) = this(0, array.length, array)
//
//		def this(array :Array[E], start :Int, length :Int) = this(
//			if (start>=0) start min array.length else throw new IndexOutOfBoundsException(s"${arrayString(array)}($start, $length)"),//IndexOutOfBoundsException(s"$outer([${array.length}], $start, $length)"),
//			length min (array.length-start) max 0,
//			array
//		)
//
//		def this(start :Int, array :Array[E], end :Int) = this(array, start, (end max 0) - start)


		def end = start+length

		def copy = new ArraySection[E](arrayCopy(array, start, length), 0, length)

		implicit def specialization = Specialized.ofClass(array.getClass.getComponentType).asInstanceOf[Specialized[E]]



//		def apply() = specialize(this)(specialization)

		override def toString = s"${array.getClass.getName}[${array.length}]($start..$end)"
	}

	private[specnaz] object ArraySection {
		def share[E](array :Array[E]) :ArraySection[E] = new ArraySection(array, 0, array.length)
		
		def copy[E](array :Array[E]) :ArraySection[E] = new ArraySection(arrayCopy(array), 0, array.length)
		
		
		
		def share[E](array :Array[E], start :Int) :ArraySection[E] =
			if (start<0)
				throw new IndexOutOfBoundsException(s"ArrayContents.share(${arrayString(array)}, $start")
			else if (start>=array.length)
				new ArraySection(array, array.length, 0)
			else
				new ArraySection(array, start, array.length-start)
		
		def copy[E](array :Array[E], start :Int) :ArraySection[E] =
			if (start<0)
				throw new IndexOutOfBoundsException(s"ArrayContents.copy(${arrayString(array)}, $start")
			else if (start>=array.length)
				new ArraySection(emptyArray(array), 0, 0)
			else
				new ArraySection(arrayCopy(array, start, array.length-start))
		


		def share[E](array :Array[E], start :Int, length :Int) :ArraySection[E] =
			if (start<0)
				throw new IndexOutOfBoundsException(s"ArrayContents.share(${arrayString(array)}, $start, $length")
			else if (start >= array.length)
			     new ArraySection(array, array.length, 0)
			else {
				val available = array.length-start
				new ArraySection(array, start,
					if (length>=available) available
					else if (length<0) 0
					else length
				)
			}
		
		
		def copy[E](array :Array[E], start :Int, length :Int) :ArraySection[E] =
			if (start<0)
				throw new IndexOutOfBoundsException(s"ArrayContents.copy(${arrayString(array)}, $start, $length")
			else if (start >= array.length || length <=0)
			     new ArraySection(emptyArray(array), 0, 0)
			else {
				val available = array.length-start
				val total = if (length>available) available else length
				new ArraySection(arrayCopy(array, start, total), 0, total)
			}

		
		def share[E](from :Int, array :Array[E], until :Int) :ArraySection[E] =
			if (from<0)
				throw new IndexOutOfBoundsException(s"ArrayContents.share($from, ${arrayString(array)}, $until")
			else if (from >= array.length || until<=from)
				new ArraySection(array, array.length, 0)
			else if (until>=array.length)
				new ArraySection(array, from, array.length-from)
			else
				new ArraySection(array, from, until-from)
		
		def copy[E](from :Int, array :Array[E], until :Int) :ArraySection[E] =
			if (from<0)
				throw new IndexOutOfBoundsException(s"ArrayContents.copy($from, ${arrayString(array)}, $until")
			else if (from >= array.length || until<=from)
			     new ArraySection(emptyArray(array), 0, 0)
			else if (until>=array.length)
			     new ArraySection(arrayCopy(array, from, array.length-from))
			else
				new ArraySection(arrayCopy(array, from, until-from))
		
		
		
	}


}
