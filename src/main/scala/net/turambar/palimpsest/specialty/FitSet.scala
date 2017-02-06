package net.turambar.palimpsest.specialty

import scala.collection.generic.{CanBuildFrom, GenericCompanion, GenericSetTemplate}
import scala.collection.{GenSet, GenTraversableOnce, SetLike, mutable}

import net.turambar.palimpsest.specialty.FitSet.BooleanSet.{BooleanSetBuilder, BooleanSetIterator}
import net.turambar.palimpsest.specialty.ByteSet.{ByteSetBuilder, ByteSetIterator}
import net.turambar.palimpsest.specialty.FitIterator.BaseIterator

/**
  * @author Marcin Mościcki
  */
trait FitSet[@specialized(Elements) E]
	extends Set[E] with SetLike[E, FitSet[E]] with GenSet[E] with GenericSetTemplate[E, FitSet]
{
	override def companion: GenericCompanion[FitSet] = FitSet
}



object FitSet extends GenericCompanion[FitSet] {
	import java.{lang=>j}
	
	
	override def empty[@specialized(Elements) E] :FitSet[E] = EmptySet()
	
	override def apply[@specialized(Elements) E](elems :E*) :FitSet[E] =
		(newBuilder[E] ++= elems).result()
	
	def newBuilder[@specialized(Elements) E] :FitBuilder[E, FitSet[E]] = NewBuilder()
	
//	override def fitBuilder[E: Specialized]: FitBuilder[E, FitSet[E]] = NewBuilder()

	
	private type SetBuilder[E] = FitBuilder[E, FitSet[E]]
	
	final private val NewBuilder = new Specialize.For[SetBuilder] {
		override def forBoolean: SetBuilder[Boolean] = new BooleanSetBuilder
		
		override def forByte: SetBuilder[Byte] = ByteSet.newBuilder //new ByteSetBuilder
	
		override def specialized[@specialized E : Specialized]: SetBuilder[E] = ???
	}
	
	final private val EmptySet = new Specialize.For[FitSet] {
		override def forBoolean = BooleanSet.Empty
		override def forByte = ByteSet.Empty
		override def specialized[@specialized E : Specialized]: FitSet[E] = ???
	}
	

	
	
	/** This is my sense of humor, if anyone wondered. */
	class BooleanSet private[FitSet] (bitmap :Int) extends FitSet[Boolean] {
		
		override final def size: Int = bitmap - (bitmap >> 1)
		
		override final def head =
			if (bitmap==0) throw new NoSuchElementException(s"Set().head")
			else (bitmap & 1) > 0
		
		override final def last =
			if (bitmap<3) throw new NoSuchElementException(s"Set($head).tail")
			else (bitmap & 2) == 0
		
		override final def tail =
			if (bitmap>0) new BooleanSet(bitmap ^ j.Integer.lowestOneBit(bitmap))
			else throw new UnsupportedOperationException(s"Set().tail")
		
		override final def isEmpty = bitmap==0
		override final def nonEmpty = bitmap>0
		
		override final def contains(elem: Boolean): Boolean =
			elem && (bitmap & 1) >0 || !elem && (bitmap & 2) > 0
		
		override final def +(elem: Boolean): FitSet[Boolean] =
			new BooleanSet(bitmap | (if (elem) 1 else 2))
		
		override def -(elem: Boolean): FitSet[Boolean] =
			new BooleanSet(bitmap & (if (elem) 2 else 1))
		
		
		override def iterator: FitIterator[Boolean] = new BooleanSetIterator(bitmap)
		
		
		override protected[this] def newBuilder: FitBuilder[Boolean, BooleanSet] = new BooleanSetBuilder
		
		override def stringPrefix = "Set[boolean]"
		
		override def toString = bitmap match {
			case 0 => "Set[boolean]()"
			case 1 => "Set[boolean](true)"
			case 2 => "Set[boolean](false)"
			case 3 => "Set[boolean](true, false)"
		}
	}
	
	
	
	object BooleanSet {
		final val Empty = new BooleanSet(0)
		
		@inline final def newBuilder = new BooleanSetBuilder
		
		class BooleanSetIterator private[BooleanSet] (private[this] var bitmap :Int)
			extends BaseIterator[Boolean] with FitIterator[Boolean]
		{
			def hasNext = bitmap != 0
			
			def head :Boolean = (bitmap & 1) > 0
			
			override def skip(): Unit = bitmap = bitmap >> (bitmap & 1 ) << 31
			
			override def next() :Boolean = { //00 => Nil; 01 => true::Nil; 10 => false::Nil; 11 => true::false::Nil
				val res = bitmap & 1 
				bitmap = bitmap >> res << 31 
				res > 0
			}
		}
		
		class BooleanSetBuilder private[FitSet] extends FitBuilder[Boolean, BooleanSet] {
			private[this] var bitmap = 0
			
			override def addOne :Boolean => Unit = { b => if (b) bitmap |= 1 else bitmap |= 2 }
			
			def count = bitmap - (bitmap >> 1)
			
			override def +=(elem1: Boolean, elem2: Boolean, elems: Boolean*): this.type = {
				if (bitmap!=3)
					if (elem1 ^ elem2) bitmap = 3
					else if (elem1 | elem2) {
						bitmap |= 1
						if (bitmap!=3 && !elems.forall(identity))
							bitmap = 3
					} else {
						bitmap |= 2
						if (bitmap!=3 && elems.exists(identity))
							bitmap = 3
					}
				this
			}
			
			override def ++=(xs: TraversableOnce[Boolean]): this.type = {
				if (xs.nonEmpty) bitmap match {
					case 1 =>
						if (!xs.forall(identity))
							bitmap = 3
					case 2 =>
						if (xs.exists(identity))
							bitmap = 3
					case 0 =>
						if (xs.forall(identity)) bitmap = 1
						else if (xs.exists(identity)) bitmap = 3
						else bitmap = 2
					case _ => 
				}
				this
			} 
			
			override def +=(elem: Boolean): this.type =
				{ if (elem) bitmap |= 1 else bitmap |= 2; this } 
			
			override def result(): BooleanSet = new BooleanSet(bitmap)
			
			override def clear(): Unit = bitmap = 0
		}
	}
}