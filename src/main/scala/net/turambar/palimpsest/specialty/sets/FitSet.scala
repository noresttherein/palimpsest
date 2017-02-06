package net.turambar.palimpsest.specialty.sets

import scala.collection.generic.{GenericCompanion, GenericSetTemplate}
import scala.collection.{GenSet, SetLike}

import net.turambar.palimpsest.specialty.FitIterator.BaseIterator
import net.turambar.palimpsest.specialty.{Elements, FitBuilder, FitIterator, Specialize, Specialized}

/**
  * @author Marcin Mościcki
  */
trait FitSet[@specialized(Elements) E]
	extends Set[E] with SetLike[E, FitSet[E]] with GenSet[E] with GenericSetTemplate[E, FitSet]
{
	override def companion: GenericCompanion[FitSet] = FitSet
}



object FitSet extends GenericCompanion[FitSet] {
	import java.{lang => j}
	
	
	override def empty[@specialized(Elements) E] :FitSet[E] = EmptySet()
	
	override def apply[@specialized(Elements) E](elems :E*) :FitSet[E] =
		(newBuilder[E] ++= elems).result()
	
	def newBuilder[@specialized(Elements) E] :FitBuilder[E, FitSet[E]] = NewBuilder()
	
//	override def fitBuilder[E: Specialized]: FitBuilder[E, FitSet[E]] = NewBuilder()

	
	private type SetBuilder[E] = FitBuilder[E, FitSet[E]]
	
	final private val NewBuilder = new Specialize.For[SetBuilder] {
		override def forBoolean: SetBuilder[Boolean] = BooleanSet.newBuilder
		
		override def forByte: SetBuilder[Byte] = ByteSet.newBuilder //new ByteSetBuilder

		override def forInt: SetBuilder[Int] = IntSet.newBuilder

		override def specialized[@specialized E : Specialized]: SetBuilder[E] = ???
	}
	
	final private val EmptySet = new Specialize.For[FitSet] {
		override def forBoolean = BooleanSet.Empty
		override def forByte = ByteSet.Empty
		override def forInt = IntSet.Empty
		
		override def specialized[@specialized E : Specialized]: FitSet[E] = ???
	}
	

	

}
