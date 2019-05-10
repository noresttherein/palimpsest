package net.turambar.palimpsest.specialty


import java.lang.Math
import scala.annotation.tailrec
import scala.collection.{GenTraversableOnce, LinearSeq, Traversable, TraversableLike, TraversableOnce, mutable}
import RuntimeType.{Fun1, Fun1Vals}
import net.turambar.palimpsest.specialty.FitBuilder.{BuilderAdapter, BuilderWrapper}
import net.turambar.palimpsest.specialty.seqs.FitSeq

import scala.collection.generic.CanBuildFrom



/** Specialized version of [[mutable.Builder]] (for any result type). */
trait FitBuilder[@specialized(Elements) -E, +To] extends mutable.Builder[E, To] {
//	protected[this] def mySpecialization :Specialized[E] = Specialized[E]



	private[specialty] def addOne :E=>Unit = { elem :E => this += elem }
	
	//this is unspecialized :(
	private[specialty] def addMany :TraversableOnce[E]=>Unit = { this ++= _ }
	
	def build :()=>To = () => result()
	
	def mapInput[@specialized(Fun1) X](f :X=>E) :FitBuilder[X, To] =
		new BuilderAdapter(this, { x :X => this += f(x) }, build)

	def flatMapInput[@specialized(Fun1) X](f :X=>GenTraversableOnce[E]) :FitBuilder[X, To] =
		new BuilderAdapter(this, { x: X => this ++= f(x).seq }, build, true)

	override def mapResult[R](f :To=>R) :FitBuilder[E, R] =
		new BuilderAdapter(this, addOne, addMany, () => f(result()), false)
	
	override def +=(elem1: E, elem2: E, elems: E*): this.type =
		this += elem1 += elem2 ++= elems

	def ++=(xs :FitTraversableOnce[E]) :this.type = {
		val it = xs.toIterator
		while(it.hasNext) this += it.next()
		this
	}

	override def ++=(xs: TraversableOnce[E]): this.type = xs match {
		case fit :FitTraversableOnce[E] =>
			this ++= fit
		case list :LinearSeq[E] =>
			@tailrec def loop(l :LinearSeq[E]=list) :Unit =
				if (l.nonEmpty) {
					this += l.head; loop(l.tail)
				}
			loop(); this
		
		case _ => xs foreach addOne; this
	}
	
	override def +=(elem: E): this.type
	
	
	
	
//	/** Method used by most concatenation/summing methods of [[FitIterable]]. */
//	def ++=(first :TraversableOnce[E], second :TraversableOnce[E]) :this.type =
//		this ++= first ++= second
	
	override def result(): To

	def result(first :TraversableOnce[E], second :TraversableOnce[E]) :To =
		(this ++= first ++= second).result()
	
	override def sizeHint(expect: Int): Unit = ()
	
	override def sizeHint(coll: TraversableLike[_, _]): Unit =
		if (ofKnownSize(coll)) sizeHint(coll.size)
	
	override def sizeHint(coll: TraversableLike[_, _], delta: Int): Unit =
		if (ofKnownSize(coll)) sizeHint(coll.size + delta)
	
	override def sizeHintBounded(size: Int, boundingColl: TraversableLike[_, _]): Unit =
		if (ofKnownSize(boundingColl)) sizeHint(size min boundingColl.size)
	
	
//	def supressHints :FitBuilder[E, To] =
//		new BuilderAdapter(new BuilderWrapper(this, build, true))
	
	/** A builder which, in case of ordered collections, will produce the result in reversed order
	  * compared to this builder.
 	  * @return a builder such that `inverse.result() == this.result().reversed` if `To` is an ordered collection.
	  */
//	def reverseResult :FitBuilder[E, To]
	
	
	def typeHint[L<:E](implicit specialization :RuntimeType[L]) :FitBuilder[E, To] = this
	
	def origin :Any = this
	
	
	@deprecated("to restricting with mutable sets", "")
	def count :Int
}



trait GenericBuilder[@specialized(Elements) E, +C[X]] extends FitBuilder[E, C[E]] {
	def like[@specialized(Elements) X] :GenericBuilder[X, C]
}




/**
  * @author Marcin Mościcki
  */
object FitBuilder {
	
	def apply[E, To](builder :mutable.Builder[E, To]) :FitBuilder[E, To] = builder match {
		case fit :FitBuilder[E, To] => fit
		case _ => new BuilderWrapper(builder)
	}
	
//	def unapply[E, What](cbf :CanBuildFrom[_, E, What]) :Option[FitBuilder[E, What]] =
	
	private def mapper[@specialized(Fun1) X, @specialized(Fun1Vals) Y, To](b :mutable.Builder[Y, To])(f :X=>Y) :X=>Unit =
		{ x :X => b += f(x) }
	
	private def traversed[@specialized(Fun1) X](appender :X=>Unit) :TraversableOnce[X]=>Unit =
		{
			case fit :FitTraversableOnce[X] => fit traverse appender
			case xs => xs foreach appender
		}

	
	implicit def builderFilters[@specialized(Fun1) X, To](target :FitBuilder[X, To]) :BuilderFilter[X, To] =
		new BuilderFilter(target)
	
	final class BuilderFilter[@specialized(Fun1) X, To](private val target :FitBuilder[X, To]) {
		@deprecated("makes a false impression of a specialized method", "")
		@inline def filterInput(p :X=>Boolean) :FitBuilder[X, To] = FitBuilder.filter(p, target)
		@inline def filterInput(p :X=>Boolean, where :Boolean) :FitBuilder[X, To] = FitBuilder.filter(p, where, target)
	}
	

	
	private def filter[@specialized(Fun1) X, To](p :X=>Boolean, builder :FitBuilder[X, To]) :FitBuilder[X, To] = {
		val append = builder.addOne
		new BuilderAdapter(builder, { x :X => if (p(x)) append(x) }, builder.build, true)
	}

	private def filter[@specialized(Fun1) X, To](p :X => Boolean, where :Boolean, builder :FitBuilder[X, To]) :FitBuilder[X, To] = {
		val append = builder.addOne
		new BuilderAdapter(builder, { x :X => if (p(x) == where) append(x) }, builder.build, true)
	}

	trait IgnoresHints extends mutable.Builder[Nothing, Any] {
		override def sizeHint(size: Int) :Unit = ()
		override def sizeHint(coll: TraversableLike[_, _]) :Unit = ()
		override def sizeHint(coll: TraversableLike[_, _], delta: Int) :Unit = ()
		override def sizeHintBounded(size: Int, boundingColl: TraversableLike[_, _]) :Unit = ()
	}

	private class BuilderWrapper[-E, +To](vanilla :mutable.Builder[E, _], override val build :()=>To, supressHints :Boolean = false)
		extends FitBuilder[E, To]
	{
		
		def this(vanilla :mutable.Builder[E, To]) = this(vanilla, () => vanilla.result())
		
		override private[specialty] def addOne :E => Unit = e => vanilla += e

		override private[specialty] def addMany :TraversableOnce[E]=>Unit = e => vanilla ++= e
		
		override def mapInput[@specialized(Fun1) X](f: (X) => E): FitBuilder[X, To] = {
			val map = mapper(vanilla)(f)
			new BuilderAdapter(vanilla, map, traversed(map), build, supressHints)
		}
		
		override def flatMapInput[@specialized(Fun1) X](f: X => GenTraversableOnce[E]): FitBuilder[X, To] = {
			val fmap :X=>Unit = { x :X => vanilla ++= f(x).seq }
			new BuilderAdapter(vanilla, fmap, traversed(fmap), build, true)
		}
		
		override def mapResult[NewTo](f: To => NewTo): FitBuilder[E, NewTo] =
			new BuilderWrapper[E, NewTo](vanilla, () => f(build()))
		
		override def ++=(xs: TraversableOnce[E]): this.type = { vanilla ++= xs; this }
		
		override def +=(elem: E): this.type = { vanilla += elem; this }
		
		override def result(): To = build()
		
		override def count: Int = ???
		
		override def clear(): Unit = vanilla.clear()
		
		override def sizeHint(expect: Int): Unit =
			if (!supressHints)
				vanilla.sizeHint(expect)
		
	}
	
	
	
	private class BuilderAdapter[@specialized(Fun1) X, +To](
			target :mutable.Builder[_, _],
			override val addOne :X=>Unit,
			override val addMany :TraversableOnce[X]=>Unit,
			override val build :()=>To,
			supressHints :Boolean
		) extends FitBuilder[X, To]
	{
		def this(target :mutable.Builder[_, _], addOne :X=>Unit, build :()=>To, supressHints :Boolean=false) =
			this(target, addOne, traversed(addOne), build, supressHints)
		
//		def this(target :mutable.Builder[_, To], addOne :X=>Unit, supressHints :Boolean=false) =
//			this(target, addOne, traversed(addOne), () => target.result(), supressHints)
		
		def this(target :FitBuilder[X, To]) = this(target, target.addOne, target.addMany, target.build, false)
		
		override def mapInput[@specialized(Fun1) E](f: E => X): FitBuilder[E, To] = {
			val addE = { e :E => addOne(f(e)) }
			new BuilderAdapter[E, To](target, addE, traversed(addE), build, supressHints)
		}
		
		override def flatMapInput[@specialized(Fun1) E](f: (E) => GenTraversableOnce[X]): FitBuilder[E, To] =
			new BuilderAdapter[E, To](target, { e :E => addMany(f(e).seq) }, build, true)
		
		override def ++=(xs: TraversableOnce[X]): this.type = { addMany(xs); this }
		
		override def +=(elem: X): this.type = { addOne(elem); this }
		
		override def result(): To = build()
		
		
		override def count: Int = ???
		
		override def clear(): Unit = target.clear()
		
		override def sizeHint(coll: TraversableLike[_, _]): Unit =
			if (!supressHints && ofKnownSize(coll))
				target.sizeHint(coll.size)
		
		override def sizeHint(coll: TraversableLike[_, _], delta: Int): Unit =
			if (!supressHints && ofKnownSize(coll))
				target.sizeHint(coll.size + delta)
		
		override def sizeHintBounded(size: Int, boundingColl: TraversableLike[_, _]): Unit =
			if (!supressHints && ofKnownSize(boundingColl))
				target.sizeHint(Math.min(size, boundingColl.size))
		
		override def sizeHint(size :Int) :Unit =
			if (!supressHints)
				target.sizeHint(size)
		
		override def mapResult[NewTo](f: To => NewTo): FitBuilder[X, NewTo] =
			new BuilderAdapter(target, addOne, addMany, () => f(build()), supressHints)
		
		
		/*
				override def reverseResult: FitBuilder[X, To] = target match {
					case fit :FitBuilder[Y, _] => new BuilderAdapter(fit.reverseResult, addOne, addMany, build)
					case _ =>
						val buffer = FitList.newBuilder[X].reverseResult //it better had dedicated implementation!
						val res = { () => addMany(buffer.result()); build() }
						new BuilderAdapter(target, buffer.addOne, buffer.addMany, res)
				}
		*/
		
	}
	
	
	
	
	
	
	/** A useful idiot, unspecialized builder which doesn't do any real work, but stores all appended elements
	  * internally as-is (including collections as whole), and requires implementing classes to provide a [[RetardedFitBuilder#realBuilder]]
	  * to do the real work when [[RetardedFitBuilder#result]] is eventually called.
	  */
	trait RetardedFitBuilder[-E, +To] extends FitBuilder[E, To] {
		//			protected def build :FitBuilder[E, To]
		
		private[this] final var elems :List[TraversableOnce[E]] = Nil
		protected[this] final var hint :Int = Int.MinValue
		
		override def sizeHint(expect: Int): Unit = hint = expect
		
		protected[this] def inOrder :List[TraversableOnce[E]] = elems.reverse
		
		@tailrec protected[this] final def guessSize(soFar :Int=0, remaining :List[TraversableOnce[E]]=elems) :Int =
			remaining match {
				case Nil => soFar
				case col::rest if ofKnownSize(col) => guessSize(soFar+col.size, rest)
				case _ => Int.MinValue
			}
		
		protected[this] def guessSpecialization :RuntimeType[E] = {
			@tailrec def rec(soFar :RuntimeType[E], remaining :List[TraversableOnce[E]]) :RuntimeType[E] =
				remaining match {
					case Nil => soFar
					case FitTraversableOnce(col)::rest if col.specialization == soFar =>
						rec(soFar, rest)
					case hd::rest if hd.isEmpty => rec(soFar, rest)
					case _ => RuntimeType.erased
				}
			elems match {
				case FitTraversableOnce(col)::rest if col.specialization!=RuntimeType.OfAnyRef =>
					rec(col.specialization.asInstanceOf[RuntimeType[E]], rest)
				case _ => RuntimeType.erased[E]
			}
		}
		
		override def +=(elem1: E, elem2: E, elems: E*): this.type =
			{ this.elems = elems::FitSeq.pair(elem1, elem2)::this.elems; this }
		
		override def ++=(xs: TraversableOnce[E]): this.type =
			{ elems = xs::elems; this }
		
		override def +=(elem: E): this.type =
			{ elems = FitSeq.single(elem)::elems; this }
		
		
		override def clear(): Unit = elems = Nil
		
		override def count :Int = {
			var total = 0; var parts = elems
			while (parts.nonEmpty) {
				total += parts.size
				parts = parts.tail
			}
			total
		}
		
	}


	/** A builder for collections `To` of elements `E` which optimistically assumes that all elements will actually be
	  * of subtype `O &lt;: E`. It appends all added elements to the first builder `optimist` for as long as the
	  * operation doesn't throw a `ClassCastException` or `ArrayStoreException`. If one of these exceptions is encountered,
	  * it falls back to the second builder `pessimist`, starting with adding `optimist.result()`. Note that if the
	  * collection types built by both builders are different, this will ''not'' be equivalent to appending all elements
	  * directly to `pessimist` in the same order, as `optimist` might have reordered them or dropped duplicates.
	  *
	  * This class is primarily useful when building collections containing elements of several other collections:
	  * while the method signature might accept any element type and declare the result as the least upper type bound of all
	  * elements, it will often be the case that all added elements are of the same type. Thus, the final element type
	  * can be initially predicted as the element type of the first added element(s). This makes it possible to build
	  * specialized collections where otherwise an erased variant would be required for type safety. It is still
	  * somewhat a hack however and should not be treated as a general purpose class, but used only in context where
	  * the types of both builders given as arguments guarantee correct behaviour.
	  *
	  * @param optimist an empty builder accepting narrowed elements.
	  * @param pessimist a fallback builder to use when an element of type other than `O` is added to this builder.
	  * @tparam O a subtype of declared element type guessed to be the actual upper bound for all elements.
	  * @tparam E declared element type of the built collection.
	  * @tparam To built collection type.
	  */
	class OptimisticFitBuilder[@specialized(Elements) O <: E, E, To <: Traversable[E]](optimist :FitBuilder[O, To], pessimist :FitBuilder[E, To])
		extends FitBuilder[E, To]
	{
		private[this] var target :FitBuilder[E, To] = try {
			optimist.asInstanceOf[FitBuilder[E, To]]
		} catch {
			case _ :Exception => pessimist
		}

		private[this] var sizeHint :Int = Int.MinValue
		private[this] var size = 0


		override def count :Int = size

		override def sizeHint(expect :Int) :Unit = {
			sizeHint = expect
			target.sizeHint(expect)
		}

		override def ++=(xs :TraversableOnce[E]) :this.type = xs match {
			case vals :Traversable[E] if ofKnownSize(vals) =>
				try {
					target ++= vals
					size += vals.size
				} catch {
					case ex @ (_ :ClassCastException | _ :ArrayStoreException) if !(target eq pessimist) =>
						val init = target.result()
						val added = init.size
						if (added < size)
							throw ex
						if (sizeHint >= 0)
							pessimist.sizeHint(sizeHint)
						pessimist ++= init
						pessimist ++= vals.toIterator.drop(added - size)
						size = size + vals.size
						target = pessimist
				}
				this
//			case head::tail =>
//				this += head; this ++= tail
			case _ =>
				xs.foreach(+=)
				this
		}

		override def +=(x :E) :this.type = {
			try {
				target += x
			} catch {
				case ex @ (_ :ClassCastException | _ :ArrayStoreException) if !(target eq pessimist) =>
					val init = target.result()
					if (init.size != size)
						throw ex
					if (sizeHint > 0)
						pessimist.sizeHint(sizeHint)
					pessimist ++= init
					pessimist += x
					target = pessimist
			}
			size += 1
			this
		}

		override def result() :To = { size = 0; target.result() }

		override def clear() :Unit = { target.clear(); size = 0 }

	}




/*
	class OldOptimisticFitBuilder[@specialized(Elements) E, To<:TraversableOnce[E]](optimist :FitBuilder[E, To], pessimist :FitBuilder[E, To])
		extends FitBuilder[E, To]
	{
		private[this] var target = optimist

		var count = 0

		override def sizeHint(expect: Int): Unit = target.sizeHint(expect)

		override def ++=(xs: TraversableOnce[E]): this.type = xs match {
			case col :Traversable[E] =>
				try {
					target ++= xs
					count = target.count
					this
				} catch {
					case e @ (_ :ClassCastException | _ :ArrayStoreException) if !(target eq pessimist) =>
						val added = target.count - count
						if (added < 0)
							throw e
						val collected = target.result()
						target = pessimist
						target ++= collected
						//							throw new Exception(s"OptimisticFitBuilder[${mySpecialization}] failed to add $col;")
						target ++= col.toIterator.drop(added)
						count = target.count
						this
				}
			case col :FitIterator[E] => col foreach { this += _ }; this
			case _ => super.++=(xs) //xs foreach { this += _}
		}



		override def +=(elem: E): this.type = try {
			target += elem; count += 1; this
		} catch {
			case e @ (_ :ClassCastException | _ :ArrayStoreException) if optimistic =>
				if (target.count!=count)
					throw e
				count += 1
				optimistic = false
				val collected = target.result()
				target = pessimist; target ++= collected; target += elem
				this
		}
		
		override def result(): To = target.result()
		
		override def clear(): Unit = target.clear()
		

//		override def reverseResult: FitBuilder[E, To] = b.reverseResult
	}
*/

	
	

}
	
