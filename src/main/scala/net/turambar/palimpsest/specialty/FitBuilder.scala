package net.turambar.palimpsest.specialty

import scala.annotation.tailrec
import scala.collection.{GenTraversableOnce, LinearSeq, Traversable, TraversableLike, TraversableOnce, mutable}

import Specialized.{Fun1, Fun1Vals}
import net.turambar.palimpsest.specialty.FitBuilder.{BuilderAdapter, BuilderWrapper}
import net.turambar.palimpsest.specialty.seqs.FitSeq



/** Specialized version of [[mutable.Builder]] (for any result type). */
trait FitBuilder[@specialized(Elements) -E, +To] extends mutable.Builder[E, To] {
	protected[this] def mySpecialization :Specialized[E] = Specialized[E]
	
	private[specialty] def addOne :E=>Unit = { elem :E => this += elem }
	
	//this is unspecialized :(
	private[specialty] def addMany :TraversableOnce[E]=>Unit = { this ++= _ }
	
	def build :()=>To = () => result()
	
	def mapInput[@specialized(Fun1) X](f :X=>E) :FitBuilder[X, To] =
		new BuilderAdapter(this, { x :X => this += f(x) }, build)

	def flatMapInput[@specialized(Fun1) X](f :X=>GenTraversableOnce[E]) :FitBuilder[X, To] =
		new BuilderAdapter(this, { x: X => this ++= f(x).seq }, build, true)
	
	
	override def +=(elem1: E, elem2: E, elems: E*): this.type =
		this += elem1 += elem2 ++= elems
	
	override def ++=(xs: TraversableOnce[E]): this.type = xs match {
		case fit :FitItems[E] => fit traverse addOne; this
		
		case list :LinearSeq[E] =>
			@tailrec def loop(l :LinearSeq[E]=list) :Unit =
				if (l.nonEmpty) {
					this += l.head; loop(l.tail)
				}
			loop(); this
		
		case _ => xs foreach addOne; this
	}
	
	override def +=(elem: E): this.type
	
	
	
	
	/** Method used by most concatenation/summing methods of [[FitIterable]]. */
	def ++=(first :TraversableOnce[E], second :TraversableOnce[E]) :this.type =
		this ++= first ++= second
	
	override def result(): To
	
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
	
	
	def typeHint[L<:E](implicit specialization :Specialized[L]) :FitBuilder[E, To] = this
	
	def source :Any = this
	
	
	
	def count :Int
}








/**
  * @author Marcin Mościcki
  */
object FitBuilder {
	
	def apply[E, To](builder :mutable.Builder[E, To]) :FitBuilder[E, To] = builder match {
		case fit :FitBuilder[E, To] => fit
		case _ => new BuilderWrapper(builder)
	}
	
	
	
	private def mapper[@specialized(Fun1) X, @specialized(Fun1Vals) Y, To](b :mutable.Builder[Y, To])(f :X=>Y) :X=>Unit =
		{ x :X => b += f(x) }
	
	private def traversed[@specialized(Fun1) X](appender :X=>Unit) :TraversableOnce[X]=>Unit =
		xs => xs match {
			case fit :FitItems[X] => fit traverse appender
			case _ => xs foreach appender
		}

	
	implicit def builderFilters[@specialized(Fun1) X, To](target :FitBuilder[X, To]) :BuilderFilter[X, To] =
		new BuilderFilter(target)
	
	final class BuilderFilter[@specialized(Fun1) X, To](private val target :FitBuilder[X, To]) {
		@inline def filterInput(p :X=>Boolean) :FitBuilder[X, To] = FitBuilder.filter(p, target)
	}
	

	
	private def filter[@specialized(Fun1) X, To](p :X=>Boolean, builder :FitBuilder[X, To]) :FitBuilder[X, To] = {
		val append = builder.addOne
		new BuilderAdapter(builder, { x :X => if (p(x)) append(x) }, builder.build, true)
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
		
		override def flatMapInput[@specialized(Fun1) X](f: (X) => GenTraversableOnce[E]): FitBuilder[X, To] = {
			val fmap :X=>Unit = { x :X => vanilla ++= f(x).seq }
			new BuilderAdapter(vanilla, fmap, traversed(fmap), build, true)
		}
		
		override def mapResult[NewTo](f: (To) => NewTo): FitBuilder[E, NewTo] =
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
		
		override def mapInput[@specialized(Fun1) E](f: (E) => X): FitBuilder[E, To] = {
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
				target.sizeHint(math.min(size, boundingColl.size))
		
		override def sizeHint(size :Int) =
			if (!supressHints)
				target.sizeHint(size)
		
		override def mapResult[NewTo](f: (To) => NewTo): FitBuilder[X, NewTo] =
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
	  *
	  * @tparam E
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
		
		protected[this] def guessSpecialization :Specialized[E] = {
			@tailrec def rec(soFar :Specialized[E], remaining :List[TraversableOnce[E]]) :Specialized[E] =
				remaining match {
					case Nil => soFar
					case FitItems(col)::rest if col.specialization == soFar =>
						rec(soFar, rest)
					case hd::rest if hd.isEmpty => rec(soFar, rest)
					case _ => Specialized.generic
				}
			elems match {
				case FitItems(col)::rest if col.specialization!=Specialized.SpecializedAnyRef =>
					rec(col.specialization.asInstanceOf[Specialized[E]], rest)
				case _ => Specialized.generic[E]
			}
		}
		
		override def +=(elem1: E, elem2: E, elems: E*): this.type =
			{ this.elems = elems::FitSeq.pair(elem1, elem2)::this.elems; this }
		
		override def ++=(xs: TraversableOnce[E]): this.type =
			{ elems = xs::elems; this }
		
		override def +=(elem: E): this.type =
			{ elems = FitSeq.single(elem)::elems; this }
		
		
		override def clear(): Unit = elems = Nil
		
		override def count = {
			var total = 0; var parts = elems
			while (parts.nonEmpty) {
				total += parts.size
				parts = parts.tail
			}
			total
		}
		
	}
	
	
	class OptimisticFitBuilder[@specialized(Elements) E, To<:TraversableOnce[E]](fit :FitBuilder[E, To], unfit : =>FitBuilder[E, To])
		extends FitBuilder[E, To]
	{
		private[this] var optimistic = true
		private[this] var b = fit
		
		var count = 0
		
		override def sizeHint(expect: Int): Unit = b.sizeHint(expect)
		
		override def ++=(xs: TraversableOnce[E]): this.type = xs match {
			case col :Traversable[E] =>
				try {
					b ++= xs
					count = b.count
					this
				} catch {
					case e @ (_ :ClassCastException | _ :ArrayStoreException) if optimistic =>
						optimistic = false
						val added = b.count - count
						if (added < 0)
							throw e
						val collected = b.result()
						b = unfit
						b ++= collected
						//							throw new Exception(s"OptimisticFitBuilder[${mySpecialization}] failed to add $col;")
						b ++= col.toIterator.drop(added)
						count = b.count
						this
				}
			case col :FitIterator[E] => col foreach { this += _ }; this
			case _ => super.++=(xs) //xs foreach { this += _}
		}
		
		
		
		override def +=(elem: E): this.type = try {
			b += elem; count += 1; this
		} catch {
			case e @ (_ :ClassCastException | _ :ArrayStoreException) if optimistic =>
				if (b.count!=count)
					throw e
				count += 1
				optimistic = false
				val collected = b.result()
				b = unfit; b ++= collected; b += elem
				this
		}
		
		override def result(): To = b.result()
		
		override def clear(): Unit = b.clear()
		

//		override def reverseResult: FitBuilder[E, To] = b.reverseResult
	}
	
	
	
	
/*
	class FilteredBuilder[@specialized(Fun1) -X, +To] (p :X=>Boolean, target :mutable.Builder[X, To], override val addOne :X=>Unit)
		extends FitBuilder[X, To]
	{
		def this(p :X=>Boolean, target :mutable.Builder[X, To]) =
			this(p, target, { x :X => if (p(x)) target += x; })
		
		def predicate[U<:X] :U=>Boolean = p
		def builder = target
		
		
		
		override def ++=(xs: TraversableOnce[X]): this.type = xs match {
			case fit :FitItems[X] => fit traverse addOne; this
			case _ => xs foreach addOne; this
		}
		
		override def +=(elem: X): this.type = { addOne(elem); this } //{ if (p(elem)) target += elem; this }
		
		override def result(): To = target.result()
		
		override def size: Int = ???
		
		override def clear(): Unit = target.clear()
	}
*/
	
	//todo: sizeHint
	
	
/*	class ComposedFitBuilder[@specialized(Fun1) -X, @specialized(Fun1Vals) Y, +To](target :FitBuilder[Y, To], f :X=>Y)
		extends FitBuilder[X, To]
	{
		/** Extracted to minimize the number of produced classes. */
		override val addOne :X=>Unit = { x => val y = f(x); target += y }
		
		override def ++=(xs: TraversableOnce[X]): this.type = xs match {
			case fit :FitItems[X] => fit traverse addOne; this
			case _ => xs foreach addOne; this
		}
		
		override def +=(elem: X): this.type = { target += f(elem); this }
		
		
		override def mapInput[@specialized(Fun1) E](g: (E) => X): FitBuilder[E, To] =
			new ComposedFitBuilder[E, Y, To](target, { e :E => f(g(e)) })
		
		override def result(): To = target.result()
		
		override def sizeHint(expect: Int): Unit = target.sizeHint(expect)
		
		override def size: Int = target.size
		
		override def clear(): Unit = target.clear()
		
		override def toString = s"($mySpecialization ++=: $target)"
	}
	*/
}
	
