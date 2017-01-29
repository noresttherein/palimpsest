package net.turambar.collection

import net.turambar.collection.Juncture.No

import scala.annotation.tailrec
import scala.collection.{AbstractSeq, LinearSeq}
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag


import Stream.cons

/** Reports occurrences of some condition in input sequence of `I`s as values of type `O`.
  * Similar to the `collect` operation of scala collections, but works on the whole input collection
  * rather than individual elements. Default implementations are lazy, essentially mapping an
  * input stream into its prefix scan.
  * 
  * 
  * Provides monadic operations with fixed input type with regard to its output type, where the boxed
  * value type `O` represents the last occurence of this value/condition in the input data. When chained
  * with `flatMap`, each subsequent expression works on the slice of input stream between the occurences of 
  * the last expression. This makes it a functional equivalent of iterating over an input sequence and updating 
  * the state between iterations.
  * For example,
  * {{
  *     val decimal = for {
  *         tens <- Juncture.collect { i :Int if i % 10 ==0 => i/10 }
  *         thirds <- Juncture.collect { i :Int if i % 3==0 => i % 10 } 
  *     } yield tens -> evens
  *     
  *     decimals(Stream.iterate(0)(_+1).take(31)) == Seq(0->0, 0->3, 0->6, 0->9, 1->2, 1->5, 1->8, 2->1, 2->4, 2->6, 3->0)
  * }}
  * is equivalent to:
  * {{
  *    def decimals(input :Stream[Int]) = {
  *        var tens :Int = 0
  *        var thirds :Int = 0
  *        var res :Seq[(Int, Int)]
  *        for { i <- input } {
  *            if (i % 10 ==0) {
  *               tens = i/10
  *               if (i % 3 ==0) thirds = i % 10
  *               res = (tens, thirds) +: res
  *            } else if (i % 3==0) {
  *                thirds = i % 10
  *                res = (tens, thrids) +: res
  *            }
  *        }
  *        res.reverse
  *    }
  * }}
  * This is different from an analogue for comprehension on scala collections in that it doesn't work on the
  * cartesian product of each monadic value, but restricts the range of each of them to the most recent values.
  * As a result of performing the iteration in parallel, computational complexity is linear with regard to input
  * data as any given element is 'mapped' at most once by every member expression. Note that there is no restriction 
  * on the length of intervals between individual occurences of any expression, in particular they need not be equal 
  * or in any other relationship.
  *
  * One caveat that must be considered here is that each subsequent expression in the for block
  * works only the suffix of the input stream starting with the first occurence of the previous expression,
  * meaning that the order is important, and swapping individual expressions may yield different results.
  * If the order in which individual occurences appear in input is unknown, [[Juncture#last]] can be used
  * to lift the expression to corresponding optional type. Such a boxed option will contain the last value
  * of the original occurence in `Some`, or `None` if it didn't occur before the current point. This is simply
  * equivalent to reporting `None` at the first element of the stream (or `Some(x)` if it happens to satisfy the
  * associated condition), and then `Some(x)` at each subsequent occurence.
  *
  * @tparam I input element type of scanned streams
  * @tparam O output type of collected occurences
  */
trait Juncture[-I, +O] extends Any { parent =>
	import Juncture._

	/** Collect all occurences of `O` in the input collection in a new collection created using
	  * the associated implicit factory. Note that this eagerly evaluates the whole input
	  * and will thus not terminate on infinite streams.
	  */
	def apply[C<:Iterable[I], R](in :C)(implicit builders :CanBuildFrom[C, O, R]) :R =
		(builders(in) ++= collect(in.toStream)).result()
	
	/** Lazily list all found occurences in the given stream. */
	def collect[T](in :Stream[I]) :Stream[O] = scan(in).flatten


	/** The heart method of this class, reports all occurences of `O` at any given position in the input stream.
	  * Unlike `collect` or `apply`, the result must include empty elements for any position in input
	  * where no occurences where found. This allows to combine several instances and relate their output
	  * to origininating input elements.
	  * @return a stream resulting from mapping every individual element in input to 'new' occurences found
	  *         in that element, or an empty sequence if none.
	  */
	def scan(in :Stream[I]) :Stream[Option[O]]

	protected def head(in :Stream[I]) :Option[O] = scan(in).head

	protected def scanner :Stream[I] => Stream[Option[O]] = scan _

	protected def scan[H<:I, T](occurrences :Stream[H]=>Stream[Option[T]]) :Juncture[H, T] = new StreamMap(occurrences)


//	def one(in :I) :Option[O] = scan(in #:: Stream.Empty).headOption.flatten
//
//	def one(in :Option[I]) :Option[O] = scan(in.toStream).headOption.flatten

	def map[T](f :O=>T) :Juncture[I, T] =
		scan { in :Stream[I] => scan(in).map(_.map(f)) }


	def flatMap[H<:I, T](f :O=>Juncture[H, T]) :Juncture[H, T] = new FlatMapJuncture(this, f)

/*
	def flatMap[H<:I, T](f :O=>Juncture[H, T]) :Juncture[H, T] = scan {
		in :Stream[H] =>
			@tailrec def next(os :Stream[Option[O]]=scan(in), gap :Int=0) :Stream[Option[T]] =
				if (os.isEmpty) Noes(gap)
				else os.head match {
					case None => next(os.tail, gap+1)
					case Some(o) => //f(o).flatMapped(f)(os, gap)(in, in -> 0)(in, 0)
//						f(o).flattened(f)(os, gap)(in, in->)
//						f(o).flatScan(flattened(f))(os, gap)(in, in->0)(in, 0)
						f(o).flattened(f)(os, gap)(in, in->0, in, 0)
				}
			next()
	}
*/

/*
	protected def catchup(past :Stream[I], last :(Stream[I], Int), present :Stream[I], offset :Int, gap :Int) :Stream[Option[O]] =
		scan(present)

	protected def catchupLast(past :Stream[I], last :(Stream[I], Int), present :Stream[I], offset :Int, gap :Int) :Stream[Option[O]] = {
		def next(prev :Stream[I], in :Stream[I], gap :Int) :Stream[Option[O]] =
			if (gap==0) scan(in) match {
				case None #:: rest => scan(last._1).head #:: rest //this wrongly marks this position as 'found'
				case res => res
			} else in.head match {
				case None => next(prev, in.tail, gap-1)
				case some => next(in, in.tail, gap-1)
			}
		next(present, present, gap)
	}

	protected def catchupNext(past :Stream[I], last :(Stream[I], Int), present :Stream[I], offset :Int, gap :Int) :Stream[Option[O]] = {
		def next(in :Stream[I], gap :Int) :Stream[Option[O]] =
			if (gap==0) scan(in)
			else next(in.tail, gap-1)
		next(present, gap)
	}

*/


	protected type Scan[X, Y] = (Stream[X], (Stream[X], Int), Stream[X], Int) => Stream[Option[Y]]
	protected type FlatScan[T, X, Y] = (Stream[Option[T]], Int) => Scan[X, Y] //(Stream[X], (Stream[X], Int), Stream[X], Int) => Stream[Option[Y]]

	protected def flattened[X <:I, Y>:O, T](successor :T=>Juncture[X, Y])(triggers :Stream[Option[T]], offset :Int) :Scan[X, Y] =
		flatScan[X, Y, T](flattened[X, Y, T](successor))(triggers, offset)(_, _)(_, _)


	protected def flatScan[X <: I, Y>:O, T](k :FlatScan[T, X, Y])
	                                       (outer :Stream[Option[T]], outerOffset :Int)
	                                       (past :Stream[X], last :(Stream[X], Int))
	                                       (present :Stream[X], offset :Int) :Stream[Option[Y]] =
	{
		def next(trigger :Stream[Option[T]])(current :Stream[X], offset :Int, inner :Stream[Option[O]]) :Stream[Option[Y]] =
			if (trigger.isEmpty)
				inner
			else trigger.head match {
				case None => inner.head #:: next(trigger.tail)(current.tail, offset+1, inner.tail)
				case Some(t) => k(trigger, offset)(past, current->offset, current, offset)
			}

		def catchup(current :Stream[X], offset :Int, inner :Stream[Option[O]]) :Stream[Option[Y]] =
			if (offset < outerOffset)
				inner.head #:: catchup(current.tail, offset+1, inner.tail)
			else
				inner.head #:: next(outer.tail)(current.tail, offset+1, inner.tail)

		catchup(present, offset, scan(present))

	}

	protected def flatMapped[H <: I, P >: O, T](successor: (T) => Juncture[H, P])
	                                                    (outer: Stream[Option[T]], outerOffset: Int)
	                                                    (past: Stream[H], last: (Stream[H], Int))
	                                                    (present: Stream[H], offset: Int): Stream[Option[P]] =
	{
		def next(trigger :Stream[Option[T]])(current :Stream[H], offset :Int, inner :Stream[Option[O]]) :Stream[Option[P]] =
			if (trigger.isEmpty)
				inner
			else trigger.head match {
				case None => inner.head #:: next(trigger.tail)(current.tail, offset+1, inner.tail)
				case Some(t) => successor(t).flatMapped(successor)(trigger, offset)(past, current->offset)(current, offset)
			}

		def catchup(current :Stream[H], offset :Int, inner :Stream[Option[O]]) :Stream[Option[P]] =
			if (offset < outerOffset)
				inner.head #:: catchup(current.tail, offset+1, inner.tail)
			else
				inner.head #:: next(outer.tail)(current.tail, offset+1, inner.tail)

		catchup(present, offset, scan(present))

	}


	def filter(condition :O=>Boolean) :Juncture[I, O] =
		scan { in :Stream[I] => scan(in).map(_.filter(condition)) }

	def foreach(op :O=>Unit) :Juncture[I, O] = scan { in :Stream[I] =>
		scan(in).map { o => o.foreach(op); o }
	}


	def ||[J<:I, T>:O](other :Juncture[J, T]) :Juncture[J, T] =
		scan { in :Stream[J] => (scan(in) zip other.scan(in)) map { case (mine, hers) => mine orElse hers }}

	def <>[J<:I, T](other :Juncture[J, T]) :Conjunction[J, O, T] = new Conjunction(this, other)


	def first :Juncture[I, O] = new ScanFirst(scanner)

	def next :Juncture[I, O] = new ScanNext(scanner)

	def last :Juncture[I, O] = new ScanLast(scanner)

	def passed :Juncture[I, O] = new StreamMap(scanner)

	def optional :Juncture[I, Option[O]] = Juncture.scanLast { in :Stream[I] =>
		val os = scan(in)
		if (os.isEmpty) Stream.Empty
		else Some(os.head) #:: os.tail.map(_.map(Some.apply) :Option[Option[O]])
	}

	def grouped :Juncture[I, Seq[O]] = new GroupedJuncture(scanner)


	def since[H<:I, T](other :Juncture[H, T]) :Juncture[H, Seq[O]] = scan { in :Stream[H] =>
		def accumulate(triggers :Stream[Option[T]], os :Stream[Option[O]], occurences :Option[Seq[O]]=None, skipped :Int=0) :Stream[Option[Seq[O]]] =
			(triggers, os) match {
				case (None #:: ts, o #:: oo) => accumulate(ts, oo, occurences.map(o ++: _), skipped+1)

				case (_ #:: ts, o #:: oo) =>
					occurences.map(_.reverse) #:: (No * skipped) #::: accumulate(ts, oo, Some(o.toSeq))

				case (empty, o #:: oo) => accumulate(empty, oo, occurences.map(o ++: _), skipped+1)

				case _ => occurences.map(_.reverse) #:: (No * skipped :Stream[Option[Seq[O]]])
			}
		accumulate(other.scan(in), scan(in))
	}

	def until[H<:I, T](other :Juncture[H, T]) :Juncture[H, Seq[O]] = scan { in :Stream[H] =>
		def accumulate(delim :Stream[Option[T]], os :Stream[Option[O]], occurences :Seq[O]=Nil) :Stream[Option[Seq[O]]] =
			(delim, os) match {
				case (None #:: ts, o #:: oo) => None #:: accumulate(ts, oo, o ++: occurences)
				case (_ #:: ts, o #:: oo) => Some(occurences.reverse) #:: accumulate(ts, oo, o.toSeq)
			}
		accumulate(other.scan(in), scan(in))
	}



}


object Juncture {

	@inline final def in[I] :Juncture[I, Unit] = AtStart

	def all[I] :Identity[I] = new Identity[I] {}

	def first[I, O](pick :I=>Option[O]) :Juncture[I, O] = new Find[I, O](pick) with FirstJuncture[I, O]

	def scanFirst[I, O](scan :Stream[I]=>Stream[Option[O]]) :Juncture[I, O] = new ScanFirst(scan)

	def firstSubclass[O :ClassTag] :Juncture[Any, O] = first(implicitly[ClassTag[O]].unapply)



	def next[I, O](pick :I=>Option[O]) :Juncture[I, O] = new Find[I, O](pick) with NextJuncture[I, O]

	def scanNext[I, O](scan :Stream[I]=>Stream[Option[O]]) :Juncture[I, O] = new ScanNext(scan)

	def nextSubclass[O :ClassTag] :Juncture[Any, O] = next(implicitly[ClassTag[O]].unapply)



	def last[I, O](pick :I=>Option[O]) :Juncture[I, O] = new Find[I, O](pick) with LastJuncture[I, O]

	def scanLast[I, O](scan :Stream[I]=>Stream[Option[O]]) :Juncture[I, O] = new ScanLast(scan)

	def lastSubclass[O :ClassTag] :Juncture[Any, O] = last(implicitly[ClassTag[O]].unapply)


	def apply[I, O](pick :I=>Option[O]) :Juncture[I, O] = new Find[I, O](pick)
//		{ in :Stream[I] => in.map(pick) } //:PassedJuncture[I, O]

	def scan[I, O](scan :Stream[I]=>Stream[Option[O]]) :Juncture[I, O] = new StreamMap[I, O](scan)

	def ofClass[O :ClassTag] :Juncture[Any, O] = apply(implicitly[ClassTag[O]].unapply)



	def group[I, O](pick :I=>Option[O]) :Juncture[I, Seq[O]] =
		new GroupedJuncture[I, O](_.map(pick))

	def scanGroup[I, O](scan :Stream[I]=>Stream[Option[O]]) :Juncture[I, Seq[O]] =
		new GroupedJuncture[I, O](scan)



	trait FirstJuncture[-I, +O] extends NextJuncture[I, O] {

		override protected def scan[H <: I, T](occurrences: (Stream[H]) => Stream[Option[T]]): Juncture[H, T] =
			new ScanFirst[H, T](occurrences)

		override def first = this
		override def next = new ScanNext(scanner)

		override def flatMap[H<:I, T](f :O=>Juncture[H, T]) :Juncture[H, T] = {
			in :Stream[H] =>
				def next(hs :Stream[H]=in, os :Stream[Option[O]]=scan(in), ts :Stream[Option[T]]=No.stream) :Stream[Option[T]] =
					if (os.isEmpty) ts
					else os.head match {
						case None => ts.head #:: next(hs.tail, os.tail, ts.tail)
						case Some(o) =>
							val inner = f(o).scan(hs)
							inner.head #:: next(hs.tail, os.tail, inner.tail)
					}
				next()
		} :FirstJuncture[H, T]


	}

	class ScanFirst[-I, +O](protected override val scanner :Stream[I]=>Stream[Option[O]]) extends FirstJuncture[I, O] {
		override def scan(in: Stream[I]): Stream[Option[O]] = scanner(in)
	}


	trait NextJuncture[-I, +O] extends Any with Juncture[I, O] {
		override protected def scan[H <: I, T](occurrences: (Stream[H]) => Stream[Option[T]]): Juncture[H, T] =
			new ScanNext(occurrences)

		override def next = this


		protected override def flatScan[H<:I, P>:O, T](successor :FlatScan[T, H, P])
		                                                (outer :Stream[Option[T]], outerOffset :Int)
		                                                (past :Stream[H], last :(Stream[H], Int))(present :Stream[H], offset :Int) :Stream[Option[P]] =
		{
			def next(trigger :Stream[Option[T]])(current :Stream[H], offset :Int, inner :Stream[Option[O]]) :Stream[Option[P]] =
				if (trigger.isEmpty)
					inner
				else trigger.head match {
					case None => inner.head #:: next(trigger.tail)(current.tail, offset+1, inner.tail)
					case Some(t) => successor(trigger, offset)(past, current->offset, current, offset)
				}

			def noes(input :Stream[H], pos :Int) :Stream[Option[P]] =
				if (pos==outerOffset) {
					val os = scan(input)
					os.head #:: next(outer.tail)(input.tail, pos+1, os.tail)
				} else
					None #:: noes(input.tail, pos+1)

			noes(present, offset)
		}

/*
		protected override def flatMapped[H<:I, P>:O, T](successor :T=>Juncture[H, P])
		                                       (outer :Stream[Option[T]], outerOffset :Int)
		                                       (past :Stream[H], last :(Stream[H], Int))(present :Stream[H], offset :Int) :Stream[Option[P]] =
		{
			def next(trigger :Stream[Option[T]])(current :Stream[H], offset :Int, inner :Stream[Option[O]]) :Stream[Option[P]] =
				if (trigger.isEmpty)
					inner
				else trigger.head match {
					case None => inner.head #:: next(trigger.tail)(current.tail, offset+1, inner.tail)
					case Some(t) => successor(t).flatMapped(successor)(trigger, offset)(past, current->offset)(current, offset)
				}

			def noes(input :Stream[H], pos :Int) :Stream[Option[P]] =
				if (pos==outerOffset) {
					val os = scan(input)
					os.head #:: next(outer.tail)(input.tail, pos+1, os.tail)
				} else
					None #:: noes(input.tail, pos+1)

			noes(present, offset)
		}
*/

	}

	class ScanNext[-I, +O](protected override val scanner :Stream[I]=>Stream[Option[O]]) extends NextJuncture[I, O] {
		override def scan(in: Stream[I]): Stream[Option[O]] = scanner(in)
	}



	trait LastJuncture[-I, +O] extends Juncture[I, O] {

		override protected def scan[H <: I, T](occurrences: (Stream[H]) => Stream[Option[T]]): Juncture[H, T] =
			new ScanLast(occurrences)

		override def last = this


		protected override def flatScan[H<:I, P>:O, T](successor :FlatScan[T, H, P])
		                                                (outer: Stream[Option[T]], outerOffset :Int)
		                                                (past: Stream[H], last: (Stream[H], Int))
		                                                (present :Stream[H], offset :Int) :Stream[Option[P]] =
		{
			def next(ts :Stream[Option[T]])(prev :(Stream[H], Int), current :Stream[H], offset :Int, inner :Stream[Option[O]]) :Stream[Option[P]] =
				if (ts.isEmpty) inner
				else ts.head match {
					case None => inner.head match {
						case None => None #:: next(ts.tail)(prev, current.tail, offset+1, inner.tail)
						case some => some #:: next(ts.tail)(current -> offset, current.tail, offset+1, inner.tail)
					}
					case Some(o) => successor(ts, offset)(past, prev, current, offset)
				}

			def catchup(last :(Stream[H], Int), current :Stream[H], offset :Int, inner :Stream[Option[O]]) :Stream[Option[P]] =
				if (offset < outerOffset) inner.head match {
					case None => None #:: catchup(last, current.tail, offset+1, inner.tail)
					case some => None #:: catchup((current, offset), current.tail, offset+1, inner.tail)
				} else inner.head match {
					case None => scan(last._1).head #:: next(outer.tail)(last, current.tail, offset+1, inner.tail)
					case some => some #:: next(outer.tail)(current->offset, current.tail, offset+1, inner.tail)
				}

			catchup(last, present, offset, scan(present))
		}


	}

	class ScanLast[-I, +O](protected override val scanner :Stream[I]=>Stream[Option[O]]) extends LastJuncture[I, O] {
		override def scan(in: Stream[I]): Stream[Option[O]] = scanner(in)
	}



	class FlatMapJuncture[-I, P, +O](parent :Juncture[I, P], fmap :P=>Juncture[I, O]) extends Juncture[I, O] {

		override def scan(in: Stream[I]): Stream[Option[O]] = {
			@tailrec def next(os :Stream[Option[P]]=parent.scan(in), gap :Int=0) :Stream[Option[O]] =
				if (os.isEmpty) Noes(gap)
				else os.head match {
					case None => next(os.tail, gap+1)
					case Some(o) => //fmap(o).flatMapped(fmap)(os, gap)(in, in -> 0)(in, 0)
						fmap(o).flattened(fmap)(os, gap)(in, in->0, in, 0)
				}
			next()
		}


	/*	override protected def flatScan[X <: I, Y >: O, T](k: FlatScan[T, X, Y])
		                                                  (outer: Stream[Option[T]], outerOffset: Int)
		                                                  (past: Stream[X], last: (Stream[X], Int))
		                                                  (present: Stream[X], offset: Int): Stream[Option[Y]] =
		{
			def continue(delimiters :Stream[Option[T]], start :Int)(in :Stream[X], last :(Stream[X], Int), current :Stream[X], pos :Int) :Stream[Option[Y]] =
				???
			@tailrec def next(os :Stream[Option[P]]=parent.scan(present), gap :Int=0) :Stream[Option[O]] =
				if (os.isEmpty) Noes(gap)
				else os.head match {
					case None => next(os.tail, gap+1)
					case Some(o) => //fmap(o).flatMapped(fmap)(os, gap)(in, in -> 0)(in, 0)
						fmap(o).flattened(fmap)(os, gap)(in, in->0, in, 0)
				}
			next()


			parent.flatScan[X, P, T]((_, _) => ???)(outer, outerOffset)(past, last)(present, offset)
		}*/

		override def flatMap[H <: I, T](f: (O) => Juncture[H, T]): Juncture[H, T] =
			new FlatMapJuncture(parent, fmap andThen (_.flatMap(f)))

		override def map[T](f: (O) => T): Juncture[I, T] = new FlatMapJuncture(parent, fmap andThen (_.map(f)))

		override def filter(condition: (O) => Boolean): Juncture[I, O] = new FlatMapJuncture(parent, fmap andThen (_.filter(condition)))

		override def foreach(op: (O) => Unit): Juncture[I, O] = new FlatMapJuncture(parent, fmap andThen (_.foreach(op)))

		override def ||[J <: I, T >: O](other: Juncture[J, T]): Juncture[J, T] = new FlatMapJuncture(parent, fmap andThen (_ || other))
	}

	class GroupedJuncture[-I, +O](items :Stream[I]=>Stream[Option[O]]) extends NextJuncture[I, Seq[O]] {

		override def scan(in: Stream[I]): Stream[Option[Seq[O]]] = {
			def fwd(os :Stream[Option[O]], acc :List[O]=Nil) :Stream[Option[Seq[O]]] =
				if (os.tail.isEmpty) Some((os.head ++: acc).reverse) #:: Stream.Empty
				else None #:: fwd(os.tail, os.head ++: acc)

			val os = items(in)
			if (os.isEmpty) Stream.Empty
			else fwd(os)
		}

		override protected def flatMapped[H <: I, P >: Seq[O], T](successor: (T) => Juncture[H, P])
		                                                         (outer: Stream[Option[T]], outerOffset: Int)
		                                                         (past: Stream[H], last: (Stream[H], Int))
		                                                         (present: Stream[H], offset: Int): Stream[Option[P]] =
		{
			def accumulate(trigger :Stream[Option[T]])(current :Stream[H], offset :Int, inner :Stream[Option[O]], acc :Seq[O]) :Stream[Option[P]] =
				if (trigger.isEmpty)
					Some(acc.reverse) #:: Noes[P](offset-outerOffset-1)
				else trigger.head match {
					case None => accumulate(trigger.tail)(current.tail, offset+1, inner.tail, inner.head ++: acc)
					case Some(t) =>
						Some((inner.head ++: acc).reverse) #:: Noes[P](offset-outerOffset-1) #:::
							successor(t).flatMapped(successor)(trigger, 0)(current, current->0)(current, 0)
				}

			def noes(input :Stream[H], pos :Int) :Stream[Option[P]] =
				if (pos==outerOffset) {
					val os = items(input)
					accumulate(outer.tail)(input.tail, pos+1, os.tail, os.head.toList)
				} else
					noes(input.tail, pos+1)

			noes(present, offset)
		}
	}




	class Find[-I, +O](mapper :I=>Option[O]) extends Juncture[I, O] {
		override def scanner = (_:Stream[I]).map(mapper)
		override def scan(in: Stream[I]): Stream[Option[O]] = in.map(mapper)
		override protected def head(in: Stream[I]): Option[O] = mapper(in.head)

/*
		override def map[T](f: (O) => T): Juncture[I, T] = new Find[I, T](mapper(_).map(f))

		override def filter(condition: (O) => Boolean): Juncture[I, O] = new Find[I, O](mapper(_).filter(condition))

		override def foreach(op: (O) => Unit): Juncture[I, O] = new Find[I, O](mapper(_).map { o => op(o); o })
*/

		override def toString = s"Find($mapper)"
	}

//	class FindFirst[-I, +O](collect :I=>Option[O]) extends Find[I, O](collect) with FirstJuncture[I, O] {
//
//	}

	private class StreamMap[-I, +O](protected override val scanner :Stream[I]=>Stream[Option[O]]) extends Juncture[I, O] {

		override def scan(in: Stream[I]): Stream[Option[O]] = scanner(in)

		override def passed: Juncture[I, O] = this
	}

	abstract class Scanner[-I, +O] {


		def toStream :Stream[Option[O]]
		def head :Option[O]
		def tail :Scanner[I, O]

		def continue[J<:I, U](tail :Scanner[J, U]) :Scanner[J, U] = tail

		def continueLast(last :Stream[I], lead :Int) :Scanner[I, O] = this
		def continueFlat[J<:I, U, P](inner: Scanner[J, P], outer: Scanner[J, U]): Scanner[J, O] = this
//		private[Scanner] def continueFrom[J<:I](stream :Stream[I]) :Scanner[J, O]
	}

	object Scanner {
		class StreamMapper[-I, +O](input :Stream[I], pick :I=>Option[O]) extends Scanner[I, O] {
			def toStream = input.map(pick)
			override def head = pick(input.head)
			override def tail = new StreamMapper(input.tail, pick)
		}

		class LastScanner[-I, +O](scan :Stream[I]=>Stream[Option[O]], val toStream :Stream[Option[O]], last :Stream[I], lead :Int=0) extends Scanner[I, O] {
			def head = toStream.head

			def tail = toStream.tail match {
				case t @ None #:: _ => new LastScanner(scan, t, last, lead+1)
				case t @ some #:: _ => new LastScanner(scan, t, last.drop(lead), 0)
				case empty => new LastScanner(scan, empty, last, lead+1)
			}

//			override def continue[J <: I, U](tail: Scanner[J, U]): Scanner[J, U] = tail.continueLast(last, lead)

			override def continueLast(last: Stream[I], lead: Int): Scanner[I, O] =
				if (toStream.isEmpty) new LastScanner(scan, toStream, last, lead)
				else if (scan(last).head.isDefined) new LastScanner(scan, toStream, last, lead)
				else this
		}

		class StreamScanner[+O](val toStream :Stream[Option[O]]) extends Scanner[Any, O] {
			override def head = toStream.head
			override def tail = new StreamScanner(toStream.tail)
		}

		class FlatMapScanner[-I, +P, +O](inner :Scanner[I, P], outer :Scanner[I, O], fmap :P=>Scanner[I, O]) extends Scanner[I, O] {
			def toStream = outer.toStream
			def head = outer.head
			def tail = inner.head match {
				case None => new FlatMapScanner(inner.tail, outer.tail, fmap)
				case Some(p) => new FlatMapScanner(inner.tail, outer continue fmap(p), fmap)
			}

			override def continue[J <: I, U](tail: Scanner[J, U]): Scanner[J, U] = tail.continueFlat[J, O, P](inner, outer)

			override def continueFlat[J <: I, U, T](inner: Scanner[J, T], outer: Scanner[J, U]): Scanner[J, O] =
				new FlatMapScanner[J, P, O](inner continue this.inner, outer continue this.outer, fmap)
		}
	}
/*
	abstract class Streamer[+O] {
		def toStream = this ++ Stream.Empty

		def ++[U>:O](stream :Stream[U]) :Stream[U]

		def ++[U>:O](stream :Streamer[U]) :Streamer[U]

		def ++:[U>:O](streamer :Streamer[U]) :Streamer[U]
	}

	object Streamer {
		case object Empty extends Streamer[Nothing] {

			override def toStream: Stream[Nothing] = Stream.Empty

			override def ++[U >: Nothing](stream: Stream[U]): Stream[U] = stream

			override def ++[U >: Nothing](stream: Streamer[U]): Streamer[U] = stream

			override def ++:[U >: Nothing](streamer: Streamer[U]): Streamer[U] = streamer
		}

		class ContinuedStreamer[+O](first :Streamer[O], second :Streamer[O]) extends Streamer[O] {
			override def toStream = first ++ second.toStream

			toStream #::: toStream

			override def ++[U >: O](stream: Stream[U]): Stream[U] = first ++ (second ++ stream)

			override def ++[U >: O](stream: Streamer[U]): Streamer[U] = ???

			override def ++:[U >: O](streamer: Streamer[U]): Streamer[U] = ???
		}
	}
	*/



	def Noes[T](length :Int) :Stream[Option[T]] =
		if (length<=0) Stream.Empty
		else None #:: Noes[T](length-1)

	case object No extends FirstJuncture[Any, Nothing] {
		final def stream = Stream.continually(None)

		def *[T](length :Int) :Stream[Option[T]] = stream.take(length)

		override def scan(in: Stream[Any]): Stream[Option[Nothing]] = in.map{ _ => None }

		override def toString = "No"
	}


	class AtStart[U](final protected val value :U) extends FirstJuncture[Any, U] {
		protected final val Mark = Some(value)

		override def scan(in: Stream[Any]): Stream[Option[U]] =
			if (in.isEmpty) Stream.Empty
			else Mark #:: No(in.tail) //.map { _ => Zero[U] }

		override def flatMap[H <: Any, T](f: (U) => Juncture[H, T]): Juncture[H, T] = f(value)

		override def map[T](f: (U) => T): Juncture[Any, T] = new AtStart(f(value))

		override def filter(condition: (U) => Boolean): Juncture[Any, U] =
			if (condition(value)) this
			else No

		override def toString = s"Start($value)"
	}

	case object AtStart extends AtStart[Unit](())




	class AtEnd[U](final protected val value :U) extends FirstJuncture[Any, U] {
		protected final val Mark = Some(value)

		override def scan(in: Stream[Any]): Stream[Option[U]] = in match {
			case h #:: t if t.isEmpty => Mark #:: Stream.Empty
			case h #:: t => None #:: scan(t)
			case _ => Stream.Empty
		}

		override def map[T](f: (U) => T): Juncture[Any, T] = new AtEnd(f(value))

		override def filter(condition: (U) => Boolean): Juncture[Any, U] =
			if (condition(value)) this
			else No

		override def toString = s"End($value)"
	}


	case object AtEnd extends AtEnd[Unit](())


	case object Index extends NextJuncture[Any, Int] {
		override def scan(in: Stream[Any]): Stream[Option[Int]] = {
			def rec(in :Stream[Any], index :Int) :Stream[Option[Int]] =
				if (in.isEmpty) Stream.empty
				else Some(index) #:: rec(in.tail, index+1)
			rec(in, 0)
		}

		override def toString = "Index"
	}

	trait Identity[T] extends Any with NextJuncture[T, T] {
		override def scan(in: Stream[T]): Stream[Option[T]] = in.map(Some.apply)

		override def toString = "Ident"
	}




	final case class <>[+L, +R] private[Juncture](rest :L, top :R) {
		def <>[O](top :O): L <> R <> O = new <>[L <> R, O](this, top)

		override def toString = s"$rest <> $top"
	}



	object <> {

		implicit def grant_<>[X](x :X) :Has_<>[X] = new Has_<>(x)

		class Has_<>[X](private val x :X) extends AnyVal {
			def <>[O](o :O) :X <> O = new <>(x, o)
		}

	}

	import <>.grant_<>

	class Conjunction[-I, +T, +H](tail :Juncture[I, T], head :Juncture[I, H]) extends Juncture[I, T<>H] {

//		override protected def scan[J <: I, O](occurrences: (Stream[J]) => Stream[Option[O]]): Juncture[J, O] =
//			head.scan(occurrences)

//		override def flatMap[J <: I, O](f: T<>H => Juncture[J, O]): Juncture[J, O] =
//			tail.flatMap{ t => head.flatMap { h => f(t<>h) } }


		override def scan(in: Stream[I]): Stream[Option[T<>H]] =
			zip(None, head.scan(in), None, tail.scan(in))


		final private[this] def zip(lastH :Option[H], heads :Stream[Option[H]], lastT :Option[T], tails :Stream[Option[T]]) :Stream[Option[T<>H]] =
			(heads, tails) match {
				case (None #:: hh, None #:: tt) =>  None #:: zip(lastH, hh, lastT, tt)

				case (h #:: hh, None #:: tt) =>
					(for { hi <- h; ti <- lastT} yield ti<>hi) #:: zip(h, hh, lastT, tt)

				case (None #:: hh, t #:: tt) =>
					(for { hi <- lastH; ti <- t} yield ti<>hi) #:: zip(lastH, hh, t, tt)

				case (h #:: hh, t #:: tt) =>
					(for { hi <- h; ti <- t} yield ti<>hi) #:: zip(h, hh, t, tt)

				case _ => Stream.Empty
			}


		override def toString = s"$head <> $tail"
	}



	class Tuple2Juncture[X, A, B](t :(Juncture[X, A], Juncture[X, B])) extends Juncture[X, (A ,B)] {

		override def scan(in: Stream[X]): Stream[Option[(A, B)]] =
			zip(None, t._1.scan(in), None, t._2.scan(in))

		final private[this] def zip(la :Option[A], as :Stream[Option[A]], lb :Option[B], bs :Stream[Option[B]]) :Stream[Option[(A, B)]] =
			(as, bs) match {
				case (None #:: ta, None #:: tb) => None #:: zip(la, ta, lb, tb)
				case (ha #:: ta, None #:: tb) => (for { b <- lb; a<-ha } yield (a, b)) #:: zip(ha, ta, lb, tb)
				case (None #:: ta, hb #:: tb) => (for { a <- la; b<-hb} yield (a, b)) #:: zip(la, ta, hb, tb)
				case (ha #:: ta, hb #:: tb) => (for { a <- ha; b <- hb } yield (a, b)) #:: zip(ha, ta, hb, tb)
			}


//		override protected def scan[H <: X, T](occurrences: (Stream[H]) => Stream[Option[T]]): Juncture[H, T] =
//			t._2.scan(occurrences)


//		override def flatMap[H <: X, T](f: ((A, B)) => Juncture[H, T]): Juncture[H, T] =
//			t._1.flatMap { _1 => t._2.flatMap { _2 => f(_1 -> _2) }}

	}



	implicit def tuple2[X, A, B](t :(Juncture[X, A], Juncture[X, B])) :Juncture[X, (A, B)] =
		new Tuple2Juncture[X, A, B](t)
//		{ in :Stream[X] =>
//			def rec(la :Option[A], as :Stream[Option[A]], lb :Option[B], bs :Stream[Option[B]]) :Stream[Option[(A, B)]] =
//				(as, bs) match {
//					case (None #:: ta, None #:: tb) => None #:: rec(la, ta, lb, tb)
//					case (ha #:: ta, None #:: tb) => (for { b <- lb; a<-ha } yield (a, b)) #:: rec(ha, ta, lb, tb)
//					case (None #:: ta, hb #:: tb) => (for { a <- la; b<-hb} yield (a, b)) #:: rec(la, ta, hb, tb)
//					case (ha #:: ta, hb #:: tb) => (for { a <- ha; b <- hb } yield (a, b)) #:: rec(ha, ta, hb, tb)
//				}
//			rec(None, t._1.scan(in), None, t._2.scan(in))
//		}


	implicit def tuple3[X, A, B, C](t :(Juncture[X, A], Juncture[X, B], Juncture[X, C])) :Juncture[X, (A, B, C)] =
		(t._1 <> t._2 <> t._3).map { case a <> b <> c => (a, b, c) }
//		for {
//			a <- t._1; b <- t._2; c <- t._3
//		} yield (a, b, c)

	implicit def tuple4[X, A, B, C, D](t :(Juncture[X, A], Juncture[X, B], Juncture[X, C], Juncture[X, D])) :Juncture[X, (A, B, C, D)] =
		(t._1 <> t._2 <> t._3 <> t._4) map { case a <> b <> c <> d => (a, b, c, d) }
//		for {
//			a <- t._1; b <- t._2; c <- t._3; d <- t._4
//		} yield (a, b, c, d)


	implicit def tuple5[X, A, B, C, D, E](t :(Juncture[X, A], Juncture[X, B], Juncture[X, C], Juncture[X, D], Juncture[X, E])) :Juncture[X, (A, B, C, D, E)] =
		(t._1 <> t._2 <> t._3 <> t._4 <> t._5) map { case a <> b <> c <> d <> e => (a, b, c, d, e) }
//		for {
//			a <- t._1; b <- t._2; c <- t._3; d <- t._4; e <- t._5
//		} yield (a, b, c, d, e)


}

