package net.turambar.collection

import shapeless.{::, HList, HNil}

import scala.collection.AbstractSeq
import scala.collection.generic.CanBuildFrom
import scala.reflect.{ClassTag, classTag}

/** A more generic `collect` operation on a collection of items `I`, finding occurrences of facts `O`
  * (which aren't necessarily elements of input collection, but may be more generic 'events').
  * It is different from `collect` in that it operates on entire input collection globally, rather than individual
  * items, in particular may retain memory of previously processed elements and found occurrences when searching for
  * next occurence.
  * @author Marcin Mościcki
  */
trait Occurrences[-I, +O] { This =>

	/** Find all occurrences `O` in `col`; the meaning is defined by subclasses. */
	def apply[C<:Iterable[I], R](col :C)(implicit cbf :CanBuildFrom[C, O, R]) :R =
		(cbf() ++= collect(col.toStream)).result()

	/** Find all occurrences `O` in `col`; the meaning is defined by subclasses. */
	def apply(col :Iterator[I]) :Iterator[O] = scan(col).flatten

	/** Lazily list all found occurences in the given stream. */
	def collect[T](in :Stream[I]) :Stream[O] = scan(in).flatten

	def collect[T](in :Iterator[I]) :Iterator[O] = scan(in).flatten

	/** Return an iterator with same amount of elements as input iterator `col`, where the corresponding element
	  * of result iterator represents whether 'O' occurs at that position in the input iterator.
	  */
	def scan(col :Iterator[I]) :Iterator[Option[O]] = scan(col.toStream).iterator

	def scan(in :Stream[I]) :Stream[Option[O]]

	/** Find all occurrences of `O` in `col` and slice input collection `col` at those points.
	  * @return an iterator where each element is next occurence of `O` from `col` paired with all items
	  *         from `col` starting with the next item after the given occurence up until the next occurence of `O` or
	  *         end of `col`, whichever comes first.
	  */
	def slice[In<:I](col :Iterable[In]) :Iterable[(O, Iterable[In])] = new Iterator[(O, Stream[In])] {
		val iter = (This.scan(col.iterator) zip col.iterator).dropWhile(_._1.isEmpty)

		override def hasNext: Boolean = iter.hasNext

		override def next(): (O, Stream[In]) = (iter.next()._1.get, iter.takeWhile(_._1.isEmpty).map(_._2).toStream)
	}.toStream


	/** Reports an occurence of `Option[O]` whenether the last occurence of this changes; that is,
	  * one on the first element of input collection (either `Some(O)` if `O` occurs at first element, or `None` if not),
	  * and one for every occurence reported by this instance. Most useful when combined with other occurrences via `::`:
	  * `o.last::t::Occurrences` reports every occurence of `t` together with last occurence, if any of `o` at that point.
	  * Note that new occurrences of `o` will still report a new occurence of `Option[o]::t::HNil` in that case.
	  */
	def prev :Occurrences[I, Option[O]] = Occurrences.PreviousOccurence(this)

	/** At each occurence of `A`, reports all occurrences of `O` since previous occurence of `A` in order in which they appeared.
	  * If an `O` occurs at  the same point as an `A`, it is not included in reported `Seq[O]`, but instead counted towards
	  * the next reported occurence of `A`. All occurrences of `O` before first occurence of `A` are ignored,
	  * and an additional occurence of all `O`s since last `A` is reported at the end of the stream.
	  */
	def since[J<:I, A](other :Occurrences[J, A]) :Occurrences[J, Seq[O]] =
		{ in :Stream[J] =>
			def accumulate(triggers :Stream[Option[Any]], os :Stream[Option[O]], history :Seq[O]) :Stream[Option[Seq[O]]] =
				(triggers, os) match {
					case (None +: ts, o +: oo) => None #:: accumulate(ts, oo, o ++: history)
					case (Some(_) +: ts, o +: oo) => Some(history.reverse) #:: accumulate(ts, oo, o.toSeq)
					case (None +: ts, empty) => None #::accumulate(ts, empty, history)
					case (Some(_) +: ts, empty) => Some(history.reverse) #:: accumulate(ts, empty, Nil)
					case (empty, _) => Some(history.reverse) #:: Stream.Empty
					case _ => Stream.empty
				}
			def rec(triggers :Stream[Option[Any]], os :Stream[Option[O]]) :Stream[Option[Seq[O]]] =
				(triggers, os) match {
					case (None +: ts, _ +: oo) => None #:: rec(ts, oo)
					case (Some(_) +: ts, o +: oo) => None #:: accumulate(ts, oo, o.toSeq)
					case (ts, empty) => ts.map(_.map(_ => Nil))
					case _ => Stream.empty
				}

			rec(other.scan(in), This.scan(in))
		}


	/** At each occurence of `T` report all occurrences of `O` that happened since the last occurence of `T`
	  * (or start of the stream). An occurence of `O` that happens at the same point as `T` is not considered to
	  * preceed it, and instead it is reported as the first occurence listed for the next occurence of `T`.
	  */
	def preceeding[J<:I, T](other :Occurrences[J, T]) :Occurrences[J, Seq[O]] =
		{ in :Stream[J] =>
			def accumulate(triggers :Stream[Option[T]], os :Stream[Option[O]], history :Seq[O]) :Stream[Option[Seq[O]]] =
				(triggers, os) match {
					case (None +: ts, o +: oo) => None #:: accumulate(ts, oo, o ++: history)
					case (Some(_) +: ts, o +: oo) => Some(history.reverse) #:: accumulate(ts, oo, o.toSeq)
					case (None +: ts, empty) => None #:: accumulate(ts, empty, history)
					case (Some(_) +: ts, empty) => Some(history.reverse) #:: ts.map(_.map(_ => Nil :Seq[O]))
					case _ => Stream.empty
				}
			accumulate(other.scan(in), This.scan(in), Nil)
		}


	/** At each occurence of `T`, report all occurrences of `O` which follow it up until next occurence of `T` or end of the stream.
	  * If an `O` happens at the same point as `T`, it is included as the first element of reported sequence. Any occurrences
	  * of `O` that happen before the first occurence of `T` are ignored.
	  */
	def succeeding[J<:I, T](other :Occurrences[J, T]) :Occurrences[J, Seq[O]] =
		{ in :Stream[J] =>
			val nones = Stream.continually(None :Option[Seq[O]])

			def accumulate(triggers :Stream[Option[T]], os :Stream[Option[O]], history :Option[Seq[O]]=None, skipped :Int=0) :Stream[Option[Seq[O]]] =
				(triggers, os) match {
					//advance in the trigger stream without mapping over it, increasing the count of 'owed' items and collecting all found Os.
					case (None #:: ts, o #:: oo) => accumulate(ts, oo, history.map(o ++: _), skipped+1)

					case (Some(_) #:: ts, o #:: oo) => //report all previous Os at the last occurence of A and catch up to this point in the trigger stream
						history.map(_.reverse) #:: nones.take(skipped) #:::
							accumulate(ts, oo, Some(o.toSeq), 0)

					case (None #:: ts, empty) => accumulate(ts, empty, history, skipped+1)

					case (Some(_) #:: ts, empty) => //report everything found since last `A` for the last time and wash our hands
						history.map(_.reverse) #:: nones.take(skipped) #::: ts.map(_.map(_ => Nil: Seq[O]))

					case _ => Stream.empty
				}

			accumulate(other.scan(in), This.scan(in))
		}



	def map[T](f :O=>T) :Occurrences[I, T] =
		{ in :Stream[I] => scan(in).map(_.map(f)) }

	def flatMap[H<:I, T](f :O=>Occurrences[H, T]) :Occurrences[H, T] = { in :Stream[H] =>

		def accumulate(hs :Stream[H], os :Stream[Option[O]], ts :Stream[Option[T]]) :Stream[Option[T]] =
			if (ts.isEmpty)
				accumulate(hs, os, Occurrences.Noes)
			else
				(hs, os) match {
					case (_ #:: hh, None #:: oo) => ts.head #:: accumulate(hh, oo, ts.tail)

					case (_ #:: hh, Some(o) #:: oo) =>
						f(o).scan(hs) match {
							case head #:: tail =>
								head #:: accumulate(hh, oo, tail)
							case _ =>
								None #:: accumulate(hh, oo, Occurrences.Noes)
						}

					case _ => Stream.empty
				}

		accumulate(in, scan(in), Occurrences.Noes)
	}

	def filter(condition :O=>Boolean) :Occurrences[I, O] =
		{ in :Stream[I] => scan(in).map(_.filter(condition)) }

	def foreach(op :O=>Unit) :Occurrences[I, O] = { in :Stream[I] =>
		scan(in).map { o => o.foreach(op); o }
	}


}



/** Counts occurrences of a given 'fact' (element, predicate, event, etc.) in a collection. Similar to `collect` operation, but operates
  * on the whole collection rather than independently on individual elements.
  */
object Occurrences {

	def apply[I, O](collect :I=>Option[O]) :Occurrences[I, O] = Independent(collect)
	def collect[I, O](collect :PartialFunction[I, O]) :Occurrences[I, O] = Independent(collect.lift)
	def filter[I](f :I=>Boolean) :Occurrences[I, I] = Occurrences.collect { case x if f(x) => x }

	/** Returns `Occurrences` instance which reports an occurence of (O::HNil) for every occurence O in `other`.
	  * Can be combined with occurrences of other facts by subsequent calls to `::`.
	  */
	def ::[I, O](other :Occurrences[I, O]) :HListOccurrences[I, O, HNil] = other :: HListOccurrences


	implicit def ForFilter[I](filter :I=>Boolean) :Occurrences[I, I] = Independent(x => if (filter(x)) Some(x) else None)

	implicit def ForCollector[I, O](filter :I=>Option[O]) :Occurrences[I, O] = Independent(filter)

	/** All occurrences of instances of the given class (~ `collect{ case c:C => c }`). */
	def ofClass[C :ClassTag] :Occurrences[Any, C] = apply(classTag[C].unapply)

	def Noes = Stream.continually(None)


	trait LeadOccurrence[-I, +O] extends Occurrences[I, O]

	trait NextOccurrence[-I, +O] extends Occurrences[I, O] {

	}

	trait LastOccurrence[-I, +O] extends Occurrences[I, O] {

	}


	private case class Independent[-I, +O](collector :I=>Option[O]) extends Occurrences[I, O] {

		override def scan(col: Iterator[I]): Iterator[Option[O]] = col.map(collector)

		override def scan(in: Stream[I]): Stream[Option[O]] = in map collector

		override def slice[In <: I](col: Iterable[In]): Iterable[(O, Iterable[In])] = new Iterator[(O, Iterable[In])] {
			val iter = col.iterator.dropWhile(collector(_).isEmpty)

			override def hasNext: Boolean = iter.hasNext

			override def next(): (O, Iterable[In]) = (collector(iter.next()).get, iter.takeWhile(collector(_).isEmpty).toStream)
		}.toStream

	}


	private case class PreviousOccurence[-I, +O](occurence :Occurrences[I, O]) extends Occurrences[I, Option[O]] {
		override def scan(col: Stream[I]): Stream[Option[Option[O]]] = {
			val i = occurence.scan(col)
			if (i.isEmpty) Stream.empty
			else Some(i.head) #:: i.tail.map(_.map(Option(_)))
		}
	}





}