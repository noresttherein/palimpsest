package net.turambar.collection

import scala.collection.{AbstractSeq, LinearSeq, breakOut, mutable}
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.Stream.StreamCanBuildFrom
import scala.reflect.ClassTag




trait Spy[-I, +O] {
	def apply[C<:Iterable[I], R](in :C)(implicit builders :CanBuildFrom[C, O, R]) :R = in match {
		case iter :Iterator[I] =>
			(builders(in) ++= collect(iter)).result()
		case _ => (builders(in) ++= collect(in.toStream)).result()
	}

	def collect(in :Iterator[I]) :Iterator[O] = snoop(in).flatten //collect { case Some(x) => x }
	def collect(in :Stream[I]) :Stream[O] = snoop(in).flatten //collect { case Some(x) => x }

	def snoop(in :Iterator[I]) :Iterator[Seq[O]] = snoop(in.toStream).iterator
	def snoop(in :Stream[I]) :Stream[Seq[O]]


	def ||[H<:I, T>:O](other :Spy[H, T]) :Spy[H, T] = { in :Stream[H] =>
		(snoop(in) zip other.snoop(in)) map {
			case (Seq(), ts) => ts
			case (os, _) => os
		}
	}

	def &&[H<:I, T>:O](other :Spy[H, T]) :Spy[H, T] = { in :Stream[H] =>
		(snoop(in) zip other.snoop(in)) map {
			case t if t._1.nonEmpty && t._2.nonEmpty => t._1 ++ t._2
			case t => Seq()
		}
	}

	def ++[H<:I, T>:O](other :Spy[H, T]) :Spy[H, T] = { in :Stream[H] =>
		(snoop(in) zip other.snoop(in)) map { t => t._1 ++ t._2 }
	}

	def map[H<:I, S](f :Juncture[I, O]=>Juncture[H, S]) :Spy[H, S] = ???
	def flatMap[H<:I, S](f :Juncture[I, O]=>Spy[H, S]) :Spy[H, S] = ???
	def filter(f :Juncture[I, O]=>Boolean) :Spy[I, O] = ???
}


object Spy {
	def apply[I, O](f :PartialFunction[I, O]) :Spy[I, O] = opt(f.lift)
	def opt[I, O](f :I=>Option[O]) :Spy[I, O] = new SpySeq({ i :I => f(i).toSeq })
	def seq[I, O](fmap :I=>Seq[O]) :Spy[I, O] = new SpySeq(fmap)

	def forClass[C :ClassTag] :Spy[Any, C] = opt(implicitly[ClassTag[C]].unapply _)

	def at[X] :Spy[X, X] = new Everything[X]



/*
	val x = for {
		n <- Spy.at[Note]

	}
*/



	private class SpySeq[-I, +O](fmap :I=>Seq[O]) extends Spy[I, O] {
		override def snoop(in :Iterator[I]) :Iterator[Seq[O]] = in.map(fmap)
		override def snoop(in: Stream[I]): Stream[Seq[O]] = in.map(fmap)
	}


	private class Everything[X] extends Spy[X, X] {
		override def snoop(in: Iterator[X]): Iterator[Seq[X]] = in.map(Seq(_))
		override def snoop(in: Stream[X]): Stream[Seq[X]] = in.map(Seq(_))
	}
}