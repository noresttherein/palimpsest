package net.turambar.collection

import net.turambar.collection.Juncture.{AtStart, NextJuncture, No}
import shapeless.{::, HList, HNil}


object NilJuncture extends AtStart[HNil](HNil) {

	def ::[I, O](head :Juncture[I, O]) :HListJuncture[I, O, HNil] = new HListJuncture(head, this)

	class HListJuncture[-I, +H, +T<:HList](head :Juncture[I, H], tail :Juncture[I, T]) extends NextJuncture[I, H::T] {

		def ::[J<:I, O](other :Juncture[J, O]) :HListJuncture[J, O, H::T] = new HListJuncture(other, this)

		override def scan(in: Stream[I]): Stream[Option[H::T]] = {
			def rec(lastH :Option[H], heads :Stream[Option[H]], lastT :Option[T], tails :Stream[Option[T]]) :Stream[Option[H::T]] =
				(heads, tails) match {
					case (None #:: hh, None #:: tt) =>  None #:: rec(lastH, hh, lastT, tt)

					case (h #:: hh, None #:: tt) =>
						(for { hi <- h; ti <- lastT} yield hi::ti) #:: rec(h, hh, lastT, tt)

					case (None #:: hh, t #:: tt) =>
						(for { hi <- lastH; ti <- t} yield hi::ti) #:: rec(lastH, hh, t, tt)

					case (h #:: hh, t #:: tt) =>
						(for { hi <- h; ti <- t} yield hi::ti) #:: rec(h, hh, t, tt)

					case _ => Stream.Empty
				}
			rec(None, head.scan(in), None, tail.scan(in))
		}

		override def toString = s"$head::$tail"
	}


//	implicit def HListJuncture[I, H, T<:HList](implicit head :Juncture[I, H], tail :Juncture[I, T]) :HListJuncture[I, H::T] =
//		new HListJuncture

//	override def scan(in: Stream[Any]): Stream[Seq[HNil]] =
//		if (in.isEmpty) Stream.Empty
//		else One(HNil) #:: in.tail.map{ _ => Nil :Seq[HNil] }


	override def toString = "HNil"
}


object TupleJunctures {
	private val @@ = NilJuncture

	implicit def tuple2[I, A, B](x :(Juncture[I, A], Juncture[I, B])) :Juncture[I, (A, B)] =
		(x._1 :: x._2 :: @@).map { case a::b::HNil => (a, b) }

	implicit def tuple3[I, A, B, C](x :(Juncture[I, A], Juncture[I, B], Juncture[I, C])) :Juncture[I, (A, B, C)] =
		(x._1 :: x._2 :: x._3 :: @@) map { case a::b::c::HNil => (a, b, c) }

	implicit def tuple4[I, A, B, C, D](x :(Juncture[I, A], Juncture[I, B], Juncture[I, C], Juncture[I, D])) :Juncture[I, (A, B, C, D)] =
		(x._1 :: x._2 :: x._3 :: x._4 :: @@) map { case a::b::c::d::HNil => (a, b, c, d) }

	implicit def tuple5[I, A, B, C, D, E](x :(Juncture[I, A], Juncture[I, B], Juncture[I, C], Juncture[I, D], Juncture[I, E])) :Juncture[I, (A, B, C, D, E)] =
		(x._1 :: x._2 :: x._3 :: x._4 :: x._5 :: @@) map { case a::b::c::d::e::HNil => (a, b, c, d, e) }

	implicit def tuple6[I, A, B, C, D, E, F](x :(Juncture[I, A], Juncture[I, B], Juncture[I, C], Juncture[I, D], Juncture[I, E], Juncture[I, F])) :Juncture[I, (A, B, C, D, E, F)] =
		(x._1 :: x._2 :: x._3 :: x._4 :: x._5 :: x._6 :: @@) map { case a::b::c::d::e::f::HNil => (a, b, c, d, e, f) }
}
