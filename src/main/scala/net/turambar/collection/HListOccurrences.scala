package net.turambar.collection

import shapeless.{::, HList, HNil}


/** Combined occurrences of subfacts `H::T`, where a new occurence is reported whenever a new occurence of any element of `H::T`
  * is found, as defined by Occurrences instances combined in this instance. Each occurence contains the last occurence
  * of every subfact at the new occurence position. In other words, it is equivallent to mapping an input collection
  * into a list `H::T` containing last occurrences of each subfact of this instance in the prefix of the input collection ending
  * with the mapped element and removing 'duplicates' - occurrences where no new occurence is reported. Note that result collection
  * will still contain equal consecutive elements whenever a new occurence of a subfact is equal to the previous occurence of that
  * subfact.
  */
case class HListOccurrences[-I, +H, +T<:HList](head :Occurrences[I, H], tail :Occurrences[I, T]) extends Occurrences[I, H::T] {

	/** Reports an occurence of `X::H::T` whenever an occurence of `X`, `H` or a member of `T` is found.
	  * All remaining elements in returned `HList` contain the values of their last occurrences.
	  */
	def ::[X<:I, Y](other :Occurrences[X, Y]) :HListOccurrences[X, Y, H::T] = new HListOccurrences(other, this)


	override def scan(in: Stream[I]): Stream[Option[H::T]] = {
		def rec(hs :Stream[Option[H]], lastH :Option[H], ts :Stream[Option[T]], lastT :Option[T]) :Stream[Option[H::T]] =
			(hs, ts) match {
				case (None +: ohs, None +: ots) => None #:: rec(ohs, lastH, ots, lastT)

				case ((sh @ Some(h)) +: ohs, (st @ Some(t)) +: ots) => Some(h::t) #:: rec(ohs, sh, ots, st)

				case ((sh @ Some(h)) +: ohs, None +: ots) => lastT.map(h::_) #:: rec(ohs, sh, ots, lastT)

				case (None +: ohs, (st @ Some(t)) +: ots) => lastH.map(_::t) #:: rec(ohs, lastH, ots, st)

				case _ => Stream.empty
			}

		rec(head.scan(in), None, tail.scan(in), None)
	}


}

object HListOccurrences extends Occurrences[Any, HNil] {
	def ::[I, O](other :Occurrences[I, O]) :HListOccurrences[I, O, HNil] = new HListOccurrences(other, this)

	override def scan(in: Stream[Any]): Stream[Option[HNil]] =
		if (in.isEmpty) Stream.empty
		else Option(HNil) #:: in.tail.map(_ => None :Option[HNil])

	override def toString = "HNil"
}