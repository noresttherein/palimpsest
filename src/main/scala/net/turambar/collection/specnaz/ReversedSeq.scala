package net.turambar.collection.specnaz




/**
  * @author Marcin Mościcki
  */
class ReversedSeq[@specialized(Reified) +E](private val original :SpecSeq[E]) extends SpecSeq[E] {
	@inline
	final override def length: Int = original.length

//	@inline
//	final override def storageClass: Class[_] = original.storageClass

//	@inline
//	final override protected def empty: SpecSeq[E] = emptyLike(original)

	@inline
	final override protected def subseq(from: Int, until: Int): SpecSeq[E] =
		new ReversedSeq(subseqOf(original, orgIdx(until), orgIdx(from)))
//		new ReversedSeq(original.subseq(orgIdx(until), orgIdx(from)))

	@inline
	final override protected[this] def at(idx: Int): E = original(orgIdx(idx))

	@inline
	final private[this] def orgIdx(idx :Int) = original.length -1 -idx
}



