package net.turambar.collection.specnaz

/**
  * @author Marcin Mościcki
  */
class GenericSpecSeq[E](private val backing :Seq[E], private val offset :Int, val length :Int) extends SpecSeq[E] {
	def this(backing :Seq[E]) = this(backing, 0, backing.length)

	override protected def subseq(from: Int, until: Int): SpecSeq[E] =
		new GenericSpecSeq[E](backing, from, until-from)

	override protected[this] def at(idx: Int): E = backing(offset+idx)

//	override def storageClass: Class[_] = classOf[AnyRef]
}
