package net.turambar.palimpsest.specialty.seqs


import net.turambar.palimpsest.specialty.ItemTypes

/**
  * @author Marcin Mościcki
  */

/*
class ArrayBufferSection[@specialized(Elements) E](source :SharedArrayBuffer[E], private[this] var from :Int, private[this] var until :Int) extends SharedArrayBuffer[E] {
	protected def array = source.arr
	protected def array_=(a :Array[E]) = source.arr = a

	protected[seqs] def offset = from

	protected[seqs] def offset_=(o :Int) :Unit = {
		source.offset += o-from
		from = o
	}

	protected[this] def len = until-from
	protected[this] def len_=(l :Int) :Unit = {
		val delta = l - (until-from)
		source.len += delta
		until += delta
	}
}


*/
