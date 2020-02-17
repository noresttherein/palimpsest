package net.noresttherein.palimpsest.seqs

import net.noresttherein.palimpsest.ItemTypes
import net.noresttherein.palimpsest.RuntimeType.ValueClass
import net.noresttherein.palimpsest.iterables.{AptIterable, MappedIterableTemplate}

/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
class ValueClassSeqProxy[@specialized(ItemTypes) V <: AnyVal, E](protected[this] override val source :AptSeq[V])
                                                               (implicit valueClass :ValueClass[V, E])
	extends ValSeq[E] with MappedIterableTemplate[V, E, ValSeq[E]]
{
	override protected[this] def forSource[@specialized(Boolean, Unit) O](f :E => O) :V => O =
		{ v => f(valueClass(v)) }

	override protected[this] def mine :V => E = valueClass(_)

	override protected[this] def at(idx :Int) :E = valueClass(source.get(idx))

	override def length :Int = source.length
}
