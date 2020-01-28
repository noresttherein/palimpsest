package net.turambar.palimpsest.specialty.seqs

import net.turambar.palimpsest.specialty.ItemTypes
import net.turambar.palimpsest.specialty.RuntimeType.ValueClass
import net.turambar.palimpsest.specialty.iterables.{FitIterable, MappedIterableTemplate}

/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
class ValueClassSeqProxy[@specialized(ItemTypes) V <: AnyVal, E](protected[this] override val source :FitSeq[V])
                                                               (implicit valueClass :ValueClass[V, E])
	extends ValSeq[E] with MappedIterableTemplate[V, E, ValSeq[E]]
{
	override protected[this] def forSource[@specialized(Boolean, Unit) O](f :E => O) :V => O =
		{ v => f(valueClass(v)) }

	override protected[this] def mine :V => E = valueClass(_)

	override protected[this] def at(idx :Int) :E = valueClass(source.get(idx))

	override def length :Int = source.length
}
