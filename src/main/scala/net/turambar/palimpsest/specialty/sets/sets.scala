package net.turambar.palimpsest.specialty

package object sets {
	final val MemberTypes = ItemTypes
	final val LargeSetElements = new Specializable.Group((Short, Int, Long, Char, Float, Double))
	final val TrieSetElements = new Specializable.Group((Short, Int, Long, Char, Float, Double))


	private[sets] def intersection[@specialized(ItemTypes) E, R](set1 :ValSet[E], set2 :ValSet[E], builder :FitBuilder[E, R]) :R = {
		val it = set1.iterator
		while (it.hasNext) {
			val e = it.next()
			if (set2.contains(e))
				builder += e
		}
		builder.result()
	}


}
