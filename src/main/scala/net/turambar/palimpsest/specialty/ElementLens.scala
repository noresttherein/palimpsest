package net.turambar.palimpsest.specialty

/** An extractor for an element from an internal collection entry, such as extracting the value from a BST node.
  * Used by various collection method implementations to decouple specialization for collection element type from
  * any specialization used by the implementation - the implementation of the method needs to be only specialized
  * for `T`, and does not need specialization for the key of a map for example, as it handles only the entry objects.
  *
  * @tparam E entry structure from a collection containing exactly one element.
  * @tparam T element type of the collection (or at least the parts of an entry that a collection method operates on).
  * @author Marcin Mościcki
  */
private[palimpsest] trait ElementLens[-E, @specialized(ItemTypes) +T] {
	def element(entry :E) :T
}
