package net.turambar.palimpsest.text

import net.turambar.palimpsest.text.StringText.StringSlice


class StringText(override val toString :String) extends Text {
	override def length(): Int = toString.length

	override def charAt(index: Int): Char = toString.charAt(index)

	override def subSequence(start: Int, end: Int): Text = new StringSlice(toString, start, end)

	def compact = this
}


object StringText {
	def apply(value :String) :StringText = new StringText(value)

	class StringSlice(source :String, start :Int, len :Int) extends Text {
		@inline override final def length = len

		@inline private[this] final def end = start + len

		@inline private[this] final def idx(index :Int) :Int =
			if (index < 0 || index >= length)
				throw new IndexOutOfBoundsException(index + "/" + len)
			else index - start


		override def charAt(index: Int): Char = source.charAt(idx(index))

		override def subSequence(start: Int, end: Int): Text =
			new StringSlice(source, idx(start), idx(end))

		def compact = new StringText(source.substring(start, end))

		override def toString = source.substring(start, end)
	}
}