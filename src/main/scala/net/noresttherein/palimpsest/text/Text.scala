package net.noresttherein.palimpsest.text

trait TextLike[+T <: TextLike[T]] extends CharSequence {
	def compact :T

	override def subSequence(start: Int, end: Int): T
}

/** A drop-in replacement for `String` and `StringLike` auto wrapper which allows polymorphic implementations,
  * such as those providing fast slicing and concatenation or optimized for searching.
  */
abstract class Text extends CharSequence with TextLike[Text] {

}



object Text {
	class TextChar(val toInt :Int) extends AnyVal {
		def toChar = toInt.toChar
//		override def toString = new String(Array(toInt))
	}

	def apply(value :String) :Text = new StringText(value)


}
