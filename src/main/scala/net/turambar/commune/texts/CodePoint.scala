package net.turambar.commune.texts

/**
  * @author Marcin Mościcki
  */
class CodePoint(val toInt :Int) extends AnyVal {
	def toChar = toInt.toChar
	def isU16 = (toInt & 0xffff0000) == 0
	
	override def toString =
		if (isU16) String.valueOf(toChar)
		else new String(Array((toInt >> 16).toChar, toInt.toChar))
}

object CodePoint extends (Int => CodePoint) {
	def apply(u32CharCode :Int) :CodePoint = new CodePoint(u32CharCode)
	
	def apply(char :Char) :CodePoint = new CodePoint(char.toInt)
	
	
	object EqualCodePoint {
		def unapply[T](any :T) :Option[Int] = any match {
			case x :Int => Some(x)
			case x :Char => Some(x.toInt)
			case x :CodePoint => Some(x.toInt)
			case x :Byte => Some(x.toInt)
			case x :Short => Some(x & 0xffff)
			case x :Long if (x & 0xffffffff00000000L) == 0L => Some(x.toInt)
			case _ => None
		}
	}
	
	
}
