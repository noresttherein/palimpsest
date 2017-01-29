package net.turambar.collection.specnaz

/**
  * @author Marcin Mościcki
  */
abstract class ValueLift[@specialized(Reified) -X, @specialized(Reified) +Y] {
	def apply(x :X) :Y
}


object ValueLift {
	type =^>[-X, +Y] = ValueLift[X, Y]

	@inline implicit final val ByteToShort :Byte =^> Short = x => x.toShort
	@inline implicit final val ByteToInt :Byte =^> Int = x => x.toInt
	@inline implicit final val ByteToLong :Byte =^> Long = x => x.toLong
	@inline implicit final val ByteToFloat :Byte =^> Float = x => x.toFloat
	@inline implicit final val ByteToDouble :Byte =^> Double = x => x.toDouble
	
	@inline implicit final val ShortToInt :Short =^> Int = x => x.toInt
	@inline implicit final val ShortToLong :Short =^> Long = x => x.toLong
	@inline implicit final val ShortToFloat :Short =^> Float = x => x.toFloat
	@inline implicit final val ShortToDouble :Short =^> Double = x => x.toDouble
	
	@inline implicit final val IntToLong :Int =^> Long = x => x.toLong
	@inline implicit final val IntToFloat :Int =^> Float = x => x.toFloat
	@inline implicit final val IntToDouble :Int =^> Double = x => x.toDouble
	
	@inline implicit final val LongToFloat :Long =^> Float = x => x.toFloat
	@inline implicit final val LongToDouble :Long =^> Double = x => x.toDouble
	
	@inline implicit final val FloatToDouble :Float =^> Double = x => x.toDouble	
	
}
