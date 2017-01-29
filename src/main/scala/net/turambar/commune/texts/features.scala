package net.turambar.commune.texts



/**
  * @author Marcin Mościcki
  */
object features {
	implicit def stringComparability(self :String) :StringEqualsText = new StringEqualsText(self)
	implicit def stringComparability(self :Text) :TextEqualsString = new TextEqualsString(self)
	
	class StringEqualsText(private val self :String) extends AnyVal {
		@inline def ==(other :Text) :Boolean =
			self.isEmpty && other.isEmpty || (new StringText(self).iterator sameElements other.iterator)
		
		@inline def !=(other :Text) :Boolean = ! ==(other)
		
		@inline def compare(other :Text) :Int =
			if (self.length==0)
				if (other.isEmpty) 0 else -1
			else if (other.isEmpty) 1
			else new StringText(self).compare(other)
		
		@inline def <(other :Text) :Boolean =
			other.nonEmpty && (self.length==0 || new StringText(self) < other)
		
		@inline def >(other :Text) :Boolean =
			self.length > 0 && (other.isEmpty || new StringText(self) > other)
		
		@inline def <=(other :Text) :Boolean =
			self.length==0 || other.nonEmpty && new StringText(self) <= other
		
		@inline def >=(other :Text) :Boolean =
			other.isEmpty || self.length>0 && new StringText(self) >= other
	}
	
	class TextEqualsString(private val self :Text) extends AnyVal {
		@inline def ==(other :String) :Boolean =
			self.isEmpty && other.length==0 || self.equals(new StringText(other))
		
		@inline def !=(other :String) :Boolean =
			! ==(other)
		
		@inline def compare(other :String) :Int =
			if (self.isEmpty)
				if (other.length==0) 0 else -1
			else if (other.length==0) 1
			else self.compare(new StringText(other))
		
		@inline def <(other :String) :Boolean =
			other.length>0 && (self.isEmpty || self < new StringText(other))
		
		@inline def <=(other :String) :Boolean =
			self.isEmpty || other.length>0 && self <= new StringText(other)
		
		@inline def >(other :String) :Boolean =
			self.nonEmpty && (other.length==0 || self >= new StringText(other))
		
		@inline def >=(other :String) :Boolean =
			other.length==0 || self.nonEmpty && self >= new StringText(other)
		
	}
	
}