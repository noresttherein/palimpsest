package net.turambar

/**
  * @author Marcin Mościcki
  */
package object palimpsest {
	/**
	  * @author Marcin Mościcki
	  */
	class LibraryError(msg :String, reason :Throwable) extends RuntimeException(msg) {
		def this(msg :String) = this(msg, null)
		def this() = this("Programming error - sorry!")
	}

	@inline final def libraryError(msg :String) :Nothing = throw new LibraryError(msg)
	@inline final def libraryError(msg :String, cause :Throwable) :Nothing = throw new LibraryError(msg, cause)

	private[palimpsest] class Found[T](val get :T) extends Exception {
		override def getMessage = s"found $get. If you see this there is an error somewhere..."
	}
}
