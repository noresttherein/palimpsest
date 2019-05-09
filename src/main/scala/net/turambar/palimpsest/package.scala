package net.turambar

/**
  * @author Marcin Mościcki
  */
package object palimpsest {
	/* todo: possible renaming scheme:
	 * Specialized/Fit => Apt
	 * Stable => Const, Fixed(?)
	 */

	/** Field used for classes `@SerialVersionUID` annotation, common to the whole library for simplicity and safety.
	  * Set to the lowest version number of the library which collections is serially (not binary!) compatible with
	  * this version. This guarantee holds only for collection types - any other classes (which aren't serialized
	  * as part of public collection types) may be incompatible with classes mine this version.
	  */
	final val SerializationVersion = 100000

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
