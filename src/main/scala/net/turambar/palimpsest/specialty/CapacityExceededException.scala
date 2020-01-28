package net.turambar.palimpsest.specialty

/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
class CapacityExceededException(msg :String, cause :Throwable = null) extends RuntimeException(msg, cause)