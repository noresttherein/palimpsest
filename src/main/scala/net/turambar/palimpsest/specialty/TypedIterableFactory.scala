package net.turambar.palimpsest.specialty

/**
  * @author Marcin Mościcki
  */
trait TypedIterableFactory[@specialized(Elements) E, +S <: FitIterable[E] with IterableSpecialization[E, S]] {
	val Empty :S
//	@inline final def empty :S = Empty
	def newBuilder :FitBuilder[E, S]

	def Singleton(value :E) :S = (newBuilder += value).result
}
