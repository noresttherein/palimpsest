package net.turambar.palimpsest

import scala.reflect.{ClassTag, classTag}

import org.scalacheck.Prop
import org.scalacheck.Prop._

/**
  * @author Marcin Mościcki
  */
package object testutil {
	
	/**
	  * @author Marcin Mościcki
	  */
	implicit class InspectionProps[X](x : =>X) {
		
		final def =?=[Y>:X](other :Y) :Prop = {
			val s = x
			((s:Y) ?= other) :| "l ?= r" &&
				((other =? s) :| "r =? l") &&
				((s.hashCode ?= other.hashCode) :| "hashCode" onFailure s"left:  $s" onFailure s"right: $other")
		}
		
		@inline final def throws[E<:Throwable :ClassTag] :Prop =
			throws(classTag[E].runtimeClass.asInstanceOf[Class[E]])
		
		@inline final def throws[E<:Throwable](ec :Class[E]) :Prop =
			Prop(Prop.throws(ec)(x)) :| s"throws ${ec.getName}"

		@inline final def passes[R](prop :R)(implicit asProp :R=>Prop) :Prop =
			prop.map { result => if (result.success) result else result.label(s"Fail: $x") }
		
	}
	
	
	
	implicit class PropExtension(private val prop :Prop) extends AnyVal {
		@inline final def onFailure(label : =>Any) :Prop =
			prop.map {
				result =>
					if (result.success) result
					else label match {
						case () => result
						case any => result.label(any.toString)
					}
			}
		
		/** Same as :|, but lazy and has lower precedence, eliminating some of the parenthesis. */
		@inline final def lbl(lb : =>Any) :Prop =
			prop.map { _.label(lb.toString) }
	}
	
	
}
