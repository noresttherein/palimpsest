package net.turambar.palimpsest.specialty

import java.time.Instant
import java.util.Date

import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop._
import net.turambar.palimpsest.specialty.Specialize.{Self, SpecializeFor}
import scala.reflect._
import scala.runtime.BoxedUnit


/**
  * @author Marcin Mościcki
  */
object SpecializedSpec extends Properties("Specialized") {
	
	
	
	implicit class ClassComparator(private val self :Class[_]) extends AnyVal {
		def is[T](other :Class[T]) :Prop = self.asInstanceOf[Class[T]] ?= other
	}
	
	implicit class SpecializationProps[E](private val self :Specialized[E]) extends AnyVal {
		def is[T](other :Class[T]) :Prop =
			(self.runType is other) :| s"$self.toType is $other" &&
				(self.emptyArray.getClass.getComponentType is other) :| s"$self.array element is $other" &&
				(self.newArray(2).getClass.getComponentType is other) :| s"$self.newArray element is $other" &&
				(self.classTag.asInstanceOf[ClassTag[T]] ?= ClassTag(other)) :| s"$self.classTag is ClassTag($other)"
	}
	
	implicit class UnitSpecializationProps(private val self :Specialized[Unit]) extends AnyVal {
		private def other = classOf[Unit]
		private def box = classOf[BoxedUnit]
		def isUnit :Prop =
			(self.runType is other) :| s"$self.toType is $other)" &&
				(self.emptyArray.getClass.getComponentType is box) :| s"$self.array element is $box" &&
				(self.newArray(2).getClass.getComponentType is box) :| s"$self.newArray element is $box" &&
				(self.classTag.asInstanceOf[ClassTag[Unit]] ?= ClassTag(other)) :| s"$self.classTag is ClassTag($other)"
		
	}
	
	//sanity checks to verify scala performs AnyVal - java primitive conversion as we expect
	
	property("scala.Byte") = (Java.Byte is classOf[Byte]) && (Java.Byte is classTag[Byte].runtimeClass)
	property("scala.Short") = (Java.Short is classOf[Short]) && (Java.Short is classTag[Short].runtimeClass)
	property("scala.Int") = (Java.Int is classOf[Int]) && (Java.Int is classTag[Int].runtimeClass)
	property("scala.Long") = (Java.Long is classOf[Long]) && (Java.Long is classTag[Long].runtimeClass)
	property("scala.Char") = (Java.Char is classOf[Char]) && (Java.Char is classTag[Char].runtimeClass)
	property("scala.Float") = (Java.Float is classOf[Float]) && (Java.Float is classTag[Float].runtimeClass)
	property("scala.Double") = (Java.Double is classOf[Double]) && (Java.Double is classTag[Double].runtimeClass)
	property("scala.Boolean") = (Java.Boolean is classOf[Boolean]) && (Java.Boolean is classTag[Boolean].runtimeClass)
	property("scala.Unit") = (Java.Unit is classOf[Unit]) && (Java.Unit is classTag[Unit].runtimeClass)
	property("scala.Any") = (Java.Any is classOf[Any]) && (Java.Any is classTag[Any].runtimeClass)
	property("scala.AnyRef") = (Java.Any is classOf[AnyRef]) && (Java.Any is classTag[AnyRef].runtimeClass)
	
	/** All possible values, different from each other. */
	property("Specializations") = Specialized.Specializations.size == 10
	
	//	property("equals") = (Specialized[String] :Any) ?= (Specialized[Instant] :Any)
	
	/** Implicit accessor with explicit types. */
	
	property("apply[Byte]") = Specialized[Byte] is classOf[Byte]
	property("apply[Short]") = Specialized[Short] is classOf[Short]
	property("apply[Int]") = Specialized[Int] is classOf[Int]
	property("apply[Long]") = Specialized[Long] is classOf[Long]
	property("apply[Char]") = Specialized[Char] is classOf[Char]
	property("apply[Float]") = Specialized[Float] is classOf[Float]
	property("apply[Double]") = Specialized[Double] is classOf[Double]
	property("apply[Boolean]") = Specialized[Boolean] is classOf[Boolean]
	property("apply[Unit]") = Specialized[Unit].isUnit
	//	property("apply[Any]") = Specialized[Any] is classOf[AnyRef]
	property("apply[AnyRef]") = Specialized[AnyRef] is classOf[AnyRef]
	//	property("apply[String]") = Specialized[String] is classOf[AnyRef]
	
	property("apply[T :ClassTag]") = {
		def byClassTag[T :ClassTag] = Specialized[T]
		
		(byClassTag[Byte] is classOf[Byte]) &&
			(byClassTag[Short] is classOf[Short]) &&
			(byClassTag[Int] is classOf[Int]) &&
			(byClassTag[Long] is classOf[Long]) &&
			(byClassTag[Char] is classOf[Char]) &&
			(byClassTag[Float] is classOf[Float]) &&
			(byClassTag[Double] is classOf[Double]) &&
			(byClassTag[Boolean] is classOf[Boolean]) &&
			(byClassTag[Unit] isUnit) &&
			(byClassTag[Any] is classOf[Any]) &&
			(byClassTag[AnyRef] is classOf[AnyRef]) &&
			(byClassTag[String] is classOf[AnyRef])
	}
	
	def spec[@specialized T] :Specialized[T] = Specialized[T]
	
	property("apply[@specialized T]") = {
		(spec[Byte] is classOf[Byte]) &&
			(spec[Short] is classOf[Short]) &&
			(spec[Int] is classOf[Int]) &&
			(spec[Long] is classOf[Long]) &&
			(spec[Char] is classOf[Char]) &&
			(spec[Float] is classOf[Float]) &&
			(spec[Double] is classOf[Double]) &&
			(spec[Boolean] is classOf[Boolean]) &&
			(spec[Unit] isUnit) &&
			(spec[Any] is classOf[Any]) &&
			(spec[AnyRef] is classOf[AnyRef]) &&
			(spec[String] is classOf[AnyRef])
	}
	
	
	/** Local specialization context */
	
	import Specialized.specializedHere
	property("specializedHere[Byte]") = specializedHere[Byte] is classOf[Byte]
	property("specializedHere[Short]") = specializedHere[Short] is classOf[Short]
	property("specializedHere[Int]") = specializedHere[Int] is classOf[Int]
	property("specializedHere[Long]") = specializedHere[Long] is classOf[Long]
	property("specializedHere[Char]") = specializedHere[Char] is classOf[Char]
	property("specializedHere[Float]") = specializedHere[Float] is classOf[Float]
	property("specializedHere[Double]") = specializedHere[Double] is classOf[Double]
	property("specializedHere[Boolean]") = specializedHere[Boolean] is classOf[Boolean]
	property("specializedHere[Unit]") = specializedHere[Unit].isUnit
	property("specializedHere[Any]") = specializedHere[Any] is classOf[AnyRef]
	property("specializedHere[AnyRef]") = specializedHere[AnyRef] is classOf[AnyRef]
	property("specializedHere[String]") = specializedHere[String] is classOf[AnyRef]
	
	
	
	def whoIs[T :Specialized] = Specialized[T]
	def whoIs2[@specialized T] = Specialized[T]
	def erase[T] = Specialized[T]
	
	def erased[T] = whoIs[T] -> whoIs2[T]
	def special[@specialized T] = whoIs[T] -> whoIs2[T]
	def typeClass[T :Specialized] = whoIs[T] -> whoIs2[T]
	def both[@specialized T :Specialized] = whoIs[T] -> whoIs2[T]
	
	property("apply:no context") = (erased[Int] :Any) ?= (Specialized[Any] -> Specialized[Any])
	property("apply:@specialized context") = (special[Int] :Any) ?= (Specialized[Int] -> Specialized[Int])
	property("apply:context bound") = (typeClass[Int] :Any) ?= (Specialized[Int] -> Specialized[Any])
	property("apply:both") = (both[Int] :Any) ?= (Specialized[Int] -> Specialized[Int])
	property("apply:implicit overrides specialization") = (both[Int](erase[Int]) :Any) ?= (Specialized[Any] -> Specialized[Int])
	
	
	
	
	object Implicitly extends Specialize[Array] {
		override def specialized[@specialized E: Specialized]: Array[E] = Specialized[E].newArray(1).asInstanceOf[Array[E]]
	}
	
	object Locally extends Specialize[Specialized] {
		override def specialized[@specialized E: Specialized]: Specialized[E] = //Specialized.locally[E]
		{println(s"Locally[${Specialized.locally[E]}](${Specialized[E]}): ${Specialized.locally[E].key}"); Specialized.locally[E] }
	}
	
	property("Specialize[Byte]()") =
		(Implicitly[Byte]().getClass.getComponentType is classOf[Byte]) :| "arrayType" &&
			(Locally[Byte]() is classOf[Byte])
	
	property("Specialize[Short]()") =
		(Implicitly[Short]().getClass.getComponentType is classOf[Short]) :| "arrayType"  &&
			(Locally[Short]() is classOf[Short])
	
	property("Specialize[Int]()") =
		(Implicitly[Int]().getClass.getComponentType is classOf[Int]) :| "arrayType"  &&
			(Locally[Int]() is classOf[Int])
	
	property("Specialize[Long]()") =
		(Implicitly[Long]().getClass.getComponentType is classOf[Long]) :| "arrayType"  &&
			(Locally[Long]() is classOf[Long])
	
	property("Specialize[Float]()") =
		(Implicitly[Float]().getClass.getComponentType is classOf[Float]) :| "arrayType"  &&
			(Locally[Float]() is classOf[Float])
	
	property("Specialize[Double]()") =
		(Implicitly[Double]().getClass.getComponentType is classOf[Double]) :| "arrayType"  &&
			(Locally[Double] is classOf[Double])
	
	property("Specialize[Char]()") =
		(Implicitly[Char]().getClass.getComponentType is classOf[Char]) :| "arrayType"  &&
			(Locally[Char] is classOf[Char])
	
	property("Specialize[Boolean]()") =
		(Implicitly[Boolean]().getClass.getComponentType is classOf[Boolean]) :| "arrayType"  &&
			(Locally[Boolean]() is classOf[Boolean])
	
	property("Specialize[Unit]()") =
		(Implicitly[Unit]().getClass.getComponentType is classOf[BoxedUnit]) :| "arrayType"  &&
			(Locally[Unit]().isUnit)
	
	property("Specialize[AnyRef]()") =
		(Implicitly[AnyRef]().getClass.getComponentType is classOf[AnyRef]) :| "arrayType"  &&
			(Locally[AnyRef]() is classOf[AnyRef])
	
	property("Specialize[Any]()") =
		(Implicitly[Any]().getClass.getComponentType is classOf[AnyRef]) :| "arrayType"  &&
			(Locally[Any]() is classOf[Any])
	
	//	property("Specialize[String]()") =
	//		(Implicitly[String]().getClass.getComponentType is classOf[AnyRef]) :| "arrayType"  &&
	//		(Locally[String]() is classOf[AnyRef])
	
	class Spec[@specialized T](val arg :T)(implicit val passedSpec :Specialized[T]) {
		def mySpec = Specialized.specializedHere[T]
		
		override def equals(other :Any) = other match {
			case spec :Spec[_] => arg==spec.arg && passedSpec==spec.passedSpec && mySpec==spec.mySpec
			case _ => false
		}
		
		override def toString = s"Spec[$mySpec]($arg)($passedSpec)"
	}
	
	object ParamCall extends Specialize.With[Spec, Specialize.Self] {
		override def specialized[@specialized E](param: E)(implicit spec :Specialized[E]): Spec[E] = new Spec(param)(spec)
	}
	
	property("Specialize[Byte](_:Byte)") = ParamCall[Byte](42.toByte) ?= new Spec[Byte](42.toByte)
	property("Specialize[Short](_:Short)") = ParamCall[Short](42.toShort) ?= new Spec[Short](42.toShort)
	property("Specialize[Int](_:Int)") = ParamCall[Int](42.toInt) ?= new Spec[Int](42.toInt)
	property("Specialize[Long](_:Long)") = ParamCall[Long](42.toLong) ?= new Spec[Long](42.toLong)
	property("Specialize[Float](_:Float)") = ParamCall[Float](42.toFloat) ?= new Spec[Float](42.toFloat)
	property("Specialize[Double](_:Double)") = ParamCall[Double](42.toDouble) ?= new Spec[Double](42.toDouble)
	property("Specialize[Char](_:Char)") = ParamCall[Char](42.toChar) ?= new Spec[Char](42.toChar)
	property("Specialize[Any](_:Any)") = ParamCall[Any](42) ?= new Spec[Any](42)
	property("Specialize[AnyRef](_:AnyRef)") = ParamCall[AnyRef](new Date(42L)) ?= new Spec[AnyRef](new Date(42L))
	//	property("Specialize[String](_:String)") = (ParamCall[String]("42") :Any) ?= new Spec[AnyRef]("42")
	
	object AnyType extends SpecializeFor[Array] {
		override def forByte: Array[Byte] = new Array[Byte](1)
		override def forShort: Array[Short] = new Array[Short](1)
		override def forChar: Array[Char] = new Array[Char](1)
		override def forInt: Array[Int] = new Array[Int](1)
		override def forLong: Array[Long] = new Array[Long](1)
		override def forFloat: Array[Float] = new Array[Float](1)
		override def forDouble: Array[Double] = new Array[Double](1)
		override def forBoolean: Array[Boolean] = new Array[Boolean](1)
		override def forUnit: Array[Unit] = new Array[Unit](1)
		override def specialized[@specialized E: Specialized]: Array[E] = new Array[AnyRef](1).asInstanceOf[Array[E]]
	}
	
	property("SpecializeFor[Byte]()") = AnyType[Byte]().getClass.getComponentType is classOf[Byte]
	property("SpecializeFor[Short]()") = AnyType[Short]().getClass.getComponentType is classOf[Short]
	property("SpecializeFor[Int]()") = AnyType[Int]().getClass.getComponentType is classOf[Int]
	property("SpecializeFor[Long]()") = AnyType[Long]().getClass.getComponentType is classOf[Long]
	property("SpecializeFor[Float]()") = AnyType[Float]().getClass.getComponentType is classOf[Float]
	property("SpecializeFor[Double]()") = AnyType[Double]().getClass.getComponentType is classOf[Double]
	property("SpecializeFor[Char]()") = AnyType[Char]().getClass.getComponentType is classOf[Char]
	property("SpecializeFor[Boolean]()") = AnyType[Boolean]().getClass.getComponentType is classOf[Boolean]
	property("SpecializeFor[Unit]()") = AnyType[Unit]().getClass.getComponentType is classOf[BoxedUnit]
	property("SpecializeFor[AnyRef]()") = AnyType[AnyRef]().getClass.getComponentType is classOf[AnyRef]
	property("SpecializeFor[Any]()") = AnyType[Any]().getClass.getComponentType is classOf[AnyRef]
	property("SpecializeFor[String]()") = AnyType[String]().getClass.getComponentType is classOf[AnyRef]
	
}
