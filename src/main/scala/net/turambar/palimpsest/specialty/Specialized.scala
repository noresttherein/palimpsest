package net.turambar.palimpsest.specialty

import java.util

import net.turambar.palimpsest.specialty.Specialized.{Erased, SpecializedKey, SpecializedInt, SpecializedLong, SpecializedFloat, SpecializedDouble, SpecializedBoolean, SpecializedUnit}

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{TypeTag, typeOf}
import scala.runtime.BoxedUnit

import net.turambar.palimpsest.specialty.Specialize.SpecializeDistinctly





/** Type class carrying information about specialization of its type parameter `E` and expands upon
  * the information provided by an appropriate [[ClassTag]].
  * `Specialized[T]`, for any type `T` attempts to best represent the type of `T` as it would appear in compiled byte code
  * of a generic class `G[T]` after erasure and any specialization of `T` is performed.
  * It serves as a common umbrella over scala specialization feature and information available from implicit [[ClassTag]]s,
  * (and, to a lesser extent, [[TypeTag]]s when available), allowing to call `@specialized` code from unspecialized,
  * force specialization where the compiler wouldn't normally generate specialized code, or even branch depending
  * on the type argument (for example, using different implementations for `Byte` and `Int`).
  *
  * As the name suggests, the focus of this class lies mainly on primitive types and enabling the option of providing
  * more efficient implementations of low-level computations while still retaining the flexibility of a generic interface.
  * In particular, the primary motivation was to have a common interface for working with arrays and allow
  * buffer-like structures to both use the most appropriate element type instead of upcasting everything to `AnyRef`
  * as [[scala.collection.mutable.ArrayBuffer]] does, and be aware about their cross-compatibility between separate instances.
  * So, while it provides more information than a [[ClassTag]] and attempts to be as efficient as possible,
  * it is not intended as a generic reflection feature or a guarantee of type safety.
  *
  * In general, this is a very tricky ground, due to several factors:
  *   - the duality of scala inbuilt 'AnyVal's, represented in runtime by both java primitives and their wrappers,
  *     which force any API to either become a leaky abstraction reflecting that duality, or 'lie', commit to
  *     potentially costly conversions, or risk casting errors - especially when dealing with arrays.
  *   - upper or lower bounds on type arguments which may affect the actual byte code signature of the method;
  *   - implementations of methods declared in more generic super types, which may have a more specialized erasure
  *     than the signature of the overriden method.
  *   - information lost during erasure, which still may cause conflicts such as with arrays (or other specialized code)
  *     with element type which is generic itself.
  *
  * While both scala compiler and `JVM` make their best attempt to hide this from user code by auto-boxing and un-boxing,
  * without being careful it is still possible to run into `ClassCastException`s, especially when working with arrays.
  *
  * An instance of [[Specialized[T] ]] may represent any runtime type assignable from `T`.
  * In particular, an instance of [[Specialized[Int] ]] (or any other inbuilt primitive)
  * may represent any possible runtime type of an expression evaluating to an `Int`:
  * `int`, `java.lang.Integer` or (erased) `java.lang.Object`. Instances representing different of these types
  * are not considered equal, despite their corresponding types being almost functionally equivalent in the bytecode.
  * For primitives, this is largely transparent - the code operating on any of these will be generally interchangeable
  * due to autoboxing. The situation is more complex for reference types, as the type hierarchy is unlimited
  * and there is no auto-conversion involved (other than identity conversion / downcasting).
  * This means there is no homomorphism between `E` and `Specialized[E]` regarding equality: both different instances of
  * `Specialized[E]` may be unequal and incompatible, and equality between `Specialized[E]` and `Specialized[F]`
  * confirms only that types `E` and `F` have identical representation in referenced contexts, but little
  * about their actual relationship - in particular, both specializations may refer to `AnyRef`.
  *
  * More specifically, two instances `Specialized[A]` and `Specialized[B]` are equal if and only if expressions of type `A` and `B`
  * would compile to the same type in the context in which the instances where obtained. Instances representing
  * different level of specialization for the same type are not considered equal, despite casting between them being usually
  * safe or even automatic.
  *
  * Instances of `Specialized[T]` can be either obtained from the companion object based on
  * a runtime class/`ClassTag`, `TypeTag` or existing specialization context.
  * Implicit values are provided directly for all value types (and several other common types), or created based
  * on any of the aforementioned sources implicitly available (in exactly that order).
  * The consequence of that fact is that the default value will always reflect the most specific representation
  * that can be determined and, in case of `AnyRef` subtypes, whenever actual information about the class is known
  * (in particular always when `T` is statically known), it will be used rather than the erased `AnyRef`,
  * which might not be most appropriate for the user, and implicit resolution will be slightly slower, usually requiring
  * creation of a new object wrapping that `ClassTag`. In cases where only root level specialization is of interest
  * (that is, if the type in question is a jvm primitive or `java.lang.Object`),
  * [[Specialized.SpecializedRoot]] instance may be requested instead.
  *
  * Implicitly provided `Specialized[T]` for all built-in value types will either represent
  * a corresponding java primitive or an erased and boxed value `Specialized[AnyRef]`,
  * depending on whether the actual type was known or erased in the context in which it was created.
  * So, `Specialized[Int]` and `Specialized[T]` (where `T` is a specialized type parameter which actual type argument was `Int`)
  * will represent java `int`; however `Specialized[T]` obtained inside an unspecialized for `Int` method `m[T]` will
  * represent boxing to `AnyRef` even if in runtime `T`'s type is `java.lang.Integer`.
  * The main purpose of this class is to differentiate in the code which specialized version of a method or class is
  * being executed:
  * {{{
  *     def typeName[@specialized T](t :T) = s"$t is a ${Specialized[T].typeName}"
  *
  *     println(typeName(1)) // "1 is a int"
  *     println(typeName(true)) //"true is a boolean"
  *     println(typeName("hamster")) //"hamster is a java.lang.Object"
  * }}}
  * or
  * {{{
  *     def intIsSpecial[@specialized(Int) T](t :T) =
  *         if (Specialized[T]==Specialized.SpecializedInt) s"hey, we got an int: ${Specialized[T].classTag}!"
  *         else s"something else: ${Specialized[T].classTag} :("
  *
  *     def any[T](t :T) = intIsSpecial(t)
  *     println(intIsSpecial(1)) // "hey, we got an int: Int"
  *     println(intIsSpecial(1.0))  //"something else: Object :("
  *     println(any(1)) //"something else: Object :("
  * }}}
  * Note that in the last case we lost specialization information because erased method `any` called the generic
  * version of `intIsSpecial`.
  *
  * A nifty secondary use case is the ability to invoke a specialized variant of a method even from the context where
  * actual type argument is erased in the byte code, by passing the type class along the call stack:
  * {{{
  *     def newArray[@specialized T] = Specialized[T].newArray(42).asInstanceOf[Array[T]]
  *
  *     object SpecArray extends SpecOp.Returning[String] {
  *         def specialized[@specialized T :Specialized] = newArray[T]
  *     }
  *
  *     //erased method without any specialization
  *     def unspecialized[T :Specialized](t :T) = SpecArray[T]()
  *
  *     println(unspecialized(1).getClass.getSimpleName) //int[]
  *     println(unspecialized(1.0).getClass.getSimpleName) //double[]
  *     println(unspecialized("hello").getClass.getSimpleName) //Object[]
  *     val strings = unspecialized("hello") //java.lang.ClassCastException: [Ljava.lang.Object; cannot be cast to [Ljava.lang.String;
  * }}}
  * Note that:
  *   1. method `unspecialized` is simply erased, no specialized variants are generated by the scala compiler
  *   2. method `newArray` doesn't require any implicit or explicit parameters, retrieving specialization information
  *      by `Specialized.apply[T]`
  *   3. `ClassCastException` is the consequence of erasure of the type parameter and cast in the `newArray` method
  *      which performs a (purely syntactic in this case) cast down from an erased array to array of the given type parameter.
  *
  * An implicit value `Specialized[T]` is always present for any concrete or abstract type `T`. If `T` is known to be
  * one of the specializable scala types in the point of resolution, a constant singleton for that primitive is returned.
  * If `T` is an abstract type or proper subclass of `AnyRef`, implicit resolution defaults to a lookup mechanism
  * which attempts to recognize if it is requested from a context where `T` is a specialized type parameter. Therefore
  * specialization context can be passed from method to method in two ways: either as a `Specialized` context bound
  * (type class-like implicit parameter), or by preserving `@specialized` annotation on the type.
  * As the implicit resolution algorithm searches local context before companion objects, any implicit parameter will override
  * default implicit values declared in [[Specialized$]]. This can lead to situations, where the two values aren't equal,
  * usually due to execution of non-specialized generic code on the call stack, while a `Specialized` instance is passed
  * as an implicit parameter. Consider:
  * {{{
  *     def whoIs[T :Specialized] = Specialized[T] //retrieve an instance from the implicit parameter
  *     def whoIs2[@specialized T] = Specialized[T] //retrieve an instance based on method specialization
  *     def erase[T] = Specialized[T]
  *
  *     def erased[T] = whoIs[T] -> whoIs2[T]
  *     def spec[@specialized T] = whoIs[T] -> whoIs2[T]
  *     def typeClass[T :Specialized] = whoIs[T] -> whoIs2[T]
  *     def both[@specialized T :Specialized] = whoIs[T] -> whoIs2[T]
  *
  *     println(erased[Int]) //"(Object, Object)"
  *     println(spec[Int]) //"(int, int)"
  *     println(typeClass[Int]) //"(int, Object)"
  *     println(both[Int]) //"(int, int)"
  *     println(both[Int](erase[Int])) //"(Object, int)"
  * }}}
  *
  * @tparam E any scala type, usually itself a type parameter of a generic method/class; this is not the final erased/unboxed type.
  * @see [[Specialize]]
  * @see [[net.turambar.palimpsest.specialty.Specialized$]]
  *
  * @author Marcin Mościcki
  */
@implicitNotFound(msg = "Cannot determine specialization of type $E. This is most likely a result of introduction of a conflicting implicit value for Specialized[$E]")
sealed trait Specialized[@specialized E] {

	/** Type to which generic methods for `E` are specialized, i.e. representation of `E` inside a method `m[E]`,
	  * after erasure and any specializations.
	  */
	type ErasedType

	/** Closest information about type `E` as represented in byte code. This is different from [[Specialized#RunType]]
	  * in that it doesn't necessarily represent erasure, but can be in particular any reference type.
	  */
	type RunType >: E

	/** Type to which values of `E` are boxed wherever a reference type is expected. For primitive types it is declared
	  * as their corresponding wrappers, for a reference type `E <: AnyRef` it is simply `E`.
	  */
	type BoxType <: AnyRef

	
/*
	@inline final implicit def cast(value :RunType) :E = value.asInstanceOf[E]
	
	@inline final implicit def box(value :E) :BoxType = value.asInstanceOf[BoxType]
	
	@inline final implicit def unbox(boxed :BoxType) :RunType = boxed.asInstanceOf[RunType]
	
	implicit def box(value :Array[E]) :Array[BoxType] =
		if (boxType.isAssignableFrom(runType))
			value.asInstanceOf[Array[BoxType]]
		else {
			val res = newBoxArray(value.length)
			Array.copy(value, 0, res, 0, value.length)
			res
		}
	
	implicit def unbox(value :Array[BoxType]) :Array[RunType] =
		if (runType.isAssignableFrom(boxType))
			value.asInstanceOf[Array[RunType]]
		else if (value.length==0)
             emptyArray
		else {
			val res = newArray(value.length)
			Array.copy(value, 0, res, 0, value.length)
			res
		}
*/
	/** The default value for type `E` as defined by the java spec. For primitive types,
	  * it would be some version of zero/`false`, for reference type the `null` value.
	  * `Unit` is represented by its scala singleton value.
	  */
	def Null :E
	
	/** Empty array which element type is the class representing `E` as par this level of specialization.
	  * This may be `E` itself, after unboxing (if `E` is specialized) or erasure (otherwise).
	  *
	  * @return `Array[Any]` (`Object[]`) if `E &lt:: AnyRef` or is an erased and boxed scala `AnyVal`, or a java array of the primitive
	  *        type corresponding to `E &lt:: AnyVal`, if `E` is known (either fully instantiated, or by being a specialized type parameter).
	  */
	def emptyArray :Array[RunType]

	/** An array storing `E` in its erased/specialized form.
	  * @return `Array[AnyRef]` or one of java's primitive arrays.
	  */
	def emptyErasedArray :Array[ErasedType]

	/** An array which can store any value of `E` after autoboxing.
	  * @return an array of some reference element type.
	  */
	def emptyBoxArray :Array[BoxType]

	/** Create an array of the given length which can be used to store values of type `E`. If `E` is known to be a built-in
	  * scala `AnyVal` instance (either because it's fully instantiated or a specialized type parameter in the context in which this instance was created),
	  * it is a corresponding java primitive array. If 'E' is erased, it is simply Array[AnyRef].
	  * In context where more type information is present (`E` is not fully erased), it may create an array for a specific,
	  * non-erased representation of `E`.
	  */
	@inline final def newArray(size :Int) :Array[RunType] = Array.ofDim[RunType](size)(classTag)
	
	@inline final def newErasedArray(size :Int) :Array[ErasedType] = Array.ofDim[ErasedType](size)(erasedClassTag)

	@inline final def newBoxArray(size :Int) :Array[BoxType] = Array.ofDim[BoxType](size)(boxClassTag)
	
	/** Class representing type parameter `E` in a specialized context. Only possible values are synthetic java
	  * classes for java primitives (i.e. `Integer.TYPE` and `Class[java.lang.Object]`.
	  */
	def runType :Class[RunType]
	
	def erasedType :Class[ErasedType]

	def boxType :Class[BoxType]
	
	/** Shortcut for `toType.getName`. */
	def className = runType.getName
	
	def erasedClassName = erasedType.getName
	
	def boxClassName = boxType.getName

	/** Scala name of the specialized type as returned by the corresponding `ClassTag`. */
	def typeName = classTag.toString
	
	def erasedTypeName = erasedClassTag.toString

	def boxTypeName = boxClassTag.toString
	
	/** `ClassTag` representing the type to which `E` is erased/specialized. */
	def classTag :ClassTag[RunType]
	
	def erasedClassTag :ClassTag[ErasedType]

	def boxClassTag :ClassTag[BoxType]
	
	/** Is this a specialization of `scala.Unit`, represented as java 'void' pseudo type? */
	@inline final def isUnit = runType == classOf[Unit]

	/** Is this a specialization of inbuilt scala 'AnyVal' type (corresponding to a java primitive)? */
	@inline final def isValueType = runType.isPrimitive

	/** Is `E` type  `AnyRef` itself (and not its subclass), i.e. is `E` assignable *from* `AnyRef`? */
	@inline final def isAnyRef = runType eq Specialized.AnyRefClass

	/** Is `E` represented in this context by a reference type (subtype of `AnyRef`), either due to being a reference type
	  * itself, or through autoboxing?
	  */
	@inline final def isRef = Specialized.AnyRefClass.isAssignableFrom(runType)

	/** Does this instance represent a type scala `Function1`'s arguments are specialized for? */
	@inline final def isFun1Arg = (this eq SpecializedInt) || (this eq SpecializedLong) || (this eq SpecializedFloat) || (this eq SpecializedDouble)

	/** Does this instance represent a type scala `Function1`'s return types are specialized for? */
	@inline final def isFun1Res =
		(this eq SpecializedInt) || (this eq SpecializedLong) || (this eq SpecializedFloat) ||
			(this eq SpecializedDouble) || (this eq SpecializedBoolean) || (this eq SpecializedUnit)


	@inline private[specialty] def call[R[X]](callback: Specialize[R])(implicit enforceSpecialization :Specialized[E]) :R[E] =
		callback.specialized(this)

	private[specialty] def call[R[X]](callback :SpecializeDistinctly[R])(implicit enforceSpecialization :Specialized[E]) :R[E]

	@inline private[specialty] final def call[R[X], P[X]](callback: Specialize.With[R, P])(param :P[E])(implicit enforceSpecialization :Specialized[E]) :R[E] =
		callback.specialized(param)(this)


	/** Two instances are guaranteed to be fully compatible if they both represent specializations of the same
	  * scala `AnyVal` type represented by the same underlying java primitive.
	  * As a result, if `Specialized[A] sameAs Specialized[B]`,
	  * than it is safe to cast values of `A` and `B` in both directions. Additionally, in a context where both
	  * `A` and `B` are directly specialized, such a cast shouldn't cause any autoboxing as both values should already
	  * be represented by the same java primitive. Note that no two instances representing usages of reference types
	  * are the same in this meaning, even a `Specialized[AnyRef]` with itself!
	  * This relation is strictly stronger than equality on `Specialized[_]`.
	  *
	  * @see [[Specialized#equals]]
	  */
	@inline final def sameAs(other :Specialized[_]) :Boolean = runType.isPrimitive && runType == other.runType
	
	@inline final def =:=(other :Specialized[_]) :Boolean = runType.isPrimitive && runType == other.runType

	/** Equates specialization of primitive types with their representations as autoboxed objects.
	  * @return `true` if `other` boxes to the same type this instance boxes to.
	  */
	def =%=(other :Specialized[_]) :Boolean = runType==other.runType || boxType==other.runType || other.boxType==boxType

	/** True if all values represented by `other` can be safely assigned to a variable of type represented by this instance (after any necessary autoboxing). */
	def <%<(other :Specialized[_]) :Boolean = other.runType.isAssignableFrom(runType) || other.boxType==runType || other.runType==boxType

	/** True if all values represented by `other` can be safely assigned to a variable of type represented by this instance. */
	def <:<(other :Specialized[_]) :Boolean = other.runType.isAssignableFrom(runType)

	/** Returns `other &lt;%&lt; this`. */
	def >%>(other :Specialized[_]) :Boolean = other <%< this

	/** Returns `other &lt;:&lt; this`. */
	def >:>(other :Specialized[_]) :Boolean = other <:< this

	/** Two instances are equal if and only if they represent same specialization of a generic method/class.
	  * This is a weaker relation than equality of corresponding type arguments E!
	  * Specializations for all `E &lt:: AnyRef` will be equal, even if they represent completely unrelated
	  * and disjoint types, thus comparing these instance doesn't give any type safety. On the other hand,
	  * specialization of a value class and erased ref will never be equal, even if the latter represents
	  * an erased generic interface of the same type/structure and java/scala autoboxing provided full runtime compatibility.
	  *
	  * @see [[Specialized#sameAs]]
	  */
	final override def equals(that :Any) = that match {
		case r :Specialized[_] => r.runType==runType
		case _ => false
	}
	
	
	private[specialty] def key :SpecializedKey[E]

	final override def hashCode = runType.hashCode


	final override def toString = s"@specialized($classTag)"
}





/** Implict `Specialized` value of third order in precedence, verifying specialization context of the caller.
  * As it will always yield a value, but possibly representing erased one despite programmer's intentions, it's worse
  * than any sure source of type information, but still better than nothing.
  */
abstract class FallbackSpecializedImplicit {

	/** Specialization resolution in the context of the caller. Implemented by [[Specialized#locally]] in the subclass.
	  * Returned instance reflects recognized runtime type of `E` in the reference point. If the call happens
	  * from within code specialized for type argument `E` (or `E` is statically known to be a scala value type),
	  * returned instance will carry information about the corresponding primitive. If `E` is erased, or known to be
	  * a reference type, returned instance represents scala `AnyRef`.
	  */
	@inline final implicit def specializedHere[@specialized E] :Specialized[E] = locally[E]

	/** Resolve local specialization information for type `E`. When called from code specialized for type `E`,
	  * either explicitly by the `@specialized` annotation, or one where `E` is fully instantiated,
	  * it will return an instance associated with the specialized type.
	  * Otherwise (including all `AnyRef` subtypes), a generic instance equal to `Specialized[Any]` is returned.
	  */
	def locally[@specialized E] :Specialized[E]


}


/** Implicit values for `Specialized` type class of secondary precedence, with lower reliability or efficiency than
  * dedicated values declared in [[Specialized$]].
  */
abstract class SecondarySpecializedImplicits extends FallbackSpecializedImplicit {

	/** Retrieve specialization information about type `T` from an implicitly available `TypeTag`.
	  * Implemented in subclaass by [[#ofType]].
	  */
	@inline final implicit def specializedType[T](implicit tpe :TypeTag[T]) :Specialized[T] = ofType[T]

	/** Retrieve specialization information for type `T` from an implicitly available `TypeTag`.
	  * 'TypeTag's are more annoying than `ClassTag`s, hence the latter has precedence, but we'll make do with what we have.
	  * Returned instance reflects best runtime type for type `T`, but not necessarily the one applicable
	  * to current context. If type `T` is abstract and not specialized, but a `TypeTag[T]` instance identifies
	  * it as a java primitive type, an instance for that primitive will be returned despite the fact that
	  * all values of `E` in that context might be erased and boxed in runtime. Don't mistake this for type safety!
	  */
	def ofType[T :TypeTag] :Specialized[T]
}


object Specialized extends SecondarySpecializedImplicits {
	import java.{lang=>j}
	
	
	/** An argument for `scala.specialized` annotation specializing for all primitives, including `Unit/void`.
	  * This is equivalent to unparameterized `@specialized`, but may be useful as a switch value.
	  */
	final val All = new Specializable.Group((Byte, Short, Int, Long, Char, Float, Double, Boolean, Unit))

	/** An argument for `scala.specialized` annotation specializing for all java primitives, excluding `Unit/void`. */
	final val Values = new Specializable.Group((Byte, Short, Int, Long, Char, Float, Double, Boolean))

	/** An argument for `scala.specialized` annotation specializing for all numeric value classes. */
	final val Numbers = new Specializable.Group((Byte, Short, Int, Long, Float, Double))

	/** Types `scala.Function1`s argument is specialized for. */
	final val Fun1 = new Specializable.Group((Int, Long, Float, Double))
	
	/** Types `scala.Function1` result type is specialized for. */
	final val Fun1Res = new Specializable.Group(Unit, Boolean, Int, Float, Long, Double)
	
	/** Result types `scala.Function1` is specialized for with the exception of `Unit`. */
	final val Fun1Vals = new Specializable.Group(Boolean, Int, Float, Long, Double)

	/** Types `scala.Function2`s arguments are specialized for. */
	final val Fun2 = new Specializable.Group((Int, Long, Double))
	
	/** Types `scala.Function2` result type is specialized for - same as `Fun1Res`. */
	final val Fun2Res = new Specializable.Group(Unit, Boolean, Int, Float, Long, Double)
	
	/** Result types `scala.Function2` is specialized for, except for `Unit` - same as `Fun1Vals`. */
	final val Fun2Vals = new Specializable.Group(Boolean, Int, Float, Long, Double)

	/** Element types `scala.Tuple2` is specialized for. */
	final val Tuple2Elem = new Specializable.Group(Int, Long, Double, Char, Boolean)


	

	/** Retrieves implicit specialization information for type 'E'. This is just a shortcut for `implicitly[Specialized[E]]`.
	  * Note that there should always be an implicit value for this parameter: if none is available in the local context,
	  * this factory is searched for a matching instance, picking a declared constant if `E` is a known specializable type,
	  * or defaulting to a lookup based on `@specialized` context. If `E` is abstract or not specializable, returned
	  * instance will equal `Specialized[Any]`.
	  *
	  * @tparam E possibly specialized type parameter of a generic method/class
	  * @return implicit value for `Specialized[E]` (which should always be available), falling back to a lookup verifying
	  *         real time type of 'E' in the place of invocation.
	  */
	@inline final def apply[E](implicit specialization :Specialized[E]) :Specialized[E] = specialization

	/** Return specialization type class instance specific to the given class.
	  * For classes representing java primitives (including `Unit/void`) the corresponding specialization constant is returned.
	  * For all others, a generic `Specialized[Any]` value is returned.
	  *
	  * @return one of instances defined in [[#Specializations]]
	  */
	final def ofClass[E](tpe :Class[E]) :Specialized[E] = (//ByClass(tpe).asInstanceOf[Specialized[E]]
		if (tpe.isPrimitive) tpe match {
			case j.Integer.TYPE => SpecializedInt
			case j.Byte.TYPE => SpecializedByte
			case j.Double.TYPE => SpecializedDouble
			case j.Long.TYPE => SpecializedLong
			case j.Character.TYPE => SpecializedChar
			case j.Boolean.TYPE => SpecializedBoolean
			case j.Float.TYPE => SpecializedFloat
			case j.Short.TYPE => SpecializedShort
			case j.Void.TYPE => SpecializedUnit
			case _ => new SpecializedRef[AnyRef](tpe.asInstanceOf[Class[AnyRef]]) //this is a weird case ...
		} else
			new SpecializedRef[AnyRef](tpe.asInstanceOf[Class[AnyRef]])

	).asInstanceOf[Specialized[E]]

	/** Usage of type `E` as a generic parameter in fully specialized context.
	  * @return one of primitive specializations or an instance representing erasure to `AnyRef`.
	  */
	final def erasedClass[E](tpe :Class[E]) :Erased[E] = (
		if (tpe.isPrimitive) tpe match {
			case j.Integer.TYPE => SpecializedInt
			case j.Byte.TYPE => SpecializedByte
			case j.Double.TYPE => SpecializedDouble
			case j.Long.TYPE => SpecializedLong
			case j.Character.TYPE => SpecializedChar
			case j.Boolean.TYPE => SpecializedBoolean
			case j.Float.TYPE => SpecializedFloat
			case j.Short.TYPE => SpecializedShort
			case j.Void.TYPE => SpecializedUnit
			case _ => SpecializedAnyRef
		} else
			SpecializedAnyRef
	).asInstanceOf[Erased[E]]
	
	/** Return specialization type class instance specific to the given class, based on an implicit `ClassTag`.
	  * Equivalent to [[ofClass]](classTag[E].runtimeClass).
	  * Note that, in context where `ClassTag[E]` is available implicitly, but `E` is an erased abstract type,
	  * returned instance will be based on that class tag and equal to the appropriate value class specialization
	  * for java primitives, despite values of `E` being autoboxed in that context.
	  *
	  * @tparam E type for which specialization should be resolved.
	  * @return an instance representing either one of java primitives or `java.lang.Object`.
	  */
	@inline final def of[E](implicit tpe :ClassTag[E]) :Specialized[E] =
		ofClass(tpe.runtimeClass).asInstanceOf[Specialized[E]]


	/** Equivalent to [[Specialized#erasedClass]]`(clazz)` for runtime class as defined by an implicit `ClassTag`. */
	def erased[E](implicit tpe :ClassTag[E]) :Erased[E] =
		erasedClass(tpe.runtimeClass).asInstanceOf[Erased[E]]
	
	
	/** Representation of any type as its autoboxed, erased form without any specialization or upper type bounds. */
	def generic[E] :Specialized[E] = SpecializedAnyRef.asInstanceOf[Specialized[E]]


	/** The best representation of static type `E` based on implicit type information
	  * once erasure is performed for reference types.
	  * @return an instance representing either a java primitive (including `void`), synthetic `Null`
	  *         or erasure/boxing (for custom value types) to `AnyRef`.
	  */
	final def ofType[E](implicit tpe :TypeTag[E]) :SpecializedExact[E] = {
		if (tpe.tpe <:< typeOf[AnyVal])
			if (tpe.tpe <:< typeOf[Int]) SpecializedInt
			else if (tpe.tpe <:< typeOf[Byte]) SpecializedByte
			else if (tpe.tpe <:< typeOf[Double]) SpecializedDouble
			else if (tpe.tpe <:< typeOf[Long]) SpecializedLong
			else if (tpe.tpe <:< typeOf[Char]) SpecializedChar
			else if (tpe.tpe <:< typeOf[Boolean]) SpecializedBoolean
			else if (tpe.tpe <:< typeOf[Float]) SpecializedFloat
			else if (tpe.tpe <:< typeOf[Short]) SpecializedShort
			else if (tpe.tpe <:< typeOf[Unit]) SpecializedUnit
			else SpecializedAnyRef //most likely custom value class which gets lifted to `AnyRef` anyway.
		else if (tpe.tpe <:< typeOf[Null]) SpecializedNull
		else SpecializedAnyRef
	}.asInstanceOf[SpecializedExact[E]]

	/** Yields the representation of type `E` in the caller's context after erasure and specialization. */
	@inline final def locally[@specialized E] :Erased[E] = {
		new SpecializedKey[E] match {
			case SpecializedAnyRef.key => SpecializedAnyRef
			case SpecializedInt.key => SpecializedInt
			case SpecializedByte.key => SpecializedByte
			case SpecializedDouble.key => SpecializedDouble
			case SpecializedLong.key => SpecializedLong
			case SpecializedChar.key => SpecializedChar
			case SpecializedBoolean.key => SpecializedBoolean
			case SpecializedFloat.key => SpecializedFloat
			case SpecializedShort.key => SpecializedShort
			case SpecializedUnit.key => SpecializedUnit
			case _ => SpecializedAnyRef
		}
	}.asInstanceOf[Erased[E]]




	/** Creates an array guaranteed to be able to hold values of type `E`, as it would appear in erased and specialized
	  * byte code. For inbuilt, specialized (by the implicit parameter) value classes a corresponding java primitive array
	  * is returned. For `AnyRef` subtypes, the actual class of the created array will be `[Object`. The downcast required
	  * to present it as `Array[E]` is erased, so any `ClassCastException`s will be delayed until the client code attempts
	  * to enforce its type to an actual concrete class.
	  * Note that it is still perfectly safe to call it if the array doesn't escape the context
	  * in which `E` is an erased type, or if `E` is a primitive.
	  *
	  * @param specialized specialization information about type `E`
	  */
	@inline final def erasedArray[E](implicit specialized :Specialized[E]) :Array[E] =
		specialized.emptyArray.asInstanceOf[Array[E]]

	/** Creates an array of the given size, guaranteed to be able to hold values of type `E`, as it would appear in erased and specialized
	  * byte code. For inbuilt, specialized (by the implicit parameter) value classes a corresponding java primitive array
	  * is returned. For `AnyRef` subtypes, the actual class of the created array will be `[Object`. The downcast required
	  * to present it as `Array[E]` is erased, so any `ClassCastException`s will be delayed until the client code attempts
	  * to enforce its type to an actual concrete class.
	  * Note that it is still perfectly safe to call it if the array doesn't escape the context
	  * in which `E` is an erased type, or if `E` is a primitive.
	  *
	  * @param specialized specialization information about type `E`
	  */
	@inline final def erasedArray[E](capacity :Int)(implicit specialized :Specialized[E]) :Array[E] =
		specialized.newArray(capacity).asInstanceOf[Array[E]]

	private final val AnyRefClass = classOf[AnyRef]
	private final val NothingClass = classOf[Nothing]
	private final val NullClass = classOf[Null]
	
	/** Maps all classes representable in `jvm` to their boxed representations. */
	final val BoxClass = Map[Class[_], Class[_]](
		classOf[Byte] -> classOf[java.lang.Byte],
		classOf[Short] -> classOf[java.lang.Short],
		classOf[Int] -> classOf[java.lang.Integer],
		classOf[Long] -> classOf[java.lang.Long],
		classOf[Char] -> classOf[java.lang.Character],
		classOf[Float] -> classOf[java.lang.Float],
		classOf[Double] -> classOf[java.lang.Double],
		classOf[Boolean] -> classOf[java.lang.Boolean],
		classOf[Unit] -> classOf[BoxedUnit]
	) withDefault identity[Class[_]]

	/** Maps all classes to their corresponding primitives (for java primitive boxes) or themselves (all other). */
	final val UnboxedClass = BoxClass.map { case (primitive, box) => box -> primitive } withDefault identity[Class[_]]

	/** Reverse map of java primitive boxes containing entries for all java primitives and `Unit`,
	  * mapping the box classes (i.e. `java.lang.Integer`) to synthetic classes representing actual primitive types (i.e. `Integer.TYPE`).
	  */
	final val PrimitiveClass = BoxClass.map{ case (primitive, box) => box -> primitive }

	/** Mapping from any `jvm` class to their representations in specialized generic methods.
	  * `classOf[V] -> classOf[V]` for all builtin scala value classes,
	  * `classOf[T] -> classOf[AnyRef]` for all others.
	  */
	final val SpecializationClass = Map[Class[_], Class[_]](
		classOf[Byte] -> classOf[Byte],
		classOf[Short] -> classOf[Short],
		classOf[Int] -> classOf[Int],
		classOf[Long] -> classOf[Long],
		classOf[Char] -> classOf[Char],
		classOf[Float] -> classOf[Float],
		classOf[Double] -> classOf[Double],
		classOf[Boolean] -> classOf[Boolean],
		classOf[Unit] -> classOf[Unit]
	) withDefaultValue classOf[Any]


	
	
	private def key[@specialized E] :SpecializedKey[E] = new SpecializedKey[E]
	private final val unspecializedKey = new SpecializedKey[AnyRef]
	
	
	
	/** Specialization for `Byte`. */
	implicit final val SpecializedByte :SpecializedPrimitive[Byte, j.Byte] =
		new SpecializedPrimitive[Byte, j.Byte](key[Byte], 0) {
			override private[specialty] def call[R[X]](callback: SpecializeDistinctly[R])(implicit enforceSpecialization: Specialized[Byte])  =
				callback.forByte
		}
		
	/** Specialization for `Short`. */
	implicit final val SpecializedShort :SpecializedPrimitive[Short, j.Short]  =
		new SpecializedPrimitive[Short, j.Short](key[Short], 0) {
			override private[specialty] def call[R[X]](callback: SpecializeDistinctly[R])(implicit enforceSpecialization: Specialized[Short])  =
				callback.forShort
		}
		
	/** Specialization for `Int`. */
	implicit final val SpecializedInt :SpecializedPrimitive[Int, j.Integer] =
		new SpecializedPrimitive[Int, j.Integer](key[Int], 0) {
			override private[specialty] def call[R[X]](callback: SpecializeDistinctly[R])(implicit enforceSpecialization: Specialized[Int])  =
				callback.forInt
		}
		 
	/** Specialization for `Long`. */
	implicit final val SpecializedLong  :SpecializedPrimitive[Long, j.Long] =
		new SpecializedPrimitive[Long, j.Long](key[Long], 0) {
			override private[specialty] def call[R[X]](callback: SpecializeDistinctly[R])(implicit enforceSpecialization: Specialized[Long])  =
				callback.forLong
		}
		
	/** Specialization for `Char`. */
	implicit final val SpecializedChar  :SpecializedPrimitive[Char, j.Character] =
		new SpecializedPrimitive[Char, j.Character](key[Char], 0) {
			override private[specialty] def call[R[X]](callback: SpecializeDistinctly[R])(implicit enforceSpecialization: Specialized[Char])  =
				callback.forChar
		}
		
	/** Specialization for `Float`. */
	implicit final val SpecializedFloat  :SpecializedPrimitive[Float, j.Float] =
		new SpecializedPrimitive[Float, j.Float](key[Float], 0) {
			override private[specialty] def call[R[X]](callback: SpecializeDistinctly[R])(implicit enforceSpecialization: Specialized[Float])  =
				callback.forFloat
		}
		
	/** Specialization for `Double`. */
	implicit final val SpecializedDouble  :SpecializedPrimitive[Double, j.Double] =
		new SpecializedPrimitive[Double, j.Double](key[Double], 0) {
			override private[specialty] def call[R[X]](callback: SpecializeDistinctly[R])(implicit enforceSpecialization: Specialized[Double])  =
				callback.forDouble
		}
		
	/** Specialization for `Boolean`. */
	implicit final val SpecializedBoolean  :SpecializedPrimitive[Boolean, j.Boolean] =
		new SpecializedPrimitive[Boolean, j.Boolean](key[Boolean], false) {
			override private[specialty] def call[R[X]](callback: SpecializeDistinctly[R])(implicit enforceSpecialization: Specialized[Boolean])  =
				callback.forBoolean
		}
		
	/** Specialization for `Unit` as java `void`. */
	implicit final val SpecializedUnit  :SpecializedPrimitive[Unit, BoxedUnit] =
		new SpecializedPrimitive[Unit, BoxedUnit](key[Unit], ()) { //todo - this is not really a primitive:
			override private[specialty] def call[R[X]](callback: SpecializeDistinctly[R])(implicit enforceSpecialization: Specialized[Unit])  =
				callback.forUnit
		}
		
	/** Specialization for `AnyRef` (and indirectly `Any` by promotion). */
	implicit object SpecializedAnyRef
		extends SpecializedRef[AnyRef] with SpecializedRoot[AnyRef]
	{
		override type RunType = AnyRef
		override type ErasedType = AnyRef
		
		override final def erasedType :Class[AnyRef] = AnyRefClass
		
		override final val key = new SpecializedKey[AnyRef]
	}

	/** Represents the synthetic type `Null` of null values. */
	implicit final val SpecializedNull :SpecializedExact[Null] =
		new SpecializedRef[Null]() with SpecializedExact[Null] {
			override type RunType = Null
		}
	


	/** Implicit specialization determined from an implicitly available `ClassTag[E]`. */
	@inline implicit final def specializedClass[E](implicit tpe :ClassTag[E]) :Specialized[E] = of[E]



	/** `Specialized` instances representing all possible method/class specialization (all primitives and erasure).
	  * `Specialization(Specialization[T])` is true for all concrete and abstract types `T`.
 	  */
	final val Specializations = Set[SpecializedRoot[_]](
		SpecializedByte,
		SpecializedShort,
		SpecializedInt,
		SpecializedLong,
		SpecializedChar,
		SpecializedFloat,
		SpecializedDouble,
		SpecializedBoolean,
		SpecializedUnit,
		SpecializedAnyRef
	)


	/**  Base trait describing context where type `T` is used as a generic type argument after erasure and specialization.
	  *  There will be an implicit value of `Erased[T]` for any type `T`, representing that upper bound.
	  *  This in particular includes all types which erase/specialize to themselves as denoted by [[SpecializedRoot]],
	  *  but also for example an `Erased[String]` denoting erased usage of `String`, while
	  *  `String` is not a 'root type' - there is no value for `SpecializedRoot[String]`.
	  */
	sealed trait Erased[@specialized T] extends Specialized[T] {
		type ErasedType = T
	}

	/** Provides access to representations of types after erasure and specialization. */
	object Erased {

		/** Fetches implicit value for [[Erased]]`[E]` representing the way it would be used
		  * in context of a generic call, after erasure or specialization. There will always be a value
		  * for every type, in the most generic scenario representing the complete erasure and boxing (for value types).
		  */
		@inline final def apply[E](implicit erasure :Erased[E]) :Erased[E] = erasure

		/** Represents erased usage of a reference type. */
		@inline implicit final def ErasedRef[E<:AnyRef] :Erased[E] = SpecializedAnyRef.asInstanceOf[Erased[E]]
		
		@inline implicit final def erasedClass[E :ClassTag] :Erased[E] = Specialized.erased[E]
	}

	/** Represents context in which type `T` erases/specializes to itself:
	  * this includes all primitive types, root `AnyRef` itself and `Null`.
	  * A more useful type in practice is its subtype [[SpecializedRoot]]
	  * which includes all the above except for the synthetic `Null` type.
	  */
	trait SpecializedExact[T] extends Specialized[T] {
		type RunType = T
	}

	/** Base trait for all types `T` which erase/specialize in a generic context to themselves.
	  * This includes all inbuilt value types when specialized as java primitives and the `AnyRef` type
	  * for the `java.lang.Object` itself.
	  */
	trait SpecializedRoot[T] extends Erased[T] with SpecializedExact[T]

	/** Base class for all instances representing a jvm` primitive type, including `void`. */
	sealed abstract class SpecializedPrimitive[@specialized E, B<:AnyRef] private[Specialized]
			(private[specialty] override final val key :SpecializedKey[E], val Null :E)
			(implicit val classTag :ClassTag[E])
		extends SpecializedExact[E] with SpecializedRoot[E] with Erased[E] with Specialized[E]//extends Specialized[E] explicitly for specialization
	{
		override type BoxType = B

		final val runType = classTag.runtimeClass.asInstanceOf[Class[E]]
		final val erasedType = runType
		final val boxType = BoxClass(runType).asInstanceOf[Class[B]]
		
		override final def erasedClassTag: ClassTag[E] = classTag
		override final val boxClassTag: ClassTag[B] = ClassTag(boxType)
		
		final val emptyArray = Array.empty[E]
		override final def emptyErasedArray: Array[E] = emptyArray
		override final val emptyBoxArray: Array[B] = Array.empty[B](boxClassTag)
	}


	/** Any representation of a reference type by a java/scala class specified by implicit `ClassTag[E]`.
	  * This includes special values of [[SpecializedAnyRef]] for erased types, [[SpecializedNull]] for
	  * the null value and any possible superclass of type `E`. In this sense, values with the same runtime type
	  * may be represented by many different instances of [[SpecializedRef]] representing different levels
	  * of erasure after taking type bounds into the equation, from full type information to `AnyRef`.
	  * @param classTag the class representing the static type assignable from `E`.
	  * @tparam E any scala type, usually itself a type parameter of a generic method/class; this is not the final erased/unboxed type.
	  */
	sealed class SpecializedRef[E >: Null <:AnyRef] private[Specialized] ()(implicit final val classTag :ClassTag[E])
		extends Specialized[E]
	{
		private[Specialized] def this(runClass :Class[E]) =
			this()(new ClassTag[E] { override def runtimeClass: Class[_] = runClass })

		
		type ErasedType = AnyRef
		type BoxType = E
		type RunType = E
		
		override def Null = null
		
		override def erasedClassTag: ClassTag[AnyRef] = implicitly[ClassTag[AnyRef]]
		
		override def boxClassTag: ClassTag[E] = classTag
		
		override def runType: Class[E] = classTag.runtimeClass.asInstanceOf[Class[E]]
		
		override def erasedType: Class[AnyRef] = AnyRefClass
		
		override def boxType: Class[E] = runType
		
		
		override def emptyArray: Array[E] = Array.empty[E]
		
		override def emptyErasedArray: Array[AnyRef] = Array.empty[AnyRef]
		
		override def emptyBoxArray: Array[E] = Array.empty[E]
		
		override private[specialty] def call[R[X]](callback: SpecializeDistinctly[R])(implicit enforceSpecialization: Specialized[E]) =
			callback.specialized[E](this)
		
		override private[specialty] def key = unspecializedKey.asInstanceOf[SpecializedKey[E]]
	}
	
	
	
	private[specialty] class SpecializedKey[@specialized E] {
		override def equals(that :Any) = that.getClass==getClass
		override def hashCode = getClass.hashCode

		def className = getClass.getName
		override def toString = className.substring(className.indexOf("$")+1)
	}
	

	


}
