package net.turambar.palimpsest.specialty

import java.util

import net.turambar.palimpsest.specialty
import net.turambar.palimpsest.specialty.Specialized.{Enforce, Erased, SpecializedBoolean, SpecializedDouble, SpecializedFloat, SpecializedInt, SpecializedLong, SpecializedUnit}

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{typeOf, TypeTag}
import scala.runtime.BoxedUnit
import net.turambar.palimpsest.specialty.Specialize.SpecializeIndividually





/** Type class describing the representation of type `T` in compiled byte code as known at the point of its implicit
  * summoning or inferred from an implicit `TypeTag` or `ClassTag`. Used in conjunction with generic classes and methods
  * to reflect run time information about their type parameters. Its primary function is providing the ability to
  * distinguish between specialized synthetic versions of a generic class or method marked as `@specialized E`
  * (with optional parameters to the annotation). Unlike `ClassTag`s and `TypeTag`s, an implicit value of this
  * class is available even in contexts where `E` is not known and represents the type after erasure and specialization.
  * In particular, it makes it possible for a generic class/trait/method to discover if it was instantiated/called
  * with one of the value types as the parameter and to which primitive java type it corresponds. In a generic, erased
  * context, `implicitly[Specialized[E]]` will denote an erased (and possibly boxed) type represented in the byte code as
  * `java.lang.Object` instances (and downcast at point of calling). This allows to pick a specific implementation
  * optimized for the given value type without passing a `ClassTag`, or to verify if two collections share the element type.
  * Lack of reliance on class tags makes it possible to integrate specialized collections with the generic standard
  * library which would be otherwise impossible due to established method signatures. For example, `ValSet[Byte]()`,
  * `ValSet[Int]()`, `ValSet[Double]()` all yield different implementations, while retaining the flexibility and
  * uniformity of the generic interface.
  *
  * While the focus is on the use with value types and their use with erased and specialized collections, an instance can,
  * potentially, represent any reference type, generalizing the concept to provide a uniform way for specifying the degree
  * of available static information about a given type. In particular, in contexts where `E` is fully instantiated
  * (i.e., the instance was for example obtained from `Specialized[String]`), its full class information is available to
  * represent lack of any type abstraction. Other reasons are the need for subclasses of generic, specialized types,
  * which are dedicated to a concrete reference type (as in `class StringSet extends ValSet[String]`), to be able
  * to correctly inform about the actual type argument, and a way to manually request a specific array type via same
  * interface.
  *
  * It thus serves as a common umbrella for information available via scala reflection, class tags, and local specialization.
  * This makes it more powerful, for example allowing to call specialized code from non-specialized based on this ype class,
  * which in turn can make generated specialized classes smaller by extracting the code actually referencing values of
  * this type from methods which can be implemented without this information. For example, the majority of collection
  * methods accepting a function working with their element type can be implemented in the generic form, deferring the
  * application point to a single, reusable specialized method.
  *
  * In general, however, relying on this class for type safety is a very tricky ground, due to the muddy relationship
  * between static and dynamic types of scala code:
  *   - the duality of scala inbuilt 'AnyVal's, represented in runtime by both java primitives and their wrappers,
  *     and thus two different `Class[_]` instances, which forces any API to either become a leaky abstraction,
  *     reflecting that duality, 'lie', committing to potentially costly conversions, or risk casting errors -
  *     especially when dealing with arrays.
  *   - upper or lower bounds on type arguments which may affect the actual byte code signature of the method are not available;
  *   - implementations of methods declared in more generic super types, which may have a more specialized erasure
  *     than the signature of the overridden method.
  *   - information lost during erasure, which still may cause conflicts such as with arrays (or other specialized code)
  *     with element type which is generic itself.
  *
  * While scala compiler and runtime make their best attempt to hide this from user code by auto-boxing and un-boxing,
  * without being careful it is still possible to run into `ClassCastException`s, especially when working with arrays.
  *
  * More formally, an instance of [[Specialized[T] ]] may represent any runtime type assignable from `T`, potentially
  * using autoboxing. For example, [[Specialized[Int]]]  may represent any possible runtime type of an expression
  * evaluating to `scala.Int`: `int`, `java.lang.Integer` or (erased) `java.lang.Object`. Instances representing each
  * of these types are not considered equal, despite their corresponding types being almost functionally equivalent
  * in the bytecode and `Int` lacking polymorphism. For primitives, this is largely transparent - the code operating
  * on any of these will be generally interchangeable due to autoboxing. The situation is more complex for reference types,
  * as the type hierarchy is unlimited and there is no auto-conversion involved (other than identity conversion / upcasting).
  * This means there is no homomorphism between `E` and `Specialized[E]` regarding equality: both different instances of
  * `Specialized[E]` may be unequal, being incompatible, and equality between `Specialized[E]` and `Specialized[F]`
  * confirms only that types `E` and `F` have identical representation in referenced contexts, but potentially little
  * about their actual relationship - in particular, both specializations may refer to `AnyRef`.
  *
  * Instances of `Specialized[T]` can be either obtained from the companion object based on a runtime class/`ClassTag`,
  * `TypeTag` or existing specialization context. Implicit values are provided directly for all value types
  * (and several other common types), or created based on any of the aforementioned sources implicitly available
  * (in exactly that order). The consequence of that fact is that the default value will always reflect the most
  * specific representation that can be determined and, in case of `AnyRef` subtypes, whenever actual information
  * about the class is known (in particular always when `T` is statically known), it will be used rather than the erased
  * `AnyRef`, which might not be most appropriate for the user, and implicit resolution will be slightly slower, usually
  * requiring creation of a new object wrapping that `ClassTag`. In cases where only root level specialization is of
  * interest (that is, if the type in question is a jvm primitive or `java.lang.Object`), [[Specialized.SpecializedRoot]]
  * instance may be requested instead.
  *
  * Implicitly provided `Specialized[T]` for all built-in value types will either represent
  * a corresponding java primitive or an erased and boxed value `Specialized[AnyRef]`,
  * depending on whether the actual type was known or erased in the context in which it was created.
  * Consider:
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
  * A secondary use case is the ability to invoke a specialized variant of a method even from the context where
  * actual type argument is erased in the byte code, by passing the type class along the call stack:
  * {{{
  *     def newArray[@specialized T] = Specialized[T].newArray(42).asInstanceOf[Array[T]]
  *
  *     object SpecArray extends Specialize[Array] {
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
  * @see [[Specialize]]  for calling specialized code from non-specialized context.
  * @see [[net.turambar.palimpsest.specialty.Specialized$]]
  *
  * @author Marcin Mościcki
  */
@implicitNotFound(msg = "Cannot determine specialization of type $E. This is most likely a result of introduction of a conflicting implicit value for Specialized[$E]")
sealed trait Specialized[@specialized E] {
//todo: rename to Specializable; Erased to Specialized; ErasedRef to Erased
	/** Type to which generic methods for `E` are specialized, i.e. representation of `E` inside a method `m[E]`,
	  * after erasure and any specializations. It is the scala alias for either `java.lang.Object` or one of java primitive types.
	  */
	type ErasedType

	/** Closest information about type `E` as represented in byte code. This is different from [[Specialized#ErasedType]]
	  * in that it doesn't necessarily represent erasure, but can be in particular any reference type.
	  *
	  * While in general values of `RunType` do not always conform to the type `E` denoted by this instance,
	  * the semantics of java generics and scala specialization as well as their runtimes with regard to autoboxing
	  * mean that usually the cast `(_ :RunType).asInstanceOf[E]` is safe as long as this instance was obtained in
	  * the context of casting. This is because either `RunType` and `E` are the same specialized value type and
	  * the cast is removed at compilation, or `E` is erased
	  */
	type RunType >: E

	/** Type to which values of `E` are boxed whenever a reference type is expected. For primitive types it is declared
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
	def default :E


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
	  * scala `AnyVal` instance (either because it's fully instantiated or a specialized type parameter in the context
	  * in which this instance was created), it is a corresponding java primitive array. If 'E' is erased, it is simply
	  * Array[AnyRef]. In context where more type information is present (`E` is not fully erased), it may create an array
	  * for a specific, non-erased superclass of `E`.
	  */
	@inline final def newArray(size :Int) :Array[RunType] = Array.ofDim[RunType](size)(classTag)

	/** The most generic array which can store elements of `E` without boxing in the context of this instance.
	  * @param size requested array length
	  * @return either one of java primitive arrays or `Array[AnyRef]`, based on specialization context for `E`.
	  */
	@inline final def newErasedArray(size :Int) :Array[ErasedType] = Array.ofDim[ErasedType](size)(erasedClassTag)

	/** A reference array (i.e. a subclass of `java.lang.Object[]`) most appropriate for storing reference version
	  * of `E`. The element type will be either the boxed value type (like `java.lang.Integer` or `java.lang.Boolean`),
	  * a specific class for instances obtained based on class tags, or `java.lang.Object` in fully erased contexts.
	  * @param size requested array length
	  */
	@inline final def newBoxArray(size :Int) :Array[BoxType] = Array.ofDim[BoxType](size)(boxClassTag)




	/** Class representing most specific information about type `E` at the point of obtaining this value. */
	def runType :Class[RunType]

	/** Class representing dynamic type for parameter `E` in a generic context after erasure and specialization.
	  * Only possible values are synthetic java classes for java primitives (i.e. `Integer.TYPE` and `Class[java.lang.Object]`.
	  */
	def erasedType :Class[ErasedType]

	/** For value types, the java class boxing this value. For reference types, equal to `runType`. */
	def boxType :Class[BoxType]


	/** Shorthand for `runType.getName`. */
	def className :String = runType.getName

	/** Shorthand for `erasedType.getName`. */
	def erasedClassName :String = erasedType.getName

	/** Shorthand for `boxType.getName`. */
	def boxClassName :String = boxType.getName


	/** Scala name of the runtime type of `E` as returned by the `ClassTag` corresponding to the `runType` class. */
	def typeName :String = classTag.toString

	/** Scala name of the type specialized for `E`, as returned by the `ClassTag` corresponding to the `erasedType` class. */
	def erasedTypeName :String = erasedClassTag.toString

	/** Scala name of the runtime type of the reference type for `E`, as returned by the `ClassTag` corresponding to the `boxType` class. */
	def boxTypeName :String = boxClassTag.toString

	/** `ClassTag` representing the type used in the bytecode to represent values of `E` in place of obtaining this instance. */
	implicit def classTag :ClassTag[RunType]

	/** `ClassTag` representing the type used in the bytecode to represent values of `E` in contexts where `E` is an unbound,
	  * but possibly specialized, type parameter of a generic type.
	  */
	def erasedClassTag :ClassTag[ErasedType]

	/** `ClassTag` representing the type to which `E` is boxed wherever a reference type is required (such as type parameters
	  * for generic, non specialized types).
	  */
	def boxClassTag :ClassTag[BoxType]



	/** Is this a specialization of `scala.Unit`, represented as java 'void' pseudo type? */
	@inline final def isUnit :Boolean = runType == classOf[Unit]

	/** Is this a specialization of inbuilt scala 'AnyVal' type (corresponding to a java primitive)? */
	@inline final def isValueType :Boolean = runType.isPrimitive

	/** Is `E` type  `AnyRef` itself (and not its subclass), i.e. is `E` assignable *from* `AnyRef`? */
	@inline final def isAnyRef :Boolean = runType eq Specialized.AnyRefClass

	/** Is `E` represented in this context by a reference type (subtype of `AnyRef`), either due to being a reference type
	  * itself, or through autoboxing?
	  */
	@inline final def isRef :Boolean = Specialized.AnyRefClass.isAssignableFrom(runType)

	/** Is all type information about `E` erased and its values are represented as instances of `java.lang.Object`? */
	@inline def isErased :Boolean = false

	/** Does this instance represent a type scala `Function1`'s arguments are specialized for? */
	@inline final def isFun1Arg :Boolean = (this eq SpecializedInt) || (this eq SpecializedLong) || (this eq SpecializedFloat) || (this eq SpecializedDouble)

	/** Does this instance represent a type scala `Function1`'s return types are specialized for? */
	@inline final def isFun1Res :Boolean =
		(this eq SpecializedInt) || (this eq SpecializedLong) || (this eq SpecializedFloat) ||
			(this eq SpecializedDouble) || (this eq SpecializedBoolean) || (this eq SpecializedUnit)




	private[specialty] def call[R[X]](callback: Specialize[R])(implicit force :Enforce[E]) :R[E] =
		callback.specialized(this)

	private[specialty] final def call[R[X], P[X]](callback: Specialize.With[R, P])(param :P[E])(implicit force :Enforce[E]) :R[E] =
		callback.specialized(param)(this)

	private[specialty] final def call[R[X], P1[X], P2[X]](callback :Specialize.With2[R, P1, P2])
	                                                     (param1 :P1[E], param2 :P2[E])(implicit force :Enforce[E]) :R[E] =
		callback.specialized(param1, param2)(this)


	private[specialty] def call[R[X]](callback :SpecializeIndividually[R])(implicit force :Enforce[E]) :R[E]



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

	/** Same as [[#sameAs]]. */
	@inline final def =:=(other :Specialized[_]) :Boolean = runType.isPrimitive && runType == other.runType


	/** Equates specialization of primitive types with their representations as autoboxed objects.
	  * @return `true` if `other` boxes to the same type this instance boxes to.
	  */
	def =%=(other :Specialized[_]) :Boolean = runType==other.runType || boxType==other.runType || other.boxType==boxType

	/** True if all values of `E` can be safely assigned to a variable represented by `other`, perhaps including boxing by
	  * scala run time.
	  */
	def <%<(other :Specialized[_]) :Boolean = other.runType.isAssignableFrom(runType) || other.boxType==runType || other.runType==boxType

	/** True if all values of `E` are directly assignable to variables defined by other. */
	def <:<(other :Specialized[_]) :Boolean = other.runType.isAssignableFrom(runType)

	/** Returns `other &lt;%&lt; this`. */
	def >%>(other :Specialized[_]) :Boolean = other <%< this

	/** Returns `other &lt;:&lt; this`. */
	def >:>(other :Specialized[_]) :Boolean = other <:< this

	/** Two instances are equal if and only if they represent same specialization of a generic method/class.
	  * This is a weaker relation than equality of corresponding type arguments E!
	  * Specializations for all `E &lt;: AnyRef` will be equal, even if they represent completely unrelated
	  * and disjoint types, thus comparing these instance doesn't give any type safety. On the other hand,
	  * specialization of a value class and erased ref will never be equal, even if the latter represents
	  * an erased generic interface of the same type/structure and java/scala autoboxing provided full runtime compatibility.
	  *
	  * @see [[Specialized#sameAs]]
	  */
	final override def equals(that :Any) :Boolean = that match {
		case r :Specialized[_] => r.runType==runType
		case _ => false
	}


	protected[specialty] def key :Enforce[E]

	final override def hashCode :Int = runType.hashCode


	override def toString = s"@specialized($classTag)"
}








/** Implicit `Specialized` value of third order in precedence, verifying specialization context of the caller.
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
	  * Implemented in subclaass by [[#asType]].
	  */
	@inline final implicit def specializedType[T](implicit tpe :TypeTag[T]) :Specialized[T] = asType[T]

	/** Retrieve specialization information for type `T` from an implicitly available `TypeTag`.
	  * 'TypeTag's are more annoying than `ClassTag`s, hence the latter has precedence, but we'll make do with what we have.
	  * Returned instance reflects best runtime type for type `T`, but not necessarily the one applicable
	  * to current context. If type `T` is abstract and not specialized, but a `TypeTag[T]` instance identifies
	  * it as a java primitive type, an instance for that primitive will be returned despite the fact that
	  * all values of `E` in that context might be erased and boxed in runtime. Don't mistake this for type safety!
	  */
	def asType[T :TypeTag] :Specialized[T]
}






object Specialized extends SecondarySpecializedImplicits {
	import java.{lang=>j}


	/** An argument for `scala.specialized` annotation specializing for all primitives, including `Unit/void`.
	  * This is equivalent to unparameterized `@specialized`, but may be useful as a switch value.
	  */
	final val All = new Specializable.Group((Byte, Short, Int, Long, Char, Float, Double, Boolean, Unit))

	/** An argument for `scala.specialized` annotation specializing for all java primitives, excluding `Unit/void`. */
	final val Primitives = new Specializable.Group((Byte, Short, Int, Long, Char, Float, Double, Boolean))

	/** An argument for the `@specialized` annotation specializing for all primitives except `Boolean` (and `Unit`). */
	final val MultiValue = new Specializable.Group((Byte, Short, Int, Long, Char, Float, Double))

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



	/** Return specialization type class instance which uses the given class as the runtime class. This represents the case
	  * where no static information is lost except for potential type parameters of `E` if it is a generic type itself.
	  * For classes representing java primitives (including `Unit/void`) the corresponding specialization constant is returned.
	  * For reference types a `SpecializedRef` instance wrapping the given class is returned.
	  * Custom value classes are likewise represented by their lifted reference type.
	  * @return a `Specialized` instance which `runType` equals the given class.
	  */
	final def asClass[E](tpe :Class[E]) :SpecializedExact[E] = (//ByClass(tpe).asInstanceOf[Specialized[E]]
		if (tpe.isPrimitive) tpe match {
			case j.Integer.TYPE => SpecializedInt
			case j.Double.TYPE => SpecializedDouble
			case j.Long.TYPE => SpecializedLong
			case j.Byte.TYPE => SpecializedByte
			case j.Character.TYPE => SpecializedChar
			case j.Boolean.TYPE => SpecializedBoolean
			case j.Float.TYPE => SpecializedFloat
			case j.Short.TYPE => SpecializedShort
			case j.Void.TYPE => SpecializedUnit
			case _ => new SpecializedRef[AnyRef](tpe.asInstanceOf[Class[AnyRef]]) //this is an impossible case ...
		} else if (tpe == classOf[AnyRef])
			SpecializedAnyRef
		else
			new SpecializedRef[AnyRef](tpe.asInstanceOf[Class[AnyRef]])
	).asInstanceOf[SpecializedExact[E]]



	/** Usage of type `E` as an unbound generic parameter in fully specialized context.
	  * If `tpe` is the token class for one of the java primitives, the corresponding constant is used to represent
	  * the appropriate specialization. All reference types as well as custom value types are represented by an instance
	  * which `runType` equals `AnyRef`; in that case, the actual information about the class of `E` is discarded.
	  * @return one of primitive specializations or an instance representing erasure to `AnyRef`.
	  */
	final def erasedClass[E](tpe :Class[E]) :Erased[E] = (
		if (tpe.isPrimitive) tpe match {
			case j.Integer.TYPE => SpecializedInt
			case j.Double.TYPE => SpecializedDouble
			case j.Long.TYPE => SpecializedLong
			case j.Byte.TYPE => SpecializedByte
			case j.Character.TYPE => SpecializedChar
			case j.Boolean.TYPE => SpecializedBoolean
			case j.Float.TYPE => SpecializedFloat
			case j.Short.TYPE => SpecializedShort
			case j.Void.TYPE => SpecializedUnit
			case _ => ErasedRef
		} else
			ErasedRef
	).asInstanceOf[Erased[E]]



	/** Return specialization type class instance specific to the given class, based on an implicit `ClassTag`.
	  * Equal to [[asClass]](classTag[E].runtimeClass).
	  * Note that, in context where `ClassTag[E]` is available implicitly, but `E` is an erased abstract type,
	  * returned instance will be based on that class tag and equal to the appropriate value class specialization
	  * for java primitives, despite values of `E` being autoboxed in that context.
	  *
	  * @tparam E type for which specialization should be resolved.
	  * @return an instance representing either one of java primitives or `java.lang.Object`.
	  */
	@inline final def as[E](implicit tpe :ClassTag[E]) :Specialized[E] =
		asClass(tpe.runtimeClass).asInstanceOf[Specialized[E]]



	/** Equals to [[Specialized#erasedClass]]`(clazz)` for runtime class as defined by an implicit `ClassTag`. */
	def erased[E](implicit tpe :ClassTag[E]) :Erased[E] =
		erasedClass(tpe.runtimeClass).asInstanceOf[Erased[E]]



	/** Representation of any type as its autoboxed, erased form without any specialization or upper type bounds.
	  * @return the same instance, which all type members are defined as AnyRef, and `runType`, `erasedType`, `boxType`
	  *         all equal `classOf[AnyRef]`.
	  */
	def generic[E] :Specialized[E] = SpecializedAnyRef.asInstanceOf[Specialized[E]]



	/** The best representation of static type `E` based on implicit type information
	  * once erasure is performed for reference types.
	  * @return an instance representing either a java primitive (including `void`), synthetic `Null`
	  *         or erasure/boxing (for custom value types) to `AnyRef`.
	  */
	override final def asType[T](implicit tag :TypeTag[T]) :SpecializedExact[T] =
		asClass(scala.reflect.runtime.universe.runtimeMirror(getClass.getClassLoader).runtimeClass(tag.tpe).asInstanceOf[Class[T]])



	/** Yields the representation of type `E` in the caller's context after erasure and specialization. */
	@inline final def locally[@specialized E] :Erased[E] = {
		new Enforce[E] match {
			case ErasedRef.key => ErasedRef
			case SpecializedInt.key => SpecializedInt
			case SpecializedDouble.key => SpecializedDouble
			case SpecializedByte.key => SpecializedByte
			case SpecializedLong.key => SpecializedLong
			case SpecializedChar.key => SpecializedChar
			case SpecializedBoolean.key => SpecializedBoolean
			case SpecializedFloat.key => SpecializedFloat
			case SpecializedShort.key => SpecializedShort
			case SpecializedUnit.key => SpecializedUnit
			case _ => ErasedRef
		}
	}.asInstanceOf[Erased[E]]


	/** Most specific specialization for the given value. If `value` is a boxed java primitive, this will be the
	  * specialization for the appropriate value type. In all other cases, it will be an instance representing
	  * `value.getClass`.
	  */
	final def forValue[E](value :E) :Specialized[E] = //asClass(UnboxedClass(value.getClass).asInstanceOf[Class[E]])
		(value match {
			case _ :j.Number => value match {
				case _ :j.Integer => SpecializedInt
				case _ :j.Double => SpecializedDouble
				case _ :j.Byte => SpecializedByte
				case _ :j.Long => SpecializedLong
				case _ :j.Float => SpecializedFloat
				case _ :j.Short => SpecializedShort
				case _ => new SpecializedRef[AnyRef](value.getClass.asInstanceOf[Class[AnyRef]])
			}
			case _ :j.Character => SpecializedChar
			case _ :j.Boolean => SpecializedBoolean
			case _ :Unit => SpecializedUnit
			case _ if value.getClass eq classOf[AnyRef] => SpecializedAnyRef
			case _ => new SpecializedRef[AnyRef](value.getClass.asInstanceOf[Class[AnyRef]])
		}).asInstanceOf[Specialized[E]]





	/** An empty array guaranteed to hold values of `E`, with most specific element type based on the information about `E`
	  * in the caller's context.
	  */
	@inline final def arrayFor[E](implicit specialized :Specialized[E]) :Array[E] =
		specialized.emptyArray.asInstanceOf[Array[E]]


	/** A new array of the given size, guaranteed to hold values of `E`, with most specific element type based on the information about `E`
	  * in the caller's context.
	  */
	@inline final def arrayFor[E](capacity :Int)(implicit specialized :Specialized[E]) :Array[E] =
		specialized.newArray(capacity).asInstanceOf[Array[E]]


	/** Creates an empty array guaranteed to be able to hold values of type `E`, as it would appear in erased and specialized
	  * byte code. For inbuilt, specialized (by the implicit parameter) value classes a corresponding java primitive array
	  * is returned. For `AnyRef` subtypes, the actual class of the created array will be `[Object`. The downcast required
	  * to present it as `Array[E]` is erased, so any `ClassCastException`s will be delayed until the client code attempts
	  * to enforce its type to an actual concrete class.
	  * Note that it is still perfectly safe to call it if the array doesn't escape the context
	  * in which `E` is an erased type, or if `E` is a primitive.
	  *
	  * @param specialized specialization information about type `E`
	  */
	@inline final def erasedArrayFor[E](implicit specialized :Specialized[E]) :Array[E] =
		specialized.emptyErasedArray.asInstanceOf[Array[E]]



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
	@inline final def erasedArrayFor[E](capacity :Int)(implicit specialized :Specialized[E]) :Array[E] =
		specialized.newErasedArray(capacity).asInstanceOf[Array[E]]



	private final val AnyRefClass = classOf[AnyRef]
	private final val NothingClass = classOf[Nothing]
	private final val NullClass = classOf[Null]


	/** Maps all classes representable in `jvm` to their boxed representations. */
	final val BoxedClass = Map[Class[_], Class[_]](
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


	/** Reverse map of java primitive boxes containing entries for all java primitives and `Unit`,
	  * mapping the box classes (i.e. `java.lang.Integer`) to synthetic classes representing actual primitive types (i.e. `Integer.TYPE`).
	  */
	final val PrimitiveClass = BoxedClass.map{ case (primitive, box) => box -> primitive }

	/** Maps all classes to their corresponding primitives (for java primitive boxes) or themselves (all other). */
	final val UnboxedClass = PrimitiveClass withDefault identity[Class[_]]




	/** Mapping from any `jvm` class to their representations in specialized generic methods.
	  * `classOf[V] -> classOf[V]` for all builtin scala value classes,
	  * `classOf[T] -> classOf[Any]` for all others.
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




	private def key[@specialized E] :Enforce[E] = new Enforce[E]
	private[this] final val unspecializedKey = new Enforce[AnyRef]



	/** Specialization for `Byte`. */
	implicit final val SpecializedByte :SpecializedPrimitive[Byte, j.Byte] =
		new SpecializedPrimitive[Byte, j.Byte](key[Byte], 0) {
			override private[specialty] def call[R[X]](callback: SpecializeIndividually[R])(implicit force: Enforce[Byte])  =
				callback.forByte
		}


	/** Specialization for `Short`. */
	implicit final val SpecializedShort :SpecializedPrimitive[Short, j.Short]  =
		new SpecializedPrimitive[Short, j.Short](key[Short], 0) {
			override private[specialty] def call[R[X]](callback: SpecializeIndividually[R])(implicit force: Enforce[Short])  =
				callback.forShort
		}


	/** Specialization for `Int`. */
	implicit final val SpecializedInt :SpecializedPrimitive[Int, j.Integer] =
		new SpecializedPrimitive[Int, j.Integer](key[Int], 0) {
			override private[specialty] def call[R[X]](callback: SpecializeIndividually[R])(implicit force: Enforce[Int])  =
				callback.forInt
		}


	/** Specialization for `Long`. */
	implicit final val SpecializedLong  :SpecializedPrimitive[Long, j.Long] =
		new SpecializedPrimitive[Long, j.Long](key[Long], 0) {
			override private[specialty] def call[R[X]](callback: SpecializeIndividually[R])(implicit force: Enforce[Long])  =
				callback.forLong
		}


	/** Specialization for `Char`. */
	implicit final val SpecializedChar  :SpecializedPrimitive[Char, j.Character] =
		new SpecializedPrimitive[Char, j.Character](key[Char], 0) {
			override private[specialty] def call[R[X]](callback: SpecializeIndividually[R])(implicit force: Enforce[Char])  =
				callback.forChar
		}


	/** Specialization for `Float`. */
	implicit final val SpecializedFloat  :SpecializedPrimitive[Float, j.Float] =
		new SpecializedPrimitive[Float, j.Float](key[Float], 0) {
			override private[specialty] def call[R[X]](callback: SpecializeIndividually[R])(implicit force: Enforce[Float])  =
				callback.forFloat
		}


	/** Specialization for `Double`. */
	implicit final val SpecializedDouble  :SpecializedPrimitive[Double, j.Double] =
		new SpecializedPrimitive[Double, j.Double](key[Double], 0) {
			override private[specialty] def call[R[X]](callback: SpecializeIndividually[R])(implicit force: Enforce[Double])  =
				callback.forDouble
		}


	/** Specialization for `Boolean`. */
	implicit final val SpecializedBoolean  :SpecializedPrimitive[Boolean, j.Boolean] =
		new SpecializedPrimitive[Boolean, j.Boolean](key[Boolean], false) {
			override private[specialty] def call[R[X]](callback: SpecializeIndividually[R])(implicit force: Enforce[Boolean])  =
				callback.forBoolean
		}


	/** Specialization for `Unit` as java `void`. */
	implicit final val SpecializedUnit  :SpecializedPrimitive[Unit, BoxedUnit] =
		new SpecializedPrimitive[Unit, BoxedUnit](key[Unit], ()) { //todo - this is not really a primitive:
			override private[specialty] def call[R[X]](callback: SpecializeIndividually[R])(implicit force: Enforce[Unit])  =
				callback.forUnit
		}


	/** Specialization for `AnyRef` (and indirectly `Any` by promotion). */
	implicit object SpecializedAnyRef
		extends SpecializedRef[AnyRef] with SpecializedRoot[AnyRef]
	{

		override final val runType :Class[AnyRef] = AnyRefClass
		override final val emptyArray :Array[AnyRef] = new Array[AnyRef](0)

		protected[specialty] override final val key = new Enforce[AnyRef]
	}




/*
	/** Represents the synthetic type `Null` of null values. */
	implicit final val SpecializedNull :SpecializedExact[Null] =
		new SpecializedRef[Null]() with SpecializedExact[Null] {

			private[specialty] override def call[R[X]](callback :SpecializeIndividually[R])(implicit enforceSpecialization :Enforce[Null]):R[Null] =
				callback.forNull
		}

	/** Represents the synthetic type `Nothing` with no values. */
	implicit final val SpecializedNothing :SpecializedExact[Nothing] =
		new SpecializedRef[Nothing]() with SpecializedExact[Nothing] {
			private[specialty] override def call[R[X]](callback :SpecializeIndividually[R])(implicit enforceSpecialization :Enforce[Nothing]) :R[Nothing] =
				callback.forNothing
		}
*/


	/** Represents an erased generic type argument referenced to as `java.lang.Object` and downcast in point of use. */
	private[this] final val ErasedRef :Erased[Any] = new Erased[Any] {
		override type RunType = Any
		override type BoxType = AnyRef

		override val runType = classOf[Any]
		override val boxType = classOf[AnyRef]

		override def classTag = ClassTag[Any](runType)
		override def boxClassTag = ClassTag[AnyRef](boxType)

		override def isErased = true

		override def default :Any = null


		override val emptyArray :Array[Any] = new Array[Any](0)
		override val emptyBoxArray :Array[AnyRef] = new Array[AnyRef](0)

		override private[specialty] def call[R[X]](callback :SpecializeIndividually[R])(implicit force :Enforce[Any]) :R[Any] =
			callback.forRef(this)

		override protected[specialty] val key = new Enforce[Any]

		override def toString = "@specialized(_)"
	}


	/** Implicit specialization determined from an implicitly available `ClassTag[E]`. */
	@inline implicit final def specializedClass[E](implicit tpe :ClassTag[E]) :Specialized[E] = as[E]



	/** `Specialized` instances representing all possible method/class specialization (all primitives and erasure).
	  * `Specialization(Specialization[T])` is true for all concrete and abstract types `T`.
 	  */
	final val Specializations = Set[Erased[_]](
		SpecializedByte,
		SpecializedShort,
		SpecializedInt,
		SpecializedLong,
		SpecializedChar,
		SpecializedFloat,
		SpecializedDouble,
		SpecializedBoolean,
		SpecializedUnit,
		ErasedRef
	)




	/**  Base trait describing context where type `T` is used as a generic type argument subject to possible erasure and
	  *  specialization. There will be an implicit value of `Erased[T]` for any type `T`, representing that upper bound,
	  *  if no more specific type information is available. This in particular includes all types which erase/specialize
	  *  to themselves as denoted by [[SpecializedRoot]], but also for example an `Erased[String]` denoting erased usage
	  *  of `String`, while `String` is not a 'root type' - there is no value for `SpecializedRoot[String]`.
	  */
	sealed trait Erased[@specialized T] extends Specialized[T] { //specialized to enforce specialization of factory method
		override type ErasedType = RunType

		override def erasedType :Class[ErasedType] = runType
		override def emptyErasedArray :Array[ErasedType] = emptyArray
		override def erasedClassTag :ClassTag[ErasedType] = classTag

		override protected[specialty] val key :Enforce[T]
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
		override type RunType = T
	}



	/** Base trait for all types `T` which erase/specialize in a generic context to themselves.
	  * This includes all inbuilt value types when specialized as java primitives and the `AnyRef` type
	  * for the `java.lang.Object` itself.
	  */
	trait SpecializedRoot[T] extends Erased[T] with SpecializedExact[T]



	/** Base class for all instances representing a ''jvm'' primitive type, including `void`. */
	sealed abstract class SpecializedPrimitive[@specialized E, B<:AnyRef] private[Specialized]
			(protected[specialty] override final val key :Enforce[E], val default :E)
			(implicit val classTag :ClassTag[E])
		extends Specialized[E] with SpecializedRoot[E]//extends Specialized[E] explicitly for specialization
	{
		override type BoxType = B

		override final val runType = classTag.runtimeClass.asInstanceOf[Class[E]]
		override final val erasedType = runType
		override final val boxType = BoxedClass(runType).asInstanceOf[Class[B]]

		override final def erasedClassTag: ClassTag[E] = classTag
		override final val boxClassTag: ClassTag[B] = ClassTag(boxType)

		override final val emptyArray = Array.empty[E]
		override final def emptyErasedArray: Array[E] = emptyArray
		override final val emptyBoxArray: Array[B] = Array.empty[B](boxClassTag)

	}



	/** Any representation of a reference type by a java/scala class specified by implicit `ClassTag[E]`. This is different
	  * from erasure in that `E` is not a value class and this instance may represent any super type of `E`.
	  * Values of the same type may be represented by many different instances of [[SpecializedRef]] representing different
	  * levels of generalisation after taking type bounds into the equation, from full type information to `AnyRef`.
	  * @param classTag the class representing the static type assignable from `E`.
	  * @tparam E any scala type, usually itself a type parameter of a generic method/class; this is not the final erased/unboxed type.
	  */
	sealed class SpecializedRef[E >: Null <:AnyRef] private[Specialized] ()(implicit final val classTag :ClassTag[E])
		extends Specialized[E]
	{
		private[Specialized] def this(runClass :Class[E]) =
			this()(new ClassTag[E] { override def runtimeClass: Class[_] = runClass })


		override type ErasedType = AnyRef
		override type BoxType = E
		override type RunType = E

		override def default :Null = null

		override def erasedClassTag: ClassTag[AnyRef] = implicitly[ClassTag[AnyRef]]

		override def boxClassTag: ClassTag[E] = classTag

		override def runType: Class[E] = classTag.runtimeClass.asInstanceOf[Class[E]]

		override def erasedType: Class[AnyRef] = AnyRefClass

		override def boxType: Class[E] = runType


		override def emptyArray: Array[E] = Array.empty[E]

		override def emptyErasedArray: Array[AnyRef] = Array.empty[AnyRef]

		override def emptyBoxArray: Array[E] = Array.empty[E]

		override private[specialty] def call[R[X]](callback: SpecializeIndividually[R])(implicit force: Enforce[E]) =
			callback.forRef[E](this)

		override protected[specialty] def key :Enforce[E] = unspecializedKey.asInstanceOf[Enforce[E]]
	}








//	private[specialty] type SpecializedKey[X] = Enforce[X]


	/** A token generic class specialized on its type parameter used to enforce specialization of a method by adding
	  * it as an implicit parameter. Implicit value is available for any type argument.
	  *
	  * It defines equality in terms of its runtime class, with two instances being equal '''iff''' `getClass`
	  * returns the same object for both of them. As scala specialization is done by introducing separate synthetic
	  * subclasses for all specialized type parameters, comparing a locally created instance with predefined constants
	  * for all java primitives lets one discover if executed code is specialized and for which value class.
	  */
	final class Enforce[@specialized X] private[Specialized] () {
		override def equals(that :Any) :Boolean = that.getClass eq getClass
		override def hashCode :Int = getClass.hashCode

		def className :String = getClass.getName
		override def toString :String = className.substring(className.indexOf("$")+1)
	}

	object Enforce {
		private[this] final val instance = new Enforce[Any]

		@inline implicit def forceSpecialization[X] :Enforce[X] = instance.asInstanceOf[Enforce[X]]
	}
}
