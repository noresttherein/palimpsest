package net.turambar.palimpsest.specialty.iterables

import net.turambar.palimpsest.specialty.RuntimeType.Specialized.{Fun1Vals, Fun2}
import net.turambar.palimpsest.specialty.{AptBuilder, Vals, RuntimeType, ofKnownSize}

import scala.collection.{GenIterable, GenTraversableOnce}
import scala.collection.generic.CanBuildFrom


/** Base class for [[AptIterable]] hierarchy containing default implementations of all methods
  * which don't require specialization (or can be implemented by delegating to a smaller subset of specialized methods).
  * Exists to decrease the class sizes of concrete types as well as static forwarding to trait-implemented methods.
  * All implementations provided here, unless stated to the contrary, delegate to the corresponding iterator methods and,
  * in case of collection results, append the iterator as a whole to `newBuilder`.
  * @author Marcin Mościcki
  */
abstract class IterableFoundation[+E, +Repr] extends IterableTemplate[E, Repr]