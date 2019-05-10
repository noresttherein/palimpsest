package net.turambar.palimpsest.specialty.tries

import net.turambar.palimpsest.specialty.{?, Blank, FitIterator, Sure}
import net.turambar.palimpsest.specialty.RuntimeType.{Fun1, Fun2}
import net.turambar.palimpsest.specialty.tries.TrieElements.ElementOf
import net.turambar.palimpsest.specialty.tries.Trie.KeyTypes


/** Selected collection operations working on tries of type `T`. The actual element type is externalized into
  * an accessor SAM trait [[TrieElements.ElementOf]] which returns the element for a given trie node. This allows
  * to treat the same trie as a collection of various element types, for example keys, values and key-value pairs
  * of maps. 
  * 
  * These methods are declared here instead of directly in [[TrieTemplate]] as trie implementations may choose to
  * declare their self type `T` as a subtype of this trait type parameter `T`, that is require the element accessor
  * to accept more generic trie nodes as arguments as it would otherwise.
  * @tparam K type of keys stored in tries `S` and `T`
  * @tparam S argument trie type for [[ElementOf]] accessor parameter of all methods declared here
  * @tparam T produced trie type. Generally `T &lt;: S`, but it is not enforced on this level.
  */
trait TrieElements[+K, +S, +T] extends TrieTemplate[K, T] { //this :T =>

//	def find_?[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean, where :Boolean=true): ?[E]
//
//	def exists[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean) :Boolean = find_?(elements)(f).isDefined
//
//	def forall[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean) :Boolean = find_?(elements)(f, false).isEmpty
//
//	def count[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean) :Int
//
//	def partition[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean) :(T, T)
//	//todo: size out parameter?
//	def filter[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean, where :Boolean = true) :T
//
//
//	def dropWhile[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean) :T
//
//	def takeWhile[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean) :T
//
//	def span[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean) :(T, T)

	def find_?[E](elements :ElementOf[E, S])(f :E => Boolean, where :Boolean=true): ?[E]

	def exists[E](elements :ElementOf[E, S])(f :E => Boolean) :Boolean = find_?(elements)(f).isDefined

	def forall[E](elements :ElementOf[E, S])(f :E => Boolean) :Boolean = find_?(elements)(f, false).isEmpty

	def count[E](elements :ElementOf[E, S])(f :E => Boolean) :Int


	def partition[E](elements :ElementOf[E, S])(f :E => Boolean) :(T, T)
	//todo: size out parameter?
	def filter[E](elements :ElementOf[E, S])(f :E => Boolean, where :Boolean = true) :T



	def dropWhile[E](elements :ElementOf[E, S])(f :E => Boolean) :T

	def takeWhile[E](elements :ElementOf[E, S])(f :E => Boolean) :T

	def span[E](elements :ElementOf[E, S])(f :E => Boolean) :(T, T)



	def foreach[@specialized(Fun1) E, @specialized(Unit) O](elements :ElementOf[E, S])(f :E=>O) :Unit

	def reverseForeach[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Unit) :Unit




	def foldLeft[@specialized(Fun2) E, @specialized(Fun2) O](elements :ElementOf[E, S])(acc :O)(f :(O, E) => O) :O

	def foldRight[@specialized(Fun2) E, @specialized(Fun2) O](elements :ElementOf[E, S])(acc :O)(f :(E, O) => O) :O

	def copyToArray[@specialized(TrieElements.Types) E](elements :ElementOf[E, S])(array :Array[E], from :Int, max :Int) :Int

	def iterator[@specialized(TrieElements.Types) E](elements :ElementOf[E, S]) :FitIterator[E]

	def reverseIterator[@specialized(TrieElements.Types) E](elements :ElementOf[E, S]) :FitIterator[E]
}



object TrieElements {

	trait ElementOf[@specialized(Types) +E, -T] {
		def elementOf(keyNode :T) :E

//		def unsure(keyNode :T): ?[E] =

		def satisfies(keyNode :T, f :E => Boolean) :Boolean = f(elementOf(keyNode))

		def satisfying(keyNode :T, f :E => Boolean, where :Boolean = true): ?[E] = {
			val e = elementOf(keyNode)
			if (f(elementOf(keyNode)) == where) Sure(e) else Blank
		}

	}

	trait ElementCounter[+E, -T] extends ElementOf[E, T] {
		def count :Int
	}


	/** Types of elements possibly stored in various [[Trie]] implementations. [[ElementOf]] is specialized for these types. */
	final val Types = new Specializable.Group((Short, Int, Long, Char, Float, Double))

	class TrieKeys[@specialized(KeyTypes) K, T <: Trie[K, T]] extends ElementOf[K, T] {
		override def elementOf(keyNode :T) :K = keyNode.key
		override def satisfies(keyNode :T, f :K => Boolean) :Boolean = f(keyNode.key)

		override def satisfying(keyNode :T, f :K => Boolean, where :Boolean): ?[K] = {
			val k = keyNode.key
			if (f(k) == where) Sure(k) else Blank
		}
	}






	trait EmptyElements[+K, +S, +T] extends TrieElements[K, S, T] { this :T =>

//		override def find_?[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean, where :Boolean): ?[E] = Blank
//		override def exists[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean) :Boolean = false
//		override def forall[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean) :Boolean = true
//		override def count[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean) :Int = 0
//
//		override def partition[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean) :(T, T) = (this, this)
//		override def filter[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean, where :Boolean) :T = this
//
//		override def dropWhile[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean) :T = this
//		override def takeWhile[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean) :T = this
//		override def span[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean) :(T, T) = (this, this)

		override def find_?[E](elements :ElementOf[E, S])(f :E => Boolean, where :Boolean): ?[E] = Blank
		override def exists[E](elements :ElementOf[E, S])(f :E => Boolean) :Boolean = false
		override def forall[E](elements :ElementOf[E, S])(f :E => Boolean) :Boolean = true
		override def count[E](elements :ElementOf[E, S])(f :E => Boolean) :Int = 0

		override def partition[E](elements :ElementOf[E, S])(f :E => Boolean) :(T, T) = (this, this)
		override def filter[E](elements :ElementOf[E, S])(f :E => Boolean, where :Boolean) :T = this

		override def dropWhile[E](elements :ElementOf[E, S])(f :E => Boolean) :T = this
		override def takeWhile[E](elements :ElementOf[E, S])(f :E => Boolean) :T = this
		override def span[E](elements :ElementOf[E, S])(f :E => Boolean) :(T, T) = (this, this)


		override def foreach[@specialized(Fun1) E, @specialized(Unit) O](elements :ElementOf[E, S])(f :E => O) :Unit = ()
		override def reverseForeach[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Unit) :Unit = ()




		override def foldLeft[@specialized(Fun2) E, @specialized(Fun2) O](elements :ElementOf[E, S])(acc :O)(f :(O, E) => O) :O = acc
		override def foldRight[@specialized(Fun2) E, @specialized(Fun2) O](elements :ElementOf[E, S])(acc :O)(f :(E, O) => O) :O = acc
		override def copyToArray[@specialized(TrieElements.Types) E](elements :ElementOf[E, S])(array :Array[E], from :Int, max :Int) :Int = 0

		override def iterator[@specialized(TrieElements.Types) E](elements :ElementOf[E, S]) :FitIterator[E] = FitIterator.empty[E]

		override def reverseIterator[@specialized(TrieElements.Types) E](elements :ElementOf[E, S]) :FitIterator[E] = FitIterator.empty[E]
	}



	trait LeafElement[+K, +S, +T <: S] extends TrieElements[K, S, T] { this :T =>

//		override def find_?[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean, where :Boolean): ?[E] = {
//			val e = elements.elementOf(this)
//			if (f(e) == where) Sure(e) else Blank
//		}
//
//		override def exists[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean) :Boolean =
//			f(elements.elementOf(this))
//
//		override def forall[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean) :Boolean =
//			f(elements.elementOf(this))
//
//		override def count[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean) :Int =
//			if (f(elements.elementOf(this))) 1 else 0
//
//		override def partition[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean) :(T, T) =
//			if (f(elements.elementOf(this))) (this, emptyTrie) else (emptyTrie, this)
//
//		override def filter[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean, where :Boolean) :T =
//			if (f(elements.elementOf(this)) == where) this else emptyTrie
//
//
//		override def dropWhile[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean) :T =
//			if (f(elements.elementOf(this))) emptyTrie else this
//
//		override def takeWhile[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean) :T =
//			if (f(elements.elementOf(this))) this else emptyTrie
//
//		override def span[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Boolean) :(T, T) =
//			if (f(elements.elementOf(this))) (this, emptyTrie) else (emptyTrie, this)


		override def find_?[E](elements :ElementOf[E, S])(f :E => Boolean, where :Boolean): ?[E] =
			elements.satisfying(this, f)

		override def exists[E](elements :ElementOf[E, S])(f :E => Boolean) :Boolean =
			elements.satisfies(this, f)

		override def forall[E](elements :ElementOf[E, S])(f :E => Boolean) :Boolean =
			elements.satisfies(this, f)

		override def count[E](elements :ElementOf[E, S])(f :E => Boolean) :Int =
			if (elements.satisfies(this, f)) 1 else 0


		override def partition[E](elements :ElementOf[E, S])(f :E => Boolean) :(T, T) =
			if (elements.satisfies(this, f)) (this, emptyTrie) else (emptyTrie, this)

		override def filter[E](elements :ElementOf[E, S])(f :E => Boolean, where :Boolean) :T =
			if (f(elements.elementOf(this)) == where) this else emptyTrie


		override def dropWhile[E](elements :ElementOf[E, S])(f :E => Boolean) :T =
			if (elements.satisfies(this, f)) emptyTrie else this

		override def takeWhile[E](elements :ElementOf[E, S])(f :E => Boolean) :T =
			if (elements.satisfies(this, f)) this else emptyTrie

		override def span[E](elements :ElementOf[E, S])(f :E => Boolean) :(T, T) =
			if (elements.satisfies(this, f)) (this, emptyTrie) else (emptyTrie, this)




		override def foreach[@specialized(Fun1) E, @specialized(Unit) O](elements :ElementOf[E, S])(f :E => O) :Unit =
			f(elements.elementOf(this))

		override def reverseForeach[@specialized(Fun1) E](elements :ElementOf[E, S])(f :E => Unit) :Unit =
			f(elements.elementOf(this))



		override def foldLeft[@specialized(Fun2) E, @specialized(Fun2) O](elements :ElementOf[E, S])(acc :O)(f :(O, E) => O) :O =
			f(acc, elements.elementOf(this))

		override def foldRight[@specialized(Fun2) E, @specialized(Fun2) O](elements :ElementOf[E, S])(acc :O)(f :(E, O) => O) :O =
			f(elements.elementOf(this), acc)

		override def copyToArray[@specialized(TrieElements.Types) E](elements :ElementOf[E, S])(array :Array[E], from :Int, max :Int) :Int =
			if (max > 0 && from < array.length) {
				array(from) = elements.elementOf(this);1
			} else 0

		override def iterator[@specialized(TrieElements.Types) E](elements :ElementOf[E, S]) :FitIterator[E] =
			FitIterator(elements.elementOf(this))

		override def reverseIterator[@specialized(TrieElements.Types) E](elements :ElementOf[E, S]) :FitIterator[E] =
			FitIterator(elements.elementOf(this))
	}
	
	
	
	
}