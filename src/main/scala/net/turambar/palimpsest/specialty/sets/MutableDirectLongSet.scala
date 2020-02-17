package net.turambar.palimpsest.specialty.sets
/*
import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize
import net.turambar.palimpsest.specialty.{AptBuilder, FitTraversableOnce}
import net.turambar.palimpsest.specialty.Specialized.Fun2
import net.turambar.palimpsest.specialty.iterables.EmptyIterableFoundation
import net.turambar.palimpsest.specialty.sets.DirectLongSet.{BranchLike, Empty, Singleton, StableBranch, StableLongTrie, delimitedPrefix}


/** A mutable variant of a Trie holding `Long` values with their binary format as a path.
  * Unlike [[net.turambar.palimpsest.specialty.sets.DirectLongSet.StableLongTrie]], there is only one implementation and the set consists only
  * of instances of this class and [[Empty]] as leaves.
  * If this is an empty set, then `common` must equal zero, and both `zeros` and `ones` must be [[Empty]].
  * If this is a singleton set, then `common` must equal the one element and both `zeros` and `ones` must be `Empty`
  * If this set contains more then one element, then `common` represents the path to this node.
  * and '''both''' `left` and `right` must be non-empty sets (as this is the divergence point between the two).
  *
  * @param _size number of elements in this set
  * @param common path to this node. If this is empty, must be zero; if this is singleton, must equal stored element.
  *               In other cases, the lowest set bit denotes the position on which both children differ.
  *               All bits above that are common to all elements in this set. In other words, common is `path | diffBit`.
  * @param left non-empty subset containing all elements which have a clear bit on the position of `diffBit` (as obtained from `common`). Empty if `size`<=1.
  * @param right non-empty subset containing all elements which have a set bit on the position of `diffBit` (as obtained from `common`). Empty if `size`<=1.
  */
@deprecated("This standalone implementation should be replaced with one based on generic tries", "sometime")
private[sets] final class MutableDirectLongSet(private[this] var _size :Int, private[this] var common :Long, private[sets] var left: DirectLongSet, private[sets] var right: DirectLongSet)
	extends BranchLike[MutableDirectLongSet] with OfKnownSize
			with MutableSet[Long] with SetSpecialization[Long, MutableDirectLongSet] with MutableSetSpecialization[Long, MutableDirectLongSet] with DirectLongSet
{

	/** A singleton mutable set. */
	def this(singleton :Long) = this(1, singleton, Empty, Empty)

	/** An empty mutable set. */
	def this() = this(0, 0L, Empty, Empty)

	def this(common :Long, left: DirectLongSet, right: DirectLongSet) = this(left.size + right.size, common, left, right)

	@inline def path = if (_size==1) common else common & (common-1) //clear lowest set bit
	@inline def diffBit = if (_size==1) 0L else common & -common //mask for the lowest set bit
	@inline def mask = //mask for the shared prefix in the binary representation of all elements in this set
		if (_size==1) -1 //all bits set
		else {
			val bit = common & -common; -bit ^ bit
		}

	@inline override private[sets] def delimitedPath = common
	override def size = _size

	override def belongs(elem :Long) = (elem & -common | ~elem & common) == (common & -common)

	override def isEmpty = size==0
	override def nonEmpty = size>0

	override def head = if (_size==1) common else left.head
	override def headOption = if (_size==1) Some(common) else left.headOption
	override def last = if (_size==1) common else right.last
	override def lastOption = if (_size==1) Some(common) else right.lastOption

	override def tail :MutableDirectLongSet =
		if (left.nonEmpty) filtered(left.tail, right)
		else if (size==1) empty
		else filtered(Empty, right.tail) //right is Empty, so will throw UnsupportedOperatoinException

	override def init :MutableDirectLongSet  =
		if (right.nonEmpty) filtered(left, right.init)
		else if (size==1) empty
		else filtered(left.init, Empty) //left is Empty, so will throw UnsupportedOperationException

	override def empty :MutableDirectLongSet = new MutableDirectLongSet(0, 0L, Empty, Empty)


	override def contains(elem: Long) = _size match {
		case n if n>1 =>
			val DiffBit = common & -common
			(elem & -common | ~elem & common) ^ (elem & DiffBit) match {
				case 0 => right.contains(elem)
				case DiffBit => left.contains(elem)
				case _ => false
			}
		case 1 => common == elem
		case _ => false
	}

	override def +(elem: Long) = _size match {
		case 0 => new MutableDirectLongSet(elem)
		case 1 =>
			if (common==elem) this
			else MutableDirectLongSet.join(this, new MutableDirectLongSet(elem))
		case _ =>
			val DiffBit = common & -common
			(elem & -common | ~elem & common) ^ (elem & DiffBit) match {
				case 0 => new MutableDirectLongSet(common, left, right + elem)
				case DiffBit => new MutableDirectLongSet(common, left+elem, right)
				case _ => MutableDirectLongSet.join(this, new MutableDirectLongSet(elem))
			}
	}

	override def -(elem: Long) = _size match {
		case 0 => this
		case 1 =>
			if (common==elem) new MutableDirectLongSet()
			else this
		case _ =>
			val DiffBit = common & -common
			(elem & -common | ~elem & common) ^ (elem & DiffBit) match {
				case 0 => filtered(left, right-elem)
				case DiffBit => filtered(left-elem, right)
				case _ => this
			}
	}

	override def +=(elem :Long) :this.type = { add(elem); this }

	override def add(elem: Long): Boolean = _size match {
		case 0 => become(elem); true
		case 1 => common!=elem && {
			become(new MutableDirectLongSet(common), new MutableDirectLongSet(elem))
			true
		}
		case total =>
			val DiffBit = common & -common
			(elem & -common | ~elem & common) ^ (elem & DiffBit) match {
				case 0 => (right.asInstanceOf[MutableDirectLongSet] add elem) && {
					_size = left.size + right.size; true
				}
				case DiffBit => (left.asInstanceOf[MutableDirectLongSet] add elem) && {
					_size = left.size + right.size; true
				}
				case _ =>
					become(new MutableDirectLongSet(total, common, left, right), new MutableDirectLongSet(elem))
					true
			}
	}


	override def -=(elem :Long) :this.type = { remove(elem); this }

	override def remove(elem: Long): Boolean = _size match {
		case 0 => false
		case 1 => common == elem && { clear(); true }
		case total =>
			val DiffBit = common & -common
			(elem & -common | ~elem & common) ^ (elem & DiffBit) match {
				case 0 => (right.asInstanceOf[MutableDirectLongSet] remove elem) && {
					update(); true
				}
				case DiffBit => (left.asInstanceOf[MutableDirectLongSet] remove elem) && {
					update(); true
				}
				case _ => false
			}
	}





	override def ++=(xs: FitTraversableOnce[Long]) :this.type = xs match {
		case trie :MutableDirectLongSet => trie.size match {
			case 0 =>
				this
			case 1 =>
				this += trie.delimitedPath
			case _ => _size match {
				case 0 =>
					_size = trie.size; common = trie.delimitedPath
					left = trie.left.clone(); right = trie.right.clone()
					this
				case 1 =>
					become(trie.clone(), new MutableDirectLongSet(common))
					this
				case _ =>
					val path1 = common; val path2 = trie.delimitedPath
					val diff = path1 ^ path2
					if (diff==0L) {
						left.asInstanceOf[MutableDirectLongSet] ++= trie.left
						right.asInstanceOf[MutableDirectLongSet] ++= trie.right
						_size = left.size + right.size
					} else {
						val point1 = path1 & -path1; val point2 = path2 & -path2
						val mask1 = -point1 ^ point1; val mask2 = -point2 ^ point2
						if ((diff & mask1) == 0L) {//path1 is prefix of path2
							if ((path2 & point1) == 0L)
								left.asInstanceOf[MutableDirectLongSet] ++= trie
							else
								right.asInstanceOf[MutableDirectLongSet] ++= trie
							_size = left.size + right.size
						} else if ((diff & mask2)==0L) //path2 is prefix of path1
							if ((path1 & point2)==0L) {
								this ++= trie.left
								left = new MutableDirectLongSet(common, left, right)
								right = trie.right
								common = path2
								_size = left.size + right.size
							}else {
								this ++= trie.right
								right = new MutableDirectLongSet(common, left, right)
								common = path2
								_size = left.size + right.size
							}
						else //path1 and path2 differ on their common section
							become(new MutableDirectLongSet(_size, common, left, right), trie)
					}
					this
			}
		}
		case _ => super.++=(xs)
	}

	override def --=(xs: FitTraversableOnce[Long]) = super.--=(xs)

	override def dropTake(from: Int, until: Int) :MutableDirectLongSet =
		if (until<=from || from >= size)
			new MutableDirectLongSet()
		else if (size==1) new MutableDirectLongSet(common)
		else {
			val lsize = left.size
			if (until <= lsize)
				left.asInstanceOf[MutableDirectLongSet].dropTake(from, until)
			else if (from >= lsize)
				right.asInstanceOf[MutableDirectLongSet].dropTake(from-lsize, until-lsize)
			else new MutableDirectLongSet(
				common,
				left.asInstanceOf[MutableDirectLongSet].dropTake(from, lsize),
				right.asInstanceOf[MutableDirectLongSet].dropTake(0, until-lsize)
			)
		}

	override def filter(p: (Long) => Boolean, ourTruth: Boolean): MutableDirectLongSet =
		if (ourTruth) filter(p) else filterNot(p)
//		filtered(left.filter(p, where), right.filter(p, where))
	override def filter(p: (Long) => Boolean) = filtered(left.filter(p), right.filter(p))

	override def filterNot(p: (Long) => Boolean) = filtered(left.filterNot(p), right.filterNot(p))

	override def retain(p: (Long) => Boolean) :Unit = _size match {
		case 0 =>
		case 1 => if (!p(path)) clear()
		case _ =>
			left.retain(p); right.retain(p)
			update()
	}


	override def foreach[@specialized(Unit) U](f: (Long) => U) =
		if (_size>1) {
			left.foreach(f); right.foreach(f)
		} else if (size==1)
			f(common)

	override def reverseForeach(f: (Long) => Unit) =
		if (_size>1) {
			right.reverseTraverse(f); left.reverseTraverse(f)
		} else if (_size==1)
			f(common)


	override def forall(p: (Long) => Boolean) = _size match {
		case 0 => true
		case 1 => p(common)
		case _ => left.forall(p) && right.forall(p)
	}

	override def exists(p: (Long) => Boolean) = _size match {
		case 0 => false
		case 1 => p(common)
		case _ => left.exists(p) || right.exists(p)
	}

	override def count(p: (Long) => Boolean) = _size match {
		case 0 => 0
		case 1 => if (p(common)) 1 else 0
		case _ => left.count(p) + right.count(p)
	}

	override def find(p: (Long) => Boolean) = _size match {
		case 0 => None
		case 1 => if (p(common)) Some(common) else None
		case _ => left.find(p) orElse right.find(p)
	}

	override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, Long) => O) = _size match {
		case 0 => z
		case 1 => op(z, common)
		case _ => right.foldLeft(left.foldLeft(z)(op))(op)
	}

	override def foldRight[@specialized(Fun2) O](z: O)(op: (Long, O) => O) = _size match {
		case 0 => z
		case 1 => op(common, z)
		case _ => left.foldRight(right.foldRight(z)(op))(op)
	}

	@inline override final def clear() :Unit = {
		_size = 0; common = 0L
		left = Empty; right = Empty
	}

	/** Callback for fixing up invariants after filtering subtrees.
	  * Must update size and, if any of subtrees became empty, reduce itself to the other tree.
	  */
	private def update() :Unit =
		if (left.isEmpty)
			if (right.isEmpty) clear()
			else become(right.asInstanceOf[MutableDirectLongSet])
		else if (right.isEmpty)
			become(left.asInstanceOf[MutableDirectLongSet])
		else
			_size = left.size + right.size


	@inline final private[this] def become(singleton :Long) :Unit = {
		common = singleton; _size = 1
	}

	@inline final private[this] def become(other :MutableDirectLongSet) :Unit = {
		common = other.delimitedPath; _size = other.size
		left = other.left; right = other.right
	}


	protected def become(x :MutableDirectLongSet, y :MutableDirectLongSet) :Unit = {
		val xPath = x.path
		common = delimitedPrefix(xPath, y.path)
		if ((xPath & common & -common) == 0L) {
			left = x; right = y
		} else {
			left = y; right = x
		}
		_size = x.size + y.size
	}


	def filtered(l: DirectLongSet, r: DirectLongSet) :MutableDirectLongSet =
		if (l.isEmpty) r match {
			case _ if r.isEmpty => empty
			case m :MutableDirectLongSet => m
			case _ => throw new MatchError(s"Illegal child of a MutableLongTrie - neither a MutableLongTrie nor Empty: $l")
		} else if (r.isEmpty)
			l.asInstanceOf[MutableDirectLongSet]
		else
			new MutableDirectLongSet(common, l, r)

	override def stable :StableLongTrie = _size match {
		case 0 => DirectLongSet.Empty
		case 1 => new DirectLongSet.Singleton(common)
		case _ => new StableBranch(common, left.asInstanceOf[MutableDirectLongSet].stable, right.asInstanceOf[MutableDirectLongSet].stable)
	}


	override def clone() :MutableDirectLongSet = new MutableDirectLongSet(_size, common, left.clone(), right.clone())

	override def stringPrefix = "MutableSet[Long]"

	override def count: Int = size
}





object MutableDirectLongSet {

//	object Empty extends MutableLongTrie(0, 0L, null, null) {
//		left = this; right = this
//	}

	def empty :MutableSet[Long] = new MutableDirectLongSet()
	def newBuilder :AptBuilder[Long, MutableSet[Long]] = new MutableDirectLongSet()
	def singleton(value :Long) :MutableSet[Long] = new MutableDirectLongSet() += value



	private def join(x :MutableDirectLongSet, y :MutableDirectLongSet) :MutableDirectLongSet = {
		val xPath = x.path
		val common = delimitedPrefix(xPath, y.path)
		if ((xPath & common & - common) == 0L)
			new MutableDirectLongSet(common, x, y)
		else
			new MutableDirectLongSet(common, y, x)
	}

}
*/