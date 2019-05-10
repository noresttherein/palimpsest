package net.turambar.palimpsest.specialty.seqs

import net.turambar.palimpsest.specialty.{Elements, FitIterator, FitTraversableOnce}
import net.turambar.palimpsest.specialty.FitIterable.IterableViewFoundation
import net.turambar.palimpsest.specialty.RuntimeType.Fun1Res

import scala.collection.{mutable, GenTraversableOnce}

/**
  */
class OptimisticFitBuffer[U, @specialized(Elements) E <: U](private[this] var optimistic :FitBuffer[E])
	extends IterableViewFoundation[U, U, FitBuffer[U]] with FitBuffer[U]
{

	@inline final protected[this] def source :FitSeq[U] = target //if (spec!=null) spec else erased

	override protected def forSource[@specialized(Fun1Res) O](f :U => O) :U => O = f

	override protected def mine :U => U = identity[U]

	private[this] var target :FitBuffer[U] = try {
		optimistic.asInstanceOf[FitBuffer[U]]
	} catch {
		case _ :ClassCastException =>
			val elems = optimistic
			optimistic = null
			FitBuffer[U](elems :_*)
	}


	override def length :Int = target.length

	override def iterator :FitIterator[U] = target.iterator


	override protected def at(idx :Int) :U = target.get(idx)


	override protected def section(from :Int, until :Int) :FitBuffer[U] =
		if (optimistic != null)
			new OptimisticFitBuffer[U, E](sectionOf(target, from, until).asInstanceOf[FitBuffer[E]])
		else
			sectionOf(target, from, until)


	override def indexOf[O >: U](elem :O, start :Int) :Int =
		try {
			target.indexOf(elem, start)
		} catch {
			case _ :ClassCastException | _ :ArrayStoreException if optimistic != null =>
				target = FitBuffer[U](optimistic :_*)
				optimistic = null
				target.indexOf(elem, start)
		}


	override protected def set(idx :Int, elem :U) :Unit =
		try {
			target.uncheckedUpdate(idx, elem)
		} catch {
			case _ :ClassCastException if optimistic != null =>
				target = FitBuffer[U](optimistic :_*)
				optimistic = null
				target.uncheckedUpdate(idx, elem)
		}


	override def update(idx :Int, elems :TraversableOnce[U]) :Unit =
		if (optimistic != null) {
			val es = FitBuffer.of(optimistic.specialization)
			try {
				es ++= elems.asInstanceOf[TraversableOnce[E]]
			} catch {
				case _ :ClassCastException | _ :ArrayStoreException if optimistic != null =>
					target = FitBuffer[U](optimistic :_*)
					optimistic = null
					return
			}
			target.update(idx, es)
		} else
			target.update(idx, elems)


	override def update(fromIndex :Int, value :U, count :Int) :Unit =
		try {
			target.update(fromIndex, value, count)
		} catch {
			case _ :ClassCastException | _ :ArrayStoreException if optimistic != null =>
				target = FitBuffer[U](optimistic :_*)
				optimistic = null
				target.update(fromIndex, value, count)
		}




	override def overwrite(start :Int, length :Int) :FitBuffer[U] = ??? //todo
//		if (spec!=null) new OptimisticFitBuffer[U, E](spec.overwrite(start, length))


	override def +=(elem :U) :this.type =
		if (optimistic != null) {
			val e = try { elem.asInstanceOf[E] } catch {
				case _ :ClassCastException =>
					target = FitBuffer[U](optimistic :_*)
					optimistic = null
					target += elem
					return this
			}
			target += e
			this
		} else {
			target += elem
			this
		}


	override def ++=(xs :TraversableOnce[U]) :this.type =
		if (optimistic != null) {
			val es = FitBuffer.of(optimistic.specialization)
			try { es ++= xs.asInstanceOf[TraversableOnce[E]] } catch {
				case _ :ClassCastException =>
					target = FitBuffer[U](optimistic :_*)
					optimistic = null
					target ++= xs
					return this
			}
			optimistic ++= es
			this
		} else {
			target ++= xs
			this
		}

	override def ++=(elems :FitTraversableOnce[U]) :this.type =
		this ++= (elems :TraversableOnce[U])


	override def +=:(elem :U) :this.type =
		if (optimistic != null) {
			val e = try { elem.asInstanceOf[E] } catch {
				case _ :ClassCastException =>
					target = FitBuffer[U](optimistic :_*)
					optimistic = null
					elem +=: target
					return this
			}
			e +=: target
			this
		} else {
			elem +=: target
			this
		}


	override def insertAll(n :Int, elems :Traversable[U]) :Unit =
		if (optimistic != null) {
			try { elems foreach { _.asInstanceOf[E] } } catch {
				case _ :ClassCastException =>
					target = FitBuffer[U](optimistic :_*)
					optimistic = null
					target.insertAll(n, elems)
					return
			}
			optimistic.insertAll(n, elems.asInstanceOf[Traversable[E]])
		} else {
			target.insertAll(n, elems)
		}


	override def remove(n :Int) :U = target.remove(n)

	override def clear() :Unit = target.clear()

	override def transform(f :U => U) :this.type = {
		target.transform(f)
		this
	}

	override def stringPrefix :String =
		if (optimistic != null) s"FitBuffer[_>:${optimistic.specialization}]"
		else "FitBuffer[_]"


}
