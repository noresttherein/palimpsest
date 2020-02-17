package net.noresttherein.palimpsest.seqs

import net.noresttherein.palimpsest.{ItemTypes, Vals}
import net.noresttherein.palimpsest.iterables.IterableViewTemplate
import net.noresttherein.palimpsest.RuntimeType.Specialized.Fun1Res

import scala.collection.{mutable, GenTraversableOnce}

/**
  */
class OptimisticAptBuffer[U, E <: U](private[this] var optimistic :AptBuffer[E])
	extends AptBuffer[U] with IterableViewTemplate[U, AptBuffer[U]]
{ outer =>

	@inline final protected def source :AptBuffer[U] = target

	private[this] var target :AptBuffer[U] =
		optimistic.asInstanceOf[AptBuffer[U]]



	override def length :Int = target.length


	override protected def at(idx :Int) :U = target.get(idx)

//	override protected def section(from :Int, until :Int) :AptBuffer[U] =
//		if (optimistic != null)
//			new OptimisticAptBuffer[U, E](optimistic.slice(from, until))
//		else
//			target.slice(from, until)


	protected def goPessimistic() :Unit = {
		target = AptBuffer[U](optimistic :_*)
		optimistic = null
	}

	override def indexOf[O >: U](elem :O, start :Int) :Int =
		try {
			target.indexOf(elem, start)
		} catch {
			case _ :ClassCastException | _ :ArrayStoreException if optimistic != null =>
				goPessimistic()
				target.indexOf(elem, start)
		}


	override protected def set(idx :Int, elem :U) :Unit =
		try {
			target.trustedSet(idx, elem)
		} catch {
			case _ :ClassCastException | _ :ArrayStoreException if optimistic != null =>
				goPessimistic()
				target.trustedSet(idx, elem)
		}


	override def update(idx :Int, elems :TraversableOnce[U]) :Unit =
		if (optimistic != null) {
			val es = AptBuffer.of(optimistic.runtimeType)
			try {
				es ++= elems.asInstanceOf[TraversableOnce[E]]
			} catch {
				case _ :ClassCastException | _ :ArrayStoreException if optimistic != null =>
					goPessimistic()
			}
			target.update(idx, es)
		} else
			target.update(idx, elems)


	override def update(fromIndex :Int, value :U, count :Int) :Unit =
		try {
			target.update(fromIndex, value, count)
		} catch {
			case _ :ClassCastException | _ :ArrayStoreException if optimistic != null =>
				goPessimistic()
				target.update(fromIndex, value, count)
		}




	override def overwrite(start :Int, length :Int) :AptBuffer[U] =
		new OptimisticAptBuffer[U, U](target.overwrite(start, length)) {
			protected override def goPessimistic() :Unit = {
				outer.goPessimistic()
				optimistic = null
				target = outer.source.overwrite(start, length)
			}
		}


	override def +=(elem :U) :this.type = {
		val count = target.length
		try { target += elem.asInstanceOf[U] } catch {
			case _ :ClassCastException | _ :ArrayStoreException if optimistic != null =>
				target.trimEnd(target.length - count)
				goPessimistic()
				target += elem
		}
		this
	}


	override def ++=(xs :TraversableOnce[U]) :this.type = xs match {
		case _ if optimistic == null =>
			target ++= xs; this
		case repeated :Traversable[U] => //we can reread them if needed
			val count = target.length
			try { target ++= xs.asInstanceOf[Traversable[U]] } catch {
				case _ :ClassCastException | _ :ArrayStoreException if optimistic != null =>
					val extras = target.length - count //we don't take any unnecessary chances
					target.trimEnd(extras)
					goPessimistic()
					target ++= repeated.toIterator.drop(extras)
			}
			this
		case _ => //need to copy the items
			val es = AptBuffer.of(optimistic.runtimeType)
			try { es ++= xs.asInstanceOf[TraversableOnce[E]] } catch {
				case _ :ClassCastException | _ :ArrayStoreException =>
					goPessimistic()
					target ++= xs
					return this
			}
			optimistic ++= es
			this
	}


	override def ++=(elems :Vals[U]) :this.type =
		this ++= (elems :TraversableOnce[U])


	override def +=:(elem :U) :this.type = {
		val count = target.length
		try { elem +=: target } catch {
			case _ :ClassCastException | _ :ArrayStoreException if optimistic != null =>
				target.trimStart(target.length - count)
				goPessimistic()
				elem +=: target
		}
		this
	}


	override def insertAll(n :Int, elems :Traversable[U]) :Unit =
		if (optimistic != null) {
			val xs = try { AptBuffer.of(target.runtimeType) ++= elems } catch {
				case _ :ClassCastException =>
					goPessimistic()
					elems
			}
			target.insertAll(n, xs)
		} else {
			target.insertAll(n, elems)
		}


	override def remove(n :Int) :U = target.remove(n)

	override def clear() :Unit = target.clear()

	override def transform(f :U => U) :this.type = {
		goPessimistic()
		target.transform(f)
		this
	}

	override def stringPrefix :String =
		if (optimistic != null) s"AptBuffer[_>:${optimistic.runtimeType}]"
		else "AptBuffer[_]"


}
