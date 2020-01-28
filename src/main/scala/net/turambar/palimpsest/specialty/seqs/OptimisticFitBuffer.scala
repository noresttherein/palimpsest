package net.turambar.palimpsest.specialty.seqs

import net.turambar.palimpsest.specialty.{ItemTypes, FitTraversableOnce}
import net.turambar.palimpsest.specialty.iterables.IterableViewTemplate
import net.turambar.palimpsest.specialty.RuntimeType.Specialized.Fun1Res

import scala.collection.{mutable, GenTraversableOnce}

/**
  */
class OptimisticFitBuffer[U, E <: U](private[this] var optimistic :FitBuffer[E])
	extends FitBuffer[U] with IterableViewTemplate[U, FitBuffer[U]]
{ outer =>

	@inline final protected def source :FitBuffer[U] = target

	private[this] var target :FitBuffer[U] =
		optimistic.asInstanceOf[FitBuffer[U]]



	override def length :Int = target.length


	override protected def at(idx :Int) :U = target.get(idx)

//	override protected def section(from :Int, until :Int) :FitBuffer[U] =
//		if (optimistic != null)
//			new OptimisticFitBuffer[U, E](optimistic.slice(from, until))
//		else
//			target.slice(from, until)


	protected def goPessimistic() :Unit = {
		target = FitBuffer[U](optimistic :_*)
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
			val es = FitBuffer.of(optimistic.runtimeType)
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




	override def overwrite(start :Int, length :Int) :FitBuffer[U] =
		new OptimisticFitBuffer[U, U](target.overwrite(start, length)) {
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
			val es = FitBuffer.of(optimistic.runtimeType)
			try { es ++= xs.asInstanceOf[TraversableOnce[E]] } catch {
				case _ :ClassCastException | _ :ArrayStoreException =>
					goPessimistic()
					target ++= xs
					return this
			}
			optimistic ++= es
			this
	}


	override def ++=(elems :FitTraversableOnce[U]) :this.type =
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
			val xs = try { FitBuffer.of(target.runtimeType) ++= elems } catch {
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
		if (optimistic != null) s"FitBuffer[_>:${optimistic.runtimeType}]"
		else "FitBuffer[_]"


}
