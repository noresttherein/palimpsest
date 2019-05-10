package net.turambar.palimpsest.specialty.seqs


import scala.reflect.ClassTag

import net.turambar.palimpsest.specialty.{forceFit, Elements, FitCompanion, RuntimeType, SpecializableIterable}
import net.turambar.palimpsest.testutil._
import org.scalacheck.Prop._
import org.scalacheck.Properties


/**
  * @author Marcin Mościcki
  */
class FitSeqProps[+S[@specialized(Elements) X] <: FitSeq[X] with SpecializableIterable[X, S]]
		(companion :FitCompanion[S])
//	extends Properties(companion.toString)
{
	
}



object FitSeqProps extends App {

	
	
	implicit class SharedArrayChecks[E](val seq :FitSeq[E]) extends AnyVal {
		def sized(size :Int) = (seq.size ?= size) && (seq.length ?= size) :| s"size is $size"
		
		def contents(at :Int=>E) = ((seq :Seq[E]) ?= (0 until seq.length).map(at)) :| "correct elements"
		//all((0 until seq.size).map(i => (seq(i) ?= at(i)) :| s"seq($i)==${at(i)}") :_*) :| "correct elements"
		
		def indexes(at :Int=>E) = all((0 until seq.length).map { i => seq(i) ?= at(i) } :_*) :| "indexes correctly"
		
		def checksRange =
			(seq(-1).throws[IndexOutOfBoundsException] && seq(seq.length).throws[IndexOutOfBoundsException]) :| "checks range"
		
	}
	
	class BasicSeqProperties[E :RuntimeType](seq :FitSeq[E], values :Int=>E, size :Int, dummy :E) extends Properties(seq.toString) {
		implicit def classTag = RuntimeType[E].classTag.asInstanceOf[ClassTag[E]]
		
		def valueSeq :Seq[E] = Stream.tabulate(size)(values)
		
		def this(seq :FitSeq[E], values :Seq[E], dummy :E) = this(seq, values, values.size, dummy)
		
		property("size") = seq.sized(size)
		property("indexing") = seq.contents(values)
		property("rangeChecked") = seq.checksRange
		property("equals") = seq =?= valueSeq

		property("head") = (
			if (size==0) seq.head.throws[NoSuchElementException]
			else seq.head ?= values(0)
			) :| "head"
		
		property("headOption") = seq.headOption ?= (if (size>0) Some(values(0)) else None)
		
		property("tail") = (
			if (size==0) seq.tail.throws[UnsupportedOperationException]
			else seq.tail.toSeq ?= ((1 until size).map(values)(forceFit) :FitSeq[E])
			) :| "tail"
		
		
		property("last") = (
			if (size==0) seq.last.throws[NoSuchElementException]
			else seq.last ?= values(size-1)
			) :| "last"
		
		property("lastOption") = seq.lastOption ?= (if (size>0) Some(values(size-1)) else None)

		property("init") = (
			if (size==0) seq.init.throws[UnsupportedOperationException]
			else (seq.init :Seq[E]) ?= valueSeq.init
		) :| "init"
		
		
		property("toArray") = seq.toArray.toSeq == seq && (seq :Seq[Any]).toArray[Any].toSeq == seq
		
		property("copyToArray") = {
//			val ints = new Array[Int](10); (0 until 10).foreach { i => ints(i) = -1 }
			val buff = RuntimeType.arrayFor(10)
			buff.indices foreach { buff(_) = dummy }
			seq.copyToArray(buff, 1, 8)
			val copied = seq.length min 8
			(buff.toSeq ?= dummy +: (0 until copied).map(values) ++: Array.fill(9-copied)(dummy)) :| "copyToArray"
		} && {
			val refs = new Array[Any](10)
			seq.copyToArray(refs, 1, 8)
			val copied = Math.min(seq.length, 8)
			(refs.toSeq ?= null +: (0 until copied).map(values) ++: Array.fill(9-copied)(null)) :| "copyToArray(Array[Any])"
		}
		
		
		
	}	
}
