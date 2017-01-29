package net.turambar.collection

import scala.annotation.tailrec


trait Scanner[-I, +O] extends Any {
	def collect(in :Stream[I]) :Stream[O] = scan(in).flatten
	def scan(in :Stream[I]) :Stream[Option[O]]
//	def first[J<:I, O](collect :PartialFunction[J, O]) = Juncture.first(collect.lift)
//	def next[J<:I, O](collect :PartialFunction[J, O]) = Juncture.next(collect.lift)
//	def last[J<:I, O](collect :PartialFunction[J, O]) = Juncture.last(collect.lift)
	
	def filter(satisfying :O=>Boolean) :Scanner[I, O] //=
//		{ in :Stream[I] => scan(in).map(_.filter(satisfying)) }

	def map[T](result :O=>T) :Scanner[I, T] //=
//		{ in :Stream[I] => scan(in).map(_.map(result)) }
	
	def flatMap[H<:I, T](and :O=>Scanner[H, T]) :Scanner[H, T] //=
//		{ in :Stream[H] => ??? }
	
	def flattened[H<:I, P>:O](succ :H=>Scanner[H, P])() :Scanner[H, P] = ???
}


object Scanner {
	import streams._
	
	@inline def first[I, O](pick :I=>Option[O]) :Scanner[I, O] = ???
	@inline def next[I, O](pick :I=>Option[O]) :Scanner[I, O] = ???
	@inline def last[I, O](pick :I=>Option[O]) :Scanner[I, O] = ???
	@inline def prev[I, O](pick :I=>Option[O]) :Scanner[I, O] = ???
	
	@inline def apply[I] = new ScanningOf[I] {}
	
	trait ScanningOf[I] extends Any {
		@inline def first[O](pick :I=>Option[O]) :Scanner[I, O] = Scanner.first(pick)
		@inline def next[O](pick :I=>Option[O]) :Scanner[I, O] = Scanner.next(pick)
		@inline def last[O](pick :I=>Option[O]) :Scanner[I, O] = Scanner.last(pick)
		@inline def prev[O](pick :I=>Option[O]) :Scanner[I, O] = Scanner.prev(pick)
	}
	
	@inline def collect[I] = new CollectScanningOf[I] {}
	
	trait CollectScanningOf[I] extends Any {
		@inline def first[O](collect :PartialFunction[I, O]) :Scanner[I, O] = Scanner.first(collect.lift)
		@inline def next[O](collect :PartialFunction[I, O]) :Scanner[I, O] = Scanner.next(collect.lift)
		@inline def last[O](collect :PartialFunction[I, O]) :Scanner[I, O] = Scanner.last(collect.lift)
		@inline def prev[O](collect :PartialFunction[I, O]) :Scanner[I, O] = Scanner.prev(collect.lift)
	}
	
	
	abstract class OptionScan[-I, +O](private val select :I=>Option[O]) extends Scanner[I, O] {
		override def scan(in: Stream[I]): Stream[Option[O]] = in.map(select)
	}
	
//	implicit def CollectScan[I, O](collect :PartialFunction[I, O]): OptionScan[I, O] = new OptionScan(collect.lift)
	
//	class Head[I, O](input :Stream[I], )
	abstract sealed class ScanWindow[+I] {
		def input :Stream[I]
		def isEmpty :Boolean = input.isEmpty
	}
	
	trait ScanFirst[-I, +O] extends Any with Scanner[I, O] {
		override def filter(satisfying: (O) => Boolean): ScanFirst[I, O] = 
			{ in :Stream[I] => scan(in).map(_.filter(satisfying)) }
		
		override def map[T](result: (O) => T): ScanFirst[I, T] =
			{ in :Stream[I] => scan(in).map(_.map(result)) } //:ScanFirst[I, T]
		
		override def flatMap[H <: I, T](and: (O) => Scanner[H, T]): ScanFirst[H, T] =
			new ScanFirstFlatMap(this, and)
		
		override def flattened[H <: I, P >: O](succ: H => Scanner[H, P])(): Scanner[H, P] = ???
	}
	
	class ScanFirstFlatMap[-I, F, +O](parent :ScanFirst[I, F], fmap :F=>Scanner[I, O]) extends ScanFirst[I, O] {
		
		override def scan(in: Stream[I]): Stream[Option[O]] = {
			def next(input :Stream[I]=in, fs :Stream[Option[F]]=parent.scan(in), os :Stream[Option[O]]=Nones.stream) :Stream[Option[O]] =
				if (fs.isEmpty) os
				else fs.head match {
					case None => os.head #:: next(input.tail, fs.tail, os.tail)
					case Some(f) =>
						val inner = fmap(f).scan(input)
						inner.head #:: next(input.tail, fs.tail, inner.tail)
				}
			next()
		}
		
		override def toString = parent.toString + ".flatMap"
	}
	
/*
	class ScanNextFlatMap[-I, F, O](parent :ScanNext[I, F], fmap :F=>Scanner[I, O]) extends ScanFirst[I, O] {
		override def scan(in: Stream[I]): Stream[Option[O]] = {
			def next(input :Stream[I]=in, fs :Stream[Option[F]]=scan(in), os :Stream[Option[O]]=Nones.stream) :Stream[Option[O]] =
				if (fs.isEmpty) ts
				else os.head match {
					case None => ts.head #:: next(hs.tail, os.tail, ts.tail)
					case Some(o) =>
						val inner = f(o).scan(hs)
						inner.head #:: next(hs.tail, os.tail, inner.tail)
				}
			next()
			@tailrec def next(fs :Stream[Option[F]]=parent.scan(in), lead :Int=0) :Stream[Option[O]] =
				if (fs.isEmpty) Nones(lead)
				else fs.head match {
					case None => next(fs.tail, lead+1)
					case Some(o) => //fmap(o).flatMapped(fmap)(os, gap)(in, in -> 0)(in, 0)
						fmap(o).flattened(fmap)(fs, lead)(in, in->0, in, 0)
				}
			next()
			
		}
		override def toString = parent.toString + ".flatMap"
	}
	
	*/
	
	
	trait ScanNext[-I, +O] extends Any with Scanner[I, O] {
		override def filter(satisfying: (O) => Boolean): ScanNext[I, O] =
			{ in :Stream[I] => scan(in).map(_.filter(satisfying)) }
		
		override def map[T](result: (O) => T): ScanNext[I, T] =
			{ in :Stream[I] => scan(in).map(_.map(result)) } //:ScanNext[I, T]
		
		override def flatMap[H <: I, T](and: (O) => Scanner[H, T]): Scanner[H, T] = ???
	}
	
	
	trait ScanLast[-I, +O] extends Any with Scanner[I, O] {
		override def filter(satisfying: (O) => Boolean): ScanLast[I, O] =
			{ in :Stream[I] => scan(in).map(_.filter(satisfying)) }
		
		override def map[T](result: (O) => T): ScanLast[I, T] =
			{ in :Stream[I] => scan(in).map(_.map(result)) } //:ScanLast[I, T]
		
		override def flatMap[H <: I, T](and: (O) => Scanner[H, T]): Scanner[H, T] = ???
	}
	
	
	trait ScanPrev[-I, +O] extends Any with Scanner[I, O] {
		override def filter(satisfying: (O) => Boolean): ScanPrev[I, O] =
			{ in :Stream[I] => scan(in).map(_.filter(satisfying)) }
		
		override def map[T](result: (O) => T): ScanPrev[I, T] =
			{ in :Stream[I] => scan(in).map(_.map(result)) } //:ScanPrev[I, T]
		
		override def flatMap[H <: I, T](and: (O) => Scanner[H, T]): Scanner[H, T] = ???
	}
	
	
	
	object streams {
		object Nones {
			@inline def stream[T] :Stream[Option[T]] = Stream.cons(None, stream)
			
			@inline def *[T](length :Int) :Stream[Option[T]] =
				if (length<=0) Stream.Empty
				else Stream.cons(None, *(length-1))
		}
		
	}
	
	
}