
object playground extends App {

	trait A[+T] {
		protected[this] def set(t :T) :Unit
	}

	trait B extends A[B] {
		set(this)
	}

	class C extends B with A[C] {
		override protected def set(t :C) :Unit = ()
	}


	new C
}
