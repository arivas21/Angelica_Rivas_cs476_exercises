object exercises extends App {

  //Implement the method isSorted given its signature, which checks whether an Array[String]
  // and it is sorted according to a given comparison function:
  //def isSorted(as: Array[String], ordered: (String,String) => Boolean): Boolean.

  def isSorted(as: Array[String], Ordered: (String,String) => Boolean): Unit = {
    @annotation.tailrec
    def Compute(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (!Ordered(as(n), as(n + 1))) false
      else Compute(n + 1)

    Compute(0)

    println(isSorted(Array("Functional", "Exercises"), (x: String, y: String) => x.length < y.length))

  }




  //Implement the method isSorted given its signature, which checks whether an Array[A] where A is a type variable
  // and it is sorted according to a given comparison function:
  //def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean.

  def isSorted[A](as: Array[A], Ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def Compute(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (!Ordered(as(n), as(n + 1))) false
      else Compute(n + 1)

    Compute(0)
  }

  println(isSorted(Array(4, 5), (x: Int, y: Int) => x < y))
  println(isSorted(Array(6, 3), (x: Int, y: Int) => x < y))



  //Implement the higher-order function that composes two functions given its signature:
  //def compose[A,B,C](f: B => C, g: A => B): A => C

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  val f = (a: Int) => a / 2
  val g = (b: Int) => b + 2


  println(compose(f, g)(3))
  println(compose(f, g)(1) == compose(g, f)(1) )





  //Implement uncurry, which reverses the transformation of curry.
  // Note that since => associates to the right, A => (B => C) can be written as A => B => C.
  // The signature of this function is the following:  def uncurry[A,B,C](f: A => B => C): (A, B) => C

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  val x = (a: Int, b: Int) => a + b
  val y = (a: Int) => (b: Int) => a + b

  println(uncurry(y)(1, 1) == y(1)(1))
  println(uncurry(y)(1, 1) == x(1, 1))

  // Implement the method curry given its signature.
  //def curry[A,B,C](f: (A, B) => C): A => (B => C)

  def curry[A,B,C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  val c = (a: Int, b: Int) => a + b
  val s = (a: Int) => (b: Int) => a + b


  println(curry(c)(1)(1) == c(1, 1))
  println(curry(c)(1)(1) == s(1)(1))






}
