package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case Some(x) => Some(f(x))
      case _ => None
    }

  def getOrElse[B>:A](default: => B): B =
    this match {
      case Some(x) => x
      case _ => default
    }

  //  flatMap[B](f: (A) ⇒ Option[B]): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] =
//    this map (Some(_)) getOrElse ob
//    this map ((o: A) => Some(o)) getOrElse ob
    this match {
      case Some(x) => this
      case _ => ob
    }

  def filter(f: A => Boolean): Option[A] =
  //  flatMap(a => if (f(a)) Some(a) else None)
    this match {
      case Some(x) if f(x) => this
      case _ => None
    }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
//    val m = mean(xs).getOrElse(0.0)
//    val d: Double = xs.foldLeft(0.0)((acc: Double, x: Double) => acc + math.pow(x - m, 2))/xs.length
//    Some(d)
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    println(s"a's type: ${a.getClass} value: $a")
    println(s"b's type: ${b.getClass} value: $b")
    val result = a.flatMap((valueOfA: A) => b.map((valueOfB: B) => f(valueOfA, valueOfB)))
    println(s"result's type: ${result.getClass} value: $result")
    result
    //    a flatMap (aa => b map (bb => f(aa, bb)))
  }

  def map3[A,B,C,D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] =
    a.flatMap(aa => b.flatMap(bb => c.map(cc => f(aa, bb, cc))))


  def sequence[A](a: List[Option[A]]): Option[List[A]] =
//mine:    traverse(a)((p: Option[A]) => p.map(a => a))
    traverse(a)(p => {println(s"p: $p"); p})

  def sequenceOrig[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
   a.foldRight[Option[List[B]]](Some(Nil)) ((h,acc) => { println(s"h: $h"); map2(f(h),acc)(_ :: _)})
//mine:            a.foldRight(Some(List()): Option[List[B]])((x: A,y: Option[List[B]]) => map2(f(x), y)(_ :: _))
}

//For sequence's cal to traverse A = Option[B].  Which means that def traverse[B](a: List[Option[B]])(f: Option[B] => Option[B]): Option[List[B]]
//sequence: List[Option[Int]] => Option[List[Int]]
//  traverse(List[Option[Int]]) (Option[Int] => Option[Int])
//    List[Option[Int]].foldRight[Option[List[Int]]](Some(Nil)((Option[Int], Option[List[Int]]) => map2(Option[Int], Option[List[Int]])(_ :: _))
//map2: Option[A], Option[List[A]] => Option[List[A]]
/*
h: Some(3)
p: Some(3)
a's type: class fpinscala.errorhandling.Some value: Some(3)
b's type: class fpinscala.errorhandling.Some value: Some(List())
result's type: class fpinscala.errorhandling.Some value: Some(List(3))
h: Some(2)
p: Some(2)
a's type: class fpinscala.errorhandling.Some value: Some(2)
b's type: class fpinscala.errorhandling.Some value: Some(List(3))
result's type: class fpinscala.errorhandling.Some value: Some(List(2, 3))
h: Some(1)
p: Some(1)
a's type: class fpinscala.errorhandling.Some value: Some(1)
b's type: class fpinscala.errorhandling.Some value: Some(List(2, 3))
result's type: class fpinscala.errorhandling.Some value: Some(List(1, 2, 3))
res0: fpinscala.errorhandling.Option[List[Int]] = Some(List(1, 2, 3))
 */