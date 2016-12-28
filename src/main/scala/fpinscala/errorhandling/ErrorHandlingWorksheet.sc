import fpinscala.errorhandling._

val o1 = Some(10)
val o2 = Some(20)
val o3 = Some(30)

val x = math.pow(3, 3)

val l1: List[Double] = List(5, 5, 5, 5, 5)
val l2: List[Double] = List(3, 4, 5, 6, 7)
val l3: List[Double] = List(1, 3, 5, 7, 9)

val l4: List[Option[Int]] = List(None, o1, o2, o3)

Option.sequence(l4)

//l1.flatMap(_ + 2)

val sumOfL1 = l1.foldLeft(0.0)(_ + _)
val meanOfL1 = sumOfL1/l1.length
//val varianceOfL1 = l1.flatMap(math.pow(_ - meanOfL1, 2))

//val sumOfL2 = l2.foldLeft(0)(_ + _)
//val meanOfL2 = sumOfL2/l2.length
//
//
//val sumOfL3 = l3.foldLeft(0)(_ + _)
//val meanOfL3 = sumOfL3/l3.length


Option.variance(l1)
Option.variance(l2)
Option.variance(l3)

val option1 = Some(4)
val option2 = Some(90)

option1.map(_ + 10)
option1.flatMap((v: Int) => Some(v + 10))
None.getOrElse(Some(1))
None.getOrElse(11)
None.orElse(Some(99))
Option.map2(option1, option2)(_ * _)