import fpinscala.datastructures._

//var tree1: Branch.type = Branch

val tree2: Leaf[Int] = Leaf(8)
val tree3: Branch[Int] =
  Branch(
    Branch(
      Leaf(3),
      Leaf(8)),
    Branch(Leaf(2), Leaf(9)))

tree2.value

Tree.fold(tree3)(x => 1)((b1, b2) => b1 + b2)

//Tree.fold(tree)((x) => 1)((b1, b2) => 1 + 1)
def sizeViaFold[A](t: Tree[A]): Int =
Tree.fold(t)(a => 1)(1   + _ + _)

def maximumViaFold(t: Tree[Int]): Int =
  Tree.fold(t)(a => a)(_ max _)

def depthViaFold[A](t: Tree[A]): Int =
  Tree.fold(t)(a => 0)((d1, d2) => 1 + (d1 max d2))

def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
  Tree.fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
def t2 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(9),Leaf(3)))
sizeViaFold(t) //shouldBe 5
maximumViaFold(t) //shouldBe 3
depthViaFold(t) //shouldBe 2
sizeViaFold(t) //shouldBe 7
maximumViaFold(t) //shouldBe 9
depthViaFold(t) //shouldBe 2
mapViaFold(t)(_ % 2 == 0) //shouldBe Branch(Branch(Leaf(false), Leaf(true)), Leaf(false))
