package ru.primetalk.funtik.environment.genereator


sealed trait Tree[T]

case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]
case class Leaf[T](value: T) extends Tree[T]

object Tree {
  def apply[T](nodes: Tree[T]*): Tree[T] = nodes match {
    case List(a, b) => Node(a, b)
    case List(a) => a
    case Nil => throw new IllegalArgumentException("Cannot construct tree from an empty list of nodes")
    case a :: tail => apply(a, apply(tail : _ *))
  }

  def eliminate[T, A](leaf: T => A)(node: (A, A) => A): Tree[T] => A = {
    case Leaf(v) => leaf(v)
    case Node(left, right) =>
      val l = eliminate(leaf)(node)(left)
      val r = eliminate(leaf)(node)(right)
      node(l, r)
  }
}
