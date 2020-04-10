package ru.primetalk.funtik.environment.generator


sealed trait Tree[T]

case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]
case class Leaf[T](value: T) extends Tree[T]

object Tree {
  def constructNode[T](nodes: Tree[T]*): Tree[T] = nodes match {
    case List(a, b) => Node(a, b)
    case List(a) => a
    case Nil => throw new IllegalArgumentException("Cannot construct tree from an empty list of nodes")
    case a :: tail => constructNode(a, constructNode(tail : _ *))
  }
}
