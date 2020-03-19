package ru.primetalk.funtik.environment.genereator


sealed trait Tree[T]

case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]
case class Leaf[T](value: T) extends Tree[T]
