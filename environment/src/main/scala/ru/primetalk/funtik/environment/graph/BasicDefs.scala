package ru.primetalk.funtik.environment.graph

import scala.annotation.tailrec

trait BasicDefs {

  type Edge[T] = (T, T)

  type GraphEdges[T] = List[(T, T)]

  type Predicate[T] = T => Boolean

  type GraphAsSetOfEdges[T] = (T, T) => Boolean

  type GraphDependencies[T] = Map[T, Set[T]]

  type Tree[T] = Map[T, T]
  // Seq is considered as Set of vertices.
  // The order is maintained
  type GraphAsFunction[T] = T => List[T]

  type WeightedGraphAsFunction[T, W] = T => List[(T, W)]

  type ReversePath[T] = List[T]

  //  type GraphAsStatefulFunction =
  def convertEdgesToDirectDependenciesOnlyForTrees[T](edges: GraphEdges[T]): GraphDependencies[T] =
    edges
      .foldLeft(Map[T, Set[T]]()) {
        case (acc, (s, e)) =>
          acc ++
            Map(
              s -> acc.getOrElse(s, Set()), // just add starting node to keys.
              e -> (acc.getOrElse(e, Set()) + s)
            )
      }

  def invertEdges[T](edges: GraphEdges[T]): GraphEdges[T] =
    edges.map { case (a, b) => (b, a) }

  def convertEdgesToParentToTree[T](edges: GraphEdges[T]): Tree[T] = {
    @tailrec
    def loop(rest: List[(T, T)], tree: Tree[T] = Map()): Tree[T] = rest match {
      case Nil => tree
      case (c, p) :: t =>
        tree.get(c) match {
          case Some(existing) =>
            throw new IllegalArgumentException(s"There are at least two parents for $c: Set($existing, $p)")
          case None =>
            loop(t, tree.updated(c, p))
        }
    }

    loop(edges)
  }

  def convertEdgesToUndirectedGraph[T](edges: GraphEdges[T]): GraphDependencies[T] = {
    val edges2 = edges.flatMap { edge => Seq(edge, (edge._2, edge._1)) }
    convertEdgesToDirectedGraph(edges2)
  }

  def convertEdgesToDirectedGraph[T](edges: GraphEdges[T]): GraphDependencies[T] =
    edges
      .foldLeft(Map[T, Set[T]]()) {
        case (acc, (s, e)) =>
          val oldSet: Set[T] = acc.getOrElse(s, Set())
          acc ++ Map(s -> oldSet.+(e))
      }

  def convertDependenciesToFunction[T](deps: GraphDependencies[T]): GraphAsFunction[T] =
    p => deps.getOrElse(p, Set()).toList
}
