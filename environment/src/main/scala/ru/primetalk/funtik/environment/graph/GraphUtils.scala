package ru.primetalk.funtik.environment.graph

object GraphUtils extends BasicDefs {
  // finds all nodes reachable from any of the `start` nodes
  def findConnectedComponent[T](start: List[T], g: GraphAsFunction[T]): Set[T] = {
    @scala.annotation.tailrec
    def findConnectedComponent0(toVisit: List[T], found: Set[T]): Set[T] = toVisit match {
      case head :: tail =>
        if(found.contains(head))
          findConnectedComponent0(tail, found)
        else {
          val connected = g(head)
          findConnectedComponent0(connected reverse_::: tail, found + head)
        }
      case Nil => found
    }
    findConnectedComponent0(start, Set())
  }

  // finds all connected components starting from any of the given nodes
  @scala.annotation.tailrec
  def connectedComponents[T](toVisit: List[T], g: GraphAsFunction[T], found: List[Set[T]] = Nil): List[Set[T]] = toVisit match {
    case head :: next =>
      val s = GraphUtils.findConnectedComponent(List(head), g)
      connectedComponents(toVisit.filterNot(s.contains),g, s :: found)
    case Nil => found
  }

}
