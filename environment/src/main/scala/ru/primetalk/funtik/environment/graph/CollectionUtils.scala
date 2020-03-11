package ru.primetalk.funtik.environment.graph

object CollectionUtils {

  /**
   * argMin searches for such a value in the list that provides minimal value of f
   * @param s should not be empty
   * @return one of the elements in s where f(s) is minimal
   */
  def argMin[T](s: List[T], f: T => Int): (T, Int) =
    s.tail.foldLeft((s.head, f(s.head))){
      case ((tmin, fmin), tnew) =>
        val fnew = f(tnew)
        if(fnew < fmin)
          (tnew, fnew)
        else
          (tmin, fmin)
    }

  /**
   * Find two elements of the two sets with minimal distance.
   * This algorithm does not guarantee that the distance is a global minimum. It's only a local one.
   * @param dist distance function
   * @return
   */
  def almostShortestDistance[T](s1: List[T], s2: List[T], dist: T => T => Int): (T, T, Int) = {
    val p1 = s1.head
    val (p2, _) = argMin(s2, dist(p1))
    val (p11, d) = argMin(s1, dist(p2))
    (p11, p2, d)
  }
}
