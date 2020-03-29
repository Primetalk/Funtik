package ru.primetalk.funtik.environment

import ru.primetalk.funtik.environment.geom2d.{Geom2dUtils, Vector2dBasicSyntax}
import Geom2dUtils.{Direction, Position, Rectangle, Vector, directions8, mainDirections, vector2d}
import ru.primetalk.funtik.environment.graph.GraphUtils
import ru.primetalk.funtik.environment.graph.CollectionUtils
import ru.primetalk.funtik.environment.graph.GraphUtils.GraphAsFunction
import Vector2dBasicSyntax._
import ru.primetalk.funtik.environment.geom2d.Vector2dSyntax._
import Geom2dUtils._
import spire.syntax.all._

import scala.annotation.tailrec
import scala.reflect.ClassTag

// TODO: DisplayView - rotation on n*90; shift; constrain size; flip up/down
// DONE showDisplay
// TODO: DrawDisplay on canvas (Scala.js)
// TODO: Remove state. Mutable array could be provided from outside as an implicit context
// TODO: Use refined type for array size,vector size.
case class Display[T: ClassTag](offset: Vector, size: Vector)(init: Option[() => Array[Array[T]]] = None) {
  lazy val rect: Rectangle = Rectangle(offset, size)

  // initial: () => T = () => {implicitly[Numeric[T]].zero}
  /** We only allocate array when it's needed*/
  lazy val array: Array[Array[T]] =
    init.getOrElse(() => Array.ofDim[T](size._2, size._1)).apply()

  def lineY(y: Int): Array[T] = array(y - minY)

  val minX: Int = offset._1
  val maxXplusExtra1: Int = minX + size._1
  val maxX: Int = maxXplusExtra1 - 1

  val minY: Int = offset._2
  val maxYplusExtra1: Int = minY + size._2
  val maxY: Int = maxYplusExtra1 - 1

  def xs = Range(minX, maxXplusExtra1)
  def ys = Range(minY, maxYplusExtra1)

  def isWithinRange(p: Position): Boolean =
    p._1 >= minX && p._1 <= maxX &&
      p._2 >= minY && p._2 <= maxY

  def adjacentPositions(p: Position): Seq[Position] =
    mainDirections.map(_ + vector2d(p)).filter(isWithinRange)

  def positionsAround(p: Position): Seq[Position] =
    directions8.map(_ + p).filter(isWithinRange)

  def points: List[Position] =
    (for{
      j <- ys
      i <- xs
    } yield (i, j)
      ).toList

  def pointsFiltered(pred: T => Boolean): List[Position] =
    points.filter(p => pred(apply(p)))

  def values: Seq[T] =
    for{
      j <- ys
      i <- xs
    } yield apply(i, j)

  def valuesOnEdges: Set[T] =
    {
      val jValues = for {
        j <- ys
        v <- Seq(apply((minX, j)), apply((maxX, j)))
      } yield v

      val iValues = for {
        i <- xs
        v <- Seq(apply((i, minY)), apply((i, maxY)))
      } yield v

      iValues.toSet ++ jValues.toSet
    }

  def valuesAround(p: Position): Seq[T] = {
    positionsAround(p)
      .map(apply)
  }
  /** Enumerates all positions on edges.
    * O(N+M)
    * The order is not guaranteed.
    * It might be considered as Set.
    */
  def edges: Seq[Position] = {
    if(maxX < minX || maxY < minY)
      Seq()
    else if(maxX == minX)
      ys.map((minX, _))
    else if(maxY == minY)
      xs.map((_, minY))
    else
      xs.map((_, minY)) ++
        xs.map((_, maxY)) ++
        (minY + 1).until(maxY).map((minX, _)) ++
        (minY + 1).until(maxY).map((maxX, _))
  }

  def apply(position: Position): T = {
    val p = position - offset
    array(p.y)(p.x)
  }

  /**
    * Updates the position if it is present. Ignores otherwise.
    * This might be helpful if we search for some condition in a given area and don't
    * want to store large array of data.
    */
  def safeUpdate(position: Position, v: T): Boolean = {
    val wasUpdated = isWithinRange(position)
    val p = position - offset
    if(wasUpdated) {
      array(p._2)(p._1) = v
    }
    wasUpdated
  }

  def update(position: Position, v: T): Unit = {
    val p = position - offset
    array(p._2)(p._1) = v
  }

  /** Sum of all elements in rect inclusive boundaries.
    * Rectangle should be within display boundaries.
    */
  def inclusiveRectSum(topLeft: Position, bottomRight: Position)(implicit num: Numeric[T]): T = {
    val tl = topLeft - offset
    val br = bottomRight - offset

    @tailrec
    def go(i: Int, j: Int, accum: T): T = {
      if (j > br._2)
        accum
      else {
        if (i > br._1)
          go(tl._1, j + 1, accum)
        else
          go(i + 1, j, num.plus(accum, array(i)(j)))
      }
    }

    go(tl._1, tl._2, num.zero)
  }

  //    d.array = array.transpose
  def transpose: Display[T] = {
    val d = Display[T](offset.transpose, size.transpose)()
    for{
      p <- points
      pp = p.transpose
    } {
      d(pp) = apply(p)
    }
    d
  }

  /** Draws the function on this display. */
  def renderFunction(f: Position => T): Unit = {
    for{
      p <- points
    } {
      this(p) = f(p)
    }
  }

  def draw(positions: List[Position], t: T): Unit =
    for{ p <- positions } {
      this(p) = t
    }

  /** Fill display with the given value.*/
  def fillAll(value: => T): Unit = {
    def arr = Array.fill(size._1)(value)
    for{
      j <- 0 until size._2
    } {
      array(j) = arr
    }
  }

  def showDisplay(colWidth: Int = 1)(show: T => String = _.toString): String = {
    (for{
      y <- ys
    } yield {
      lineY(y)
        .map(show)
        .map(_.padTo(colWidth, ' ')).mkString
    }).mkString("\n")
  }

  /** Transform this display according to cellular automaton rules. */
  def produceByLocalRules(rules: (T, Seq[T]) => T): Display[T] = {
    val d = new Display[T](offset, size)()
    for{
      p <- points
      v = valuesAround(p)
      next = rules(apply(p), v)
    } {
      d(p) = next
    }
    d
  }

  def map[B: ClassTag](f: T => B): Display[B] = {
    val a: Array[Array[B]] = array.map(_.map(f).toArray)
    new Display[B](offset, size)(Some(() => a))
  }

  def flipY: Display[T] = {
    val a: Array[Array[T]] = array.reverse
    new Display[T]((offset._1, -offset._2), size)(Some(() => a))
  }
}

object Display {
  def apply[T: ClassTag](rect: Rectangle): Display[T] = {
    new Display[T](rect.topLeft, rect.size)()
  }

  def readCharDisplay(lines: Seq[String]): Display[Char] = {
    val size = (lines.head.length, lines.length)
    val a = lines.map(_.toCharArray).toArray
    val d = Display[Char]((0,0), size)(Some(() => a))
    d
  }

  def eq[T](d1: Display[T], d2: Display[T]): Boolean = {
    d1.offset == d2.offset &&
    d1.size == d2.size &&
    d1.array.zip(d2.array)
      .forall{ case (a,b) => a.sameElements(b) }
  }

  def showPoints[T: ClassTag](points: Seq[Position], point: T, empty: T): Display[T] = {
    val rect = Geom2dUtils.boundingRect(points)
    val d = Display[T](rect)
    d.fillAll(empty)
    points.foreach(p => d(p) = point)
    d
  }

  // displayAsGraph converts display to a graph.
  def displayAsGraph[T](d: Display[T], directions: List[Direction], isFreeCell: T => Boolean): GraphAsFunction[Position] =
    p => directions.
      map(p + _).
      filter(p => isFreeCell(d(p)))

  // finds all not connected parts of the graph.
  def connectedComponents2[T](d: Display[T], directions: List[Direction], isFreeCell: T => Boolean): List[Set[Position]] = {
    val g = displayAsGraph(d, directions, isFreeCell)
    GraphUtils.connectedComponents(d.pointsFiltered(isFreeCell), g)
  }

  def connectAllSomehow[T](d: Display[T], directions: List[Direction], freeCell: T): Unit = {
    val components = connectedComponents2(d, directions, (_: T) == freeCell)
    val componentsAsLists: List[List[Position]] = components.map(_.toList)
    val pairsOfComponents: List[List[List[Position]]] = componentsAsLists.sliding(2, 1).toList
    pairsOfComponents.foreach{
      case List(lst1, lst2) =>
        val (p1, p2, _) = CollectionUtils.almostShortestDistance[Position](lst1, lst2, a => b => Geom2dUtils.manhattanDistance(a, b))
        val positions = Geom2dUtils.bresenhamLine(p1.x + 0.5, p1.y + 0.5, p2.x + 0.5, p2.y + 0.5)
        positions.foreach{ p => d(p) = freeCell }
    }
  }
}
