package ru.primetalk.funtik.environment

import ru.primetalk.funtik.environment.generator.utils.Random.RandomStateValue

import scala.concurrent.duration.{Duration, DurationInt, FiniteDuration}
import ru.primetalk.funtik.environment.generator.{BSPTree, Tree}
import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._
import cats.data.State
import ru.primetalk.funtik.environment.geom2d.{CollisionShape, Vector2d}
import ru.primetalk.funtik.environment.solid.SolidBodyModel._
import squants.time.Milliseconds

trait MechanicsImplT extends EnvironmentModel {

  class MechanicsImpl[S](val robotStrategy: RobotStrategy[S], val initialRobotState: S) extends ModelMechanics[S] {

    private def toPoints: Tree[Rectangle] => List[Vector2d[Int]] =
      Tree.eliminate[Rectangle, List[Position]](_.edges)(_ reverse_::: _)

    private def toRooms: Tree[Rectangle] => List[Rectangle] =
      Tree.eliminate[Rectangle, List[Rectangle]](List(_))(_ reverse_::: _)


    def generateDisplay: State[RandomStateValue, WorldWithRooms] = {
      val rect = Rectangle(Vector2d(-40, -30), Vector2d(80, 60))
      BSPTree(minSideSize = 40)(rect)
        .map(t => toPoints(t) -> toRooms(t)).map { case (points, rooms) =>
        val worldMap =  Display.showPoints(points, true, false)
        WorldWithRooms(worldMap, rooms)
      }
    }

    val defaultDuration: FiniteDuration = 40.milliseconds

    /** the returned Duration is the next event */
    override def start(wallTimeMs: Long): State[RandomStateValue, (WorldState[S], Duration)] = {
      generateDisplay.map { world =>

        val material = MaterialParticleState(
          position = Vector2d(1.0, 1.0),
          speed = Up.toDouble,
          Milliseconds(wallTimeMs) / su.time
        )
        val solidBody = SolidBodyState(material, 0.0, 0.1)
        (
          WorldState(RobotEnvState(solidBody), world, initialRobotState),
          defaultDuration
        )
      }
    }

    import monocle.macros.GenLens

    private val stateRobotMemoryLens = GenLens[WorldState[S]](_.robotMemory)

    private val stateMaterialLens = GenLens[WorldState[S]](_.robotEnvState) composeLens
      GenLens[RobotEnvState](_.solidBodyState) composeLens
      GenLens[SolidBodyState](_.materialParticle)


    /**
     * Receives an event from scaffolding (like real timer, key press, mouse click).
     * Despite that we try to perform deterministic modelling, we still need
     * to generate white noise for signals. Hence, Random state.
     */
    override def handleEvent(
                              state: WorldState[S],
                              e: ModellingEvent
                            ): State[RandomStateValue, (WorldState[S], Duration)] = e match {
      case ScaffoldingTimePassed(simulationTime) =>
        val time = Milliseconds(simulationTime) / su.time
        println(s"TICK! $time")
        val newState = discreteIntegrate(state, time)
        State.pure(newState, defaultDuration)
    }

    def discreteIntegrate(state: WorldState[S], wallTime: Double): WorldState[S] = {

      def discreteIntegrate0(worldState: WorldState[S], simulationTime: Double): WorldState[S] = {
        println(s"discreteIntegrate0! $simulationTime")
        val lines = worldLines(worldState)
        println("Lines count: " + lines.size)
        println("Lines:\n" + lines.map(l => l.p1 -> l.p2).mkString("\n"))
        val materialParticle = worldState.robotEnvState.solidBodyState.materialParticle
        println(s"Material $materialParticle")
        val collisions = lines.flatMap(materialParticle.detectNearestCollision)
        println("Collisions " + collisions.size)
        import Ordering.Double.TotalOrdering
        val deltaTime = collisions.minOption.getOrElse(throw new IllegalStateException("No collision detected"))
        println(s"deltaTime = $deltaTime")
        val minHitTime = math.abs(deltaTime) + simulationTime
        val hitBeforeWallTime = minHitTime <= simulationTime

        val integrationTime = Math.min(minHitTime, simulationTime)
        println("IntegrateTime: "  + integrationTime)
        val timeIntegratedState = tickTime(worldState, integrationTime.toLong)

        if(hitBeforeWallTime) {
          val sensorData = HitObstacle()
          val (newMemory, commands) = robotStrategy(worldState.robotMemory, sensorData)
          assert(commands.size == 1)

          val newWorldState = commands.head match {
            case SetSpeed(left, right) =>
              println(s"new speed $left, $right")
              val newSpeed = Vector2d(left, right)
              val material = timeIntegratedState.robotEnvState.solidBodyState.materialParticle.setSpeed(minHitTime, newSpeed)
              val newState = stateRobotMemoryLens.set(newMemory)(
                stateMaterialLens.set(material)(worldState)
              )
              discreteIntegrate0(newState, simulationTime)
            case other =>
              println(s"ERROR - Unexpected command ${other.getClass}")
              worldState
          }
          newWorldState
        } else {
          timeIntegratedState
        }
      }

      discreteIntegrate0(state, wallTime)
    }

    def tickTime(worldState: WorldState[S], time: Long): WorldState[S] = {
      val materialState = worldState.robotEnvState.solidBodyState.materialParticle.integrate(time)
      val timeSensor  = TimePassed(time.toLong)
      val (newMemory, commands) = robotStrategy(worldState.robotMemory, timeSensor)
      assert(commands.isEmpty, "expected empty commands")
      stateRobotMemoryLens.set(newMemory)(
        stateMaterialLens.set(materialState)(worldState)
      )
    }



    def worldLines(state: WorldState[S]): Seq[CollisionShape.LineSegment[Double]] = {
      state.worldData.rooms.flatMap{ r =>
        Seq(
          CollisionShape.LineSegment(r.topLeft.toDouble, r.topRight.toDouble),
          CollisionShape.LineSegment(r.topRight.toDouble, r.bottomRight.toDouble),
          CollisionShape.LineSegment(r.bottomRight.toDouble, r.bottomLeft.toDouble),
          CollisionShape.LineSegment(r.bottomLeft.toDouble, r.topLeft.toDouble)
        )
      }
    }
  }

}