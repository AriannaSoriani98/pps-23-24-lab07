package ex2

import scala.util.Random

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(val robot: Robot, batteryDec : Int) extends Robot:
  export robot.{position, direction}
  private var battery: Int = 100
  private def consume(): Boolean =
    battery = battery - batteryDec
    battery >= 0
  override def act(): Unit =
    if consume() then robot.act()
  override def turn(dir: Direction): Unit =
    if consume() then robot.turn(dir)

class RobotCanFail(val robot: Robot, probability: Double) extends Robot:
  export robot.{position,direction}
  require(probability <= 100 && probability >= 0)
  private def fail() : Boolean =
    (Random.nextDouble() * 100) > probability
  override def act(): Unit =
    if fail() then robot.act()
  override def turn(dir:Direction):Unit =
    if fail() then robot.turn(dir)

class RobotRepeated(val robot: Robot, repeat: Int) extends Robot:
  export robot.{position,direction, turn}
  require(repeat>=0)
  override def act(): Unit =
    for
      _ <- 1 to repeat
    yield
      robot.act()

@main def testRobot(): Unit =
  val robot = LoggingRobot(SimpleRobot((0, 0), Direction.North))
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East
