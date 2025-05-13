package ex2

type Position = (Int, Int)
val BATERY: Int = 10
val DEC_BAT: Int = 2

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

class RobotWithBattery(val robot: Robot) extends Robot:
  var batery: Int = BATERY
  export robot.{position, direction, turn}
  override def act(): Unit =
      if batery >= DEC_BAT then
        robot.act();
        batery = batery - DEC_BAT


class RobotCanFail(val robot: Robot, val prob: Int) extends Robot:
  val random = new scala.util.Random
  export robot.{position, direction, turn}

  override def act(): Unit =
    val randNum = random.nextInt(10)
    if randNum >= prob then robot.act()

class RobotRepeated(val robot: Robot, val reps: Int) extends Robot:
  export robot.{position, direction}

  override def act(): Unit =
    for (i<-0 until reps)
      robot.act()

  override def turn(dir: Direction): Unit =
    for (i <- 0 until reps)
      robot.turn(dir: Direction)


@main def testRobot(): Unit =
  val robot = LoggingRobot(SimpleRobot((0, 0), Direction.North))
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East
