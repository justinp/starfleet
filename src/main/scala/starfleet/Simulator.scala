package starfleet

import scala.language.postfixOps

import java.io.PrintWriter
import scala.io.Source
import Field._

/* First, we need a data structure to represent the field. Most of the requirements deal with the field only as it
 * relates to the ship, so I'm going to represent the field that way. I like case classes for stuff like this. It
 * should know how to represent itself the way it needs to appear in the report. We don't actually need it to be
 * 3D for any of the requirements.  It's enough to know the distance to the first mine. I'm making an assumption that
 * the density of mines is low, so instead of using an array of arrays, I'm just going to represent it as a map of
 * mine XY coordinates to distance (Z).
 */

object Field {
  // This gives us an easy lookup table to map letters to depths. Unfortunately, depths are represented as negatives,
  // so we can't just index directly. I'll make a few methods so that the caller doesn't have to think about the
  // negation.

  private[this] val zChars = List('*') ++ ('a' to 'z') ++ ('A' to 'Z')

  def depthToChar(z: Int) = zChars(-z)
  def charToDepth(zChar: Char) = -zChars.indexOf(zChar)

  // Here's how we load a field from disk. Actually, I'll make it take a string because that will be easier for
  // unit testing. Scala Source will give me an easy way to bridge the gap between File and String.

  def load(source: String): Field = {
    // We need to put these into a list.  Otherwise, we'll exhaust the iterator the first time through and we need
    // multiple passes.

    val lines = Source.fromString(source).getLines.toList

    // The number of lines indicates where the center should be vertically and therefore what the offset should be
    // as we're assigning Y coordinates to the mines. The number of characters in each line should do the same for
    // the horizontal center. I'm assuming that we have good input here (e.g., that all lines have the same number of
    // characters).

    val xoffset = ( lines.head.size - 1 ) / 2
    val yoffset = ( lines.size - 1 ) / 2

    val mines =
      lines.zipWithIndex flatMap { case (yline, y) =>
        yline.zipWithIndex flatMap { case (zChar, x) =>
          if ( zChar == '.' )
            None
          else
            Some((x - xoffset, y - yoffset) -> charToDepth(zChar))
        }
      }

    Field(mines.toMap)
  }

  // It probably doesn't really matter for performance in something this small, but out of habit, I tend to put things
  // like this that only really need to be constructed once and then used over and over into the companion object.

  def ALPHA_PATTERN = Iterable((-1, -1), (-1, 1), (1, -1), (1, 1))
  def BETA_PATTERN = Iterable((-1, 0), (0, -1), (0, 1), (1, 0))
  def GAMMA_PATTERN = Iterable((-1, 0), (0, 0), (1, 0))
  def DELTA_PATTERN = Iterable((0, -1), (0, 0), (0, 1))
}

/* Field is an immutable data structure. When we perform transformations on the field (by moving the ship, dropping
 * and firing) we get a new instance with the changes reflected. This would make it a bit easier for us to build a
 * decision tree if we wanted to take a stab at optimizing the script to clear all mines.
 */

case class Field(mines: Map[(Int, Int), Int]) {

  // When our ship moves in the plane, it appears as though all the mines move in the opposite direction.  This is
  // due to my decision represent the field in this way (relative to the ship).

  private[this] def translate(xd: Int, yd:Int) = this.copy(mines = mines map { case ((x, y), z) =>
    (x - xd, y - yd) -> z
  })

  // These offsets are from the point of view of the ship, not the mines.  Translate corrects for the perspective.

  def north = translate(0, -1)
  def south = translate(0, 1)
  def east = translate(1, 0)
  def west = translate(-1, 0)

  // This is how we transform the field when the ship drops 1km.

  def drop = this.copy(mines = mines.mapValues(_ + 1))

  // Transform the field by removing all mines at the given offsets from the ship at (0,0).  This is really easy since
  // we're storing the mines keyed off their relative distance from the ship.

  private[this] def demine(pattern: Iterable[(Int, Int)]) = this.copy(mines = this.mines -- pattern)

  def alpha = demine(ALPHA_PATTERN)
  def beta = demine(BETA_PATTERN)
  def gamma = demine(GAMMA_PATTERN)
  def delta = demine(DELTA_PATTERN)

  override lazy val toString = {
    // We need to know the full range of XY coordinates we need to represent. The ship has to be in the middle, so
    // take the largest absolute value in each direction and use that to offset everything for display.  These aren't
    // really the width and height of the display, despite the names. They are the width and height on each side of
    // the center.  So, the real width of the display is ( w + 1 + w ). Height is similar.

    val (w, h) =
      if ( mines.isEmpty ) {
        (0, 0)
      } else {
        (mines.keys.map(_._1.abs).max, mines.keys.map(_._2.abs).max)
      }

    // Now that we know the offsets, we can draw a rectangle out of the appropriate characters.

    ( -h to h ) map { y =>
      ( -w to w ) map { x =>
        mines.get(x, y) map Field.depthToChar getOrElse '.'
      } mkString
    } mkString("\n")
  }
}


object Simulator {

  sealed trait Result {
    val score: Int
  }

  object Fail extends Result {
    override val score = 0
    override val toString = "fail (0)"
  }

  case class Pass(override val score: Int) extends Result {
    override val toString = s"pass ($score)"
  }

  /* This method contains all the logic for simulating.  It's not directly in main because that would make it harder
   * to write unit tests for the logic.
   */

  def run(initialField: Field, script: List[String], pw: PrintWriter): Result = {

    /* State is the state of our simulation at any point. It encapsulates the current state of the field, plus the
     * remaining script input and any metrics that we need to calculate the score at the end.
     *
     * It's also immutable for the same reason as Field.  Even though I'm not optimizing for score yet, this would
     * make it easier to explore the entire state space.
     */

    case class State(field: Field,
                     script: Iterable[String],
                     commands: Iterable[String] = Iterable.empty,
                     stepCount: Int = 1,
                     shotCount: Int = 0,
                     moveCount: Int = 0)
    {
      // Used internally to handle the bookkeeping associated with consuming a fire command and updating the field.

      private[this] def fire(fn: Field => Field): State =
        this.copy(field = fn(field),
                  shotCount = shotCount + 1,
                  commands = commands.tail)

      // Attempts to consume the first available command as a fire command and update the state accordingly.  If there
      // is no command, the state is unaffected.  If there is an unrecognized command, we assume that it's a move
      // command and do nothing here.

      private def handleOptionalFireCommand: State =
        commands.headOption match {
          case Some("alpha") => fire(_.alpha)
          case Some("beta") => fire(_.beta)
          case Some("gamma") => fire(_.gamma)
          case Some("delta") => fire(_.delta)
          case _ => this // no error, no change, could be a move (None)
        }

      // Used internally to handle the bookkeeping associated with consuming a move command and updating the field.

      private[this] def move(fn: Field => Field): State =
        this.copy(field = fn(field),
          moveCount = moveCount + 1,
          commands = commands.tail)

      // Attempts to consume the first available command as a move command and update the state accordingly.  If there
      // is no command, the state is unaffected.  If there is an unrecognized command, it's an error. There's no
      // provision in the requirements that says what to do in this case, so I'm just aborting the simulation with
      // an exception.

      private def handleOptionalMoveCommand: State =
        commands.headOption match {
          case Some("north") => move(_.north)
          case Some("south") => move(_.south)
          case Some("east") => move(_.east)
          case Some("west") => move(_.west)
          case None => this // no move specified, no change in state
          case x => throw new IllegalStateException(s"unknown command: $x")
        }

      // Validates that there are no extra commands on the script line.  The only way to get to this point would be
      // after consuming a valid move command. There's no provision in the requirements that says what to do in this
      // case, so I'm just aborting the simulation with an exception.

      private def handleExtraneousCommand: State =
        commands.headOption match {
          case Some(x) => throw new IllegalStateException(s"invalid extraneous command: $x")
          case None => this
        }

      // Wraps up the logic of carrying out the commands on a given script line.

      private def handleCommands: State =
        handleOptionalFireCommand.handleOptionalMoveCommand.handleExtraneousCommand

      // Handles the state transformation associated with the ship dropping 1km.

      private def drop: State = this.copy(field = field.drop)

      // Handles the situation where the script has been completely consumed. In all cases, at this point, we can
      // calculate the result, depending on the state.

      private def scriptExhausted: Result =
        if ( ! field.mines.isEmpty ) {
          // Didn't clear all mines (case 2)...
          Fail
        } else {
          // Cleared all mines with no steps left (case 4)...
          val initialMines = initialField.mines.size
          val startingScore = initialMines * 10
          val shotPenalty = math.min(initialMines * 5, shotCount * 5)
          val movePenalty = math.min(initialMines * 3, moveCount * 2)
          Pass(startingScore - shotPenalty - movePenalty)
        }

      // Examines the state to see if we've reached completion and can provide a result.  If so, this returns a Some
      // containing the result.  If it's not a terminal state, return None.

      private def getResult: Option[Result] =
        if ( field.mines.values.exists(_ == 0) )
          // Passed a mine (case 1)...
          Some(Fail)
        else if ( ! script.isEmpty && field.mines.isEmpty )
          // All mines are cleared but steps remain (case 3)...
          Some(Pass(1))
        else
          // Inconclusive - nonterminal state
          None

      // All the required side effects here muddy up the logic a bit. In short, see if we have another step to
      // perform. If we don't we're done and can simply return the result. If so, do it and then see if we're done.
      // If not, recurse.

      def simulate: Result =
        script.headOption map { line =>
          pw.println(s"Step $stepCount")
          pw.println()
          pw.println(field)
          pw.println()
          pw.println(line)
          pw.println()

          // Modify the state here by staging the next script line as a list of commands.

          val newState = this.copy(stepCount = stepCount + 1,
                                   script = script.tail,
                                   commands = line.split("\\s+")).handleCommands.drop

          pw.println(newState.field)
          pw.println()

          // See if we've reached an exit condition.  If so, return it.  Otherwise, handle the next step.
          newState.getResult getOrElse newState.simulate
        } getOrElse {
          // We haven't hit an exit condition, yet we're out of script.
          scriptExhausted
        }

    }

    val result = State(initialField, script).simulate
    pw.println(result)
    pw.println()

    result
  }

  // This is just a small driver that makes it possible to launch the simulator from the command line, specifying
  // the field and script files.  It mostly delegates to the run method above.

  def main(args: Array[String]): Unit = {
    val field = Field.load(Source.fromFile(args(0)).mkString)
    val script = Source.fromFile(args(1)).getLines.toList
    val pw = new PrintWriter(System.out)
    run(field, script, pw)
    pw.flush()
  }
}
