package starfleet

import scala.language.postfixOps
import java.io.PrintWriter

import scala.io.Source
import scalaz._

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
}

/* Field is an immutable data structure. When we perform transformations on the field (by moving the ship, dropping
 * and firing) we get a new instance with the changes reflected. This would make it a bit easier for us to build a
 * decision tree if we wanted to take a stab at optimizing the script to clear all mines.
 */

case class Field(mines: Map[(Int, Int), Int])

object Simulator {

  // These are the terminal states of the simulation.

  sealed trait Result {
    val score: Int
    val transcript: List[SimulationState]
  }

  private case class Fail(transcript: List[SimulationState]) extends Result {
    override val score = 0
  }

  private case class Pass(score: Int, transcript: List[SimulationState]) extends Result

  /* State is the state of our simulation at any point. It encapsulates the current state of the field, plus the
   * remaining script input and any metrics that we need to calculate the score at the end.
   *
   * It's also immutable for the same reason as Field.  Even though I'm not optimizing for score yet, this would
   * make it easier to explore the entire state space.
   */

  case class SimulationState(field: Field,
                             history: List[SimulationState],
                             script: Iterable[String],
                             commands: Iterable[String],
                             shotCount: Int,
                             moveCount: Int)
  {
    val initialField: Field = history.headOption.map(_.initialField).getOrElse(field)
  }

  private object SimulationState {
    def apply(initialField: Field, script: Iterable[String]): SimulationState =
      new SimulationState(initialField, Nil, script, Iterable.empty, 0, 0)
  }

  private type UnitState[S] = State[S, Unit]

  private def UnitState[S](f: S => S): UnitState[S] = State(prev => (f(prev), ()))

  private val noop: UnitState[SimulationState] = UnitState(identity)

  // Generically modify the field that's part of the state with a mutator function.

  private def updateField(fn: Field => Field): UnitState[SimulationState] = UnitState { prev =>
    prev.copy(field = fn(prev.field))
  }

  private val consumeCommand: UnitState[SimulationState] = UnitState { prev =>
    prev.copy(commands = prev.commands.tail)
  }

  // These are the defined firing patterns, which will be needed when we see a fire command.

  private type Coordinates = (Int, Int)
  private case class FiringPattern(coords: Coordinates*)

  private val AlphaPattern = FiringPattern((-1, -1), (-1,  1), (1, -1), (1,  1))
  private val BetaPattern  = FiringPattern((-1,  0), ( 0, -1), (0,  1), (1,  0))
  private val GammaPattern = FiringPattern((-1,  0), ( 0,  0), (1,  0))
  private val DeltaPattern = FiringPattern(( 0, -1), ( 0,  0), (0,  1))

  // Transform the field by removing all mines at the given offsets from the ship at (0,0).  This is really easy since
  // we're storing the mines keyed off their relative distance from the ship.

  private def demine(pattern: FiringPattern) = updateField { prev =>
    prev.copy(mines = prev.mines -- pattern.coords)
  }

  // Used internally to handle the bookkeeping associated with consuming a fire command and updating the field.

  private def fire(pattern: FiringPattern): UnitState[SimulationState] =
    for {
      _ <- demine(pattern)
      _ <- incrementShotCount
      _ <- consumeCommand
    } yield ()


  private val incrementShotCount: UnitState[SimulationState] = UnitState { prev =>
    prev.copy(shotCount = prev.shotCount + 1)
  }

  private val nextCommand: State[SimulationState, Option[String]] = State.gets(_.commands.headOption)

  // Attempts to consume the first available command as a fire command and update the state accordingly.  If there
  // is no command, the state is unaffected.  If there is an unrecognized command, we assume that it's a move
  // command and do nothing here.

  private val handleOptionalFireCommand: UnitState[SimulationState] =
    for {
      cmd <- nextCommand
      _ <- cmd match {
             case Some("alpha") => fire(AlphaPattern)
             case Some("beta")  => fire(BetaPattern)
             case Some("gamma") => fire(GammaPattern)
             case Some("delta") => fire(DeltaPattern)
             case _ => noop // no error, no change, could be a move (None)
           }
    } yield ()

  // When our ship moves in the plane, it appears as though all the mines move in the opposite direction.  This is
  // due to my decision represent the field in this way (relative to the ship).

  private def translate(dir: Direction) = updateField { prev =>
    val Direction(xd, yd) = dir
    prev.copy(mines = prev.mines map { case ((x, y), z) =>
      (x - xd, y - yd) -> z
    })
  }

  // These offsets are from the point of view of the ship, not the mines.  Translate corrects for the perspective.

  private case class Direction(xd: Int, yd: Int)
  private val north = Direction( 0, -1)
  private val south = Direction( 0,  1)
  private val east  = Direction( 1,  0)
  private val west  = Direction(-1,  0)

  // Used internally to handle the bookkeeping associated with consuming a move command and updating the field.

  private val incrementMoveCount: UnitState[SimulationState] = UnitState { prev =>
    prev.copy(moveCount = prev.moveCount + 1)
  }

  private def move(dir: Direction): UnitState[SimulationState] =
    for {
      _ <- translate(dir)
      _ <- incrementMoveCount
      _ <- consumeCommand
    } yield ()

  // Attempts to consume the first available command as a move command and update the state accordingly.  If there
  // is no command, the state is unaffected.  If there is an unrecognized command, it's an error. There's no
  // provision in the requirements that says what to do in this case, so I'm just aborting the simulation with
  // an exception.

  private val handleOptionalMoveCommand: UnitState[SimulationState] =
    for {
      cmd <- nextCommand
      _ <- cmd match {
             case Some("north") => move(north)
             case Some("south") => move(south)
             case Some("east") => move(east)
             case Some("west") => move(west)
             case None => noop // no move specified, no change in state
             case x => throw new IllegalStateException(s"unknown command: $x") // TODO: return errors, no throw
           }
    } yield ()

  // Validates that there are no extra commands on the script line.  The only way to get to this point would be
  // after consuming a valid move command. There's no provision in the requirements that says what to do in this
  // case, so I'm just aborting the simulation with an exception.

  private val handleExtraneousCommand: UnitState[SimulationState] =
    for {
      cmd <- nextCommand
    } yield {
      cmd match {
        case Some(x) => throw new IllegalStateException(s"invalid extraneous command: $x") // TODO: no throw
        case None => ()
      }
    }

  // Wraps up the logic of carrying out the commands on a given script line.

  private val handleCommands: UnitState[SimulationState] =
    for {
      _ <- handleOptionalFireCommand
      _ <- handleOptionalMoveCommand
      _ <- handleExtraneousCommand
    } yield ()

  // Handles the state transformation associated with the ship dropping 1km.

  private val drop: UnitState[SimulationState] = updateField { prev =>
    prev.copy(mines = prev.mines.mapValues(_ + 1))
  }

  // Examines the state to see if we've reached completion and can provide a result.  If so, this returns a Some
  // containing the result.  If it's not a terminal state, return None.

  private val getResult: State[SimulationState, Option[Result]] = State.gets { state =>
    if ( state.field.mines.values.exists(_ == 0) )
    // Passed a mine (case 1)...
      Some(Fail(state :: state.history))
    else if ( state.script.nonEmpty && state.field.mines.isEmpty )
    // All mines are cleared but steps remain (case 3)...
      Some(Pass(1, state :: state.history))
    else
    // Inconclusive - nonterminal state
      None
  }

  // Handles the situation where the script has been completely consumed. In all cases, at this point, we can
  // calculate the result, depending on the state.

  private val scriptExhausted: State[SimulationState, Result] = State.gets { state =>
    if ( ! state.field.mines.isEmpty ) {
      // Didn't clear all mines (case 2)...
      Fail(state :: state.history)
    } else {
      // Cleared all mines with no steps left (case 4)...
      val initialMines = state.initialField.mines.size
      val startingScore = initialMines * 10
      val shotPenalty = math.min(initialMines * 5, state.shotCount * 5)
      val movePenalty = math.min(initialMines * 3, state.moveCount * 2)
      Pass(startingScore - shotPenalty - movePenalty, state :: state.history)
    }
  }

  // All the required side effects here muddy up the logic a bit. In short, see if we have another step to
  // perform. If we don't we're done and can simply return the result. If so, do it and then see if we're done.
  // If not, recurse.

  private val hasAnotherLine: State[SimulationState, Boolean] = State.gets(x => x.script.nonEmpty)

  // Push the current state onto the history

  private val incrementStep: UnitState[SimulationState] = UnitState { prev =>
    prev.copy(history = prev :: prev.history,
              script = prev.script.tail)
  }

  // Modify the state here by staging the next script line as a list of commands (and removing the line).

  private val splitLineIntoCommands: UnitState[SimulationState] = UnitState { prev =>
    prev.copy(commands = prev.script.head.split("\\s+").toIterable)
  }

  private def handleStep: State[SimulationState, Result] =
    for {
      _ <- splitLineIntoCommands
      _ <- incrementStep
      _ <- handleCommands
      _ <- drop
      resultOption <- getResult
      result <- resultOption match {
                  case Some(x) => State.state[SimulationState, Result](x)
                  case None => simulate
                }
    } yield result

  private val simulate: State[SimulationState, Result] =
    for {
      hasLine <- hasAnotherLine
      result <- if ( hasLine ) handleStep else scriptExhausted
    } yield result


  /* This method contains all the logic for simulating.  It's not directly in main because that would make it harder
   * to write unit tests for the logic.
   */

  def run(initialField: Field, script: List[String]): (SimulationState, Result) = {
    val result = SimulationState(initialField, script)
    simulate.run(result)
  }

  // Functions for displaying a transcript of the simulation.

  def transcript(result: Result): String = {

    def renderField(field: Field): Iterable[String] = {
      // We need to know the full range of XY coordinates we need to represent. The ship has to be in the middle, so
      // take the largest absolute value in each direction and use that to offset everything for display.  These aren't
      // really the width and height of the display, despite the names. They are the width and height on each side of
      // the center.  So, the real width of the display is ( w + 1 + w ). Height is similar.

      val (w, h) =
        if ( field.mines.isEmpty ) {
          (0, 0)
        } else {
          (field.mines.keys.map(_._1.abs).max, field.mines.keys.map(_._2.abs).max)
        }

      // Now that we know the offsets, we can draw a rectangle out of the appropriate characters.

      ( -h to h ) map { y =>
        ( -w to w ) map { x =>
          field.mines.get(x, y) map Field.depthToChar getOrElse '.'
        } mkString
      }
    }


    def renderResult(r: Result): Iterable[String] = Iterable(r match {
      case Fail(_) => "fail (0)"
      case Pass(n, _) => s"pass ($n)"
    })

    val blankLine = Iterable("")

    val lines =
      result.transcript.reverse.sliding(2).zipWithIndex.flatMap { case (List(s0, s1), n) =>
        Iterable(
          Iterable(s"Step ${n + 1}"),
          blankLine,
          renderField(s0.field),
          blankLine,
          Iterable(s0.script.head),
          blankLine,
          renderField(s1.field),
          blankLine,
        ).flatten
      } ++ renderResult(result) ++ blankLine

    lines.mkString("", "\n", "\n")
  }


  // This is just a small driver that makes it possible to launch the simulator from the command line, specifying
  // the field and script files.  It mostly delegates to the run method above.

  def main(args: Array[String]): Unit = {
    val field = Field.load(Source.fromFile(args(0)).mkString)
    val script = Source.fromFile(args(1)).getLines.toList
    val (_, result) = run(field, script)
    println(transcript(result))
  }
}
