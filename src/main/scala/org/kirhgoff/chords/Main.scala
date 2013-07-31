package org.kirhgoff.chords

import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex
import scala.Predef._

//------------------------------------------------------------
// Model part
//------------------------------------------------------------

/**
 * Class keeps the intervals between nodes and provides the ability
 * to get any step's overall interval
 */
class RelativeScale(val intervals: List[Int]) {
  val accumulatedIntervals: List[Int] = intervals.scanLeft(0)(_ + _).dropRight(1)
  //println(s"Intervals       : $intervals")
  //println(s"Intervals (acc) : $accumulatedIntervals")

  def step(number: Int): Int = accumulatedIntervals(number % intervals.length) //TODO write test
}


object NoteFactory {
  //TODO remake using semitones
  val absoluteScale = new RelativeScale(List(2, 2, 1, 2, 2, 2, 1))
  val doMajor: List[String] = List("C", "D", "E", "F", "G", "A", "H")
  //last is C of next octave
  val inSemitones: Map[String, Int] = Map((doMajor zip (0 to doMajor.length).map(absoluteScale.step(_))): _*)
  println(s"Map in semitones: $inSemitones")

  //assert section
  assert(absoluteScale.intervals.length == doMajor.length)
  assert(absoluteScale.accumulatedIntervals.length == doMajor.length)

  val relativeScale: RelativeScale = new RelativeScale(List(2, 2, 1, 2, 2, 2, 1))

  def getNote(in: String) = new Note(inSemitones(in))

  def root = doMajor(0)

  def chordFromSteps(steps: List[Int]) = new Chord(steps.map(number => new Note(relativeScale.step(number))))

  def majorChord(root: String)(x: Chord): Chord = chordFromSteps(List(1, 3, 5))
  def major(root: String): Chord = chordFromSteps(List(1, 3, 5))

  //TODO use the fucken root!
  def names = doMajor
}

/**
 * Note. Keeps the interval to absolute root
 */

class Note(val semitones: Int) {
  def semitoneUp = new Note(semitones + 1)

  def semitoneDown = new Note(semitones - 1)

  override def toString = "note" + NoteFactory.root + sign + semitones

  def sign = if (semitones >= 0) "+" else "-"
}

/**
 * Set of notes, provides ability to change the flavors of chord,
 * change the notes correspondingly
 */
class Chord(val notes: List[Note]) {
  override val toString = notes.toString

  def semitoneUp = new Chord(notes.map(_.semitoneUp))

  def semitoneDown = new Chord(notes.map(_.semitoneDown))
  def makeSept = new Chord(notes)
}

//----------------------------------------------------------------
// Parser model
//----------------------------------------------------------------
trait ChordModification {
  def apply(in: Chord): Chord
}

case class Major(note: String) extends ChordModification {
  override def apply(in: Chord): Chord = {
    NoteFactory.majorChord(note)(in)
  }
}

//TODO use partial function for both
case class Diez() extends ChordModification {
  override def apply(in: Chord): Chord = {
    in.semitoneUp
  }
}

case class Bemol() extends ChordModification {
  override def apply(in: Chord): Chord = {
    in.semitoneDown
  }
}

object RegexUtils {

  class RichRegex(underlying: Regex) {
    def matches(s: String) = underlying.pattern.matcher(s).matches
  }

  implicit def regexToRichRegex(r: Regex) = new RichRegex(r)
}

//----------------------------------------------------------------
// Parser itself
//----------------------------------------------------------------
object Modifications {

}

class ChordParser {
  def parseChord(in:String):Chord = {
    internalParse(null, in.toList)
  }

  def internalParse (chord:Chord, in:List[Char]):Chord = {
    in match {
      case Nil => return chord
      case head :: tail if (Character.isUpperCase (head)) => internalParse(NoteFactory.major(head.toString), in.tail)
      case '#' :: tail => internalParse(chord.semitoneUp, in.tail)
      case 'b' :: tail => internalParse(chord.semitoneDown, in.tail)
      case '7' :: tail => internalParse(chord.makeSept, in.tail)
    }
  }
}

class ChordParserStandard extends RegexParsers {
  def tone: Parser[ChordModification] = ("A" | "B" | "C" | "D" | "E" | "F" | "G") ^^ {s => new Major(s)}

  def diez: Parser[ChordModification] = "#" ^^ {s => new Diez}

  def bemol: Parser[ChordModification] = "b" ^^ {s => new Bemol}

  def theParser = tone ~ (diez | bemol).? ~ (diez | bemol).?

  def parseChord(input: String): Option[ChordModification] = {
    val parser = new ChordParserStandard
    val result = parser.parseAll(parser.theParser, input) match {
      case parser.NoSuccess(result, next) => result
      case parser.Success(result, next) => result
      case parser.Failure(result, next) => result
      case parser.Error(result, next) => result
    }
    println(s"Match result = $result, type: ${result.getClass.getCanonicalName}")

    val ~(a, b) = result
    println(s"Result: a = $a, b = $b, types: ${a.getClass.getSimpleName}, ${b.getClass.getSimpleName}")

    None
  }
}

object Main extends ChordParser {
  def main(args: Array[String]) = {
    //println("Result:" + parseChord ("B"))
    //val modification: ((Chord) => Chord) = parseChord("A#")
    println("Result:" + parseChord("Ab"))
  }
}
