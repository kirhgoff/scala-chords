package org.kirhgoff.chords

import scala.util.parsing.combinator.RegexParsers

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


object ScaleShift {
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
  def majorChord(in: String): Chord = chordFromSteps(List(1, 3, 5))
}

/**
 * Note. Keeps the interval to absolute root
 */

class Note(val semitones: Int) {
  def semitoneUp = new Note(semitones + 1)
  def semitoneDown = new Note(semitones - 1)

  override def toString = "note" + ScaleShift.root + sign + semitones
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
}

//----------------------------------------------------------------
// Parser model
//----------------------------------------------------------------
trait ChordModification {
  def apply(in: Chord): Chord
}

case class Major(note: String) extends ChordModification {
  override def apply(in: Chord): Chord = {
    ScaleShift.majorChord(note)
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

//----------------------------------------------------------------
// Parser itself
//----------------------------------------------------------------
class ChordParser extends RegexParsers {
  def tone: Parser[ChordModification] = ("A" | "B" | "C" | "D" | "E" | "F" | "G" ) ^^ (s => new Major(s))
  def diez: Parser[ChordModification] = "#" ^^ (_ => new Diez())
  def bemol: Parser[ChordModification] = "b" ^^ (_ => new Bemol())

  def theParser = tone ~ (diez | bemol).?

  def parseChord(input: String) = {
    val parser = new ChordParser
    parser.parseAll(parser.theParser, input) match {
      case parser.Success(result:ChordModification, next) => result.apply(null)
      case parser.NoSuccess(msg, _) => msg
    }
  }
}

object Main extends ChordParser {
  def main(args:Array[String]) = {
    //println("Result:" + parseChord ("B"))
    println("Result:" + parseChord ("A#"))
  }
}
