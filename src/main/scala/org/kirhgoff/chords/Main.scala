package org.kirhgoff.chords

import scala.Predef._

//------------------------------------------------------------
// Model
//------------------------------------------------------------

/**
 * AbsoluteScale: keep note names and interval for them from C
 * Lets the extender be able to translate between notes and semitones
 */
trait AbsoluteScale {
  val names: List[String] = List("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
  val inSemitones: Map[String, Int] = Map(names zip (0 to names.length): _*)
  val absoluteRoot = names(0)

  def getNoteName(interval: Int) = names(interval % names.length)
  def getIntervalByName(note:String) = inSemitones(note)
}

/**
 * ShiftedScale: scale, shifted against AbsoluteScale
 * and able to calculate absolute values of intervals in scale
 * regarding to C
 */
class ShiftedScale (val root:String) extends AbsoluteScale {
  val shiftValue: Int = getIntervalByName(root) - getIntervalByName(absoluteRoot)
  def absolute(semitone:Int) = semitone + shiftValue //TODO write a test
}
/**
 * HarmonicScale: keeps the intervals between nodes and provides the ability
 * to translate intervals to scale steps and back
 */
class HarmonicScale(root:String, val intervals: List[Int]) extends ShiftedScale(root) {
  val accumulatedIntervals: List[Int] = intervals.scanLeft(0)(_ + _).dropRight(1)
  def getIntervalForStep(number: Int): Int = absolute(accumulatedIntervals((number - 1) % intervals.length)) //TODO write test
  //def getStepForInterval(semitones: Int) = {}
}

/**
 * Note. Keeps the interval to absolute root
 */

class Note(val interval: Int) extends AbsoluteScale {
  def semitoneUp = new Note(interval + 1)
  def semitoneDown = new Note(interval - 1)
  def sign = if (interval >= 0) "+" else "-"
  override def toString = getNoteName(interval)
}

/**
 * Set of notes, provides ability to change the flavors of chord,
 * change the notes correspondingly
 */
class Chord(val notes: List[Note]) {
  def semitoneUp = new Chord(notes.map(_.semitoneUp))
  def semitoneDown = new Chord(notes.map(_.semitoneDown))
  def makeSept = new Chord(notes) //TODO
  override def toString = notes.toString
  def toList = notes
}

/**
 * ChordBuilder: factory object to work with chords
 */

object ChordBuilder  {
  val MajorIntervals = List(2, 2, 1, 2, 2, 2, 1)
  val MinorIntervals = List(2, 1, 2, 2, 1, 2, 2)

  def buildChordBySteps(root:String, steps: List[Int], intervals:List[Int]) = {
    val scale = new HarmonicScale(root, intervals)
    new Chord(steps.map(number => new Note(scale.getIntervalForStep(number))))
  }
  def buildMajorChord(root: String): Chord = buildChordBySteps(root, List(1, 3, 5), MajorIntervals)
  def buildMinorChord(root: String): Chord = buildChordBySteps(root, List(1, 3, 5), MinorIntervals)
}

/**
 * ChordParser: the parser
 */

object ChordParser {
  def parseChord(in: String): Chord = {
    internalParse(null, in.toList)
  }

  def internalParse(chord: Chord, in: List[Char]): Chord = {
    in match {
      case Nil => return chord
      case head :: tail if (Character.isUpperCase(head)) => internalParse(ChordBuilder.buildMajorChord(head.toString), tail)
      case '#' :: tail => internalParse(chord.semitoneUp, in.tail)
      case 'b' :: tail => internalParse(chord.semitoneDown, in.tail)
      case '7' :: tail => internalParse(chord.makeSept, in.tail)
    }
  }
}


object Main {
  def main(args: Array[String]) = {
    println("Result:" + ChordParser.parseChord("E#"))
  }
}
