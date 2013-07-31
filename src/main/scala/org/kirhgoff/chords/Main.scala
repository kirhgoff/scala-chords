package org.kirhgoff.chords

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
  def sign = if (semitones >= 0) "+" else "-"
  override def toString = "note" + NoteFactory.root + sign + semitones
}

/**
 * Set of notes, provides ability to change the flavors of chord,
 * change the notes correspondingly
 */
class Chord(val notes: List[Note]) {
  def semitoneUp = new Chord(notes.map(_.semitoneUp))
  def semitoneDown = new Chord(notes.map(_.semitoneDown))
  def makeSept = new Chord(notes)
  override def toString = notes.toString
}


class ChordParser {
  def parseChord(in: String): Chord = {
    internalParse(null, in.toList)
  }

  def internalParse(chord: Chord, in: List[Char]): Chord = {
    in match {
      case Nil => return chord
      case head :: tail if (Character.isUpperCase(head)) => internalParse(NoteFactory.major(head.toString), in.tail)
      case '#' :: tail => internalParse(chord.semitoneUp, in.tail)
      case 'b' :: tail => internalParse(chord.semitoneDown, in.tail)
      case '7' :: tail => internalParse(chord.makeSept, in.tail)
    }
  }
}

object Main extends ChordParser {
  def main(args: Array[String]) = {
    println("Result:" + parseChord("Ab"))
  }
}
