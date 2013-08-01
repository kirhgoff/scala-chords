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
  def overallNotes = names.size

  def getNoteForAbsoluteInterval(interval: Int) = names(interval % names.length)
  def absoluteInterval(note:String) = inSemitones(note)

  //TODO this is simplification, change it?
  //move note to the closest octave and then if negative
  //move it up one octave to make it positive
  def normalize(semitone:Int) = {
    val result = semitone % overallNotes + (if (semitone >= 0) 0 else overallNotes)
    //println (s"normalize $semitone result=$result")
    result
  }
}

/**
 * ShiftedScale: scale, shifted against AbsoluteScale
 * and able to calculate absolute values of intervals in scale
 * regarding to C
 */
class ShiftedScale (val root:String) extends AbsoluteScale {
  val shiftValue: Int = normalize(absoluteInterval(root) - absoluteInterval(absoluteRoot))
  //Semitones from absolute root to this number of the semitones from the scale root
  def absolute(relative:Int) = normalize(relative + shiftValue) //TODO write a test

  assert(shiftValue >= 0)
}
/**
 * HarmonicScale: keeps the intervals between nodes and provides the ability
 * to translate intervals to scale steps and back
 */
class HarmonicScale(root:String, val intervals: List[Int]) extends ShiftedScale(root) {
  val accumulatedIntervals: List[Int] = intervals.scanLeft(0)(_ + _).dropRight(1)
  def getIntervalForStep(number: Int): Int = absolute(accumulatedIntervals((number - 1) % intervals.length)) //TODO write test
  def getNoteForStep(step:Int) = getNoteForAbsoluteInterval(getIntervalForStep(step))
  //def getStepForInterval(semitones: Int) = {}
}

/**
 * Note. Keeps the interval to absolute root
 */

class Note(val absolute: Int) extends AbsoluteScale {
  def semitoneUp = new Note(absolute + 1)
  def semitoneDown = new Note(absolute - 1)
  def sign = if (absolute >= 0) "+" else "-"

  override def toString = getNoteForAbsoluteInterval(absolute)

  override def equals(that:Any) = that match {
    case note:Note => note.absolute == absolute
    case _ => false
  }
}

object Note extends AbsoluteScale {
  def make(note:String) = new Note(absoluteInterval(note))
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

/**
 *
 */

class NoteString (root:String) extends ShiftedScale(root) {
  def getFretForNote(note:Note) = {
    val absolute = note.absolute
    //println(s"getFretForNote absolute=$absolute shiftValue=$shiftValue")

    if (absolute < shiftValue) overallNotes + absolute - shiftValue //move up one octave
    else absolute - shiftValue
  }
  def fret(note:String) = getFretForNote(Note.make(note))
}

class Fingering {

}

/**
 * Tuning: represents some string instrument tuning
 */
class Tuning (val stringRoots:List[String]) {
  val strings:List[NoteString] = stringRoots.map (new NoteString(_))
  def generateFingerings(chord:Chord):List[Fingering] = {
    Nil
  }
}

object Tuning {
  val GuitarTuning = new Tuning (List("E", "A", "G", "D", "G", "E"))
  val Ukulele = new Tuning (List("G", "C", "E", "A"))
}


object Main {
  def main(args: Array[String]) = {
    val chord = ChordParser.parseChord("E#")

    val fingerings:List[Fingering] = Tuning.Ukulele.generateFingerings(chord)
    println("Result:" + fingerings)
  }
}
