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
 * NoteString: gives a fret position for given note
 */

class NoteString (root:String) extends ShiftedScale(root) {
  def getFretForNote(note:Note) = {
    val absolute = note.absolute
    //println(s"getFretForNote absolute=$absolute shiftValue=$shiftValue")

    if (absolute < shiftValue) overallNotes + absolute - shiftValue //move up one octave
    else absolute - shiftValue
  }
  def fret(note:String) = getFretForNote(Note.make(note))
  def note(fret:Int) = new Note(absolute(fret)) //Probably move up
  override def toString = "String [" + root + "]"
}

/**
 * Tuning: represents some string instrument tuning
 */

object Tuning {
  val GuitarTuning = new Tuning (List("E", "A", "G", "D", "B", "E"))
  val Ukulele = new Tuning (List("G", "C", "E", "A"))
}

class Tuning (reversedStringRoots:List[String]) {
  val strings:List[NoteString] = reversedStringRoots.reverse.map (new NoteString(_))
  def rawFingerings(chord:Chord):List[List[Int]] = {
    val notesOnStrings:List[List[Int]] = strings.map(string => chord.notes.map(note => string.getFretForNote(note)))
    val initial:List[List[Int]] = List(List[Int]())//first.scanLeft(List(List[Int]))(x:Int => List[Int]())
    notesOnStrings.foldLeft(initial)((acc:List[List[Int]], item:List[Int]) => {
      for {
        note:Int <- item
        fingering:List[Int] <- acc
      } yield {
         note :: fingering
      }
    })
  }

  def fingerings(chord:Chord) = {
    val raw = rawFingerings(chord)
    var fingerings:List[Fingering] = raw.map(new Fingering(this, chord, _))
    fingerings = fingerings.filter(_.hasAllNotes)
    fingerings = fingerings.filter(_.maxFretDistance < 4)
    fingerings
  }
}

/**
 * Fingering: merges chord and fingering on concrete instrument tuning,
 * provides ability to access both chord and fingering attributes
 */
class Fingering(val tuning:Tuning, val chord:Chord, reversePositions:List[Int]) {
  val positions = reversePositions.reverse
  def hasAllNotes() = {
    //TODO think about the fact that we always iterate by strings
    val positionNotes:Set[Note] = (0 to positions.size -1).map (index => noteByString(index)).flatten.toSet
    val chordNotes:Set[Note] = chord.notes.toSet
    positionNotes diff chordNotes isEmpty //TODO FIXME
  }
  def maxFretDistance:Int = {
    (for {
      x <- positions
      y <- positions
    } yield {
      (x, y)
    }).foldLeft(0)((max, pair) => {
      val diff = Math.abs(pair._1 - pair._2)
      if (diff > max)
        diff
      else
        max
    })
  }
  /**
   * Returns note played on provided string
   */
  def noteByString(num:Int):Option[Note] = {
    //println (s"noteByString num=$num positions=$positions strings=${tuning.strings}")
    if (num > positions.size) return None
    val string = tuning.strings(num)
    val fret = positions(num)
    val note: Note = string.note(fret)
    //println (s"noteByString string=$string fret=$fret result=$note")
    Some(note)
  }

  override def toString = {
    positions.mkString(" ") + " [" +
    (0 to positions.size -1).map(index => {
      val pos = positions(index)
      val noteOption = noteByString(index)
      noteOption match {
        case None => "X" //Error
        case Some(note) => note.toString
      }
    }).mkString(" ") + "]"
  }

}

/**
 * ChordParser: the parser
 */

object ChordParser {
  def parse(in: String): Chord = {
    internalParse(null, in.toList)
  }

  def internalParse(chord: Chord, in: List[Char]): Chord = {
    //println (s"Internal parse: chord=$chord in is [$in]")
    val result = in match {
      case Nil => return chord
      case head :: tail if (Character.isUpperCase(head)) => internalParse(ChordBuilder.buildMajorChord(head.toString), tail)
      case '#' :: tail => internalParse(chord.semitoneUp, in.tail)
      case 'b' :: tail => internalParse(chord.semitoneDown, in.tail)
      case '7' :: tail => internalParse(chord.makeSept, in.tail)
      case _ => null
    }
    //println (s"Returning $result")
    result
  }
}


object Main {
  def main(args: Array[String]) = {
    val chord = ChordParser.parse("C")

    val fingerings = Tuning.Ukulele.fingerings(chord)
    println(fingerings.mkString("\n"))
    println (s"Result ${fingerings.size}")
  }
}
