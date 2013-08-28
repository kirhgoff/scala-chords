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
  def getAbsoluteIntervalForNote(note:String):Int = note.toList match {
    case a :: b :: c :: tail => throw new RuntimeException("Too much symbols")
    case root :: 'b' :: Nil  => names.indexOf(note) - 1
    case root :: '#' :: Nil => names.indexOf(note) + 1
    case 'H' :: Nil => getAbsoluteIntervalForNote("B")
    case note if names.contains(note) => names.indexOf(note)
    case somethingElse => throw new RuntimeException(s"Dont now such note $somethingElse")
  }
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
class ShiftedScale (value:Int) extends AbsoluteScale {
  val shiftValue: Int = normalize(value)
  //Semitones from absolute root to this number of the semitones from the scale root
  def absolute(relative:Int) = normalize(relative + shiftValue)
  def relative(absolute:Int) = normalize(absolute - shiftValue)
  def semitoneUp = new ShiftedScale(shiftValue + 1)
  def semitoneDown = new ShiftedScale(shiftValue - 1)

  assert(shiftValue >= 0)
}

object ShiftedScale extends AbsoluteScale {
  def fromNote(root:String) = new ShiftedScale(absoluteInterval(root))
}

/**
 * HarmonicScale: keeps the intervals between nodes and provides the ability
 * to translate intervals to scale steps and back
 */

class HarmonicScale(intervals: List[Int]) {
  val accumulatedIntervals: List[Int] = intervals.scanLeft(0)(_ + _).dropRight(1)
  def stepsCount = intervals.length
  def relativeForStep(step: Int): Int = accumulatedIntervals((step - 1) % intervals.length)
  def stepForRelative(relative: Int) : Option[Int] = {
    relative match {
      case _ if (relative < 0 || relative > stepsCount)  => None
      case found if accumulatedIntervals.toSeq.contains(relative) => Some(accumulatedIntervals.indexOf(relative))
      case _ => None
    }
  }
  def applyDiff (other: HarmonicScale, positions:List[Int]): List[Int] = {
    val diff = other.accumulatedIntervals zip accumulatedIntervals map {
      case (first, second) => {
        first - second
      }
    }
    positions.map{
      position => {
        if (other.accumulatedIntervals.contains(position)) {
          val index = other.accumulatedIntervals.indexOf(position)
          position - diff(index)
        }
        else {
          position
        }
      }
    }
  }
}

/**
 * Set of notes, provides ability to change the flavors of chord,
 * change the notes correspondingly
 */
class Chord(val shift:ShiftedScale, val scale:HarmonicScale, val positions: List[Int]) {
  def semitoneUp = new Chord(shift.semitoneUp, scale, positions)
  def semitoneDown = new Chord(shift.semitoneDown, scale, positions)
  def makeSept = new Chord(shift, scale, positions :+ scale.relativeForStep(7))
  def makeMinor = {
    //create new scale
    //find differences between scales
    //apply differences to positions
    val newScale = new HarmonicScale(Chord.MinorIntervals)
    new Chord(shift, newScale, newScale.applyDiff(scale, positions))
  }
  def notes = positions.map(relative => new Note(shift.absolute(relative)))
  override def toString = notes.toString
  def toList = notes
}

/**
 * Chord: factory object to work with chords
 */

object Chord  {
  val MajorIntervals = List(2, 2, 1, 2, 2, 2, 1)
  val MinorIntervals = List(2, 1, 2, 2, 1, 2, 2)

  def buildChordBySteps(root:String, steps: List[Int], intervals:List[Int]) = {
    val shift = ShiftedScale.fromNote(root)
    val scale = new HarmonicScale(intervals)
    val positions = steps.map(scale.relativeForStep(_))
    new Chord(shift, scale, positions)
  }
  def buildMajorChord(root: String): Chord = buildChordBySteps(root, List(1, 3, 5), MajorIntervals)
  def buildMinorChord(root: String): Chord = buildChordBySteps(root, List(1, 3, 5), MinorIntervals)
}

/**
 * ChordParser: the parser, creates Chords for their string representation
 */

object ChordParser {
  def parse(in: String): Chord = {
    internalParse(null, in.toList)
  }

  def internalParse(chord: Chord, in: List[Char]): Chord = {
    //println (s"Internal parse: chord=$chord in is [$in]")
    val result = in match {
      case Nil => return chord
      case head :: tail if (Character.isUpperCase(head)) => internalParse(Chord.buildMajorChord(head.toString), tail)
      case '#' :: tail => internalParse(chord.semitoneUp, in.tail)
      case 'b' :: tail => internalParse(chord.semitoneDown, in.tail)
      case '7' :: tail => internalParse(chord.makeSept, in.tail)
      case 'm' :: tail => internalParse(chord.makeMinor, in.tail)
      case _ => internalParse(chord, in.tail) //TODO
    }
    //println (s"Returning $result")
    result
  }
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
 * NoteString: gives a fret position for given note
 */

class NoteString (val root:Int) extends ShiftedScale(root) {
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

object NoteString extends AbsoluteScale {
  def fromNote (root:String) = new NoteString(absoluteInterval(root))
}

/**
 * Tuning: represents some string instrument tuning
 */

object Tuning {
  val GuitarTuning = new Tuning (List("E", "A", "G", "D", "B", "E"))
  val Ukulele = new Tuning (List("G", "C", "E", "A"))
}

class Tuning (reversedStringRoots:List[String]) {
  val strings:List[NoteString] = reversedStringRoots.reverse.map (NoteString.fromNote(_))

  def rawFingerings(chord:Chord):List[List[Int]] = {
    val notesOnStrings:List[List[Int]] = strings.map(
      string => chord.notes.map(
        note => string.getFretForNote(note)
      )
    )
    val initial:List[List[Int]] = List(List[Int]())
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
  def noteByString(stringNumber:Int):Option[Note] = {
    //println (s"noteByString num=$num positions=$positions strings=${tuning.strings}")
    if (stringNumber > positions.size) return None
    val string = tuning.strings(stringNumber)
    val fret = positions(stringNumber)
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
        case Some(note) => s"${note.toString}"
      }
    }).mkString(" ") + "]"
  }

}

class GuitarTab(val fingering:Fingering) {

}

object Main {
  def main(args: Array[String]) = {
    val chordString = "G7"
    val chord = ChordParser.parse(chordString)

    val fingerings = Tuning.Ukulele.fingerings(chord)
    println (fingerings.mkString("\n"))
    //println(fingerings.map(GuitarTab.view(_)))
    println (s"Result ${fingerings.size}")
  }
}
