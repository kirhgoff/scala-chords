package org.kirhgoff.chords

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

class ChordsTests extends Specification with Scope {

  object Scale extends AbsoluteScale

  "AbsoluteScLe" should {
    "give correct numbers for known notes" in {
      Scale.getNoteForAbsoluteInterval(0) shouldEqual "C"
      Scale.absoluteInterval("C") shouldEqual 0
      Scale.absoluteInterval("C#") shouldEqual 1
      Scale.getNoteForAbsoluteInterval(5) shouldEqual "F"
      Scale.absoluteInterval("F") shouldEqual 5
      Scale.absoluteInterval("E") shouldEqual 4
    }
  }

  "ShiftedScale" should {
    "give correct shifts for notes" in {
      new ShiftedScale("C#").absolute(1) shouldEqual 2
      new ShiftedScale("E").absolute(1) shouldEqual 5

      val scale = new ShiftedScale("E")
      scale.absolute(1) shouldEqual Scale.absoluteInterval("F")
      Note.make(Scale.getNoteForAbsoluteInterval(scale.absolute(4))) shouldEqual Note.make("G#")
    }
  }

  "HarmonicScale" should {
     "calculate correct steps for tones" in {
       //TODO refactor with helper methods
       var scale = new HarmonicScale("D", ChordBuilder.MajorIntervals) //DFA
       scale.getIntervalForStep(1) shouldEqual 2
       scale.getIntervalForStep(3) shouldEqual 6
       scale.getIntervalForStep(5) shouldEqual 9

       scale = new HarmonicScale("C", ChordBuilder.MajorIntervals)
       scale.getIntervalForStep(1) shouldEqual 0
       scale.getIntervalForStep(3) shouldEqual 4
       scale.getIntervalForStep(5) shouldEqual 7

       scale.getNoteForStep(1) shouldEqual "C"
       scale.getNoteForStep(3) shouldEqual "E"
       scale.getNoteForStep(5) shouldEqual "G"

       scale = new HarmonicScale("G", ChordBuilder.MajorIntervals)
       scale.getNoteForStep(1) shouldEqual "G"
       scale.getNoteForStep(3) shouldEqual "B"
       scale.getNoteForStep(5) shouldEqual "D"
     }
  }

  "ChordBuilder" should {
    "build different chords" in {
      ChordBuilder.buildMajorChord("F").toList.map(_.toString) shouldEqual List("F", "A", "C")
    }
  }

  "Parser" should {
    "give proper notes for simple chords" in {
      parse("C") shouldEqual List("C", "E", "G")
      parse("F") shouldEqual List("F", "A", "C")
    }
    "work with diez/bemols" in {
      parse("G#") shouldEqual List("G#", "C", "D#")
      parse("Db") shouldEqual List("C#", "F", "G#")
    }
  }

  "NoteString" should {
    "give correct fret positions" in {
      var string = new NoteString ("E")
      string.fret ("E") shouldEqual 0
      string.fret ("G") shouldEqual 3
      string.fret ("G#") shouldEqual 4
      string.fret ("A") shouldEqual 5
      string.fret ("B") shouldEqual 7
      string.fret ("C") shouldEqual 8
      string.fret ("D") shouldEqual 10

      string = new NoteString ("B")
      string.fret ("C") shouldEqual 1
      string.fret ("D") shouldEqual 3
      string.fret ("D#") shouldEqual 4
      string.fret ("E") shouldEqual 5
      string.fret ("F") shouldEqual 6
      string.fret ("G") shouldEqual 8
      string.fret ("A") shouldEqual 10
      string.fret ("B") shouldEqual 0
    }
  }

  def parse(in:String) = ChordParser.parseChord(in).toList.map(_.toString)
}
