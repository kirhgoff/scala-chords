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

    "correctly translate between absolute and relative" in {
      var scale = new ShiftedScale("C")
      scale.absolute(0) shouldEqual scale.relative(0)
      scale.absolute(3) shouldEqual scale.relative(3)

      scale = new ShiftedScale("B")
      //B root(0, ?) - C absolute root (1. 0) - C# (2, 1) - D (3, 2)
      scale.relative(2) shouldEqual 3
      scale.absolute (12) shouldEqual 11
    }
  }

  "HarmonicScale" should {
     "calculate correct steps for tones" in {
       //TODO refactor with helper methods
       var scale = new HarmonicScale("D", ChordBuilder.MajorIntervals) //DFA
       scale.absoluteForStep(1) shouldEqual 2
       scale.absoluteForStep(3) shouldEqual 6
       scale.absoluteForStep(5) shouldEqual 9

       scale = new HarmonicScale("C", ChordBuilder.MajorIntervals)
       scale.absoluteForStep(1) shouldEqual 0
       scale.absoluteForStep(3) shouldEqual 4
       scale.absoluteForStep(5) shouldEqual 7

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

  "Fingering" should {
    "be able to see the notes in frets" in {
      val tuning = Tuning.GuitarTuning
      val fingering = new Fingering(tuning, chord("Am"), List(0, 0, 2, 2, 1, 0)) //Standard Am
      fingering.noteByString(0).get shouldEqual note("E")
      fingering.noteByString(1).get shouldEqual note("C")
      fingering.noteByString(2).get shouldEqual note("E")
      fingering.noteByString(3).get shouldEqual note("A")
      fingering.noteByString(4).get shouldEqual note("A")
      fingering.noteByString(5).get shouldEqual note("E")
    }
  }

  def parse(in:String) = ChordParser.parse(in).toList.map(_.toString)
  def chord(in:String) = ChordParser.parse(in)
  def note(in:String) = Note.make(in)
}
