package org.kirhgoff.chords

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

class ChordsTests extends Specification with Scope {

  object Scale extends AbsoluteScale

  "AbsoluteScLe" should {
    "give correct numbers for known notes" in {
      Scale.getNoteName(0) shouldEqual "C"
      Scale.getIntervalByName("C") shouldEqual 0
      Scale.getNoteName(5) shouldEqual "F"
      Scale.getIntervalByName("F") shouldEqual 5
    }
  }

  "HarmonicScale" should {
     "calculate correct steps for tones" in {
       var scale = new HarmonicScale("C", ChordBuilder.MajorIntervals)
       Scale.getNoteName(scale.getIntervalForStep(1)) shouldEqual "C"
       Scale.getNoteName(scale.getIntervalForStep(3)) shouldEqual "E"
       Scale.getNoteName(scale.getIntervalForStep(5)) shouldEqual "G"

       scale = new HarmonicScale("G", ChordBuilder.MajorIntervals)
       Scale.getNoteName(scale.getIntervalForStep(1)) shouldEqual "G"
       Scale.getNoteName(scale.getIntervalForStep(3)) shouldEqual "B"
       Scale.getNoteName(scale.getIntervalForStep(5)) shouldEqual "D"
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

  def parse(in:String) = {
    ChordParser.parseChord(in).toList.map(_.toString)
  }
}
