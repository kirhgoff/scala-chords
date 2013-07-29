package org.kirhgoff.chords

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

class ChordsTests extends Specification with Scope {

  "Chords" should {
    "be able to parse simple chords" in {
      Parser.parse("A") shouldEqual "A, B, C"
    }
  }

}
