package astrac.itc.event

import cats.kernel.Eq
import cats.laws.discipline.TraverseTests
import cats.syntax.functor.*
import higherkindness.droste.syntax.unfix.*
import munit.DisciplineSuite
import org.scalacheck.Arbitrary

import astrac.itc.Gens.eventGen

class EventFSpec extends DisciplineSuite:
  given [A: Arbitrary]: Arbitrary[EventF[A]] =
    Arbitrary(
      Arbitrary.arbitrary[A].flatMap(a => eventGen.map(_.unfix.map(_ => a))),
    )

  given [A: Eq]: Eq[EventF[A]] = Eq.fromUniversalEquals

  checkAll(
    "EventF.Traverse",
    TraverseTests[EventF].traverse[Int, Int, String, Long, cats.Id, Option],
  )
