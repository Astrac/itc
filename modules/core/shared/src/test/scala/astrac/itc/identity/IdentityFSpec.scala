package astrac.itc.identity

import cats.kernel.Eq
import cats.laws.discipline.TraverseTests
import cats.syntax.functor.*
import higherkindness.droste.syntax.unfix.*
import munit.DisciplineSuite
import org.scalacheck.Arbitrary

import astrac.itc.Gens.identityGen

class IdentityFSpec extends DisciplineSuite:
  given [A: Arbitrary]: Arbitrary[IdentityF[A]] =
    Arbitrary(
      Arbitrary.arbitrary[A].flatMap(a => identityGen.map(_.unfix.map(_ => a))),
    )

  given [A: Eq]: Eq[IdentityF[A]] = Eq.fromUniversalEquals

  checkAll(
    "IdentityF.Traverse",
    TraverseTests[IdentityF].traverse[Int, Int, String, Long, cats.Id, Option],
  )
