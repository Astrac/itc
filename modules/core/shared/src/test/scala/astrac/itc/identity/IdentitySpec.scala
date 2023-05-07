package astrac.itc.identity

import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop

import astrac.itc.identity.syntax.*
import astrac.itc.Gens.identityGen

class IdentitySpec extends ScalaCheckSuite:

  test(
    "Splitting and summing the results gives back original identity",
  ) {
    Prop.forAll(identityGen) { id =>
      val (s1, s2) = id.split
      val sum = s1.sum(s2)

      assertEquals(sum, id)
    }
  }

  test(
    "Summing an identity with an anonymous identity should return the original identity",
  ) {
    Prop.forAll(identityGen)(id => assertEquals(id.sum(itcId"0"), id))
  }

  test(
    "Summing an identity with the '1' identity gives the '1' identity",
  ) {
    Prop.forAll(identityGen)(id => assertEquals(id.sum(itcId"1"), itcId"1"))
  }

  test("Sum commutes") {
    Prop.forAll(identityGen, identityGen) { (id1, id2) =>
      assertEquals(id1.sum(id2), id2.sum(id1))
    }
  }

  test(
    "Splitting and multiplying the results give the anonymous identity",
  ) {
    Prop.forAll(identityGen) { id =>
      val (s1, s2) = id.split
      val prod = s1.prod(s2)

      assertEquals(prod, itcId"0")
    }
  }

  test(
    "Multiplying an identity with an anonymous identity returns the anonymous identity",
  ) {
    Prop.forAll(identityGen)(id => assertEquals(id.prod(itcId"0"), itcId"0"))
  }

  test(
    "Multiplying an identity with the '1' identity gives back the original identity",
  ) {
    Prop.forAll(identityGen)(id => assertEquals(id.prod(itcId"1"), id))
  }

  test("Prod commutes") {
    Prop.forAll(identityGen, identityGen) { (id1, id2) =>
      assertEquals(id1.prod(id2), id2.prod(id1))
    }
  }

  test("Stringify then parse to get back the original identity") {
    Prop.forAll(identityGen)(id =>
      assertEquals(Identity.parse(id.stringify.asString), Right(id)),
    )
  }

end IdentitySpec
