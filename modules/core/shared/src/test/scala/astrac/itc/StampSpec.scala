package astrac.itc

import munit.ScalaCheckSuite
import org.scalacheck.Prop

import astrac.itc.Gens.stampGen

class StampSpec extends ScalaCheckSuite:

  test("Simple scenario") {
    val (alice, bob) = Stamp.genesis.fork

    val aliceEv = alice.newEvent
    val aliceEv2 = aliceEv.newEvent
    val bobEv = bob.newEvent

    val (aliceSync, bobSync) = aliceEv.sync(bobEv)

    assert(aliceEv.event.isConcurrent(bobEv.event))
    assert(aliceEv.event.isStrictlyLess(aliceEv2.event))
    assert(aliceSync.event.isEqual(bobSync.event))
  }

  test("Forking and joining the results return the original stamp") {
    Prop.forAll(stampGen) { stamp =>
      val (s1, s2) = stamp.fork
      assertEquals(s1.join(s2), stamp)
    }
  }

  test(
    "Forking and calling newEvent on the two stamps will generate concurrent stamps",
  ) {
    Prop.forAll(stampGen) { stamp =>
      val (s1, s2) = stamp.fork
      assert(s1.newEvent.event.isConcurrent(s2.newEvent.event))
    }
  }

  test(
    "newEvent will generate a stamp that is strictly greater than the original",
  ) {
    Prop.forAll(stampGen) { stamp =>
      assert(stamp.event.isStrictlyLess(stamp.newEvent.event))
    }
  }

  test("Join commutes") {
    Prop.forAll(stampGen, stampGen) { (s1, s2) =>
      assertEquals(s1.join(s2), s2.join(s1))
    }
  }

  test("Sync commutes") {
    Prop.forAll(stampGen, stampGen) { (s1, s2) =>
      assertEquals(s1.sync(s2), s2.sync(s1))
    }
  }
end StampSpec
