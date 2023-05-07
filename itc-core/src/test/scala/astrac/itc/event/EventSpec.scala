package astrac.itc.event

import higherkindness.droste.syntax.unfix.*
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop

import astrac.itc.event.syntax.*
import astrac.itc.identity.syntax.*
import astrac.itc.Gens.eventGen
import astrac.itc.Gens.identityGen
import astrac.itc.Gens.longGen

class EventSpec extends ScalaCheckSuite:

  test("Only builds normalised events") {
    Prop.forAll(eventGen)(event => assert(isNormalised(event)))
  }

  test("Joining an event with itself gives back the original") {
    Prop.forAll(eventGen)(event => assertEquals(event.join(event), event))
  }

  test(
    "Joining an event with a leaf higher than or equal to its max gives back the leaf",
  ) {
    val gen =
      eventGen.flatMap(e =>
        longGen.suchThat(_ >= e.max).map(n => (e, Event.leaf(n))),
      )

    Prop.forAll(gen)((event, leaf) => assertEquals(event.join(leaf), leaf))
  }

  test(
    "Joining an event with a leaf lower than or equal to its min gives back the event",
  ) {
    val gen =
      eventGen.flatMap(e =>
        longGen.suchThat(_ <= e.min).map(n => (e, Event.leaf(n))),
      )

    Prop.forAll(gen)((event, leaf) => assertEquals(event.join(leaf), event))
  }

  test("Join commutes") {
    Prop.forAll(eventGen, eventGen)((ev1, ev2) =>
      assertEquals(ev1.join(ev2), ev2.join(ev1)),
    )
  }

  test("Intersect commutes") {
    Prop.forAll(eventGen, eventGen)((ev1, ev2) =>
      assertEquals(ev1.intersect(ev2), ev2.intersect(ev1)),
    )
  }

  test("Stringify then parse to get back the original event") {
    Prop.forAll(eventGen)(e =>
      assertEquals(Event.parse(e.stringify.asString), Right(e)),
    )
  }

  test("Parse invalid event") {
    assert(Event.parse("[]").isLeft)
    assert(Event.parse("[100, 50, 10").isLeft)
    assert(Event.parse("[100, 50, 10, 12]").isLeft)
    assert(Event.parse("ABC").isLeft)
  }

  test("Joining with an empty event gives back the original") {
    Prop.forAll(eventGen) { event =>
      assertEquals(event.join(Event.zeroLeaf), event)
    }
  }

  test("Intersecting with an empty event gives the empty event") {
    Prop.forAll(eventGen) { event =>
      assertEquals(event.intersect(Event.zeroLeaf), Event.zeroLeaf)
    }
  }

  test("Intersecting an event with itself gives back the original") {
    Prop.forAll(eventGen) { event =>
      assertEquals(event.intersect(event), event)
    }
  }

  test("Capping an event to a certain height and then calculating its max") {
    Prop.forAll(eventGen, longGen) { (event, cap) =>
      if event.max > cap then assertEquals(event.cap(cap).max, cap)
      else assertEquals(event.cap(cap).max, event.max)
    }
  }

  test("Capping an event to its min gives back a leaf") {
    Prop.forAll(eventGen) { event =>
      assertEquals(event.cap(event.min), Event.leaf(event.min))
    }
  }

  test(
    "Filling an event with an identity gives an event that is greater or equal to the given one",
  ) {
    Prop.forAll(eventGen, identityGen) { (event, identity) =>
      assert(event.leq(event.fill(identity)))
    }
  }

  test(
    "Growing an event with a non-empty identity gives an event that is strictly greater than the given one",
  ) {
    Prop.forAll(eventGen, identityGen.suchThat(_ != itcId"0")) {
      (event, identity) =>
        val (grown, _) = event.grow(identity)
        assert(event.leq(grown) && !grown.leq(event))
    }
  }

  test(
    "Splitting a non-empty identity and growing an event with the two parts gives two concurrent events",
  ) {
    Prop.forAll(eventGen, identityGen.suchThat(_ != itcId"0")) {
      (event, identity) =>
        val (id1, id2) = identity.split
        val (grown1, _) = event.grow(id1)
        val (grown2, _) = event.grow(id2)
        assert(!grown1.leq(grown2) && !grown2.leq(grown1))
    }
  }

  test(
    "Growing an event with a '0' identity gives the event back",
  ) {
    Prop.forAll(eventGen)(event => assertEquals(event.grow(itcId"0")._1, event))
  }

  test(
    "Filling an event with a '1' identity gives a leaf with the max of the given event",
  ) {
    Prop.forAll(eventGen) { event =>
      assertEquals(event.fill(itcId"1"), Event.leaf(event.max))
    }
  }

  test(
    "Filling an event with a '0' identity gives the event back",
  ) {
    Prop.forAll(eventGen)(event => assertEquals(event.fill(itcId"0"), event))
  }

  test("Filling an event with an identity") {
    val event = itcEv"[150, [0, 100, 0], [50, 0, 50]]"

    assertEquals(event.fill(itcId"[0, 1]"), itcEv"[150, [0, 100, 0], 100]")
    assertEquals(event.fill(itcId"[1, 0]"), itcEv"[200, 50, [0, 0, 50]]")
    assertEquals(event.fill(itcId"[1, [1, 0]]"), itcEv"250")
  }

  test("Growing an event with an identity") {
    assertEquals(itcEv"0".grow(itcId"0"), itcEv"0" -> (0, 0))
    assertEquals(itcEv"0".grow(itcId"1"), itcEv"1" -> (0, 0))
    assertEquals(itcEv"[1, 0, 1]".grow(itcId"1"), itcEv"[2, 0, 1]" -> (0, 0))
    assertEquals(itcEv"0".grow(itcId"[0, 1]"), itcEv"[0, 0, 1]" -> (1, 0))
    assertEquals(itcEv"0".grow(itcId"[1, 0]"), itcEv"[0, 1, 0]" -> (1, 0))
    assertEquals(itcEv"0".grow(itcId"[1, [0, 1]]"), itcEv"[0, 1, 0]" -> (1, 0))

    assertEquals(
      itcEv"0".grow(itcId"[0, [0, 1]]"),
      itcEv"[0, 0, [0, 0, 1]]" -> (2, 0),
    )

    assertEquals(itcEv"[1, 0, 1]".grow(itcId"[1, [0, 1]]"), itcEv"2" -> (0, 1))

    assertEquals(
      itcEv"[1, 0, [0, 1, 0]]".grow(itcId"[[1, 0], [0, 1]]"),
      itcEv"[1, 0, 1]" -> (0, 2),
    )
  }

  private def isNormalised(e: Event): Boolean =
    e.unfix match
      case EventF.Leaf(value)   => true
      case EventF.Node(_, l, r) => l.min == 0 || r.min == 0

end EventSpec
