package astrac.itc

import munit.ScalaCheckSuite

class StampSpec extends ScalaCheckSuite:

  test("Simple scenario") {
    val (alice, bob) = Stamp.genesis.fork

    val aliceEv = alice.event
    val aliceEv2 = aliceEv.event
    val bobEv = bob.event

    val (aliceSync, bobSync) = aliceEv.sync(bobEv)

    assert(aliceEv.isConcurrent(bobEv))
    assert(aliceEv.isStrictlyLess(aliceEv2))
    assert(aliceSync.isEq(bobSync))
  }
