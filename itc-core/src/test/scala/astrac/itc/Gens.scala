package astrac.itc

import org.scalacheck.Gen

import astrac.itc.event.Event
import astrac.itc.identity.Identity

object Gens:
  def eventGen: Gen[Event] =
    for
      isLeaf <- Gen.choose(0, 100).map(_ > 0)
      tree <- if isLeaf then eventLeafGen else eventNodeGen
    yield tree

  def identityGen: Gen[Identity] =
    for
      isLeaf <- Gen.choose(0, 100).map(_ > 40)
      tree <- if isLeaf then identityLeafGen else identityNodeGen
    yield tree

  def identityNodeGen: Gen[Identity] =
    Gen
      .zip(identityGen, identityGen)
      .map(z => Identity.node(z._1, z._2))

  def identityLeafGen: Gen[Identity] =
    Gen.oneOf[0 | 1](0, 1).map(Identity.leaf)

  def eventNodeGen: Gen[Event] =
    Gen
      .zip(longGen, eventGen, eventGen)
      .map(z => Event.node(z._1, z._2, z._3))

  def eventLeafGen: Gen[Event] =
    longGen.map(Event.leaf)

  val longGen: Gen[Long] = Gen.choose(0, 1000)
end Gens
