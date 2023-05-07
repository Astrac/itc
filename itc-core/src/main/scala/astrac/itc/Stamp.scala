package astrac.itc

import astrac.itc.event.Event
import astrac.itc.identity.Identity

case class Stamp(identityItc: Identity, eventItc: Event):
  def peek: AnonymousStamp =
    AnonymousStamp(eventItc)

  def isStrictlyLess(other: Stamp): Boolean =
    eventItc.leq(other.eventItc) && !other.eventItc.leq(eventItc)

  def isLeq(other: Stamp): Boolean =
    eventItc.leq(other.eventItc)

  def isConcurrent(other: Stamp): Boolean =
    !eventItc.leq(other.eventItc) && !other.eventItc.leq(eventItc)

  def isEq(other: Stamp): Boolean =
    eventItc.leq(other.eventItc) && other.eventItc.leq(eventItc)

  def fork: (Stamp, Stamp) =
    val (id1, id2) = identityItc.split
    (Stamp(id1, eventItc), Stamp(id2, eventItc))

  def join(other: Stamp): Stamp =
    Stamp(
      identityItc.sum(other.identityItc),
      eventItc.join(other.eventItc),
    )

  def event: Stamp =
    val filled = eventItc.fill(identityItc)

    if eventItc != filled then Stamp(identityItc, filled)
    else Stamp(identityItc, filled.grow(identityItc)._1)

  def send: (Stamp, AnonymousStamp) =
    val afterEvent = event
    (afterEvent, afterEvent.peek)

  def receive(other: AnonymousStamp): Stamp =
    join(Stamp(Identity.leaf(0), other.event)).event

  def sync(other: Stamp): (Stamp, Stamp) =
    join(other).fork
end Stamp

object Stamp:
  lazy val genesis: Stamp = Stamp(Identity.leaf(1), Event.zeroLeaf)

case class AnonymousStamp(event: Event):
  def identity: Identity = Identity.anonymous
