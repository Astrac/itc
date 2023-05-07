package astrac.itc

import astrac.itc.event.Event
import astrac.itc.identity.Identity

case class Stamp(identity: Identity, event: Event):
  def peek: AnonymousStamp =
    AnonymousStamp(event)

  def fork: (Stamp, Stamp) =
    val (id1, id2) = identity.split
    (Stamp(id1, event), Stamp(id2, event))

  def join(other: Stamp): Stamp =
    Stamp(
      identity.sum(other.identity),
      event.join(other.event),
    )

  def newEvent: Stamp =
    val filled = event.fill(identity)

    if event != filled then Stamp(identity, filled)
    else Stamp(identity, filled.grow(identity)._1)

  def send: (Stamp, AnonymousStamp) =
    val afterEvent = newEvent
    (afterEvent, afterEvent.peek)

  def receive(other: AnonymousStamp): Stamp =
    join(Stamp(Identity.leaf(0), other.event)).newEvent

  def sync(other: Stamp): (Stamp, Stamp) =
    join(other).fork
end Stamp

object Stamp:
  lazy val genesis: Stamp = Stamp(Identity.leaf(1), Event.zeroLeaf)

case class AnonymousStamp(event: Event):
  def identity: Identity = Identity.anonymous
