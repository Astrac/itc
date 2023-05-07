package astrac.itc.simulator

import java.nio.ByteBuffer
import java.util.UUID

import astrac.itc.Stamp

case class Event(id: Event.Id, stamp: Stamp, registeredBy: NodeId)

object Event:
  opaque type Id = String
  object Id:
    def random(): Id = ShortUUID.random().asString
    def apply(value: String): Id = value

  def withRandomId(stamp: Stamp, registeredBy: NodeId): Event =
    Event(Id.random(), stamp, registeredBy)
