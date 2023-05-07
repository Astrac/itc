package astrac.itc.simulator

import cats.kernel.Order

import astrac.itc.Stamp

opaque type NodeId = String

object NodeId:
  given Order[NodeId] = Order.fromOrdering[String]
  def random(): NodeId = ShortUUID.random().asString
  def apply(value: String): NodeId = value
