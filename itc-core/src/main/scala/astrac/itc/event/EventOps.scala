package astrac.itc.event

import higherkindness.droste.syntax.fix.*
import higherkindness.droste.syntax.unfix.*

import astrac.itc.event.ops.*
import astrac.itc.identity.Identity

/**
 * The treeSink and treeLift functions are private because they can generate
 * invalid events if height is negative or bigger than the leaf value or node
 * height.
 */
trait EventOps:
  extension (event: Event)
    inline def fold[B](
        ifLeaf: EventF.Leaf[Event] => B,
        ifNode: EventF.Node[Event] => B,
    ): B = event.unfix.fold(ifLeaf, ifNode)

    private[event] inline def treeSink(height: Long): Event =
      event.unfix.treeSink(height).fix

    private[event] inline def treeLift(height: Long): Event =
      event.unfix.treeLift(height).fix

    inline def leq(other: Event): Boolean = Leq(event, other)
    inline def min: Long = event.fold(_.value, _.height)
    inline def max: Long = Max(event)
    inline def join(other: Event): Event = Join(event, other)
    inline def intersect(other: Event): Event = Intersect(event, other)
    inline def fill(id: Identity): Event = Fill(id, event)
    inline def grow(id: Identity): (Event, Grow.Cost) = Grow(id, event)
    inline def stringify: EventString = Stringify(event)
    inline def cap(max: Long): Event = Cap(event, max)

  extension (eventF: EventF[Event])
    inline def fold[B](
        ifLeaf: EventF.Leaf[Event] => B,
        ifNode: EventF.Node[Event] => B,
    ): B =
      eventF match
        case l @ EventF.Leaf(_)       => ifLeaf(l)
        case n @ EventF.Node(_, _, _) => ifNode(n)

    private[event] inline def treeLift(height: Long): EventF[Event] =
      fold(
        l => Event.leaf(l.value + height).unfix,
        n => Event.node(n.height + height, n.left, n.right).unfix,
      )

    private[event] inline def treeSink(height: Long): EventF[Event] =
      fold(
        l => Event.leaf(l.value - height).unfix,
        n => Event.node(n.height - height, n.left, n.right).unfix,
      )
end EventOps
