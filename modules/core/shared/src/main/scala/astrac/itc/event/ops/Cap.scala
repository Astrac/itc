package astrac.itc.event.ops

import higherkindness.droste.scheme
import higherkindness.droste.Algebra

import astrac.itc.event.*

object Cap:
  def apply(event: Event, limit: Long): Event =
    scheme.cata(algebra).apply(event)(limit)

  private val algebra: Algebra[EventF, Long => Event] =
    Algebra {
      case EventF.Leaf(value) =>
        limit => Event.leaf(math.min(value, limit))

      case EventF.Node(n, el, er) =>
        limit =>
          if n >= limit then Event.leaf(limit)
          else Event.node(n, el(limit - n), er(limit - n))
    }
