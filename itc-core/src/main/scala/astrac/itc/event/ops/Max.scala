package astrac.itc.event.ops

import higherkindness.droste.scheme
import higherkindness.droste.Algebra

import astrac.itc.event.*

object Max:
  def apply(event: Event): Long =
    scheme.cata(algebra).apply(event)

  private val algebra: Algebra[EventF, Long] = Algebra {
    case EventF.Leaf(n)         => n
    case EventF.Node(n, el, er) => n + math.max(el, er)
  }
