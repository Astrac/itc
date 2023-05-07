package astrac.itc.event.ops

import higherkindness.droste.Algebra

import astrac.itc.event.Event
import astrac.itc.event.EventF

private object Normalization:
  val algebra: Algebra[EventF, Event] = Algebra {
    case EventF.Leaf(n)         => Event.leaf(n)
    case EventF.Node(n, el, er) => Event.node(n, el, er)
  }
