package astrac.itc.event.ops

import higherkindness.droste.scheme
import higherkindness.droste.Algebra

import astrac.itc.event.*

type EventString = Stringify.EventString

object Stringify:

  opaque type EventString = String
  extension (s: EventString) inline def asString: String = s

  def apply(id: Event): EventString =
    scheme.cata(algebra).apply(id)

  def algebra = Algebra[EventF, String] {
    case EventF.Leaf(n)         => n.toString
    case EventF.Node(n, el, er) => s"[$n, $el, $er]"
  }
