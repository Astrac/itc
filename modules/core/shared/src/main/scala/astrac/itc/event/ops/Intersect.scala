package astrac.itc.event.ops

import higherkindness.droste.scheme
import higherkindness.droste.syntax.unfix.*
import higherkindness.droste.Coalgebra

import astrac.itc.event.*

object Intersect:

  def apply(event1: Event, event2: Event): Event =
    scheme
      .hylo(Normalization.algebra, coalgebra)
      .apply((event1.unfix, event2.unfix))

  private val coalgebra: Coalgebra[EventF, (EventF[Event], EventF[Event])] =
    Coalgebra {
      case (EventF.Leaf(ln), EventF.Leaf(rn)) =>
        EventF.Leaf(math.min(ln, rn))

      case (EventF.Leaf(ln), EventF.Node(rn, rel, rer)) =>
        intersectNodes(
          (ln, EventF.Leaf(0), EventF.Leaf(0)),
          (rn, rel.unfix, rer.unfix),
        )

      case (EventF.Node(ln, lel, ler), EventF.Leaf(rn)) =>
        intersectNodes(
          (ln, lel.unfix, ler.unfix),
          (rn, EventF.Leaf(0), EventF.Leaf(0)),
        )

      case (EventF.Node(ln, lel, ler), EventF.Node(rn, rel, rer)) =>
        intersectNodes((ln, lel.unfix, ler.unfix), (rn, rel.unfix, rer.unfix))
    }

  private def intersectNodes(
      n1: (Long, EventF[Event], EventF[Event]),
      n2: (Long, EventF[Event], EventF[Event]),
  ): EventF.Node[(EventF[Event], EventF[Event])] =
    val ((ln, lel, ler), (rn, rel, rer)) =
      if n1._1 < n2._1 then (n1, n2)
      else (n2, n1)

    EventF.Node(
      ln,
      (lel, rel.treeLift(rn - ln)),
      (ler, rer.treeLift(rn - ln)),
    )

end Intersect
