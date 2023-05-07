package astrac.itc.event.ops

import cats.syntax.functor.*
import higherkindness.droste.data.prelude.*
import higherkindness.droste.scheme
import higherkindness.droste.syntax.fix.*
import higherkindness.droste.syntax.unfix.*
import higherkindness.droste.CVAlgebra

import astrac.itc.event.*
import astrac.itc.identity.*

object Fill:

  def apply(id: Identity, event: Event): Event =
    scheme.zoo.histo(algebra).apply(event)(id)

  private val algebra: CVAlgebra[EventF, Identity => Event] =
    CVAlgebra { event => identity =>
      (identity.unfix, event) match
        case (IdentityF.Leaf(0), e) =>
          e.map(_.forget).fix

        case (IdentityF.Leaf(1), e) =>
          Event.leaf(
            e.map(_.forget).fix.max,
          )

        case (_, EventF.Leaf(n)) =>
          Event.leaf(n)

        case (
              IdentityF.Node(IdentityF.Leaf(1), ir),
              EventF.Node(n, el, er),
            ) =>
          val rPrime = er.head(ir)
          Event.node(
            n,
            Event.leaf(math.max(el.forget.max, rPrime.min)),
            rPrime,
          )

        case (
              IdentityF.Node(il, IdentityF.Leaf(1)),
              EventF.Node(n, el, er),
            ) =>
          val lPrime = el.head(il)
          Event.node(
            n,
            lPrime,
            Event.leaf(math.max(er.forget.max, lPrime.min)),
          )

        case (IdentityF.Node(il, ir), EventF.Node(n, el, er)) =>
          Event.node(n, el.head(il), er.head(ir))
    }
end Fill
