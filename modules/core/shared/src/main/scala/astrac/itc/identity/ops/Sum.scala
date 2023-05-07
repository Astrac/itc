package astrac.itc.identity.ops

import cats.syntax.functor.*
import higherkindness.droste.data.prelude.*
import higherkindness.droste.scheme
import higherkindness.droste.syntax.fix.*
import higherkindness.droste.syntax.unfix.*
import higherkindness.droste.CVAlgebra

import astrac.itc.identity.*

object Sum:
  def apply(id1: Identity, id2: Identity): Identity =
    scheme.zoo.histo(algebra).apply(id1)(id2)

  private val algebra: CVAlgebra[IdentityF, Identity => Identity] =
    CVAlgebra { id1 => id2 =>
      (id1, id2.unfix) match
        case (IdentityF.Leaf(0), b) =>
          b.fix

        case (a, IdentityF.Leaf(0)) =>
          a.map(_.forget).fix

        case (IdentityF.Leaf(1), _) | (_, IdentityF.Leaf(1)) =>
          Identity.leaf(1)

        case (IdentityF.Node(ll, lr), IdentityF.Node(rl, rr)) =>
          Identity.node(ll.head(rl), lr.head(rr))
    }
