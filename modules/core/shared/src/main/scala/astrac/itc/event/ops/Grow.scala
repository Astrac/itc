package astrac.itc.event.ops

import cats.syntax.functor.*
import cats.syntax.order.*
import cats.syntax.semigroup.*
import higherkindness.droste.data.prelude.*
import higherkindness.droste.data.Attr
import higherkindness.droste.scheme
import higherkindness.droste.syntax.unfix.*
import higherkindness.droste.CVAlgebra

import astrac.itc.event.*
import astrac.itc.identity.*

object Grow:
  type Cost = (Int, Int)

  def apply(id: Identity, event: Event): (Event, Cost) =
    scheme.zoo.histo(algebra).apply(id)(event)

  private val algebra: CVAlgebra[IdentityF, Event => (Event, Cost)] =
    CVAlgebra { identity => event =>
      (identity, event.unfix) match
        case (IdentityF.Leaf(0), _) =>
          (event, (0, 0))

        case (IdentityF.Leaf(1), EventF.Leaf(n)) =>
          (Event.leaf(n + 1), (0, 0))

        case (IdentityF.Leaf(1), EventF.Node(n, el, er)) =>
          (Event.node(n + 1, el, er), (0, 0))

        case (IdentityF.Node(IdentityF.Leaf(0), ir), EventF.Node(n, el, er)) =>
          val (erPrime, cost) = ir.head(er)
          (Event.node(n, el, erPrime), cost |+| (0, 1))

        case (IdentityF.Node(il, IdentityF.Leaf(0)), EventF.Node(n, el, er)) =>
          val (elPrime, cost) = il.head(el)
          (Event.node(n, elPrime, er), cost |+| (0, 1))

        case (id @ IdentityF.Node(_, _), event @ EventF.Leaf(n)) =>
          choose(id, EventF.Node(n, Event.zeroLeaf, Event.zeroLeaf), (1, 0))

        case m @ (id @ IdentityF.Node(_, _), event @ EventF.Node(_, _, _)) =>
          choose(id, event, (0, 1))
    }

  private def choose(
      id: IdentityF.Node[Attr[IdentityF, Event => (Event, Cost)]],
      event: EventF.Node[Event],
      additionalCost: Cost,
  ): (Event, Cost) =
    val IdentityF.Node(il, ir) = id
    val EventF.Node(n, el, er) = event
    val (elPrime, elCost) = il.head(el)
    val (erPrime, erCost) = ir.head(er)

    if erPrime == er || (elPrime != el && elCost <= erCost) then
      (Event.node(n, elPrime, er), elCost |+| additionalCost)
    else (Event.node(n, el, erPrime), erCost |+| additionalCost)

end Grow
