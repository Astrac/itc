package astrac.itc.simulator

import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.Applicative
import cats.Traverse
import higherkindness.droste.data.Fix
import higherkindness.droste.scheme
import higherkindness.droste.syntax.fix.*
import higherkindness.droste.syntax.unfix.*
import higherkindness.droste.util.DefaultTraverse
import higherkindness.droste.Coalgebra

case class CausalityTreeF[A](
    event: Event,
    predecessors: List[A],
    concurrents: List[A],
)

object CausalityTreeF:
  given Traverse[CausalityTreeF] = ???

type CausalityTree = Fix[CausalityTreeF]

object CausalityTree:
  def root(event: Event): CausalityTree =
    CausalityTreeF(event, Nil, Nil).fix

  def add(newEvent: Event, tree: CausalityTree): CausalityTree =
    scheme
      .ana(Coalgebra[CausalityTreeF, CausalityTreeF[CausalityTree]] {
        case ev @ CausalityTreeF(event, predecessors, concurrents) =>
          if newEvent.stamp.event.isLessOrEqual(event.stamp.event) then

            val (predecessorsLessThanNew, otherPredecessors) =
              predecessors.partition(
                _.unfix.event.stamp.event.isStrictlyLess(newEvent.stamp.event),
              )

            val newEventConcurrents =
              predecessors.partition(
                _.unfix.event.stamp.event.isStrictlyLess(newEvent.stamp.event),
              )

            CausalityTreeF(
              event,
              CausalityTreeF(
                newEvent,
                predecessorsLessThanNew,
                Nil,
              ) :: otherPredecessors
                .map(_.unfix),
              concurrents.map(_.unfix),
            )
          else if event.stamp.event.isLessOrEqual(newEvent.stamp.event) then
            val (concurrentsLessThanNew, otherConcurents) =
              concurrents.partition(
                _.unfix.event.stamp.event.isStrictlyLess(newEvent.stamp.event),
              )

            CausalityTreeF(
              newEvent,
              ev :: concurrentsLessThanNew.map(_.unfix),
              otherConcurents.map(_.unfix),
            )
          else
            CausalityTreeF(
              event,
              predecessors.map(_.unfix),
              CausalityTreeF(newEvent, Nil, Nil) :: concurrents.map(_.unfix),
            )
      })
      .apply(tree.unfix)
end CausalityTree
