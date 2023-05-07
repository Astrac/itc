package astrac.itc.event.ops

import cats.Functor
import higherkindness.droste.data.Attr
import higherkindness.droste.data.Coattr
import higherkindness.droste.scheme
import higherkindness.droste.syntax.unfix.*
import higherkindness.droste.CVAlgebra
import higherkindness.droste.CVCoalgebra

import astrac.itc.event.*

object Leq:
  def apply(ev1: Event, ev2: Event): Boolean =
    scheme.zoo.chrono(algebra, coalgebra).apply((ev1.unfix, ev2.unfix))

  private val coalgebra: CVCoalgebra[LeqF, (EventF[Event], EventF[Event])] =
    CVCoalgebra {
      case (EventF.Leaf(ln), EventF.Leaf(rn))       => Result(ln <= rn)
      case (EventF.Leaf(ln), EventF.Node(rn, _, _)) => Result(ln <= rn)
      case (EventF.Node(ln, lel, ler), EventF.Leaf(rn)) =>
        And(
          Coattr.roll(Result(ln <= rn)),
          Coattr.pure((lel.unfix.treeLift(ln), EventF.Leaf(rn))),
          Coattr.pure((ler.unfix.treeLift(ln), EventF.Leaf(rn))),
        )
      case (EventF.Node(ln, lel, ler), EventF.Node(rn, rel, rer)) =>
        And(
          Coattr.roll(Result(ln <= rn)),
          Coattr.pure((lel.unfix.treeLift(ln), rel.unfix.treeLift(rn))),
          Coattr.pure((ler.unfix.treeLift(ln), rer.unfix.treeLift(rn))),
        )
    }

  private val algebra: CVAlgebra[LeqF, Boolean] = CVAlgebra {
    case Result(r)                               => r
    case And(Attr(a, _), Attr(b, _), Attr(c, _)) => a && b && c
  }

  sealed private trait LeqF[A]
  private case class Result[A](isLeq: Boolean) extends LeqF[A]
  private case class And[A](v: A, l: A, r: A) extends LeqF[A]

  private given Functor[LeqF] = new Functor[LeqF]:
    override def map[A, B](fa: LeqF[A])(f: A => B): LeqF[B] =
      fa match
        case Result(r)    => Result(r)
        case And(a, b, c) => And(f(a), f(b), f(c))

end Leq
