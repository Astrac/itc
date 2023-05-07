package astrac.itc.event

import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.Applicative
import cats.Show
import cats.Traverse
import higherkindness.droste.syntax.fix.*
import higherkindness.droste.util.DefaultTraverse

enum EventF[A]:
  case Leaf[A](value: Long) extends EventF[A]
  case Node[A](height: Long, left: A, right: A) extends EventF[A]

object EventF:
  given EventOps: EventOps()

  given Show[EventF[Event]] = Show.show(_.fix.stringify.asString)

  given Traverse[EventF] = new DefaultTraverse[EventF]:
    override def traverse[G[_]: Applicative, A, B](fa: EventF[A])(
        f: A => G[B],
    ): G[EventF[B]] =
      fa match
        case EventF.Leaf(v) =>
          EventF.Leaf(v).pure
        case EventF.Node(h, l, r) =>
          (f(l), f(r)).mapN((gl, gr) => EventF.Node(h, gl, gr))

type NormalizedEventF[A] = EventF[A]
