package astrac.itc.identity

import cats.syntax.apply.*
import cats.Applicative
import cats.Show
import cats.Traverse
import higherkindness.droste.syntax.fix.*
import higherkindness.droste.util.DefaultTraverse

enum IdentityF[A]:
  case Leaf[A] private[identity] (v: 0 | 1) extends IdentityF[A]
  case Node[A] private[identity] (left: A, right: A) extends IdentityF[A]

object IdentityF:
  given IdentityOps: IdentityOps()

  given Show[IdentityF[Identity]] = Show.show(_.fix.stringify.asString)

  given Traverse[IdentityF] =
    new DefaultTraverse[IdentityF]:
      def traverse[G[_]: Applicative, A, B](
          fa: IdentityF[A],
      )(f: A => G[B]): G[IdentityF[B]] =
        fa match
          case IdentityF.Leaf(v) =>
            Applicative[G].pure(IdentityF.Leaf(v))
          case IdentityF.Node(l, r) =>
            (f(l), f(r)).mapN(IdentityF.Node(_, _))
