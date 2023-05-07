package astrac.itc.internal

import cats.syntax.functor.*
import cats.Monad
import cats.Traverse
import higherkindness.droste.*

private[itc] object RecSchemes:
  def zygoM[M[_]: Monad, F[_]: Traverse, R, A, B](
      algebra: Algebra[F, A],
      ralgebra: RAlgebraM[A, M, F, B],
  )(using project: Project[F, R]): R => M[B] =
    kernel.hyloM[M, F, R, (A, B)](
      fab =>
        ralgebra
          .run(fab)
          .map(
            algebra
              .run(fab.map(_._1)) -> _,
          ),
      project.coalgebra.run.andThen(Monad[M].pure(_)),
    ) andThen (_.map(_._2))
