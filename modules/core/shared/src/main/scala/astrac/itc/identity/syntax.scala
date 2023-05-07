package astrac.itc.identity

import cats.syntax.either.*
import org.typelevel.literally.Literally

object syntax:
  extension (inline ctx: StringContext)
    inline def itcId(inline args: Any*): Identity =
      ${ IdentityLiteral('ctx, 'args) }

  object IdentityLiteral extends Literally[Identity]:
    def validate(s: String)(using Quotes) =
      Identity
        .parse(s)
        .bimap(_.getMessage(), _ => '{ Identity.unsafeParse(${ Expr(s) }) })
