package astrac.itc.event

import cats.syntax.either.*
import org.typelevel.literally.Literally

object syntax:
  extension (inline ctx: StringContext)
    inline def itcEv(inline args: Any*): Event =
      ${ EventLiteral('ctx, 'args) }

  object EventLiteral extends Literally[Event]:
    def validate(s: String)(using Quotes) =
      Event
        .parse(s)
        .bimap(_.getMessage(), _ => '{ Event.unsafeParse(${ Expr(s) }) })
