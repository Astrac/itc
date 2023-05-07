package astrac.itc.identity.ops

import higherkindness.droste.scheme
import higherkindness.droste.Algebra

import astrac.itc.identity.*

type IdentityString = Stringify.IdentityString

object Stringify:

  opaque type IdentityString = String
  extension (s: IdentityString) inline def asString: String = s

  def apply(id: Identity): IdentityString =
    scheme.cata(algebra).apply(id)

  def algebra = Algebra[IdentityF, String] {
    case IdentityF.Leaf(v)    => v.toString()
    case IdentityF.Node(l, r) => s"[$l, $r]"
  }
