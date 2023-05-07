package astrac.itc.identity

import higherkindness.droste.data.Fix
import higherkindness.droste.syntax.fix.*
import higherkindness.droste.syntax.unfix.*

import astrac.itc.identity.ops.Parse

type Identity = Fix[IdentityF]

object Identity:
  val anonymous: Identity = leaf(0)

  def leaf(v: 0 | 1): Identity =
    (IdentityF.Leaf(v): IdentityF[Identity]).fix

  def node(l: Identity, r: Identity): Identity =
    (l.unfix, r.unfix) match
      case (IdentityF.Leaf(ln), IdentityF.Leaf(rn)) if (ln == rn) =>
        Identity.leaf(ln)
      case _ =>
        (IdentityF.Node(l, r): IdentityF[Identity]).fix

  def parse(string: String): Parse.Result[Identity] =
    Parse(string)

  def unsafeParse(string: String): Identity =
    Parse(string).toTry.get
