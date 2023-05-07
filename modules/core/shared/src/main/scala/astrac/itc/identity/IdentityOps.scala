package astrac.itc.identity

import astrac.itc.identity.ops.*

trait IdentityOps:
  extension (id: Identity)
    def prod(other: Identity): Identity = Prod(id, other)
    def split: (Identity, Identity) = Split(id)
    def stringify: IdentityString = Stringify(id)
    def sum(other: Identity): Identity = Sum(id, other)
