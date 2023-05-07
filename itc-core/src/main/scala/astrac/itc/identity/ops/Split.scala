package astrac.itc.identity.ops

import higherkindness.droste.data.prelude.*
import higherkindness.droste.data.Attr
import higherkindness.droste.scheme
import higherkindness.droste.CVAlgebra

import astrac.itc.identity.*

import Identity.leaf
import Identity.node

object Split:
  def apply(id: Identity): (Identity, Identity) =
    scheme.zoo.histo(algebra).apply(id)

  private val algebra: CVAlgebra[IdentityF, (Identity, Identity)] =
    CVAlgebra {
      case IdentityF.Leaf(0) =>
        (leaf(0), leaf(0))

      case IdentityF.Leaf(1) =>
        (node(leaf(1), leaf(0)), node(leaf(0), leaf(1)))

      case IdentityF.Node(Attr(_, IdentityF.Leaf(0)), ir) =>
        val (rl, rr) = ir.head
        (node(leaf(0), rl), node(leaf(0), rr))

      case IdentityF.Node(il, IdentityF.Leaf(0)) =>
        val (ll, lr) = il.head
        (node(ll, leaf(0)), node(lr, leaf(0)))

      case IdentityF.Node(il, ir) =>
        val (ll, lr) = il.head
        val (rl, rr) = ir.head
        (node(ll, rl), node(lr, rr))
    }
