package astrac.itc.identity.ops

import cats.syntax.either.*
import org.parboiled2.*
import org.parboiled2.Parser.DeliveryScheme.Either

import astrac.itc.identity.Identity

object Parse:
  case class Error(id: String, inner: ParseError)
      extends Exception(
        s"Cannot parse identity `$id` - error: ${inner.toString}",
        inner,
      )

  type Result[A] = Either[Error, A]

  def apply(id: String): Result[Identity] =
    IdentityStringParser(id).InputLine.run().leftMap(Error(id, _))

private class IdentityStringParser(val input: ParserInput) extends Parser:
  def WS = rule(quiet(zeroOrMore(anyOf(" \t \n"))))

  def InputLine: Rule1[Identity] = rule(NodeOrLeaf ~ EOI)

  def NodeOrLeaf: Rule1[Identity] = rule(WS ~ (Node | Leaf) ~ WS)

  def Leaf: Rule1[Identity] = rule(
    ch('0') ~> (() => Identity.leaf(0)) |
      ch('1') ~> (() => Identity.leaf(1)),
  )

  def Node: Rule1[Identity] =
    rule('[' ~ NodeOrLeaf ~ ',' ~ NodeOrLeaf ~ ']' ~> Identity.node)
