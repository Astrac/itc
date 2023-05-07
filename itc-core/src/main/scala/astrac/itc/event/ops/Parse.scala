package astrac.itc.event.ops

import cats.syntax.either.*
import org.parboiled2.*
import org.parboiled2.Parser.DeliveryScheme.Either

import astrac.itc.event.Event

object Parse:
  type Result[A] = Either[Error, A]

  def apply(id: String): Result[Event] =
    EventStringParser(id).InputLine.run().leftMap(Error(id, _))

  case class Error(id: String, inner: ParseError)
      extends Exception(
        s"Cannot parse Event `$id` - error: ${inner.getMessage()}",
        inner,
      )

private class EventStringParser(val input: ParserInput) extends Parser:
  def WS = rule(quiet(zeroOrMore(anyOf(" \t \n"))))

  def InputLine: Rule1[Event] = rule(NodeOrLeaf ~ EOI)

  def NodeOrLeaf: Rule1[Event] = rule(WS ~ (Node | Leaf) ~ WS)

  def Digits: Rule1[Long] =
    rule(capture(oneOrMore(CharPredicate.Digit)) ~> (_.toLong))

  def Leaf: Rule1[Event] = rule(Digits ~> Event.leaf)

  def Node: Rule1[Event] =
    rule('[' ~ Digits ~ ',' ~ NodeOrLeaf ~ ',' ~ NodeOrLeaf ~ ']' ~> Event.node)
