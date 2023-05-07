package astrac.itc.event

import higherkindness.droste.data.Fix
import higherkindness.droste.syntax.fix.*
import higherkindness.droste.syntax.unfix.*

import astrac.itc.event.ops.*
import astrac.itc.identity.Identity

type Event = Fix[EventF]

object Event:
  val zeroLeaf: Event = leaf(0)

  def leaf(v: Long): Event = (EventF.Leaf(v): NormalizedEventF[Event]).fix

  def node(h: Long, l: Event, r: Event): Event =
    val minLR = math.min(l.min, r.min)
    val normH = h + minLR
    val normL = l.treeSink(minLR)
    val normR = r.treeSink(minLR)

    if normL == zeroLeaf && normR == zeroLeaf then leaf(normH)
    else (EventF.Node(normH, normL, normR): EventF[Event]).fix

  def parse(string: String): Parse.Result[Event] =
    Parse(string)

  def unsafeParse(string: String): Event =
    Parse(string).toTry.get
