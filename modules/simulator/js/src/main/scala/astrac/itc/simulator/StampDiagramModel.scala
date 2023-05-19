package astrac.itc.simulator

import higherkindness.droste.scheme

import astrac.itc.Stamp

opaque type StampX = Double
object StampX:
  def apply(value: Double): StampX =
    assert(value >= 0 && value <= 1)
    value

case class Point(x: StampX, h: Long)

case class Interval[A](from: StampX, to: StampX, value: A)

case class IdentityDiagramModel(blocks: Vector[Interval[0 | 1]])

case class EventDiagramModel()

// case class Poly(points: List[Point])
//
// case class IdentitySegment(polygons: List[Poly])
//
// case class StampDiagramModel(identitySegments: Vector[IdentitySegment])
//
// object StampDiagramModel:
//   def apply(stamp: Stamp): StampDiagramModel =
//     StampDiagramModel(scheme.cata())
