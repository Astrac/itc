package astrac.itc.simulator

import cats.syntax.all.*
import munit.ScalaCheckSuite

import astrac.itc.Stamp

object SimulationSpec:
  val simulation = for
    node1 <- Network.withInitialNode()
    _ <- node1.event()
    node2 <- node1.fork()
    node3 <- node1.fork()
    _ <- node2.event()
    _ <- node1.send(node3)
    _ <- node3.send(node2)
    _ <- node3.event()
    node4 <- node2.fork()
    node5 <- node3.fork()
    _ <- node5.send(node1)
    _ <- node1.event()
    _ <- node5.event()
    _ <- node3.send(node5)
    _ <- node5.send(node2)
    _ <- node2.event()
    _ <- node2.send(node5)
    _ <- node1.event()
    _ <- node3.send(node4)
    _ <- node2.send(node1)
    _ <- node1.send(node4)
    _ <- node2.send(node4)
  yield ()

class SimulatorSpec extends ScalaCheckSuite:

  test("Foo") {
    val res = SimulationSpec.simulation.runS(EmptyNetwork).value
    println(res)
  }
