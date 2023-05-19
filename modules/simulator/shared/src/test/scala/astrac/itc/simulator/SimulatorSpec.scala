package astrac.itc.simulator

import cats.syntax.all.*
import munit.ScalaCheckSuite

import astrac.itc.Stamp

class SimulatorSpec extends ScalaCheckSuite:

  val simulation = for
    node1 <- Network.withInitialNode(NodeId("N1"))
    _ <- node1.event()
    node2 <- node1.fork(NodeId("N2"))
    _ <- node2.event()
  yield (node1, node2)

  test("Foo") {
    val (net, (node1, node2)) = simulation.run(EmptyNetwork).value
    println()
    println(net.nodes(node1.nodeId))
    println(net.nodes(node2.nodeId))
    println()
    val events = net.nodes(node2.nodeId).get.knownEvents.toList.flatMap(_._2)
    println(net)
    val ev1 = events.head
    val tree = events.tail.foldLeft(CausalityTree.root(ev1))((t, e) =>
      CausalityTree.add(e, t),
    )

    println(tree)
  }

object SimulationSpec:
// val simulation = for
//   node1 <- Network.withInitialNode(NodeId("N1"))
//   _ <- node1.event()
//   node2 <- node1.fork(NodeId("N2"))
//   node3 <- node1.fork(NodeId("N3"))
//   _ <- node2.event()
//   _ <- node1.send(node3)
//   _ <- node3.send(node2)
//   _ <- node3.event()
//   node4 <- node2.fork(NodeId("N4"))
//   node5 <- node3.fork(NodeId("N5"))
//   _ <- node5.send(node1)
//   _ <- node1.event()
//   _ <- node5.event()
//   _ <- node3.send(node5)
//   _ <- node5.send(node2)
//   _ <- node2.event()
//   _ <- node2.send(node5)
//   _ <- node1.event()
//   _ <- node3.send(node4)
//   _ <- node2.send(node1)
//   _ <- node1.send(node4)
//   _ <- node2.send(node4)
// yield (node4)

end SimulationSpec
