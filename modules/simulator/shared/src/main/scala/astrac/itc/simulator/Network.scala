package astrac.itc.simulator

import cats.data.IndexedState
import cats.data.NonEmptyMap
import cats.data.State
import cats.syntax.functor.*

import astrac.itc.Stamp

case object EmptyNetwork
case class Network(nodes: NonEmptyMap[NodeId, Node]):
  def modifyNodes(
      f: NonEmptyMap[NodeId, Node] => NonEmptyMap[NodeId, Node],
  ): Network =
    copy(nodes = f(nodes))

case class Node(
    nodeId: NodeId,
    stamp: Stamp,
    ownEvents: Vector[Event] = Vector.empty,
    knownEvents: Map[NodeId, List[Event]] = Map.empty,
):
  def event(eventId: Event.Id = Event.Id.random()): State[Network, Unit] =
    State.modify(_.modifyNodes(_.updateWith(nodeId) { node =>
      val event = Event(eventId, stamp, nodeId)
      node.copy(
        stamp = node.stamp.newEvent,
        ownEvents = node.ownEvents :+ event,
      )
    }))

  def fork(newNodeId: NodeId = NodeId.random()): State[Network, Node] =
    val (thisNodeStamp, newNodeStamp) = stamp.fork
    val newNode = Node(newNodeId, newNodeStamp)
    State
      .modify[Network](_.modifyNodes { nodes =>
        nodes
          .updateWith(nodeId)(_.copy(stamp = thisNodeStamp))
          .add(newNodeId -> newNode)
      })
      .as(newNode)

  def send(other: Node): State[Network, Unit] =
    State.modify(_.modifyNodes { nodes =>
      val (newStamp, sentStamp) = stamp.send
      nodes
        .updateWith(nodeId)(_.copy(stamp = newStamp))
        .updateWith(other.nodeId)(n =>
          n.copy(
            stamp = n.stamp.receive(sentStamp),
            knownEvents = mergeKnownEvents(n.knownEvents, knownEvents),
          ),
        )
    })

  extension [A](l: List[A])
    def singleOrFail = l match
      case x :: Nil => x
      case _        => sys.error("Expected single element in list")

  // This is wrong for several reasons
  private def mergeKnownEvents(
      e1: Map[NodeId, List[Event]],
      e2: Map[NodeId, List[Event]],
  ): Map[NodeId, List[Event]] =
    val keys = e1.keySet ++ e2.keySet
    keys.map { id =>
      val l1 = e1.getOrElse(id, Nil)
      val l2 = e2.getOrElse(id, Nil)

      id -> (l1 ++ l2)
        .groupBy(_.id)
        .view
        .mapValues(_.distinct.singleOrFail)
        .values
        .toList
    }.toMap

end Node

object Network:

  def withInitialNode(
      id: NodeId = NodeId.random(),
  ): IndexedState[EmptyNetwork.type, Network, Node] =
    val node = Node(id, Stamp.genesis)
    IndexedState(_ => (Network(NonEmptyMap.of(id -> node)), node))
