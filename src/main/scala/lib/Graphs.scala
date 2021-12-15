package lib

import scala.annotation.tailrec
import scala.collection.mutable

object Graphs:
  object dfs:
    def apply[A](start: A)(nf: A => Iterable[A]): Iterable[A] =
      def _dfs(s: A, seen: Iterable[A]): Iterable[A] =
        if seen.iterator.contains(s) then seen
        else
          val neighbors = nf(s).filterNot(seen.iterator.contains)
          neighbors.foldLeft(Iterable(s) ++ seen)((b, a) => _dfs(a, b))

      _dfs(start, Nil)

  object bfs:
    def apply[A](start: A)(nf: A => Iterable[A]): Map[A, Int] =
      @tailrec
      def _bfs(seen: Map[A, Int], unseen: Map[A, Int]): Map[A, Int] =
        val neighbors = for {
          (node, cost) <- unseen; newNode <- nf(node)
        } yield newNode -> (cost + 1)
        val seen2   = seen ++ unseen
        val unseen2 = neighbors.filterNot(n => seen.contains(n._1))
        if unseen2.isEmpty then seen2 else _bfs(seen2, unseen2)

      _bfs(Map.empty, Map(start -> 0))

  object aStar:
    def apply[A](start: A, goal: A)(cf: (A, A) => Int)(nf: A => Iterable[A])(
        hf: A => Int
    ): (Map[A, Int], Option[(A, Int)]) =
      val seen: mutable.Map[A, Int] = mutable.Map.empty
      val unseen: mutable.PriorityQueue[(Int, Int, A)] =
        mutable.PriorityQueue.empty(Ordering.by(-_._1))
      unseen.enqueue((hf(start), 0, start))
      while unseen.nonEmpty do
        val (_, dist, node) = unseen.dequeue()
        if !seen.contains(node) then
          seen(node) = dist
          if node == goal then return (seen.toMap, Some(node -> dist))
          else
            def visit(n: A, d: Int) =
              if !seen.contains(n) then unseen.enqueue((dist + d + hf(n), dist + d, n))
            nf(node).map(n => (n, cf(node, n))).foreach(n => visit(n._1, n._2))

      (seen.toMap, None)

  object dijkstra:
    def apply[A](start: A, goal: A)(nf: A => Iterable[A])(
        cf: (A, A) => Int
    ): (Map[A, Int], Option[(A, Int)]) =
      val seen: mutable.Map[A, Int]               = mutable.Map.empty
      val unseen: mutable.PriorityQueue[(Int, A)] = mutable.PriorityQueue.empty(Ordering.by(-_._1))
      unseen.enqueue((0, start))
      while unseen.nonEmpty do
        val (dist, node) = unseen.dequeue()
        if !seen.contains(node) then
          seen(node) = dist
          if node == goal then return (seen.toMap, Some(node -> dist))
          else
            def visit(n: A, d: Int) =
              if !seen.contains(n) then unseen.enqueue((dist + d, n))
            nf(node).map(n => (n, cf(node, n))).foreach(n => visit(n._1, n._2))

      (seen.toMap, None)
