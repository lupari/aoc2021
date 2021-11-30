package lib

import scala.annotation.tailrec
import scala.collection.mutable

object Graphs {

  object DFS {
    def apply[A](start: A)(nf: A => Iterable[A]): Iterable[A] = {
      def _dfs(s: A, seen: Iterable[A]): Iterable[A] = {
        if (seen.iterator.contains(s)) seen
        else {
          val neighbors = nf(s).filterNot(seen.iterator.contains)
          neighbors.foldLeft(Iterable(s) ++ seen)((b, a) => _dfs(a, b))
        }
      }

      _dfs(start, Nil)
    }
  }

  object BFS {
    def apply[A](start: A)(nf: A => Iterable[A]): Map[A, Int] = {
      @tailrec
      def _bfs(seen: Map[A, Int], unseen: Map[A, Int]): Map[A, Int] = {
        val neighbors = for { (node, cost) <- unseen; newNode <- nf(node) } yield
          newNode -> (cost + 1)
        val seen2   = seen ++ unseen
        val unseen2 = neighbors.filterNot(n => seen.contains(n._1))
        if (unseen2.isEmpty) seen2 else _bfs(seen2, unseen2)
      }

      _bfs(Map.empty, Map(start -> 0))
    }
  }

  object AStar {
    def apply[A](start: A, goal: A)(cf: (A, A) => Int)(nf: A => Iterable[A]): Map[A, Int] = {
      val open  = mutable.ListBuffer[(A, Int)]((start, 0))
      val costs = mutable.Map[A, Int](start -> 0)
      while (open.nonEmpty) {
        val current = open.minBy(_._2)
        open -= current
        if (current._1 == goal) open.clear()
        else {
          val newCost   = costs(current._1) + 1
          val neighbors = nf(current._1)
          for (next <- neighbors) {
            if (!costs.contains(next) || newCost < costs(next)) {
              costs(next) = newCost
              val priority = newCost + cf(goal, next)
              open += ((next, priority))
            }
          }
        }
      }
      costs.toMap
    }
  }

}
