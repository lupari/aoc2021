package lib

import scala.annotation.tailrec

object Parsers:
  type Op = Char

  object AST:
    trait Node
    case class Value(v: Int)                         extends Node
    case class Tree(op: Op, left: Node, right: Node) extends Node

    def eval(node: Node): Long = node match
      case Tree(op, l, r) =>
        op match
          case '+' => eval(l) + eval(r)
          case '-' => eval(l) - eval(r)
          case '*' => eval(l) * eval(r)
          case '/' => eval(l) / eval(r)
      case Value(op) => op.toLong

    def parse(expr: String)(precedence: (Op, Op) => Boolean): Node =

      def compose(nodes: List[Node], ops: List[Op]): List[Node] =
        ops.foldLeft(nodes)((ns, op) => Tree(op, ns.tail.head, ns.head) +: ns.tail.tail)

      @tailrec
      def helper(xs: List[Char], ops: List[Op], acc: List[Node]): Node = xs match
        case Nil => compose(acc, ops).head
        case h :: t =>
          h match
            case n if n.isDigit => helper(t, ops, Value(n.asDigit) +: acc)
            case '('            => helper(t, h +: ops, acc)
            case ')' =>
              val (out, ops2) = ops.span(_ != '(')
              helper(t, ops2.tail, compose(acc, out))
            case _ =>
              val (out, ops2) = ops.span(o => o != '(' && precedence(o, h))
              helper(t, h +: ops2, compose(acc, out))

      helper(expr.filterNot(_ == ' ').toList, Nil, Nil)

  object Postfix:

    def eval(expr: List[Char]): Long =

      @tailrec
      def helper(xs: List[Char], stack: List[Long]): Long = xs match
        case Nil => stack.head
        case h :: t =>
          h match
            case n if n.isDigit => helper(t, n.asDigit.toLong +: stack)
            case _ =>
              val (right, left) = (stack.head, stack.tail.head)
              val v = h match
                case '+' => left + right
                case '-' => left - right
                case '*' => left * right
                case '/' => left / right
              helper(t, v +: stack.drop(2))

      helper(expr, Nil)

    def parse(expr: String)(precedence: (Op, Op) => Boolean): List[Char] =

      @tailrec
      def helper(xs: List[Char], ops: List[Op], acc: List[Char]): List[Char] = xs match
        case Nil => acc ++ ops
        case h :: t =>
          h match
            case n if n.isDigit => helper(t, ops, acc :+ n)
            case '('            => helper(t, h +: ops, acc)
            case ')' =>
              val (out, ops2) = ops.span(_ != '(')
              helper(t, ops2.tail, acc ++ out)
            case _ =>
              val (out, ops2) = ops.span(o => o != '(' && precedence(o, h))
              helper(t, h +: ops2, acc ++ out)

      helper(expr.filterNot(_ == ' ').toList, Nil, Nil)
