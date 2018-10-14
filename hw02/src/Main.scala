import Util._

object Main extends Homework02 {

  def interp(muwae: MUWAE, env: Map[String, List[Int]]): List[Int] = muwae match {
    case Num(nums) => nums
    case Add(left, right) => binOp((a, b) => a + b, interp(left, env), interp(right, env))
    case Sub(left, right) => binOp((a, b) => a - b, interp(left, env), interp(right, env))
    case With(name, expr, body) => interp(body, env + (name -> interp(expr, env)))
    case Id(id) => env.get(id) match {
      case Some(v) => v
      case None => error("free identifiers! : $id")
    }
    case Min(left, mid, right) => List( (interp(left, env) ++ interp(mid, env) ++ interp(right, env)) .min )
    case Max(left, mid, right) => List( (interp(left, env) ++ interp(mid, env) ++ interp(right, env)) .max )
  }


  def binOp(
    op: (Int, Int) => Int,
    ls: List[Int],
    rs: List[Int]
  ): List[Int] = ls match {
    case Nil => Nil
    case l :: rest =>
      def f(r: Int): Int = op(l, r)
      rs.map(f) ++ binOp(op, rest, rs)
  }

  def run(str: String): List[Int] = interp(MUWAE(str), Map())

  def ownTests: Unit = {
    // tests for free-ids
    test(run("{+ {1 2} {3 4}}"), List(4, 5, 5, 6))
    test(run("{- {+ {1 2} {3 4}} {1 2}}"), List(3, 2, 4, 3, 4, 3, 5, 4))
    test(run("{- {10 2 1} {3 2}}"), List(7, 8, -1, 0, -2, -1))
    test(run("{with {x {1 2}} {+ x {4 3}}}"), List(5, 4, 6, 5))
    test(run("{with {x 9} {+ x {with {x 3} x}}}"), List(12))
    test(run("{with {x 100} {+ x {with {y 3} x}}}"), List(200))
    test(run("{with {x 5} {+ x {with {x 3} 10}}}"), List(15))
    test(run("{with {x {7 5}} {+ x x}}"), List(14, 12, 12, 10))
    test(run("{with {x {1 2}} {+ x {4 3}}}"), List(5, 4, 6, 5))
    test(run("{with {x 2} {- {+ x x} x}}"), List(2))
    test(run("{+ {muwae-min 3 5 7} {muwae-min 10 100 1000}}"), List(13))
    test(run("{+ {muwae-min 9 3 7} {muwae-max 6 2 20}}"), List(23))
    test(run("{with {x 10} {muwae-max x 2 3}}"), List(10))
    test(run("{with {x 20} {with {y 5} {with {z {10 20}} {+ z {muwae-max {+ x y} 0 12}}}}}"), List(35, 45))
    test(run("{with {x 20} {with {y 5} {with {z {10 20}} {+ z {muwae-min {+ x y} 0 12}}}}}"), List(10, 20))
    test(run("{with {x {muwae-min 3 9 5}} {with {y {- x 3}} y}}"), List(0))
    test(run("{with {x {muwae-max 2 3 5}} {muwae-min x 7 6}}"), List(5))
    test(run("{with {x {muwae-max 9 7 10}} {muwae-max 8 x {+ 1 x}}}"), List(11))
    test(run("{- {muwae-min 6 4 5} {muwae-max 2 3 4}}"), List(0))
    test(run("{with {x {+ 7 2}} {muwae-min x 7 0}}"), List(0))
    test(run("{+ {muwae-min 9 3 7} {muwae-max 6 2 20}}"), List(23))
    test(run("{with {x {13}} {muwae-min x 1 12}}"), List(1))
    test(run("{with {x {muwae-min 2 1 3}} {+ x x}}"), List(2))
    test(run("{with {a 10} {with {b 19} {with {c 2} {muwae-min a b c}}}}"), List(2))
    test(run("{with {x 3} {muwae-max 3 4 {+ x x}}}"), List(6))
    test(run("{with {a 10} {with {b 19} {with {c 2} {muwae-max a b c}}}}"), List(19))
    test(run("{with {x {muwae-min 2 5 4}} {+ x x}}"), List(4))
    test(run("{with {x {muwae-max 2 5 4}} {+ x x}}"), List(10))
    test(run("{with {x {- 11 3}} {muwae-max x {+ x x} {- x x}}}"), List(16))
    test(run("{with {x {- 11 3}} {muwae-min x {+ x x} {- x x}}}"), List(0))
    test(run("{muwae-min {+ 4 4} {with {x 5} {+ x {with {x 3} 10}}} 3}"), List(3))
    test(run("{muwae-max {+ 4 4} {with {x 5} {+ x {with {x 3} 10}}} 3}"), List(15))
    test(run("{with {x {13}} {muwae-min x 1 12}}"), List(1))
    test(run("{with {x {10} } {muwae-max x 2 3}}"), List(10))
    test(run("{with {x {muwae-min 2 1 3}} {+ x x}}"), List(2))
    test(run("{with {x {muwae-max 2 1 3}} {+ x x}}"), List(6))
    test(run("{with {x 2} {muwae-min x 3 10}}"), List(2))
    test(run("{with {x 2} {muwae-max x 3 10}}"), List(10))
    test(run("{muwae-min {+ 4 4} 2 3} "), List(2))
    test(run("{muwae-max {+ 4 4} 2 3} "), List(8))
    test(run("{with {x 10} {muwae-min x 2 3}}"), List(2))
    test(run("{with {x 10} {muwae-max x 2 3}}"), List(10))
    test(run("{with {x {10}} {muwae-max x 2 3}}"), List(10))
    test(run("{muwae-min {+ 3 4} 5 6}"), List(5))
    test(run("{muwae-max {+ 3 4} 5 6}"), List(7))
    test(run("{with {x {10}} {muwae-min x {3} {5}}}"), List(3))
    test(run("{with {x {10}} {muwae-max x {3} {5}}}"), List(10))
    test(run("{muwae-min {3} 4 5}"), List(3))
    test(run("{muwae-max {3} 4 {5}}"), List(5))
    test(run("{+ {10 100 1000 10000} {muwae-min {- 3 4} 5 6}}"), List(9, 99, 999, 9999))
  }
}
