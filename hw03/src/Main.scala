import Util._

object Main extends Homework03 {

  trait FWAEValue
  case class NumV(num: Int) extends FWAEValue
  case class Clov(params: List[String], body: FWAE, env: Map[String, FWAEValue]) extends FWAEValue
  case class RecV(rec: Map[String, FWAEValue]) extends FWAEValue

  def interp_clov(clov: FWAEValue): String = clov match {
    case NumV(num) => num.toString
    case Clov(params, body, env) => "function"
    case RecV(rec) => "record"
  }

  def interp(e: FWAE, env: Map[String, FWAEValue]): FWAEValue = e match {
    case Num(num) => NumV(num)
    case Add(left, right) => NumV(interp_clov(interp(left, env)).toInt + interp_clov(interp(right, env)).toInt)
    case Sub(left, right) => NumV(interp_clov(interp(left, env)).toInt - interp_clov(interp(right, env)).toInt)
    case With(name, value, body) => interp(body, env + (name -> interp(value, env)))
    case Id(name) => env.get(name) match {
      case Some(v) => {
        v
      }
      case None => error("free identifiers! : $name")
    }
    case App(func, args) => interp(func, env) match {
      case Clov(params, body, fenv) => {
        val map = (params zip args.map(arg => interp(arg, env))).toMap
        interp(body, fenv ++ map)
      }
      case v => error(s"not a closure: $v")
    }
    case Fun(params, body) => Clov(params, body, env)
    case Rec(rec) => RecV(rec map {case (key, value) => (key, interp(value, env))})
    case Acc(expr, name) => interp(expr, env) match {
      case RecV(rec) => rec.get(name) match {
        case Some(v) => v
        case None => error("no such field!")
      }
    }
    case _ => error("wrong arity")
  }

  def run(str: String): String = interp_clov(interp(FWAE(str), Map()))

  def ownTests: Unit = {
    test(run("{record {a 10} {b {+ 1 2}}}"), "record")
    test(run("{access {record {a 10} {b {+ 1 2}}} b}"), "3")
    test(run("{with {g {fun {r} {access r c}}} {g {record {a 0} {c 12} {b 7}}}}"), "12")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    test(run("{with {f {fun {a b} {+ a b}}} {with {g {fun {x} {- x 5}}} {with {x {f 2 5}} {g x}}}}"), "2")
    test(run("{with {f {fun {x y} {+ x y}}} {f 1 2}}"), "3")
    test(run("{with {f {fun {} 5}} {+ {f} {f}}}"), "10")
    test(run("{with {h {fun {x y z w} {+ x w}}} {h 1 4 5 6}}"), "7")
    test(run("{with {f {fun {} 4}} {with {g {fun {x} {+ x x}}} {with {x 10} {- {+ x {f}} {g 4}}}}}"), "6")
    test(run("{record {a 10} {b {+ 1 2}}}"), "record")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    test(run("{with {x 3} {with {y 5} {access {record {a x} {b y}} a}}}"), "3")
    test(run("{with {f {fun {a b} {+ {access a a} b}}} {with {g {fun {x} {+ 5 x}}} {with {x {f {record {a 10} {b 5}} 2}} {g x}}}}"), "17")
    test(run("{with {f {fun {a b c d e} {record {a a} {b b} {c c} {d d} {e e}}}} {access {f 1 2 3 4 5} c}}"), "3")
    test(run("{with {f {fun {a b c} {record {a a} {b b} {c c}}}} {access {f 1 2 3} b}}"), "2")
    test(run("{with {f {fun {a b c} {record {x a} {y b} {z c} {d 2} {e 3}}}} {access {f 1 2 3} y}}"), "2")
    test(run("{with {f {fun {a b c} {record {x a} {y b} {z c} {d 2} {e 3}}}} {access {f 1 2 3} d}}"), "2")
    test(run("{with {f {fun {x} {+ 5 x}}} {f {access {access {record {a {record {a 10} {b {- 5 2}}}} {b {access {record {x 50}} x}}} a} b}}}"), "8")
    test(run("{access {record {a 10} {b {+ 1 2}}} b}"), "3")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    test(run("{record {a 10}}"), "record")
    test(run("{access {record {a 10}} a}"), "10")
    test(run("{access {record {a {+ 1 2}}} a}"), "3")
    test(run("{fun {x} x}"), "function")
    test(run("{access {record {a {record {b 10}}}} a}"), "record")
    test(run("{access {access {record {a {record {a 10}}}} a} a}"), "10")
    test(run("{access {access {record {a {record {a 10} {b 20}}}} a} a}"), "10")
    test(run("{access {access {record {a {record {a 10} {b 20}}}} a} b}"), "20")
    test(run("{+ {access {record {a 10}} a} {access {record {a 20}} a}}"), "30")
    test(run("{+ {access {record {a 10}} a} {access {record {a 20}} a}}"), "30")
    test(run("{record {a 10}}"), "record")
    test(run("{record {a {- 2 1}}}"), "record")
    test(run("{access {record {a 10}} a}"), "10")
    test(run("{access {record {a {- 2 1}}} a}"), "1")
    test(run("{access {record {a {record {b 10}}}} a}"), "record")
    test(run("{access {access {record {a {record {a 10}}}} a} a}"), "10")
    test(run("{access {access {record {a {record {a 10} {b 20}}}} a} a}"), "10")
    test(run("{access {access {record {a {record {a 10} {b 20}}}} a} b}"), "20")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    test(run("{with {y {record {x 1} {y 2} {z 3}}} {access y y}}"), "2")
    test(run("{with {y {record {x 1} {y 2} {z 3}}} {access y z}}"), "3")
    test(run("{record {a 10} {b {+ 1 2}}}"), "record")
    test(run("{access {record {a 10} {b {+ 1 2}}} b}"), "3")
    test(run("{with {g {fun {r} {access r c}}} {g {record {a 0} {c 12} {b 7}}}}"), "12")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
  }
}
