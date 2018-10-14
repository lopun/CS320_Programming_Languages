// Concrete Syntax
<FWAE> ::= <num>
      | { + <FWAE> <FWAE>}
      | { - <FWAE> <FWAE>}
      | { with { <id> <FWAE> } <FWAE> }
      | <id>
      | { <FWAE> <FWAE> }
      | { fun <id> <FWAE> }

// Abstract Syntax
trait FWAE
case class Num(n: Int) extends FWAE
case class Add(l: FWAE, r: FWAE) extends FWAE
case class Sub(l: FWAE, r: FWAE) extends FWAE
case class With(x: String, i: FWAE, b: FWAE) extends FWAE
case class Id(x: Stirng) extends FWAE
case class Fun(x: String, b: FWAE) extends FWAE
case class App(f: FWAE, a: FWAE) extends FWAE

trait FWAEValue
case class NumV(n: Int) extends FWAEValue
case class CloV(param: String, body: FWAE, env: Env) extends FWAEValue

type Env = Map[String, FWAEValue]

// Scala Implementation
def lookup(name: String, env: Env): Int = {
  env.get(name) match {
    case Some(v) => v
    case None => error(s"free identifier: $name")
  }
}

def lookupFD(name: String: FDs): FunDef = fs match {
  case Nil => error(s"lookupFD: unknown function: $name")
  case h :: t =>
    if (h.f == name) h
    else lookupFD(name, t)
}

def interp(e: FWAE, env: Env): Int = e match {
  case Num(n) => NumV(n)
  case Add(l, r) => numVAdd(interp(l, env) + interp(r, env))
  case Sub(l, r) => numVSub(interp(l, env) - interp(r, env))
  case With(x, i, b) => interp(b, env + (x -> interp(i, env)))
  case Id(x) => lookup(x, env)
  case FunDef(x, b) => CloV(x, b, env)
  case App(f, a) => interp(f, env) match {
    case CloV(x, b, fenv) => interp(b, fenv + (x -> interp(a, env)))
    case v => error(s"not a closure: $v")
  }
}