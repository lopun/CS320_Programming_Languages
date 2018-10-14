// Concrete Syntax
<RCFAE> ::= <num>
      | { + <RCFAE> <RCFAE>}
      | { - <RCFAE> <RCFAE>}
      | <id>
      | { <RCFAE> <RCFAE> }
      | { fun {<id>} <RCFAE> }
      | {if0 <RCFAE> <RCFAE> <RCFAE>}
      | {recfun {<id> <id>} <RCFAE>}

// Abstract Syntax
trait RCFAE
case class Num(n: Int) extends RCFAE
case class Add(l: RCFAE, r: RCFAE) extends RCFAE
case class Sub(l: RCFAE, r: RCFAE) extends RCFAE
case class Id(x: Stirng) extends RCFAE
case class Fun(x: String, b: RCFAE) extends RCFAE
case class App(f: RCFAE, a: RCFAE) extends RCFAE
case class If0(c: RCFAE, t: RCFAE,  t: RCFAE) extends RCFAE
case class Rec(f: String, x: String, b: RCFAE) extends RCFAE

trait RCFAEValue
case class NumV(n: Int) extends RCFAEValue
case class CloV(param: String, body: RCFAE, var env: Env) extends RCFAEValue

type Env = Map[String, RCFAEValue]

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

def interp(e: RCFAE, env: Env): Int = e match {
  case Num(n) => NumV(n)
  case Add(l, r) => numVAdd(interp(l, env) + interp(r, env))
  case Sub(l, r) => numVSub(interp(l, env) - interp(r, env))
  case Id(x) => lookup(x, env)
  case FunDef(x, b) => CloV(x, b, env)
  case App(f, a) => interp(f, env) match {
    case CloV(x, b, fenv) => interp(b, fenv + (x -> interp(a, env)))
    case v => error(s"not a closure: $v")
  }
  case If0(c, t, f) => interp(c, env) match {
    case NumV(0) => interp(t, env)
    case _ => interp(f, env)
  }
  case Rec(f, x, b) => {
    val cloV = CloV(x, b, env)
    cloV.env = env + (f -> cloV)
    cloV
  }
}

// Operational Semantics

e ::= n	            Num(n)
  | e + e           Add(l, r)
  | e - e           Sub(l, r)
  | x               Id(x)
  | ?x.e           Fun(x, b)
  | e e             App(f, a)
  | if0 e e e       If0(c, t, f)
  | uf.?x.e        Rec(f, x, b)
