// Concrete Syntax
<FWAE> ::= <num>
      | { + <FWAE> <FWAE>}
      | { - <FWAE> <FWAE>}
      | <id>
      | { <FWAE> <FWAE> }
      | { fun <id> <FWAE> }

// Abstract Syntax
trait FWAE
case class Num(n: Int) extends FWAE
case class Add(l: FWAE, r: FWAE) extends FWAE
case class Sub(l: FWAE, r: FWAE) extends FWAE
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
  case Id(x) => lookup(x, env)
  case FunDef(x, b) => CloV(x, b, env)
  case App(f, a) => interp(f, env) match {
    case CloV(x, b, fenv) => interp(b, fenv + (x -> interp(a, env)))
    case v => error(s"not a closure: $v")
  }
}

// Operational Semantics

e ::= name	        Num(n)
  | e + e           Add(l, r)
  | e - e           Sub(l, r)
  | val x = e in e  With(x, i, b)
  | x               Id(x)

sigma ㅏ e => v
sigma ㅏ n => n

sigma ㅏ e1 => n1 || sigma ㅏ e2 => n2
--------------------------------------
sigma ㅏ e1 + e2 => n1 + n2

sigma ㅏ e1 - e2는 똑같이.

x <- Domain(sigma)
----------------------
sigma ㅏ x => sigma(x)

sigma ㅏ e1 => v1 || sigma[x -> v1] ㅏ e2 => v2
-------------------------------
sigma ㅏ val x = e1 in e2 => v2