// Concrete Syntax
<LC> ::= <id>
      | { <LC> <LC> }
      | { fun {<id>} <LC> }

// Abstract Syntax
trait FAE
case class Num(n: Int) extends FAE
case class Add(l: FAE, r: FAE) extends FAE
case class Sub(l: FAE, r: FAE) extends FAE
case class Id(x: Stirng) extends FAE
case class Fun(x: String, b: FAE) extends FAE
case class App(f: FAE, a: FAE) extends FAE

trait FAEValue
case class NumV(n: Int) extends FAEValue
case class CloV(param: String, body: FAE, env: Env) extends FAEValue

type Env = Map[String, FAEValue]

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

def interp(e: FAE, env: Env): Int = e match {
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

e ::= n	            Num(n)
  | e + e           Add(l, r)
  | e - e           Sub(l, r)
  | x               Id(x)
  | λx.e           Fun(x, b)
  | e e             App(f, a)

σ ㅏ e => v
σ ㅏ n => n

σ ㅏ e1 => n1 || σ ㅏ e2 => n2
--------------------------------------
σ ㅏ e1 + e2 => n1 + n2

σ ㅏ e1 - e2는 똑같이.

x <- Domain(σ)
----------------------
σ ㅏ x => σ(x)

σ ㅏ λx.e => <λx.e, σ>

σ ㅏ e1 => <λx.e, σ`> || σ ㅏ ex => v2
|| σ`[x -> v2] ㅏ e => v
----------------------------------------------------
σ ㅏ e1 e2 => v