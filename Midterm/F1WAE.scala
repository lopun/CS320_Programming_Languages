// Concrete Syntax
<FunDef> ::= { deffun { <id> <id> } <F1WAE> }

<F1WAE> ::= <num>
      | { + <F1WAE> <F1WAE>}
      | { - <F1WAE> <F1WAE>}
      | { with { <id> <F1WAE> } <F1WAE> }
      | <id>
      | { <id> <F1WAE> }

// Abstract Syntax
trait FunDef
case class FunDef(f: String, x: Stirng, b: F1WAE)

trait F1WAE
case class Num(n: Int) extends F1WAE
case class Add(l: F1WAE, r: F1WAE) extends F1WAE
case class Sub(l: F1WAE, r: F1WAE) extends F1WAE
case class With(x: String, i: F1WAE, b: F1WAE) extends F1WAE
case class Id(x: Stirng) extends F1WAE
case class App(f: String, a: F1WAE) extends F1WAE


type Env = Map[String, Int]
type FDs = List[FunDef]

// Scala Implementation
def lookup(name: String, env: Env): Int = {
  env.get(name) match {
    case Some(v) => v
    case None => error(s"free identifier: $name")
  }
}

def lookupFD(name: String, fs: FDs): FunDef = fs match {
  case Nil => error(s"lookupFD: unknown function: $name")
  case h :: t =>
    if (h.f == name) h
    else lookupFD(name, t)
}

def interp(e: F1WAE, env: Env, fs: FDs): Int = e match {
  case Num(n) => n
  case Add(l, r) => interp(l, env, fs) + interp(r, env)
  case Sub(l, r) => interp(l, env, fs) - interp(r, env, fs)
  case With(x, i, b) => interp(b, env + (x -> interp(i, env, fs)), fs)
  case Id(x) => lookup(x, env)
  case App(f, a) => lookupFD(f, fs) match {
    case FunDef(fname, pname, body) => {
      val aval = interp(a, env, fs)
      interp(body, Map() + (pname -> aval), fs)
    }
  }
}