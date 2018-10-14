// Concrete Syntax
<WAE> ::= <num>
      | { + <WAE> <WAE>}
      | { - <WAE> <WAE>}
      | { with { <id> <WAE> } <WAE> }
      | <id>

// Abstract Syntax
trait WAE
case class Num(n: Int) extends WAE
case class Add(l: WAE, r: WAE) extends WAE
case class Sub(l: WAE, r: WAE) extends WAE
case class With(x: String, i: WAE, b: WAE) extends WAE
case class Id(x: Stirng) extends WAE

type Env = Map[String, Int]

// Scala Implementation
def lookup(name: String, env: Env): Int = {
  env.get(name) match {
    case Some(v) => v
    case None => error(s"free identifier: $name")
  }
}

def interp(wae: WAE, env: Env): Int = wae match{
  case Num(n) => n
  case Add(l, r) => interp(l, env) + interp(r, env)
  case Sub(l, r) => interp(l, env) - interp(r, env)
  case With(x, i, b) => interp(b, env + (x -> interp(i, env)))
  case Id(x) => interp(lookup(x, env), env)
}

// Operational Semantics

e ::= name	        Num(n)
  | e + e           Add(l, r)
  | e - e           Sub(l, r)
  | val x = e in e  With(x, i, b)
  | x               Id(x)

σ ㅏ e => v
σ ㅏ n => n

σ ㅏ e1 => n1 || σ ㅏ e2 => n2
--------------------------------------
σ ㅏ e1 + e2 => n1 + n2

σ ㅏ e1 - e2는 똑같이.

x <- Domain(σ)
----------------------
σ ㅏ x => σ(x)

σ ㅏ e1 => v1 || σ[x -> v1] ㅏ e2 => v2
-------------------------------
σ ㅏ val x = e1 in e2 => v2