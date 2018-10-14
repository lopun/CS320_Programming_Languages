// Concrete Syntax
<FAE> ::= <num>
      | { + <FAE> <FAE>}
      | { - <FAE> <FAE>}
      | <id>
      | { <FAE> <FAE> }
      | { fun {<id>} <FAE> }

// Operational Semantics

e ::= x
  || e e
  || ?x.e