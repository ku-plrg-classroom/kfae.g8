package kuplrg

trait Template {

  def eval(str: String): String =
    import Cont.*
    def aux(k: Cont, s: Stack): Value = step(k, s) match
      case (EmptyK, List(v)) => v
      case (k, s)            => aux(k, s)
    aux(EvalK(Map.empty, Expr(str), EmptyK), List.empty).str

  def step(k: Cont, s: Stack): (Cont, Stack)
}
