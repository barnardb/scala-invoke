package io.github.barnardb.scalainvoke

/**
  * Evidence that function type {{{F}}} has return type {{{R}}}
  *
  * This allows method signatures to be expressed that accept functions of any arity while constraining their return types,
  * which allows us to work around some typing quirks that I couldn't get around with overloaded methods.
  *
  * @tparam F a function type (one of [[scala.Function0]] through [[scala.Function22]])
  * @tparam R the return type of {{{F}}}
  */
final abstract class FunctionReturning[F, R]

object FunctionReturning {

  implicit def function0 [                                                                  R] = null.asInstanceOf[FunctionReturning[()                                                                 => R, R]]
  implicit def function1 [A,                                                                R] = null.asInstanceOf[FunctionReturning[(A)                                                                => R, R]]
  implicit def function2 [A, B,                                                             R] = null.asInstanceOf[FunctionReturning[(A, B)                                                             => R, R]]
  implicit def function3 [A, B, C,                                                          R] = null.asInstanceOf[FunctionReturning[(A, B, C)                                                          => R, R]]
  implicit def function4 [A, B, C, D,                                                       R] = null.asInstanceOf[FunctionReturning[(A, B, C, D)                                                       => R, R]]
  implicit def function5 [A, B, C, D, E,                                                    R] = null.asInstanceOf[FunctionReturning[(A, B, C, D, E)                                                    => R, R]]
  implicit def function6 [A, B, C, D, E, F,                                                 R] = null.asInstanceOf[FunctionReturning[(A, B, C, D, E, F)                                                 => R, R]]
  implicit def function7 [A, B, C, D, E, F, G,                                              R] = null.asInstanceOf[FunctionReturning[(A, B, C, D, E, F, G)                                              => R, R]]
  implicit def function8 [A, B, C, D, E, F, G, H,                                           R] = null.asInstanceOf[FunctionReturning[(A, B, C, D, E, F, G, H)                                           => R, R]]
  implicit def function9 [A, B, C, D, E, F, G, H, I,                                        R] = null.asInstanceOf[FunctionReturning[(A, B, C, D, E, F, G, H, I)                                        => R, R]]
  implicit def function10[A, B, C, D, E, F, G, H, I, J,                                     R] = null.asInstanceOf[FunctionReturning[(A, B, C, D, E, F, G, H, I, J)                                     => R, R]]
  implicit def function11[A, B, C, D, E, F, G, H, I, J, K,                                  R] = null.asInstanceOf[FunctionReturning[(A, B, C, D, E, F, G, H, I, J, K)                                  => R, R]]
  implicit def function12[A, B, C, D, E, F, G, H, I, J, K, L,                               R] = null.asInstanceOf[FunctionReturning[(A, B, C, D, E, F, G, H, I, J, K, L)                               => R, R]]
  implicit def function13[A, B, C, D, E, F, G, H, I, J, K, L, M,                            R] = null.asInstanceOf[FunctionReturning[(A, B, C, D, E, F, G, H, I, J, K, L, M)                            => R, R]]
  implicit def function14[A, B, C, D, E, F, G, H, I, J, K, L, M, N,                         R] = null.asInstanceOf[FunctionReturning[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)                         => R, R]]
  implicit def function15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O,                      R] = null.asInstanceOf[FunctionReturning[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)                      => R, R]]
  implicit def function16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P,                   R] = null.asInstanceOf[FunctionReturning[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)                   => R, R]]
  implicit def function17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,                R] = null.asInstanceOf[FunctionReturning[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)                => R, R]]
  implicit def function18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, S,             R] = null.asInstanceOf[FunctionReturning[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, S)             => R, R]]
  implicit def function19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, S, T,          R] = null.asInstanceOf[FunctionReturning[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, S, T)          => R, R]]
  implicit def function20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, S, T, U,       R] = null.asInstanceOf[FunctionReturning[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, S, T, U)       => R, R]]
  implicit def function21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, S, T, U, V,    R] = null.asInstanceOf[FunctionReturning[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, S, T, U, V)    => R, R]]
  implicit def function22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, S, T, U, V, W, R] = null.asInstanceOf[FunctionReturning[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, S, T, U, V, W) => R, R]]

}
