Scala Invoke
============

Scala Invoke lets you turn any function, method or constructor into functions that have a consistent signature, 
with compile-time verification that you have an appropriate strategy for deriving each parameter. 


Future Work
-----------

- It should be possible to get have multiple parameter extractions fail,
  and get all of the errors. E.g., invoking an invocable that returns an `A` might return an `Either[Seq[String], () => A]`.

- It should be possible to build function invokers where some parameters are manually specified.

- More concrete invocation strategies should be provided,
  both for direct use and to demonstrate how users might create their own strategies.
