Scala Invoke
============

Scala Invoke lets you turn any function, method or constructor into a functions that have a consistent signature, 
with compile-time verification that you have an appropriate strategy for deriving each parameter. 


Future Work
-----------

- The FunctionInvoker and MethodInvoker classes aren't useful. They should be replaced with Function1 and Function2.

- It should be possible to build function invokers where some parameters are manually specified.

- It should be possible to split parameter extraction and invocation into two phases,
  so that multiple invokers can be used in such a way that nothing is invoked unless all parameter extraction succeeds.

- There should be a clean way of extracting parameters when the name isn't known,
  like when building an extractor from a function value.  

- For method invocations, it should be possible to extract the target from the environment.

- More concrete invocation strategies should be provided,
  both for direct use and to demonstrate how users might create their own strategies.
  
- Use FunctionReturning to simplify InvocationStrategy.functionInvoker declaration.
