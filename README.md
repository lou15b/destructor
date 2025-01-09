`destructor`: Helpers for coding `=destroy` hooks
=================================================

As Araq has [noted](https://forum.nim-lang.org/t/12332#76637), it is usually
not necessary to explicitly code an `=destroy` proc for an `object` or `ref object`
type - in most cases the compiler automatically generates whatever is necessary.

There are couple of circumstances, however, where an explicitly coded `=destroy`
proc is necessary:
* The object's fields includes values that are allocated and managed by a foreign
(i.e. non-Nim) library
* The object containers pointers or `cursor` references that are explicitly managed
by the code

There is another, less common, circumstance where explicitly coded `=destroy`
procs may be necessary. That is in test/debug scenarios, where one needs to verify
that object destructors have been actually called. Unfortunately there is no way
(that I am aware of) to determine whether an automatically generated destructor
has been called. The usual way around this is to compile
with `-d:useMalloc` and then use `valgrind` to find any memory leaks
(i.e. heap objects that haven't been destroyed).

Unfortunately, using `valgrind` in the above manner is not always effective. When
the code in question makes use of a foreign library that manages a large number of
resources, such as opengl and sdl2, the output from `valgrind` is overwhelmend by
messages triggered by the foreign library code, and it can be difficult if not
impossible to get at the messages that are purely relevant to the nim code.

This package is intended to provide the following:
* A simple way to implement a custom `=destroy` hook when one is required.
* A simple facility that implements a custom `=destroy` hook
with trace output for a test/debug situation, but that lets Nim automatically
implement its own `=destroy` hook for a normal situation.

Custom `=destroy` hook
----------------------

For the first case the `destructor` macro, together with the `destroyFields` macro,
generates the implementation of the `=destroy` hook
for an `object` or `ref object` type. There is also an option to
generate printing of trace messages for debug purposes, if desired.

The general form used to invoke the `destructor` macro looks like (using method call
syntax):
```nim
   DestructeeType.destructor([identifier = <variable name>]
       [, tagfield = <identifier>.<field name>]):
     <custom destructor code>
     DestructeeType.destroyFields(identifier.field1, ..., identifier.fieldN)
     <more custom destructor code>
     DestructeeType.destroyFields(identifier.fieldM, ..., identifier.fieldQ)
     <still more custom destructor code>
     <... etc.>
```
The macro arguments are:
```
   `DestructeeType` = the type (typedesc) of the entity being destroyed
   `identifier` (optional) = the identifier that refers to the entity being
                           destroyed.
                           Default is "x"
   `tagfield` (optional) = the field whose value identifies the individual
                         instance being destroyed in the destructor's trace
                         message. See `Destructor trace messages` below.
                         Default is no tag field
```
The body code of the macro invocation consists of two types of code statements:
   - any custom user code required for the destructor
   - one or more `destroyFields(...)` call statements, specifying fields for which
     `=destroy` calls are to be generated. There is no limit to the number of
     field arguments in a single  `destroyFields(...)` statement. An `=destroy`
     call is generated for each field argument, in same
     order as it appears in the `destroyFields(...)` call.

The above code statements can be mingled in whatever way is appropriate.

If the destructee type is a subclass of a base type (i.e. the type is
`object of` or `ref object of`), then an `=destroy` call for that base type is
generated at the end of the code. (Note that this is not done if the base type is
`RootObj` or `RootRef`.)

Note that invoking `destructor` with no body code, such as below, will cause a
forward declaration of the `=destroy` hook to be generated.
```nim
   DestructeeType.destructor()
```

Trace-generating `=destroy` hook for test/debug
-----------------------------------------------

The template `traceDestructor` invokes the `destructor` macro to be invoked, but only
if the compile option `-d:traceDestructors` is used, which meeans that trace messages
are printed.

if the compile option `-d:traceDestructors` is not used, then nothing (actually,
a single `discard` stataement) is generated, and the Nim compiler automatically generates
generates its own implementation of the `=destroy` hook as part of its operation.

Use of the `traceDestructor` template is similar to the `destructor` macro:
```nim
   DestructeeType.traceDestructor([identifier = <variable name>]
       [, tagfield = <identifier>.<field name>]):
     <custom destructor code>
     DestructeeType.destroyFields(identifier.field1, ..., identifier.fieldN)
     <more custom destructor code>
     DestructeeType.destroyFields(identifier.fieldM, ..., identifier.fieldQ)
     <still more custom destructor code>
     <... etc.>
```

A forward declaration would be generated by:
```nim
   DestructeeType.traceDestructor()
```

Again, if the compile option `-d:traceDestructors` is not used, then nothing is
generated.

Coding examples
---------------

A few simple examples for illustration (more examples can be found, and executed,
in the `when isMainModule:` block of `destructor.nim`).

The examples here are based on the following `ref object` type definition:
```nim
  type
    SimpleT = ref object
     name: string
     otherString: string
```

First, the simplest possible destructor definition:
```nim
 destructor(SimpleT):
   destroyFields(x.name, x.otherString)
```
Note the use of `x` (the default) to represent the entity being destroyed. Note
also that a single statement can be used for all of the object's fields.

Second, let's assume that the field `name` is used to identify the instance of the
`SimpleT` object. Note that this option only affects destructor trace messages
(see `Destructor trace messages` below).
```nim
 destructor(SimpleT, tagfield = x.name):
   destroyFields(x.name, x.otherString)
```

Third, let's suppose we want to use "xyz" instead of "x" to represent the
entity being destroyed.
```nim
 destructor(SimpleT, identifier = xyz, tagfield = xyz.name):
   destroyFields(xyz.name, xyz.otherString)
```

Finally, let's put some custom user code into the destructor.
```nim
 destructor(SimpleT, identifier = xyz, tagfield = xyz.name):
   if xyz.otherString == "Call":
     # If otherString is "Call" then use the Call convention to invoke destroyFields
     destroyFields(xyz.name, xyz.otherString)
   else:
     # Otherwise use the Command convention to invoke destroyFields
     destroyFields xyz.name, xyz.otherString
```

Destructor trace messages
-------------------------

If the compile option `-d:traceDestructors` is specified, then the `destructor`
macro will generate statements to print JSON-like trace messages at:
- the beginning (and end) of the `=destroy` body,
- before (and after) the `=destroy` call each for each field, and
- before (and after) the `=destroy` call for the base type.

The trace message printed after each call and at the end of the destructor is
merely a JSON element terminator with a comma `},`.

For example, consider the following code:
```nim
type
  SimpleT = ref object
    name: string
    otherString: string
  
  TestT = ref object of RootRef
    simpleX: SimpleT
  
destructor(SimpleT, identifier = xyz, tagfield = xyz.name):
  destroyFields(xyz.name, xyz.otherString)
  
destructor(TestT):
  destroyFields(x.simpleX)
  
when isMainModule:
  proc testCase() =
    let t1 {.used.} = TestT(simpleX: SimpleT(name: "ph name", otherString: "xfghxfg"))
  
  testCase()
```

Compiling the above code with option "-d:traceDestructors" and executing it
gives the following output:
```
 "Destructor for type TestT": {
 "destroy field simpleX of type SimpleT": {
 "Destructor for type SimpleT with name = 'ph name'": {
 "destroy field name of type string": {
 },
 "destroy field otherString of type string": {
 },
 },
 },
 },
```
Pasting the output into a JSON formatter gives:
```
 "Destructor for type TestT": {
   "destroy field simpleX of type SimpleT": {
     "Destructor for type SimpleT with name = 'ph name'": {
       "destroy field name of type string": {},
       "destroy field otherString of type string": {},
     },
   },
 },
```
Notice the following:
- Nested indentation showing that destroying field `simpleX` caused
the destructor for type `SimpleT` to be invoked.
- The identification of the `SimpleT` instance being destroyed was
that whose tag field `name` had the value `ph name`

