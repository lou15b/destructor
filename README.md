`destructor`: A helper macro for coding `=destroy` hooks
========================================================

The `destructor` generates the definition for the `=destroy` hook for an object or
ref object type. The intent is to eliminate most of the boilerplate code
involved in writing an `=destroy` hook. There is also an option to
automatically generate printing of trace messages for debug purposes, if desired.

The general form used to invoke the destructor macro looks like:
```
   destructor(DestructeeType[, identifier = <variable name>]
       [, tagfield = <identifier>.<field name>]):
     <custom destructor code>
     destroyFields(identifier.field1, ..., identifier.fieldN)
     <more custom destructor code>
     destroyFields(identifier.fieldM, ..., identifier.fieldQ)
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

Coding examples
---------------

A few simple examples for illustration (more examples can be found, and executed,
in the `when isMainModule:` block of `destructor.nim`).

The examples here are based on the following object type definition:
```
  type
    SimpleObj = object
     name: string
     otherString: string
```

First, the simplest possible destructor definition:
```
 destructor(SimpleObj):
   destroyFields(x.name, x.otherString)
```
Note the use of `x` (the default) to represent the entity being destroyed. Note
also that a single statement can be used for all of the object's fields.

Second, let's assume that the field `name` is used to identify the instance of the
`SimpleObj` object. Note that this option only affects destructor trace messages
(see `Destructor trace messages` below).
```
 destructor(SimpleObj, tagfield = x.name):
   destroyFields(x.name, x.otherString)
```

Third, let's suppose we want to use "xyz" instead of "x" to represent the
entity being destroyed.
```
 destructor(SimpleObj, identifier = xyz, tagfield = xyz.name):
   destroyFields(xyz.name, xyz.otherString)
```

Finally, let's put some custom user code into the destructor.
```
 destructor(SimpleObj, identifier = xyz, tagfield = xyz.name):
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
```
type
  SimpleObj = object
    name: string
    otherString: string
  
  TestObj = object of RootObj
    simpleObj: SimpleObj
  
destructor(SimpleObj, identifier = xyz, tagfield = xyz.name):
  destroyFields(xyz.name, xyz.otherString)
  
destructor(TestObj):
  destroyFields(x.simpleObj)
  
when isMainModule:
  proc testCase() =
    let t1 {.used.} = TestObj(simpleObj: SimpleObj(name: "ph name", otherString: "xfghxfg"))
  
  testCase()
```

Compiling the above code with option "-d:traceDestructors" and executing it
gives the following output:
```
 "Destructor for type TestObj": {
 "destroy field simpleObj of type SimpleObj": {
 "Destructor for type SimpleObj with name = 'ph name'": {
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
 "Destructor for type TestObj": {
   "destroy field simpleObj of type SimpleObj": {
     "Destructor for type SimpleObj with name = 'ph name'": {
       "destroy field name of type string": {},
       "destroy field otherString of type string": {},
     },
   },
 },
```
Notice the following:
- Nested indentation showing that destroying field `simpleObj` caused
  the destructor for type `SimpleObj` to be invoked.
- The identification of the `SimpleObj` instance being destroyed was
  that whose tag field `name` had the value `ph name`

