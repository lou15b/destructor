A helper macro for coding destructors (`\`=destroy\`` procs)

Macro to generate the definition for the `\`=destroy\`` hook for an object or
ref object type.
The general form of a the macro's invocation looks like:
   destructor(DestructeeType[, identifier = <variable name>]
       [, tagfield = <identifier>.<field name>]):
     <custom destructor code>
     destroyFields(identifier.field1, ..., identifier.fieldN)
     <more custom destructor code>
     destroyFields(identifier.fieldM, ..., identifier.fieldQ)
     <still more custom destructor code>
     <... etc.>
The macro arguments are:
   DestructeeType = the type (typedesc) of the entity being destroyed
   identifier (optional) = the identifier that refers to the entity being
                           destroyed.
                           Default is "x"
   tagfield (optional) = the field whose value identifies the individual
                         instance being destroyed in the destructor's trace
                         message. See below.
                         Default is no tag field
The body code of the macro invocation consists of two types of code statements:
   - any custom user code required for the destructor
   - one or more "destroyFields(...) call statements, specifying fields for which
     `=destroy` calls are to be generated. The calls are  generated in same
     order as they appear in the call.
The above code statements can be mingled in whatever way is appropriate.

If the destructee type is a subclass of a base type (i.e. "object of" or
"ref object of"), then an `=destroy` call for the base type is
generated at the end. (Note that this is not done if the base type is
`RootObj' or `RootRef`.)

Coding examples

A few simple examples. They are based on the following object type definition:
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
Note the use of "x" (the default) to represent the entity being destroyed.

Second, let's assume that the field "name" is used to identify the instance of the
SimpleObj object.
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

If the compile option "-d:traceDestructors" is specified, then the destructor
macro will generate JSON-like trace messages at the beginning (and end) of the
`=destroy` body, before (and after) the `=destroy` call each for each field,
and before (and after) the `=destroy` call for the base type. (The
trace message printed after each call and at the end of the destructor is
merely a JSON element terminator "}".)

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
- Nested indentation showing that destroying field "simpleObj" caused
  the destructor for type "SimpleObj" to be invoked.
- The identification of the SimpleObj instance being destroyed was
  that whose tag field "name" had the value "ph name"

