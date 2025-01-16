import macros
import tables

when defined(traceDestructors):
  proc jsonEnd(): NimNode =
    ## Generates code to print a JSON element end
    return quote do:
      echo "},"

# =================================================================================
# Procs to generate `=destroy` calls for the base type and associated trace message
# =================================================================================

when defined(traceDestructors):
  proc destroyBaseObjectJsonStart(destructeeType: NimNode, baseType: NimNode): NimNode =
    ## Generates code to print the JSON element start for the base type destructor call
    result = newCall("echo")
    result.add(newStrLitNode("\"destroy " & $destructeeType & " base type " & 
      $baseType & "\": {"))

proc destroyBaseObject(objectIdentifier: NimNode, baseType: NimNode): NimNode =
  ## Generates the `=destroy` call for the base type of an object (non-ref) destructee
  ## Generates code equivalent to:
  ##    `=destroy`(baseType(x))
  
  # assert objectIdentifier.kind == nnkIdent
  # assert baseType.kind == nnkSym
  # assert baseType.symKind == nskType

  result = newNimNode(nnkStmtList)

  let castArg = newCall(baseType, objectIdentifier)
  let destructorCall = newCall("=destroy", castArg)
  result.add destructorCall

proc destroyBaseRefObject(objectIdentifier: NimNode, baseType: NimNode): NimNode =
  ## Generates the `=destroy` call for the base type of a ref object destructee
  ## Generates code equivalent to:
  ##    `=destroy`(typeof(BaseRefType()[])(x))
  
  # assert objectIdentifier.kind == nnkIdent
  # assert baseType.kind == nnkSym
  # assert baseType.symKind == nskType

  result = newNimNode(nnkStmtList)

  # Expression to get the anonymous object type from the base reference type
  # i.e. "typeof BaseRefType()[]"
  #   First we need to create a reference to an object for the base reference type
  #   i.e. "BaseRefType()"
  let refCode = newCall(baseType)

  #   Second we need to de-reference the newly created reference to get the
  #   underlying object
  #   i.e. "BaseRefType()[]"
  var objCode = newNimNode(nnkDerefExpr)
  objCode.add(refCode)

  #   Third we need to extract the type of the de-referenced object to get the
  #     base object type
  #   i.e. "typeof(BaseRefType()[])"
  let typeofCode = newCall("typeof", objCode)

  #   Fourth we need to cast the object to be destroyed to the base object type
  #   i.e. "typeof(BaseRefType()[])(x)"
  let castArg = newCall(typeofCode, objectIdentifier)

  # Finally, generate the destructor call
  let destructorCall = newCall("=destroy", castArg)
  result.add(destructorCall)

proc destroyBaseType(destructeeType: NimNode, destructeeKind: NimNodeKind,
    destructeeIdentifier: NimNode, baseType: NimNode): NimNode =
  result = newNimNode(nnkStmtList)
  when defined(traceDestructors):
    # Print the JSON element start for the call to the base type destructor
    result.add(destroyBaseObjectJsonStart(destructeeType, baseType))

  # Call to the base type destructor
  if destructeeKind == nnkObjectTy:
    result.add(destroyBaseObject(destructeeIdentifier, baseType))
  elif destructeeKind == nnkRefTy:
    result.add(destroyBaseRefObject(destructeeIdentifier, baseType))
  else:
    error "destructor macro only supports object and ref object types (destroyBaseType)"

  when defined(traceDestructors):
    # Print the JSON element end for the call
    result.add(jsonEnd())

  # echo "\n Generated code from destroyBaseType:\n", repr(result), "\n\n"

# ===============================================================================
# Procs to generate `=destroy` calls for the fields and associated trace messages
# ===============================================================================

when defined(traceDestructors):
  proc getTypeString(typeNode: NimNode): string =
    ## Returns the string representation of the AST for a type
    if typeNode.kind == nnkSym:
      result = $typeNode
    else:
      result = repr(typeNode)

  proc getFieldIdent(fieldNode: NimNode): NimNode =
    if fieldNode[0].kind == nnkIdent:
      # Private field (no post-fix "*")
      result = fieldNode[0]
    elif fieldNode[0].kind == nnkPostfix:
      # Field is marked public ("*")
      result = fieldNode[0][1]
    elif fieldNode[0].kind == nnkPragmaExpr:
      # Field has a pragma
      result = getFieldIdent(fieldNode[0])
    else:
      error "Unrecognized field declaration (getFieldIdent): " & repr(fieldNode)

  proc getFieldTypeString(fieldIdent: NimNode, ownerFieldList: NimNode): string =
    ## Returns the string representation of an object field's type
    ##    fieldIdent = the ident node of the field
    ##    ownerFieldList = the node containing the object's field definitions
    let fieldName = $fieldIdent
    var index = 0
    for field in ownerFieldList:
      let ident = getFieldIdent(field)
      inc index
      if $ident == fieldName:
        result = getTypeString(field[1])
        break

  proc destroyFieldJsonStart(objectFieldIdentNode: NimNode, ownerFieldList: NimNode): NimNode =
    ## Generates code to print the JSON element start for a call to the destructor
    ## of an object's field
    result = newCall("echo")
    let fieldIdent = objectFieldIdentNode[1]
    let fieldTypeString = getFieldTypeString(fieldIdent, ownerFieldList)
    result.add(newStrLitNode("\"destroy field " & $fieldIdent & " of type " &
      fieldTypeString & "\": {"))

proc destroyFieldCodeStmt(fieldNode: NimNode): NimNode =
  ## Generates code for a call to `=destroy` for a single field of an object.
  ## The generated code is equivalent to:  `=destroy`(<field>.addr[])
  ## where <field> is the fully qualified field variable (e.g. "x.field")
  ##    fieldNode = AST node of the fully qualified field variable
  #
  # Note:
  # The form of the  `=destroy` call generated by this proc is the same for
  # *all* fields, regardless of whether a destructor has been defined for the
  # field's type:
  #     `=destroy`(x.field.addr[])
  #
  # The reasons are as follows:
  #   - If `=destroy` is not invoked for a field in a destructor, then that field
  #     is not touched - memory leak if the field involves any heap storage.
  #     This means that `=destroy` must be called for all fields that involve
  #     heap storage, even though an `=destroy` may not have been defined for the
  #     field's type.
  #   - Calling
  #         `=destroy`(x.field)
  #     for any field whose type does *not* have an `=destroy` hook defined causes
  #     a compile error.
  #   - Experiment has shown that calling
  #         `=destroy`(x.field.addr[])
  #     works for all field types, whether an `=destroy` hook has been defined
  #     for the type or not.
  #   - One could define different calls for fields with and without `=destroy`
  #     hook definitions (say "destroyFieldsWithHook" and "destroyFieldsWithoutHook"),
  #     but this imposes on the user the requirement that they must determine
  #     whether an `=destroy` hook has been defined for every field in the object.
  #     This becomes quite onerous when third-party libraries are used.
  #
  # So it was decided to use the same call form for all fields.
  #
  let addrNode = newDotExpr(fieldNode, newIdentNode("addr"))
  var derefNode = newNimNode(nnkBracketExpr)
  derefNode.add(addrNode)
  result = newCall("=destroy", derefNode)

proc buildDestroyFieldCodeStmts(fieldNode: NimNode, destructeeTypeImpl: NimNode): NimNode =
  ## Generates the statement(s) for the `=destroy` call for a single field of an object,
  ## together with the statements to print the trace JSON messages if required.
  ##    fieldNode = AST node of the fully qualified field variable (e.g. "x.field")
  ##    destructeeTypeImpl = (typed) AST of the destructee's type definition

  let destructeeKind = destructeeTypeImpl[2].kind
  # Get the field list from the destructee type. The AST position depends on
  # whether the type is object or ref object
  var ownerFieldList: NimNode
  if destructeeKind == nnkObjectTy:
    ownerFieldList = destructeeTypeImpl[2][2]
  elif destructeeKind == nnkRefTy:
    ownerFieldList = destructeeTypeImpl[2][0][2]
  else:
    error("Destructee must be either an obect or a reference to an object")
  # assert ownerFieldList.kind == nnkRecList

  result = newStmtList()
  when defined(traceDestructors):
    # Generate a statement to print the initial line of the JSON element for the
    # field destruction call
    result.add(destroyFieldJsonStart(fieldNode, ownerFieldList))
  # Generate the `=destroy` call
  result.add(destroyFieldCodeStmt(fieldNode))
  when defined(traceDestructors):
    # Generate a statement to print the final line of the JSON element for the
    # field destruction call
    result.add(jsonEnd())

  # echo "\n*** Generated code from buildDestroyFieldCodeStmts:\n", repr(result), "\n\n"

macro destroyFields*(destructeeType: typedesc, fields: varargs[untyped]): untyped =
  ## Macro to generate to call destructor call(s) for the specified field(s)
  ## of an object. It is intended to be used in the code passed to the
  ## "destructor" macro.
  ## The general form of the macro's invocation looks like (using method call syntax):
  ##    DestructeeType.destroyFields(x.field1, ..., x.fieldN)
  ## The macro arguments are:
  ##    DestructeeType = the type (typedesc) of the entity being destroyed
  ##    x.field = the (qualified) field for which an `=destroy` call is to be
  ##              generated
  ## 
  ## Destructor trace messages
  ## If the compile option "-d:traceDestructors" is specified, then the destructor
  ## macro will generate JSON-like trace messages before (and after) the `=destroy`
  ## call for each field,
  result = newStmtList()
  let destructeeTypeImpl = destructeeType.getImpl
  for field in fields:
    result.add(buildDestroyFieldCodeStmts(field, destructeeTypeImpl))

# ===============================================================================
# Procs to generate the `=destroy` proc declaration and associated trace messages
# ===============================================================================

proc parseCodeSpecs(codeSpecs: NimNode, tagfield: var NimNode,
      bodyCode: var NimNode) =
  ## Parses the destructor macro's untyped arguments to obtain the code generation
  ## options and the (unexpanded) body code in the macro's invocation.
  ##    codeSpecs = the list of the macro's untyped arguments
  ##    tagfield = the variable to receive the AST node of the tag field, if any.
  ##               The tag field is the destructee's field whose value is used
  ##               in the destructor's trace message to identify the particular
  ##               destructee instance being destroyed
  ##               Default is no tag field.
  ##    bodyCode = the variable to receive the AST of the body code in the
  ##               destructor macro's invocation. The body code will most likely
  ##               include at least one invocation of the "destroyFields" macro.
  
  # echo "\n##### parseCodeSpecs - AST of codeSpecs:\n", treeRepr(codeSpecs), "\n"
  # echo "Number of codeSpec params is ", codeSpecs.len

  # assert codeSpecs.kind == nnkArgList
  # if codeSpecs.len < 1 or codeSpecs[^1].kind != nnkStmtList:
  #   error("destructor must have a code body")


  # Default value
  tagfield = newEmptyNode()
  
  if codeSpecs.len > 0 and codeSpecs[^1].kind == nnkStmtList:
    # The macro invocation is for destructor code
    if codeSpecs.len > 1:
      for codeSpec in codeSpecs[0..^2]:
        if codeSpec.kind != nnkExprEqExpr:
          error("destructor argument (" & repr(codeSpec) & ") is not of the form " &
            "<option> = <value>")
        let option = codeSpec[0]
        if option.kind != nnkIdent:
          error("The lhs of the destructor argument (" & repr(option) & ") must " &
            "be a simple identifier")
        elif $option == "tagfield":
          # TODO Need better validation of the tagfield option
          if codeSpec[1].kind != nnkDotExpr:
            error("The rhs of the 'tagfield' option (" & repr(codeSpec[1]) &
              ") must be a dot expression (<destructee>.<field>)")
          tagfield = codeSpec[1]
        else:
          error("Invalid destructor option " & $option)
    # The raw body code is the last argument
    bodyCode = codeSpecs[^1]
  else:
    # Otherwise the macro invocation is for a forward reference
    bodyCode = nil

  # echo "\n##### parseCodeSpecs - results:"
  # echo "\t tagfield = ", repr(tagfield)
  # echo "\t bodyCode AST:\n", treeRepr(bodyCode)

when defined(traceDestructors):
  proc destructorJsonStart(typeNode: NimNode, tagfield: NimNode): NimNode =
    ## Generates code to print the JSON element start that records the entry
    ## into the destructee's `=destroy` proc
    var tag = "\"Destructor for type " & $typeNode
    if tagfield.kind != nnkEmpty:
      tag &= " with " & repr(tagfield[1]) & " = \'"
    
    result = newCall("echo", newStrLitNode(tag))
    if tagfield.kind != nnkEmpty:
      # Add echo argument for tagfield
      result.add(tagfield)
      result.add(newStrLitNode("\'"))
    result.add(newStrLitNode("\": {"))

proc genObjectDestroyProcParameterType(typeNameStr: string): NimNode =
  ## Generates the type portion of the `=destroy` proc's only argument
  ## for an object (i.e. non-ref)
  result = newIdentNode(typeNameStr)

proc genRefObjectDestroyProcParameterType(typeNameStr: string): NimNode =
  ## Generates the type portion of the `=destroy` proc's only argument
  ## for a ref object
  
  # Need to build the AST for the parameter type ("typeof <typeName>()[]")
  # Make an instance of the ref object: "<typeName>()"
  let refInstantiationNode = newCall(newIdentNode(typeNameStr))
  # Dereference the ref instance to get the object: "<typeName>()[]"
  var derefedObjectNode = newNimnode(nnkBracketExpr)
  derefedObjectNode.add(refInstantiationNode)
  # Get the (anonymous) type of the object: "typeof(TestRef()[])"
  result = newCall("typeof", derefedObjectNode)

proc genDestroyProcParameterType(destructeeType: NimNode,
    destructeeCategory: NimNode): NimNode =
  ## Generates the type portion of the `=destroy` proc's only argument according
  ## to whether it is an object or a ref object
  let destructeeKind = destructeeCategory.kind
  if destructeeKind == nnkObjectTy:
    result = genObjectDestroyProcParameterType($destructeeType)
  elif destructeeKind == nnkRefTy:
    result = genRefObjectDestroyProcParameterType($destructeeType)
  else:
    error "destructor macro only supports object and ref object types " &
      "(genDestroyProcParameterType)"

proc generateDestructorCode(destructeeType: NimNode, destructeeTypeImpl: NimNode,
    tagField: NimNode, bodyCode: NimNode): NimNode =
  ## Generates the complete `=destroy` proc definition for the destructor macro
  ##    destructeeType = typedesc of the destructee's type
  ##    destructeeTypeImpl = (typed) AST of the destructee's type definition
  ##    tagfield = the AST node of the tag field, if any.
  ##               The tag field is the destructee's field whose value is used
  ##               in the destructor's trace message to identify the particular
  ##               destructee instance being destroyed
  ##    bodyCode = the AST of the raw body code in the destructor macro's invocation
  ##               The body code will most likely include at least one
  ##               invocation of the "destroyFields" macro.
  
  # echo "\n##### generateDestructorCode"
  # echo "##### Impl of ", $destructeeType, " is:\n", treeRepr(destructeeTypeImpl)

  # destructor proc name definition ("`=destroy`")
  var procNameNode = newNimNode(nnkAccQuoted)
  procNameNode.add(newIdentNode("="))
  procNameNode.add(newIdentNode("destroy"))

  # destructor proc return type & parameters
  var paramNodes = newSeq[NimNode]()
  # Return type - none
  let returnTypeNode = newEmptyNode()
  paramNodes.add(returnTypeNode)

  # First (and only) parameter definition
  let destructeeIdentifier = ident("x")   # The symbol for the destructee is "x"
  var param1Node = newNimNode(nnkIdentDefs)
  param1Node.add(destructeeIdentifier)     # parameter name

  # parameter type (type of the entity being destroyed)
  # This code is specific to what kind of entity (object or ref object) is
  # to be destroyed
  # Get the type declaration of the (only) parameter in the `=destroy` proc declaration.
  let destructeeKind = destructeeTypeImpl[2].kind
  let destroyProcParameterType =
    genDestroyProcParameterType(destructeeType, destructeeTypeImpl[2])
  param1Node.add(destroyProcParameterType)
  param1Node.add(newEmptyNode())

  # Add the destructee parameter definition to the proc parameters
  paramNodes.add(param1Node)

  var bodyNode: NimNode = newEmptyNode()  # An empty body node is for a fwd declaration

  if not bodyCode.isNil:
    # The destructor invocation is for an implementation - construct the
    # `=destroy` proc body
    bodyNode = newNimNode(nnkStmtList)

    when defined(traceDestructors):
      # Generate a statement to print the initial line of the JSON element for the destructor
      let entryStmt = destructorJsonStart(destructeeType, tagField)
      bodyNode.add(entryStmt)
    
    # Add the body code
    bodyNode.add(bodyCode)

    # Append destructor call for base type if necessary
    var objectDef = destructeeTypeImpl[2]
    if objectDef.kind == nnkRefTy:
      # If the destructee is a reference type then the object definition is nested
      # one level down
      objectDef = destructeeTypeImpl[2][0]
    # assert objectDef.kind == nnkObjectTy
    if objectDef[1].kind == nnkOfInherit:
      let baseType = objectDef[1][0]
      if baseType.kind != nnkEmpty:
        let baseTypeStr = $baseType
        # echo "##### Base type of " & $destructeeType & " is " & baseTypeStr
        if baseTypeStr != "RootObj" and baseTypeStr != "RootRef":
          bodyNode.add(destroyBaseType(destructeeType, destructeeKind, destructeeIdentifier, baseType))

    when defined(traceDestructors):
      # Generate a statement to print the final line of the JSON element for the destructor
      bodyNode.add(jsonEnd())

  result = newProc(procNameNode, paramNodes, bodyNode)


# =================================================================
# The destructor macro
# =================================================================

macro destructor*(destructeeType: typedesc, codeSpecs: varargs[untyped]): untyped =
  ## Macro to generate the definition for the `=destroy` hook for an object or
  ## ref object type.
  ## The general form of the macro's invocation looks like (using method call syntax):
  ##    DestructeeType.destructor([tagfield = x.<field name>]):
  ##      <custom destructor code>
  ##      DestructeeType.destroyFields(x.field1, ..., x.fieldN)
  ##      <more custom destructor code>
  ##      DestructeeType.destroyFields(x.fieldM, ..., x.fieldQ)
  ##      <still more custom destructor code>
  ##      <... etc.>
  ## The macro arguments are:
  ##    DestructeeType = the type (typedesc) of the entity being destroyed
  ##    tagfield (optional) = the field whose value identifies the individual
  ##                          instance being destroyed in the destructor's trace
  ##                          message. See below.
  ##                          Default is no tag field
  ## The body code of the macro invocation consists of two types of code statements.
  ## Note that the symbol representing the object being destroyed is defined to be "x".
  ##    - any custom user code required for the destructor
  ##    - one or more "destroyFields(...) macro calls call statements, specifying
  ##      fields for which `=destroy` calls are to be generated. See the description
  ##      of the "destroyFields" macro for details The calls are  generated in same
  ##      order as they appear in the call.
  ## The above code statements can be mingled in whatever way is appropriate.
  ## 
  ## If the destructee type is a subclass of a base type (i.e. "object of" or
  ## "ref object of"), then an `=destroy` call for the base type is
  ## generated at the end.
  ## 
  ## Destructor trace messages
  ## If the compile option "-d:traceDestructors" is specified, then the destructor
  ## macro will generate JSON-like trace messages at the beginning (and end) of the
  ## `=destroy` body, before (and after) the `=destroy` call for each field,
  ## and before (and after) the `=destroy` call for the base type. (The end/after
  ## trace message is merely a JSON element terminator "}".)
  ## 

  let destructeeTypeImpl = destructeeType.getImpl
  # echo "\n## AST of destructeeTypeImpl :\n", treeRepr(destructeeTypeImpl)

  # var index = 0
  # for codeSpec in codeSpecs:
  #   echo "\n## AST of codeSpec ", index, ":\n", treeRepr(codeSpec)
  #   inc index
  
  var tagfield: NimNode
  var bodyCode: NimNode
  # Parse the untyped arguments
  parseCodeSpecs(codeSpecs, tagfield, bodyCode)

  # Generate the code
  result = generateDestructorCode(destructeeType, destructeeTypeImpl, tagfield,
    bodyCode)
  
  # echo "\n### Destructor generated code equivalent:\n", repr(result), "\n\n"
  # echo "\n### Destructor generated AST:\n", treeRepr(result), "\n\n"

# =================================================================
# The traceDestructor template
# =================================================================
template traceDestructor*(destructeeType: typedesc, codeSpecs: varargs[untyped]) =
  when defined(traceDestructors):
    destructor(destructeeType, codeSpecs)
  else:
    discard

# =================================================================
# Tests, showing some usage examples
# Run with "-d:traceDestructors" to get traced destructor output
# =================================================================

when isMainModule:
  # ---------------------------
  # Use Case 1:
  #   - A simple object type where one of the fields (name) identifies the
  #     object instance - i.e. "name" is the tag field
  #   - Both fields are public
  #   - Specification of the tag field
  #   - Embedding destroyFields(...) calls in other code
  #   - Invoking destroyFields using both Call and Command conventions
  #   - Use of method call syntax for macro invocation
  # ---------------------------
  type
    SimpleObj = object
      name*: string
      otherString*: string

  SimpleObj.destructor()    # Fwd declaration

  SimpleObj.destructor(tagfield = x.name):
    if x.otherString == "Call":
      SimpleObj.destroyFields(x.name, x.otherString)
    else:
      SimpleObj.destroyFields x.name, x.otherString
    discard

  proc testCase1() =
    echo "\n\nTest Case 1"
    let ph1 {.used.} = SimpleObj(name: "ph1", otherString: "sdfwEF")

  # ---------------------------
  # Use Case 2:
  #   - An object type with a field whose type has a destructor defined
  # ---------------------------
  type
    TestObj = object of RootObj
      simpleObj: SimpleObj

  TestObj.destructor:
    TestObj.destroyFields(x.simpleObj)

  proc testCase2() =
    echo "\n\nTest Case 2"
    let t1 {.used.} = TestObj(simpleObj: SimpleObj(name: "ph1", otherString: "xfghxfg"))

  # ---------------------------
  # Use Case 3:
  #   - An object type with a tuple field that contains an object whose type has a
  #     destructor defined
  # ---------------------------
  type
    TestObj2 = object of RootObj
      simpleObjTuple: tuple[str: string, simpleObj: SimpleObj]

  TestObj2.destructor():
    TestObj2.destroyFields(x.simpleObjTuple)

  proc testCase3() =
    echo "\n\nTest Case 3"
    let t1 {.used.} = TestObj2(simpleObjTuple: (str: "str1",
      simpleObj: SimpleObj(name: "tph1", otherString: "hjkhjk")))

  # ---------------------------
  # Use Case 4:
  #   - An object type that is a sub-type of another object type, both of which
  #     have a destructor defined
  # ---------------------------
  type
    TestSubObj = object of TestObj
      objname: string
      ph: SimpleObj

  TestSubObj.destructor(tagfield = x.objname):
    TestSubObj.destroyFields(x.ph, x.objname)

  proc testCase4() =
    echo "\n\nTest Case 4"
    var t1 = TestSubObj(objname: "tso1", ph: SimpleObj(name: "ph1"))
    t1.simpleObj = SimpleObj(name: "ph2")

  # ---------------------------
  # Use Case 5:
  #   - A ref object with a field that is a seq of objects
  # ---------------------------
  type
    TestRef = ref object of RootRef
      name: string
      singleSimpleObj: SimpleObj
      simpleObjSeq: seq[SimpleObj]

  TestRef.destructor(tagfield = x.name):
    TestRef.destroyFields(x.singleSimpleObj, x.simpleObjSeq, x.name)

  proc testCase5() =
    echo "\n\nTest Case 5"
    var tr = TestRef.new()
    tr.name = "tr1"
    tr.singleSimpleObj = SimpleObj(name: "sph")
    tr.simpleObjSeq= @[SimpleObj(name: "ph0"),
      SimpleObj(name: "ph1"),
      SimpleObj(name: "ph2")]

  # ---------------------------
  # Use Case 6:
  #   - A ref object with private and public fields that are cursor ref's
  #     and a field that is an array of objects
  # ---------------------------
  type
    TestRef2 = ref object of RootRef
      name: string
      testCursor1 {.cursor.}: TestRef
      testCursor2* {.cursor.}: TestRef
      simpleObjArray: array[3, SimpleObj]

  TestRef2.destructor(tagfield = x.name):
    # Note that cursor fields do not need destruction
    TestRef2.destroyFields(x.simpleObjArray, x.name)

  proc testCase6() =
    echo "\n\nTest Case 6"
    var tr = TestRef2.new()
    tr.name = "tr1"
    tr.simpleObjArray[0]= SimpleObj(name: "pha0")
    tr.simpleObjArray[1]= SimpleObj(name: "pha1")
    tr.simpleObjArray[2]= SimpleObj(name: "pha2")

  # ---------------------------
  # Use Case 7:
  #   - A ref object with a field that is a seq of ref objects
  # ---------------------------
  type
    Marker = ref object
      name: string
      tref: TestRef

    CompositeTestRef = ref object of TestRef
      mMarkers: seq[Marker]

  Marker.destructor(tagfield = x.name):
    Marker.destroyFields(x.name, x.tref)

  CompositeTestRef.destructor:
    CompositeTestRef.destroyFields(x.mMarkers)

  proc testCase7() =
    echo "\n\nTest Case 7"
    var ctr = CompositeTestRef.new()

    var marker0 = Marker.new()
    marker0.name = "marker0"
    marker0.tref = TestRef.new()
    marker0.tref.name = "tr0"
    marker0.tref.singleSimpleObj = SimpleObj(name: "sph0")
    marker0.tref.simpleObjSeq= @[SimpleObj(name: "ph0a"),
      SimpleObj(name: "ph0b"),
      SimpleObj(name: "ph0c")]
    
    var marker1 = Marker.new()
    marker1.name = "marker1"
    marker1.tref = TestRef.new()
    marker1.tref.name = "tr1"
    marker1.tref.singleSimpleObj = SimpleObj(name: "sph1")
    marker1.tref.simpleObjSeq= @[SimpleObj(name: "ph1a"),
      SimpleObj(name: "ph1b"),
      SimpleObj(name: "ph1c")]
    
    ctr.mMarkers = @[marker0, marker1]
    ctr.name = "ctr1"
    ctr.singleSimpleObj = SimpleObj(name: "sph")
    ctr.simpleObjSeq= @[SimpleObj(name: "pha"),
      SimpleObj(name: "phb"),
      SimpleObj(name: "phc")]

  # ---------------------------
  # Use Case 8:
  #   - A complex nesting of ref objects, each whose type has a destructor defined
  # ---------------------------
  type
    MetaTestRef = ref object of TestRef
      trefs: seq[TestRef]

  MetaTestRef.destructor:
    MetaTestRef.destroyFields(x.trefs)

  proc testCase8() =
    echo "\n\nTest Case 8"
    var mtr = MetaTestRef.new()

    var tref = TestRef.new()
    tref.name = "tref"
    tref.singleSimpleObj = SimpleObj(name: "sphtref")
    tref.simpleObjSeq= @[SimpleObj(name: "phtref0"),
      SimpleObj(name: "phtref1"),
      SimpleObj(name: "phtref2")]

    var ctref = CompositeTestRef.new()

    var marker0 = Marker.new()
    marker0.name = "marker0"
    marker0.tref = TestRef.new()
    marker0.tref.name = "tr0"
    marker0.tref.singleSimpleObj = SimpleObj(name: "sph0")
    marker0.tref.simpleObjSeq= @[SimpleObj(name: "ph0a"),
      SimpleObj(name: "ph0b"),
      SimpleObj(name: "ph0c")]
    
    var marker1 = Marker.new()
    marker1.name = "marker1"
    marker1.tref = TestRef.new()
    marker1.tref.name = "tr1"
    marker1.tref.singleSimpleObj = SimpleObj(name: "sph1")
    marker1.tref.simpleObjSeq= @[SimpleObj(name: "ph1a"),
      SimpleObj(name: "ph1b"),
      SimpleObj(name: "ph1c")]
    
    ctref.mMarkers = @[marker0, marker1]
    ctref.name = "ctref1"
    ctref.singleSimpleObj = SimpleObj(name: "sphctref")
    ctref.simpleObjSeq= @[SimpleObj(name: "phctrefa"),
      SimpleObj(name: "phctrefb"),
      SimpleObj(name: "phctrefc")]

    mtr.trefs = @[tref, ctref]
    mtr.name = "mtr1"
    mtr.singleSimpleObj = SimpleObj(name: "sphmtr")
    mtr.simpleObjSeq= @[SimpleObj(name: "phmtra"),
      SimpleObj(name: "phmtrb"),
      SimpleObj(name: "phmtrc")]

  # ---------------------------
  # Use Case 9:
  #   - A ref object with a field that is a Table of ref objects
  # ---------------------------
  type
    SimpleTestRef = ref object of RootRef
      name: string

    TableTestRef = ref object of RootRef
      tableField: Table[string, SimpleTestRef]

  SimpleTestRef.destructor(tagfield = x.name):
    SimpleTestRef.destroyFields(x.name)

  TableTestRef.destructor:
    TableTestRef.destroyFields(x.tableField)

  proc testCase9() =
    echo "\n\nTest Case 9"
    var ttr = TableTestRef.new()

    let value1 = SimpleTestRef.new()
    value1.name = "name1"
    ttr.tableField["name1"] = value1

    let value2 = SimpleTestRef.new()
    value2.name = "name2"
    ttr.tableField["name2"] = value2

    let value3 = SimpleTestRef.new()
    value3.name = "name3"
    ttr.tableField["name3"] = value3

  # ---------------------------
  # Use Case 10:
  #   - A ref object with a field that is a TableRef of ref objects
  # ---------------------------
  type
    RefTableTestRef = ref object of RootRef
      refTableField: TableRef[string, SimpleTestRef]

  RefTableTestRef.destructor:
    RefTableTestRef.destroyFields(x.refTableField)

  proc testCase10() =
    echo "\n\nTest Case 10"
    var rttr = RefTableTestRef.new()
    rttr.refTableField = newTable[string, SimpleTestRef]()

    let value1 = SimpleTestRef.new()
    value1.name = "name1"
    rttr.refTableField["name1"] = value1

    let value2 = SimpleTestRef.new()
    value2.name = "name2"
    rttr.refTableField["name2"] = value2

    let value3 = SimpleTestRef.new()
    value3.name = "name3"
    rttr.refTableField["name3"] = value3

  # ---------------------------
  # Use Case 11:
  #   - A ref object with a field that is a closure
  # ---------------------------
  type
    ClosureTestRef = ref object of RootRef
      closureField: proc()

  ClosureTestRef.destructor():
    ClosureTestRef.destroyFields(x.closureField)

  proc getTestClosureProc(): proc() =
    let str1 = SimpleTestRef.new()
    str1.name = "name1 for closure"
    result = proc() =
      echo "SimpleTestRef with name = ", str1.name

  proc testCase11() =
    echo "\n\nTest Case 11"
    var ctr = ClosureTestRef.new()
    ctr.closureField = getTestClosureProc()

  # ---------------------------
  # Use Case 12:
  #   - Use of the "traceDestructor" template
  # ---------------------------
  type
    SimpleObjX = object
      name*: string

  SimpleObjX.traceDestructor()  # Fwd declaration

  SimpleObjX.traceDestructor(tagfield = x.name):
    echo "##### Entered traceDestructor template for SimpleObjX"
    SimpleObjX.destroyFields(x.name)

  proc testCase12() =
    echo "\n\nTest Case 12"
    when defined(traceDestructors):
      echo "---- Compiled WITH '-d:traceDestructors' - there should be ",
        " a message indicating that the traceDestructor code was entered"
    else:
      echo "---- Compiled WITHOUT '-d:traceDestructors' - there should NOT be ",
        " a message indicating that the traceDestructor code was entered"

    let ph1 {.used.} = SimpleObjX(name: "xxxx")

  # ---------------------------

  proc main() =
    testCase1()
    testCase2()
    testCase3()
    testCase4()
    testCase5()
    testCase6()
    testCase7()
    testCase8()
    testCase9()
    testCase10()
    testCase11()
    testCase12()


  main()
  echo "\n...Done\n"
