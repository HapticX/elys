import
  ./result,
  macros,
  strutils,
  sequtils,
  hashes,
  tables,
  options


type  
  SignalKind* {.size: sizeof(int8).} = enum
    sNothing,
    sBreak,
    sContinue
  EnvVar* = ref object
    val*: ASTExpr
    isConst*: bool
    topLvl*: bool
  Environment* = ref object
    vars*: TableRef[string, EnvVar]
    lvl*: int
    modules*: seq[string]
    signal*: SignalKind
  RuntimeError* = object of ValueError


proc newEnv*(vars: TableRef[string, EnvVar], lvl: int, modules: seq[string]): Environment =
  Environment(vars: vars, lvl: lvl, modules: modules)
proc newEnv*(env: Environment): Environment =
  var vars = newTable[string, EnvVar]()
  for k, v in env.vars.pairs:
    vars[k] = EnvVar(
      val: v.val,
      isConst: v.isConst,
      topLvl: true
    )
  Environment(vars: vars, lvl: env.lvl, modules: env.modules)
proc newEnv*(): Environment =
  Environment(vars: newTable[string, EnvVar](), lvl: 0, modules: @[])


macro evalFor(t, body: untyped) =
  newProc(
    postfix(ident"eval", "*"),
    [
      ident"ASTRoot",
      newIdentDefs(ident"self", t),
      newIdentDefs(ident"env", ident"Environment")
    ],
    body,
    nnkMethodDef
  )


template nullAst*(): NullAST = NullAST(kind: akNull)
template eofStmt*(): EofStmt = EofStmt(kind: akEof)

template intAst*(v: int): IntAST = IntAST(val: v, kind: akInt)
template floatAst*(v: float): FloatAST = FloatAST(val: v, kind: akFloat)
template stringAst*(v: string): StringAST = StringAST(val: v, kind: akString)
template boolAst*(v: bool): BoolAST = BoolAST(val: v, kind: akBool)

template arrAst*(v: seq[ASTRoot]): ArrayAST =
  ArrayAST(val: v, kind: akArr)

template objAst*(v: seq[tuple[key, val: ASTRoot]]): ObjectAST =
  ObjectAST(val: v, kind: akObj)

template bracket*(e, i: ASTRoot, idxs: seq[ASTRoot]): BracketExprAST =
  BracketExprAST(index: i, expr: e, indexes: idxs, kind: akBracketExpr)

template slice*(left, right: ASTRoot, operator: string): SliceExprAST =
  SliceExprAST(l: left, r: right, op: operator, kind: akSliceExpr)

template unaryOpAst*(e: ASTRoot, o: string): UnaryOpAST =
  UnaryOpAST(kind: akUnaryOp, expr: e, op: o)

template incDecStmt*(e: ASTRoot, o: string): IncDecStmt =
  IncDecStmt(kind: akIncDec, expr: e, op: o)

template varAst*(v: string): VarAST = VarAST(name: v, kind: akVar)

template binOpAst*(left, right: ASTRoot, o: string): BinOpAST =
  BinOpAST(l: left, r: right, op: o, kind: akBinOp)

template notAst*(e: ASTRoot): NotOp =
  NotOp(expr: e, kind: akNot)

template statementList*(stmts: seq[ASTRoot]): StmtList =
  StmtList(statements: stmts, kind: akStmtList)

template assignStmtAst*(n: string, e: ASTRoot,
                        isc: bool = false, isa: bool = false,
                        aop: string = "="): AssignStmt =
  AssignStmt(
    name: n, expr: e, isConst: isc,
    isAssign: isa, assignOp: aop,
    kind: akAssign
  )

template assignBracket*(e: BracketExprAST, v: ASTRoot, o: string): AssignBracketStmt =
  AssignBracketStmt(expr: e, val: v, kind: akAssignBracket, op: o[1..^1])

template elifBranchStmt*(c: ASTRoot, b: ASTRoot): ElifBranchStmt =
  ElifBranchStmt(condition: c, body: b, kind: akElifBranch)

template elseBranchStmt*(b: ASTRoot): ElseBranchStmt =
  ElseBranchStmt(body: b, kind: akElseBranch)

template printAst*(d: seq[Result]): PrintStmt =
  PrintStmt(data: d, kind: akPrint)

template ifStmt*(c: ASTRoot, b: ASTRoot,
                 earr: seq[ElifBranchStmt], eb: Option[ElseBranchStmt]): IfStmt =
  IfStmt(
    condition: c, body: b,
    elifArray: earr, elseBranch: eb,
    kind: akIfStmt
  )

template whileStmt*(c: ASTRoot, b: ASTRoot): WhileStmt =
  WhileStmt(
    condition: c, body: b, kind: akWhile
  )

template forInStmt*(v: seq[ASTRoot], o, b: ASTRoot): ForInStmt =
  ForInStmt(vars: v, obj: o, body: b, kind: akForInStmt)

template swap*(left, right, toLeft, toRight: ASTRoot): SwapStmt =
  SwapStmt(l: left, r: right, toL: toLeft, toR: toRight, kind: akSwap)


method eval*(self: ASTRoot, env: Environment): ASTRoot {.base.} = nullAst()


func `[]=`*(env: Environment, key: string, value: ASTExpr) =
  env.vars[key].val = value


func setDef*(env: Environment, key: string, value: ASTExpr) =
  if env.vars.hasKey(key):
    env.vars[key].val = value
  else:
    env.vars[key] = EnvVar(val: value)


func `[]`*(env: Environment, key: string): ASTExpr =
  env.vars[key].val


func astName*(a: ASTRoot): string =
  case a.kind:
    of akInt: "int"  # 12090
    of akFloat: "float"  # .1, 1203.2
    of akNull: "null"  # null
    of akBool: "bool" # true/false/on/off
    of akString: "string" # ''
    of akSliceExpr: "slice" # 0..10
    of akArr: "array" # []
    of akObj: "dict" # {}
    else: "object"


func astValue*(a: ASTRoot, env: Environment): string =
  case a.kind:
    of akInt: $a.IntAST.val
    of akFloat: $a.FloatAST.val
    of akNull: "null"
    of akBool: $a.BoolAST.val
    of akString: $a.StringAST.val
    of akSliceExpr:
      "Slice(" & a.SliceExprAST.l.astValue(env) & a.SliceExprAST.op &
      a.SliceExprAST.r.astValue(env) & ")"
    of akArr:
      var
        res = ""
        i = 0
      while i < a.ArrayAST.val.len:
        if i == a.ArrayAST.val.len-1:
          res &= a.ArrayAST.val[i].eval(env).astValue(env)
        else:
          res &= a.ArrayAST.val[i].eval(env).astValue(env) & ", "
        inc i
      "[" & res & "]"
    of akObj:
      var
        res = ""
        i = 0
      let obj = a.ObjectAST.val
      while i < obj.len:
        let
          key = obj[i].key.eval(env)
          val = obj[i].val.eval(env)
        res = res & (if key.kind == akString: '"' & key.astValue(env) & '"' else: key.astValue(env))
        res &= ": "
        res = res & (if val.kind == akString: '"' & val.astValue(env) & '"' else: val.astValue(env))
        if i != obj.len-1:
          res &= ", "
        inc i
      "{" & res & "}"
    else: "object"


func toBoolean*(a: ASTRoot, env: Environment = nil): ASTRoot =
  case a.kind:
    of akBool:
      return a
    of akInt:
      return boolAst(a.IntAST.val != 0)
    of akFloat:
      return boolAst(a.FloatAST.val != 0.0)
    of akString:
      return boolAst(a.StringAST.val.len > 0)
    of akArr:
      return boolAst(a.ArrayAST.val.len > 0)
    of akObj:
      return boolAst(a.ObjectAST.val.len > 0)
    of akNull:
      return boolAst(false)
    else:
      if not env.isNil:
        raise newException(RuntimeError, "Can not get boolean from " & $a.astValue(env))


converter toBool*(ast: ASTRoot): bool =
  ast.toBoolean().BoolAST.val


func `+`(a, b: ASTRoot): ASTRoot =
  if a.kind == akInt and b.kind == akInt:
    return intAst(a.IntAST.val + b.IntAST.val)
  elif a.kind == akInt and b.kind == akFloat:
    return floatAst(a.IntAST.val.float + b.FloatAST.val)
  elif a.kind == akFloat and b.kind == akInt:
    return floatAst(a.FloatAST.val + b.IntAST.val.float)
  elif a.kind == akFloat and b.kind == akFloat:
    return floatAst(a.FloatAST.val + b.FloatAST.val)
  elif a.kind == akString and b.kind == akString:
    return stringAst(a.StringAST.val & b.StringAST.val)
  else:
    raise newException(ValueError, "Cannot plus " & a.astName & " to " & b.astName)


func `-`(a, b: ASTRoot): ASTRoot =
  if a.kind == akInt and b.kind == akInt:
    return intAst(a.IntAST.val - b.IntAST.val)
  elif a.kind == akInt and b.kind == akFloat:
    return floatAst(a.IntAST.val.float - b.FloatAST.val)
  elif a.kind == akFloat and b.kind == akInt:
    return floatAst(a.FloatAST.val - b.IntAST.val.float)
  elif a.kind == akFloat and b.kind == akFloat:
    return floatAst(a.FloatAST.val - b.FloatAST.val)
  else:
    raise newException(ValueError, "Cannot minus " & b.astName & " from " & a.astName)


func `*`*(a, b: ASTRoot): ASTRoot =
  if a.kind == akInt and b.kind == akInt:
    return intAst(a.IntAST.val * b.IntAST.val)
  elif a.kind == akInt and b.kind == akFloat:
    return floatAst(a.IntAST.val.float * b.FloatAST.val)
  elif a.kind == akFloat and b.kind == akInt:
    return floatAst(a.FloatAST.val * b.IntAST.val.float)
  elif a.kind == akFloat and b.kind == akFloat:
    return floatAst(a.FloatAST.val * b.FloatAST.val)
  elif a.kind == akString and b.kind == akInt:
    return stringAst(a.StringAST.val.repeat(b.IntAST.val))
  elif a.kind == akArr and b.kind == akInt:
    var r = a.ArrayAST.val
    for i in 0..<b.IntAST.val:
      r.add a.ArrayAST.val
    return arrAst(r)
  else:
    raise newException(ValueError, "Cannot multiply " & a.astName & " by " & b.astName)


func `/`*(a, b: ASTRoot): ASTRoot =
  if a.kind == akInt and b.kind == akInt:
    return floatAST(a.IntAST.val / b.IntAST.val)
  elif a.kind == akInt and b.kind == akFloat:
    return floatAst(a.IntAST.val.float / b.FloatAST.val)
  elif a.kind == akFloat and b.kind == akInt:
    return floatAst(a.FloatAST.val / b.IntAST.val.float)
  elif a.kind == akFloat and b.kind == akFloat:
    return floatAst(a.FloatAST.val / b.FloatAST.val)
  else:
    raise newException(ValueError, "Cannot divide " & a.astName & " by " & b.astName)


func `//`*(a, b: ASTRoot): ASTRoot =
  if a.kind == akInt and b.kind == akInt:
    return intAST(a.IntAST.val div b.IntAST.val)
  elif a.kind == akInt and b.kind == akFloat:
    return intAst(a.FloatAST.val.int div b.IntAST.val)
  else:
    raise newException(ValueError, "Cannot divide " & a.astName & " by " & b.astName)


func `%`*(a, b: ASTRoot): ASTRoot =
  if a.kind == akInt and b.kind == akInt:
    return intAst(a.IntAST.val mod b.IntAST.val)
  else:
    raise newException(ValueError, "Cannot get mod of " & b.astName & " from " & a.astName)


func `==`*(a, b: ASTRoot): ASTRoot =
  if a.kind != b.kind:
    return boolAst(false)
  case a.kind:
    of akInt: boolAst(a.IntAST.val == b.IntAST.val)
    of akFloat: boolAst(a.FloatAST.val == b.FloatAST.val)
    of akBool: boolAst(a.BoolAST.val == b.BoolAST.val)
    of akString: boolAst(a.StringAST.val == b.StringAST.val)
    of akArr: boolAst(a.ArrayAST.val == b.ArrayAST.val)
    of akObj: boolAst(a.ObjectAST.val == b.ObjectAST.val)
    of akNull: boolAst(true)
    else: boolAst(a[] == b[])


func `!=`*(a, b: ASTRoot): ASTRoot =
  boolAst(not (a == b).BoolAST.val)


func `>`*(a, b: ASTRoot): ASTRoot =
  if a.kind == akInt and b.kind == akInt:
    return boolAst(a.IntAST.val > b.IntAST.val)
  elif a.kind == akFloat and b.kind == akFloat:
    return boolAst(a.FloatAST.val > b.FloatAST.val)
  elif a.kind == akInt and b.kind == akFloat:
    return boolAst(a.IntAST.val.float > b.FloatAST.val)
  elif a.kind == akFloat and b.kind == akInt:
    return boolAst(a.FloatAST.val > b.IntAST.val.float)
  raise newException(
    ValueError, "Can not compare " & $a & " with " & $a
  )


func `>=`*(a, b: ASTRoot): ASTRoot =
  boolAst((a == b).BoolAST.val or (a > b).BoolAST.val)


func `<`*(a, b: ASTRoot): ASTRoot =
  boolAst(not (a >= b).BoolAST.val)


func `<=`*(a, b: ASTRoot): ASTRoot =
  boolAst((a == b).BoolAST.val or (a < b).BoolAST.val)


func `and`*(a, b: ASTRoot): ASTRoot =
  boolAst(a.BoolAST.val and b.BoolAST.val)


func `or`*(a, b: ASTRoot): ASTRoot =
  boolAst(a.BoolAST.val or b.BoolAST.val)


func `[]`*(a, b: ASTRoot, env: Environment): ASTRoot =
  if a.kind == akArr and b.kind == akInt:
    let idx = b.IntAST.val
    if idx < 0:
      return a.ArrayAST.val[^(-b.IntAST.val)]
    else:
      return a.ArrayAST.val[b.IntAST.val]
  elif a.kind == akString and b.kind == akSliceExpr:
    let
      left = b.SliceExprAST.l.IntAST.val
      right = b.SliceExprAST.r.IntAST.val
    if left < 0 and right < 0:
      return stringAst($a.StringAST.val[^(-left)..^(-right)])
    elif left < 0:
      return stringAst($a.StringAST.val[^(-left)..right])
    elif right < 0:
      return stringAst($a.StringAST.val[left..^(-right)])
    else:
      return stringAst($a.StringAST.val[left..right])
  elif a.kind == akArr and b.kind == akSliceExpr:
    let
      left = b.SliceExprAST.l.IntAST.val
      right = b.SliceExprAST.r.IntAST.val
    if left < 0 and right < 0:
      return arrAst(a.ArrayAST.val[^(-left)..^(-right)])
    elif left < 0:
      return arrAst(a.ArrayAST.val[^(-left)..right])
    elif right < 0:
      return arrAst(a.ArrayAST.val[left..^(-right)])
    else:
      return arrAst(a.ArrayAST.val[left..right])
  elif a.kind == akString and b.kind == akInt:
    let idx = b.IntAST.val
    if idx < 0:
      return stringAst($a.StringAST.val[^(-b.IntAST.val)])
    else:
      return stringAst($a.StringAST.val[b.IntAST.val])
  elif a.kind == akObj:
    var res: ASTRoot
    for i in a.ObjectAST.val:
      if i.key == b.eval(env):
        return i.val
    raise newException(
      ValueError,
      "Can not get element by key " & b.astValue(env) & " in dict " & a.astValue(env)
    )
  raise newException(
    ValueError,
    "Can not get element from " & a.astValue(env) & " at index " & b.astValue(env)
  )


func `[]=`*(a, b: ASTRoot, env: Environment, val: ASTRoot) =
  if a.kind == akArr and b.kind == akInt:
    let idx = b.IntAST.val
    if idx < 0:
      a.ArrayAST.val[^(-b.IntAST.val)] = val
    else:
      a.ArrayAST.val[b.IntAST.val] = val
  elif a.kind == akObj:
    var i = 0
    while i < a.ObjectAST.val.len:
      if a.ObjectAST.val[i].key == b:
        a.ObjectAST.val[i].val = val
      inc i
  else:
    raise newException(ValueError, "Can not change element of " & a.astName)


evalFor NullAST: nullAst()
evalFor IntAST: self
evalFor FloatAST: self
evalFor StringAST: self
evalFor BoolAST: self

method eval*(self: SliceExprAST, env: Environment): ASTRoot =
  self.l = self.l.eval(env)
  self.r =
    if self.op == "..":
      self.r.eval(env)
    else:
      binOpAst(self.r, intAst(1), "-").eval(env)
  self.op = ".."
  if self.l.kind notin {akInt}:
    raise newException(ValueError, "Can not to use " & self.l.astValue(env) & " in slice")
  if self.r.kind notin {akInt}:
    raise newException(ValueError, "Can not to use " & self.r.astValue(env) & " in slice")
  return self

method eval*(self: ArrayAST, env: Environment): ASTRoot =
  for i in 0..<self.val.len:
    self.val[i] = self.val[i].eval(env)
  return self

method eval*(self: ObjectAST, env: Environment): ASTRoot =
  var i = 0
  while i < self.val.len:
    self.val[i].key = self.val[i].key.eval(env)
    self.val[i].val = self.val[i].val.eval(env)
    inc i
  return self

method eval*(self: BracketExprAST, env: Environment): ASTRoot =
  let
    expr = self.expr.eval(env)
    index = self.index.eval(env)
  result = expr[index, env]
  for i in self.indexes:
    result = result[i.eval(env), env]

method eval*(self: VarAST, env: Environment): ASTRoot =
  if not env.vars.hasKey(self.name):
    raise newException(RuntimeError, "Variable " & self.name & " was not assigned before")
  env.vars[self.name].val

method eval*(self: UnaryOpAST, env: Environment): ASTRoot =
  let val = self.expr.eval(env)
  if val.kind == akInt:
    val.IntAST.val = -val.IntAST.val
    return val
  elif val.kind == akFloat:
    val.FloatAST.val = -val.FloatAST.val
    return val
  raise newException(RuntimeError, "Can not to apply unary operator '" & self.op & "' to " & $self.expr)

method eval*(self: IncDecStmt, env: Environment): ASTRoot =
  let val = self.expr.eval(env)
  case self.op:
    of "++":
      if val.kind == akInt:
        val.IntAST.val = val.IntAST.val + 1
        return val
      elif val.kind == akFloat:
        val.FloatAST.val = val.FloatAST.val + 1f
        return val
      raise newException(RuntimeError, "Can not increase " & $self.expr)
    of "--":
      if val.kind == akInt:
        val.IntAST.val = val.IntAST.val - 1
        return val
      elif val.kind == akFloat:
        val.FloatAST.val = val.FloatAST.val - 1f
        return val
      elif val.kind == akString:
        val.StringAST.val = val.StringAST.val[0..^2]
        return val
      raise newException(RuntimeError, "Can not decrease " & $self.expr)
    else:
      raise newException(RuntimeError, "Unknown inc/dec operator '" & self.op & "' for " & $self.expr)

method eval*(self: StmtList, env: Environment): ASTRoot =
  var environment = newEnv(env)
  for s in self.statements:
    result = s.eval(environment)
    env.signal = environment.signal
    if env.signal in {sContinue, sBreak}:
      break
  return result

method eval(self: BinOpAST, env: Environment): ASTRoot =
  let
    left = self.l.eval(env)
    right = self.r.eval(env)
  case self.op:
    of "+":
      return `+`(left, right)
    of "-":
      return `-`(left, right)
    of "*":
      return `*`(left, right)
    of "/":
      return `/`(left, right)
    of "%":
      return `%`(left, right)
    of "//":
      return `//`(left, right)
    of "and", "&&":
      return left and right
    of "or", "||":
      return left or right
    of "==":
      return `==`(left, right)
    of "!=":
      return `!=`(left, right)
    of ">":
      return `>`(left, right)
    of "<":
      return `<`(left, right)
    of ">=":
      return `>=`(left, right)
    of "<=":
      return `<=`(left, right)
  raise newException(RuntimeError, "Unknown operator: '" & self.op & "'")

method eval*(self: AssignStmt, env: Environment): ASTRoot =
  if self.isAssign and self.assignOp == "=":
    # var x = y
    # const x = y
    if env.vars.hasKey(self.name) and not env.vars[self.name].topLvl:
      raise newException(RuntimeError, "Variable " & self.name & " was assigned before")
    env.vars[self.name] = EnvVar(
      val: self.expr.eval(env).ASTExpr,
      isConst: self.isConst,
      topLvl: false
    )
  elif self.isAssign:
    # x += y
    # x //= 2
    if not env.vars.hasKey(self.name):
      raise newException(RuntimeError, "Variable " & self.name & " was not assigned before")
    if env.vars[self.name].isConst:
      raise newException(RuntimeError, "Const " & self.name & " can not be modified")
    env.vars[self.name].val = binOpAst(
      env.vars[self.name].val, self.expr, self.assignOp[0..^2]
    ).eval(env).ASTExpr
  elif not self.isAssign:
    # x = y
    if not env.vars.hasKey(self.name):
      raise newException(RuntimeError, "Variable " & self.name & " was not assigned before")
    if env.vars[self.name].isConst:
      raise newException(RuntimeError, "Const " & self.name & " can not be modified")
    env.vars[self.name].val = self.expr.eval(env).ASTExpr
  nullAst()


method eval*(self: AssignBracketStmt, env: Environment): ASTRoot =
  let
    val =
      if self.op == "":
        self.val.eval(env)
      else:
        binOpAst(self.expr.eval(env), self.val.eval(env), self.op)
  var values: seq[tuple[i: ASTRoot, v: ASTRoot]] = @[
    (intAst(-1).ASTRoot, self.expr.expr.eval(env)),
  ]
  let firstIndex = self.expr.index.eval(env)
  values.add (firstIndex, values[^1].v[firstIndex, env])
  for i in self.expr.indexes:
    let index = i.eval(env)
    values.add (index, values[^1].v[index, env])
  values[^1].v = val
  for i in countdown(values.len-2, 0):
    values[i].v[values[i+1].i, env] = values[i+1].v
  nullAst()


method eval*(self: IfStmt, env: Environment): ASTRoot =
  var cond = self.condition.eval(env)
  if cond.toBoolean(env).BoolAST.val:
    self.body.StmtList.parent = akIfStmt
    return self.body.eval(env)
  if self.elifArray.len > 0:
    for i in self.elifArray:
      cond = i.condition.eval(env)
      if cond.toBoolean(env).BoolAST.val:
        i.body.StmtList.parent = akIfStmt
        return i.body.eval(env)
  if self.elseBranch.isSome:
    self.elseBranch.get.body.StmtList.parent = akIfStmt
    return self.elseBranch.get.body.eval(env)
  return nullAst()


method eval*(self: WhileStmt, env: Environment): ASTRoot =
  var res: ASTRoot
  self.body.StmtList.parent = akWhile
  while self.condition.eval(env).BoolAST.val:
    res = self.body.eval(env)
    case env.signal:
      of sContinue:
        env.signal = sNothing
        continue
      of sBreak:
        env.signal = sNothing
        break
      else:
        discard
  return nullAst()


method eval*(self: PrintStmt, env: Environment): ASTRoot =
  var
    res = ""
    i = 0
  while i < self.data.len:
    if i == self.data.len-1:
      res &= $self.data[i].ast.eval(env).astValue(env)
    else:
      res &= $self.data[i].ast.eval(env).astValue(env) & ", "
    inc i
  echo res
  nullAst()


method eval*(self: BreakStmt, env: Environment): ASTRoot =
  env.signal = sBreak
  return self

method eval*(self: ContinueStmt, env: Environment): ASTRoot =
  env.signal = sContinue
  return self

method eval*(self: SwapStmt, env: Environment): ASTRoot =
  if self.l.kind != self.r.kind or self.toL.kind != self.toR.kind:
    raise newException(RuntimeError, "Can not swap different types")
  case self.l.kind:
    of akVar:
      let
        l = self.toL.eval(env)
        r = self.toR.eval(env)
      discard assignStmtAst(self.l.VarAST.name, l, false, false).eval(env)
      discard assignStmtAst(self.r.VarAST.name, r, false, false).eval(env)
    of akBracketExpr:
      let
        l = self.toL.eval(env)
        r = self.toR.eval(env)
      discard assignBracket(self.l.BracketExprAST, l, "=").eval(env)
      discard assignBracket(self.r.BracketExprAST, r, "=").eval(env)
    else:
      raise newException(RuntimeError, "Can not swap this")
  nullAst()

method eval*(self: ForInStmt, env: Environment): ASTRoot =
  let
    obj = self.obj.eval(env)
  var
    environment = newEnv(env)
  # check for variables
  for i in self.vars:
    if i.kind != akVar:
      raise newException(
        RuntimeError,
        "Can not iterate over " & obj.astValue(environment) &
        " via " & i.astValue(environment)
      )
  case self.vars.len:
    of 1:
      let variable = self.vars[0].VarAST.name
      case obj.kind:
        of akArr:
          for i in obj.ArrayAST.val:
            environment.setDef(variable, i.eval(environment).ASTExpr)
            discard self.body.eval(environment)
        of akSliceExpr:
          for i in obj.SliceExprAST.l.IntAST.val..obj.SliceExprAST.r.IntAST.val:
            environment.setDef(variable, intAst(i))
            discard self.body.eval(environment)
        of akString:
          for i in obj.StringAST.val:
            environment.setDef(variable, stringAst($i))
            discard self.body.eval(environment)
        else:
          raise newException(
            RuntimeError,
            "Can not unpack " & $self.vars.len & " variables from " &
            obj.astValue(env)
          )
    of 2:
      let
        variable1 = self.vars[0].VarAST.name
        variable2 = self.vars[1].VarAST.name
      case obj.kind:
        of akArr:
          for (index, value) in obj.ArrayAST.val.pairs:
            environment.setDef(variable2, value.eval(environment).ASTExpr)
            environment.setDef(variable1, intAst(index))
            discard self.body.eval(environment)
        of akString:
          for (index, value) in obj.StringAST.val.pairs:
            environment.setDef(variable2, stringAst($value))
            environment.setDef(variable1, intAst(index))
            discard self.body.eval(environment)
        else:
          raise newException(
            RuntimeError,
            "Can not unpack " & $self.vars.len & " variables from " &
            obj.astValue(env)
          )
    else:
      raise newException(
        RuntimeError,
        "Can not unpack " & $self.vars.len & " variables from " &
        obj.astValue(env)
      )
