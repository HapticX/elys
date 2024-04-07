import
  macros,
  strutils,
  tables,
  options


type
  ASTKind* {.size: sizeof(int8).} = enum
    akRoot,
    akExpr, akNull, akInt, akFloat, akBool, akString, akArr, akVar, akBinOp, akUnaryOp,
    akTernary,
    akBinOpExpr, akRelativeOp, akAnd, akOr, akIn, akNot,
    akEof,
    akStmt, akStmtList, akAssign, akPrint, akIncDec, akElifBranch, akElseBranch,
    akIfStmt, akBreak, akContinue, akWhile
  ASTRoot* = ref object of RootObj
    kind*: ASTKind

  ASTExpr* = ref object of ASTRoot
  NullAST* = ref object of ASTExpr
  IntAST* = ref object of ASTExpr
    val*: int
  FloatAST* = ref object of ASTExpr
    val*: float
  BoolAST* = ref object of ASTExpr
    val*: bool
  StringAST* = ref object of ASTExpr
    val*: string
  ArrayAST* = ref object of ASTExpr
    val*: seq[ASTRoot]
  VarAST* = ref object of ASTExpr
    name*: string
  BinOpAST* = ref object of ASTExpr
    op*: string
    l*, r*: ASTRoot
  UnaryOpAST* = ref object of ASTExpr
    op*: string
    expr*: ASTRoot
  TernaryOpAST* = ref object of ASTExpr
    first*, second*, third*: ASTRoot
    op1*, op2*: string
  
  BinOpExpr* = ref object of ASTRoot
  RelativeOp* = ref object of BinOpExpr
    op*: string
    l*, r*: ASTRoot
  AndOp* = ref object of BinOpExpr
    l*, r*: ASTRoot
  OrOp* = ref object of BinOpExpr
    l*, r*: ASTRoot
  InOp* = ref object of BinOpExpr
    l*, r*: ASTRoot
  NotOp* = ref object of BinOpExpr
    expr*: ASTRoot
    
  Stmt* = ref object of ASTRoot
  EofStmt* = ref object of Stmt
  StmtList* = ref object of Stmt
    statements*: seq[ASTRoot]
    parent*: ASTKind
  AssignStmt* = ref object of Stmt
    name*: string
    expr*: ASTRoot
    isConst*: bool
    isAssign*: bool
    assignOp*: string
  IncDecStmt* = ref object of Stmt
    op*: string
    expr*: ASTRoot
  ElifBranchStmt* = ref object of Stmt
    condition*: ASTRoot
    body*: ASTRoot
  ElseBranchStmt* = ref object of Stmt
    body*: ASTRoot
  IfStmt* = ref object of Stmt
    condition*: ASTRoot
    body*: ASTRoot
    elifArray*: seq[ElifBranchStmt]
    elseBranch*: Option[ElseBranchStmt]
  WhileStmt* = ref object of Stmt
    body*: ASTRoot
    condition*: ASTRoot
  ContinueStmt* = ref object of Stmt
  BreakStmt* = ref object of Stmt
  
  SignalKind* {.size: sizeof(int8).} = enum
    sNothing,
    sBreak,
    sContinue
  EnvVar* = ref object
    val*: ASTExpr
    isConst*: bool
  Environment* = ref object
    vars*: TableRef[string, EnvVar]
    lvl*: int
    modules*: seq[string]
    signal*: SignalKind
  RuntimeError* = object of ValueError


proc newEnv*(vars: TableRef[string, EnvVar], lvl: int, modules: seq[string]): Environment =
  Environment(vars: vars, lvl: lvl, modules: modules)
proc newEnv*(env: Environment): Environment =
  Environment(vars: env.vars, lvl: env.lvl, modules: env.modules)
proc newEnv*(): Environment =
  Environment(vars: newTable[string, EnvVar](), lvl: 0, modules: @[])


method `$`*(ast: ASTRoot): string {.base.} = "ASTRoot()"
method `$`*(ast: ASTExpr): string = "ASTExpr()"
method `$`*(ast: NullAST): string = "NullAST()"
method `$`*(ast: IntAST): string = "IntAST(" & $ast.val & ")"
method `$`*(ast: FloatAST): string = "FloatAST(" & $ast.val & ")"
method `$`*(ast: StringAST): string = "StringAST(" & $ast.val & ")"
method `$`*(ast: BoolAST): string = "BoolAST(" & $ast.val & ")"
method `$`*(ast: BinOpAST): string = "BinOpAST(" & $ast.l & ", " & $ast.r & ", " & ast.op & ")"
method `$`*(ast: VarAST): string = "VarAST(" & ast.name & ")"
method `$`*(ast: NotOp): string = "NotOp(" & $ast.expr & ")"
method `$`*(ast: AndOp): string = "AndOp(" & $ast.l & ", " & $ast.r & ")"
method `$`*(ast: OrOp): string = "OrOp(" & $ast.l & ", " & $ast.r & ")"
method `$`*(ast: InOp): string = "InOp(" & $ast.l & ", " & $ast.r & ")"
method `$`*(ast: RelativeOp): string = "RelativeOp(" & $ast.l & ", " & $ast.r & ", " & ast.op & ")"
method `$`*(ast: EofStmt): string = "EOFStmt()"
method `$`*(ast: StmtList): string = "StmtList(" & ast.statements.join(", ") & ")"
method `$`*(ast: IncDecStmt): string =
  case ast.op:
    of "++":
      return "Increment(" & $ast.expr & ")"
    of "--":
      return "Decrement(" & $ast.expr & ")"
method `$`*(ast: ElseBranchStmt): string = "ElseBranchStmt(" & $ast.body & ")"
method `$`*(ast: ElifBranchStmt): string = "ElifBranchStmt(" & $ast.condition & ", " & $ast.body & ")"
method `$`*(ast: IfStmt): string =
  "IfStmt(" & $ast.condition & ", " & $ast.body &
  ", [" & ast.elifArray.join(", ") & "], " &
  $ast.elseBranch & ")"
method `$`*(ast: WhileStmt): string = "WhileStmt(" & $ast.condition & ", " & $ast.body & ")"
method `$`*(ast: BreakStmt): string = "BreakStmt()"
method `$`*(ast: ContinueStmt): string = "ContinueStmt()"


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


proc nullAst*(): NullAST = NullAST(kind: akNull)
proc eofStmt*(): EofStmt = EofStmt(kind: akEof)

proc intAst*(val: int): IntAST = IntAST(val: val, kind: akInt)
proc floatAst*(val: float): FloatAST = FloatAST(val: val, kind: akFloat)
proc stringAst*(val: string): StringAST = StringAST(val: val, kind: akString)
proc boolAst*(val: bool): BoolAST = BoolAST(val: val, kind: akBool)

proc unaryOpAst*(expr: ASTRoot, op: string): UnaryOpAST =
  UnaryOpAST(kind: akUnaryOp, expr: expr, op: op)

proc incDecStmt*(expr: ASTRoot, op: string): IncDecStmt =
  IncDecStmt(kind: akIncDec, expr: expr, op: op)

proc varAst*(val: string): VarAST = VarAST(name: val, kind: akVar)

proc binOpAst*(l, r: ASTRoot, op: string): BinOpAST =
  BinOpAST(l: l, r: r, op: op, kind: akBinOp)

proc relOp*(l, r: ASTRoot, op: string): RelativeOp =
  RelativeOp(l: l, r: r, op: op, kind: akRelativeOp)
proc notAst*(expr: ASTRoot): NotOp =
  NotOp(expr: expr, kind: akNot)

proc statementList*(stmts: seq[ASTRoot]): StmtList =
  StmtList(statements: stmts, kind: akStmtList)

proc assignStmtAst*(name: string, expr: ASTRoot,
                    isConst: bool = false, isAssign: bool = false,
                    assignOp: string = "="): AssignStmt =
  AssignStmt(
    name: name, expr: expr, isConst: isConst,
    isAssign: isAssign, assignOp: assignOp,
    kind: akAssign
  )

proc elifBranchStmt*(condition: ASTRoot, body: ASTRoot): ElifBranchStmt =
  ElifBranchStmt(condition: condition, body: body, kind: akElifBranch)

proc elseBranchStmt*(body: ASTRoot): ElseBranchStmt =
  ElseBranchStmt(body: body, kind: akElseBranch)

proc ifStmt*(condition: ASTRoot, body: ASTRoot,
             elifArray: seq[ElifBranchStmt], elseBranch: Option[ElseBranchStmt]): IfStmt =
  IfStmt(
    condition: condition, body: body,
    elifArray: elifArray, elseBranch: elseBranch,
    kind: akIfStmt
  )

proc whileStmt*(condition: ASTRoot, body: ASTRoot): WhileStmt =
  WhileStmt(
    condition: condition, body: body, kind: akWhile
  )

proc continueStmt*: ContinueStmt =
  ContinueStmt(kind: akContinue)

proc breakStmt*: BreakStmt =
  BreakStmt(kind: akBreak)


method eval*(self: ASTRoot, env: Environment): ASTRoot {.base.} = nullAst()


proc astName*(a: ASTRoot): string =
  case a.kind:
    of akInt: "int"
    of akFloat: "float"
    of akNull: "null"
    of akBool: "bool"
    of akString: "string"
    of akArr: "array"
    else: "object"


proc astValue*(a: ASTRoot, env: Environment): string =
  case a.kind:
    of akInt: $a.IntAST.val
    of akFloat: $a.FloatAST.val
    of akNull: "null"
    of akBool: $a.BoolAST.val
    of akString: $a.StringAST.val
    of akArr:
      var
        res = ""
        i = 0
      while i < a.ArrayAST.val.len:
        if i == a.ArrayAST.val.len-1:
          res &= $a.ArrayAST.val[i].eval(env).astValue(env)
        else:
          res &= $a.ArrayAST.val[i].eval(env).astValue(env) & ", "
        inc i
      "[" & res & "]"
    else: "object"


proc toBoolean*(a: ASTRoot, env: Environment): ASTRoot =
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
    of akNull:
      return boolAst(false)
    else:
      raise newException(RuntimeError, "Can not get boolean from " & $a.astValue(env))


proc `+`(a, b: ASTRoot): ASTRoot =
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


proc `-`(a, b: ASTRoot): ASTRoot =
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


proc `*`*(a, b: ASTRoot): ASTRoot =
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
  else:
    raise newException(ValueError, "Cannot multiply " & a.astName & " by " & b.astName)


proc `/`*(a, b: ASTRoot): ASTRoot =
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


proc `//`*(a, b: ASTRoot): ASTRoot =
  if a.kind == akInt and b.kind == akInt:
    return intAST(a.IntAST.val div b.IntAST.val)
  elif a.kind == akInt and b.kind == akFloat:
    return intAst(a.FloatAST.val.int div b.IntAST.val)
  else:
    raise newException(ValueError, "Cannot divide " & a.astName & " by " & b.astName)


proc `%`*(a, b: ASTRoot): ASTRoot =
  if a.kind == akInt and b.kind == akInt:
    return intAst(a.IntAST.val mod b.IntAST.val)
  else:
    raise newException(ValueError, "Cannot get mod of " & b.astName & " from " & a.astName)


proc `==`*(a, b: ASTRoot): ASTRoot =
  if a.kind != b.kind:
    return boolAst(false)
  case a.kind:
    of akInt: boolAst(a.IntAST.val == b.IntAST.val)
    of akFloat: boolAst(a.FloatAST.val == b.FloatAST.val)
    of akBool: boolAst(a.BoolAST.val == b.BoolAST.val)
    of akString: boolAst(a.StringAST.val == b.StringAST.val)
    of akNull: boolAst(true)
    else: boolAst(a[] == b[])


proc `!=`*(a, b: ASTRoot): ASTRoot =
  boolAst(not (a == b).BoolAST.val)


proc `>`*(a, b: ASTRoot): ASTRoot =
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


proc `>=`*(a, b: ASTRoot): ASTRoot =
  boolAst((a == b).BoolAST.val or (a > b).BoolAST.val)


proc `<`*(a, b: ASTRoot): ASTRoot =
  boolAst(not (a >= b).BoolAST.val)


proc `<=`*(a, b: ASTRoot): ASTRoot =
  boolAst((a == b).BoolAST.val or (a < b).BoolAST.val)


proc `and`*(a, b: ASTRoot): ASTRoot =
  boolAst(a.BoolAST.val and b.BoolAST.val)


proc `or`*(a, b: ASTRoot): ASTRoot =
  boolAst(a.BoolAST.val or b.BoolAST.val)


evalFor NullAST: nullAst()
evalFor IntAST: intAst(self.val)
evalFor FloatAST: floatAst(self.val)
evalFor StringAST: stringAst(self.val)
evalFor BoolAST: boolAst(self.val)

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
    if env.vars.hasKey(self.name):
      raise newException(RuntimeError, "Variable " & self.name & " was assigned before")
    env.vars[self.name] = EnvVar(val: self.expr.eval(env).ASTExpr, isConst: self.isConst)
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


method eval*(self: BreakStmt, env: Environment): ASTRoot =
  env.signal = sBreak
  return self
method eval*(self: ContinueStmt, env: Environment): ASTRoot =
  env.signal = sContinue
  return self
