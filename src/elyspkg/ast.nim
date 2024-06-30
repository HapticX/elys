import
  ./result,
  ./utils,
  std/macros,
  std/strutils,
  std/hashes,
  std/tables,
  std/options


type  
  SignalKind* {.size: sizeof(int8).} = enum
    sNothing,
    sBreak,
    sContinue
  EnvVar* = ref object
    val*: ASTRoot
    isConst*: bool
    topLvl*: bool
  Environment* = ref object
    vars*: TableRef[string, EnvVar]
    lvl*: int
    modules*: seq[string]
    signal*: SignalKind
    returnNew*: bool
  RuntimeError* = object of ValueError


template newEnv*(vars: TableRef[string, EnvVar], lvl: int, modules: seq[string]): Environment =
  Environment(vars: vars, lvl: lvl, modules: modules)
template newEnv*(env: Environment): Environment =
  var vars = newTable[string, EnvVar]()
  for k, v in env.vars.pairs:
    vars[k] = EnvVar(
      val: v.val,
      isConst: v.isConst,
      topLvl: true
    )
  Environment(vars: vars, lvl: env.lvl, modules: env.modules)
template newEnv*(): Environment =
  Environment(vars: newTable[string, EnvVar](), lvl: 0, modules: @[])


macro evalFor(t, body: untyped) =
  newProc(
    postfix(ident("eval" & $t), "*"),
    [
      ident"ASTRoot",
      newIdentDefs(ident"self", t),
      newIdentDefs(ident"env", ident"Environment")
    ],
    body,
    nnkFuncDef
  )

func binOpAstOperator*(operator: string, line, col: int, source, filepath: ptr string): BinOperator =
  case operator
  of "+":
    return BinOperator.Add
  of "-":
    return BinOperator.Minus
  of "*":
    return BinOperator.Multiply
  of "/":
    return BinOperator.Divide
  of "%":
    return BinOperator.Mod
  of "//":
    return BinOperator.Div
  of "and", "&&":
    return BinOperator.And
  of "or", "||":
    return BinOperator.Or
  of "==":
    return BinOperator.Equals
  of "!=":
    return BinOperator.NotEquals
  of ">":
    return BinOperator.More
  of "<":
    return BinOperator.Less
  of ">=":
    return BinOperator.MoreThan
  of "<=":
    return BinOperator.LessThan
  of "..":
    return BinOperator.DotDot
  of "..<":
    return BinOperator.DotDotLess
  of ":":
    return BinOperator.TwoDots
  of "--":
    return BinOperator.MinusMinus
  of "++":
    return BinOperator.PlusPlus
  of "?":
    return BinOperator.QuestionMark
  of "=":
    return BinOperator.Assignment
  of "":
    return BinOperator.None
  else:
    syntaxError(
      "Unknown operator: '" & operator & "'",
      line, col, source, filepath
    )


template nullAst*(): NullAST = NullAST(kind: akNull)
template eofStmt*(): EofStmt = EofStmt(kind: akEof)

template intAst*(v: int): IntAST = IntAST(val: v, kind: akInt)
template floatAst*(v: float): FloatAST = FloatAST(val: v, kind: akFloat)
template stringAst*(v: string): StringAST = StringAST(val: v, kind: akString)
template boolAst*(v: bool): BoolAST = BoolAST(val: v, kind: akBool)

template arrAst*(v: seq[ASTRoot]): ArrayAST =
  ArrayAST(val: v, kind: akArr)

template arrAst*(): ArrayAST =
  ArrayAST(val: newSeq[ASTRoot](), kind: akArr)

template objAst*(v: seq[tuple[key, val: ASTRoot]]): ObjectAST =
  ObjectAST(val: v, kind: akObj)

template bracket*(e, i: ASTRoot, idxs: seq[ASTRoot]): BracketExprAST =
  BracketExprAST(index: i, expr: e, indexes: idxs, kind: akBracketExpr)

template slice*(left, right: ASTRoot, operator: string, line, col: int, source, filepath: ptr string): SliceExprAST =
  SliceExprAST(l: left, r: right, op: binOpAstOperator(operator, line, col, source, filepath), kind: akSliceExpr)

template unaryOpAst*(e: ASTRoot, o: string, line, col: int, source, filepath: ptr string): UnaryOpAST =
  UnaryOpAST(kind: akUnaryOp, expr: e, op: binOpAstOperator(o, line, col, source, filepath))

template incDecStmt*(e: ASTRoot, o: string, line, col: int, source, filepath: ptr string): IncDecStmt =
  IncDecStmt(kind: akIncDec, expr: e, op: binOpAstOperator(o, line, col, source, filepath))

template varAst*(v: string): VarAST = VarAST(name: v, kind: akVar)

template binOpAst*(left, right: ASTRoot, o: string, line, col: int, source, filepath: ptr string): BinOpAST =
  BinOpAST(l: left, r: right, op: binOpAstOperator(o, line, col, source, filepath), kind: akBinOp)

template notAst*(e: ASTRoot): NotOp =
  NotOp(expr: e, kind: akNot)

template statementList*(stmts: seq[ASTRoot]): StmtList =
  StmtList(statements: stmts, kind: akStmtList)

template assignStmtAst*(n: string, e: ASTRoot,
                        isc: bool = false, isa: bool = false,
                        aop: string = "=", line, col: int, source, filepath: ptr string): AssignStmt =
  AssignStmt(
    name: n, expr: e, isConst: isc,
    isAssign: isa, assignOp: binOpAstOperator(aop, line, col, source, filepath),
    kind: akAssign
  )

template assignBracket*(e: BracketExprAST, v: ASTRoot, o: string, line, col: int, source, filepath: ptr string): AssignBracketStmt =
  AssignBracketStmt(expr: e, val: v, kind: akAssignBracket, op: binOpAstOperator(o[1..^1], line, col, source, filepath))

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

template forInGen*(v: seq[ASTRoot], o, b: ASTRoot, c: Option[ASTRoot]): ForInGenerator =
  ForInGenerator(vars: v, obj: o, body: b, condition: c, kind: akForInGen)

template funcStmt*(n: string, a: ArrayAST, kwa: ObjectAST, b: ASTRoot): FuncStmt =
  FuncStmt(name: n, args: a, kwargs: kwa, body: b, kind: akFunc)

template callAst*(n: string, a: ArrayAST, kwa: ObjectAST): CallExprAST =
  CallExprAST(name: n, args: a, kwargs: kwa, kind: akCallExpr)

template swap*(left, right, toLeft, toRight: ASTRoot): SwapStmt =
  SwapStmt(l: left, r: right, toL: toLeft, toR: toRight, kind: akSwap)


func eval*(self: ASTRoot, env: Environment): ASTRoot


template `[]=`*(env: Environment, key: string, value: ASTRoot) =
  env.vars[key].val = value


template setDef*(env: Environment, key: string, value: ASTRoot) =
  if env.vars.hasKey(key):
    env.vars[key].val = value
  else:
    env.vars[key] = EnvVar(val: value)


template `[]`*(env: Environment, key: string): ASTRoot =
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
    of akFunc: "function"
    else: "object"


func astValue*(a: ASTRoot, env: Environment): string =
  case a.kind:
    of akInt: $a.IntAST.val
    of akFloat: $a.FloatAST.val
    of akNull: "null"
    of akBool: $a.BoolAST.val
    of akString: $a.StringAST.val
    of akSliceExpr:
      "Slice(" & a.SliceExprAST.l.astValue(env) & $a.SliceExprAST.op &
      a.SliceExprAST.r.astValue(env) & ")"
    of akArr:
      var
        res = ""
        i = 0
      let arr = a.ArrayAST.val
      while i < arr.len:
        if i == arr.len-1:
          res &= arr[i].astValue(env)
        else:
          res &= arr[i].astValue(env) & ", "
        inc i
      "[" & res & "]"
    of akObj:
      var
        res = ""
        i = 0
      let obj = a.ObjectAST.val
      while i < obj.len:
        let
          key = obj[i].key
          val = obj[i].val
        res = res & (if key.kind == akString: '"' & key.astValue(env) & '"' else: key.astValue(env))
        res &= ": "
        res = res & (if val.kind == akString: '"' & val.astValue(env) & '"' else: val.astValue(env))
        if i != obj.len-1:
          res &= ", "
        inc i
      "{" & res & "}"
    of akFunc: "function " & a.FuncStmt.name
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
        valueError(
          "Can not get boolean from " & $a.astValue(env),
          a.line, a.col, a.code, a.filepath
        )


func toNimBool*(a: ASTRoot, env: Environment = nil): bool =
  case a.kind:
    of akBool:
      return a.BoolAST.val
    of akInt:
      return a.IntAST.val != 0
    of akFloat:
      return a.FloatAST.val != 0.0
    of akString:
      return a.StringAST.val.len > 0
    of akArr:
      return a.ArrayAST.val.len > 0
    of akObj:
      return a.ObjectAST.val.len > 0
    of akNull:
      return false
    else:
      if not env.isNil:
        valueError(
          "Can not get boolean from " & $a.astValue(env),
          a.line, a.col, a.code, a.filepath
        )


converter toBool*(ast: ASTRoot): bool =
  ast.toBoolean().BoolAST.val


func `+`(l, r: ASTRoot, env: Environment): ASTRoot =
  let
    a = l.eval(env)
    b = r.eval(env)
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
    valueError(
      "Can not add " & b.astName & " to " & a.astName,
      l.line, l.col, a.code, l.filepath
    )


func `-`(l, r: ASTRoot, env: Environment): ASTRoot =
  let
    a = l.eval(env)
    b = r.eval(env)
  if a.kind == akInt and b.kind == akInt:
    return intAst(a.IntAST.val - b.IntAST.val)
  elif a.kind == akInt and b.kind == akFloat:
    return floatAst(a.IntAST.val.float - b.FloatAST.val)
  elif a.kind == akFloat and b.kind == akInt:
    return floatAst(a.FloatAST.val - b.IntAST.val.float)
  elif a.kind == akFloat and b.kind == akFloat:
    return floatAst(a.FloatAST.val - b.FloatAST.val)
  else:
    valueError(
      "Can not substract " & b.astName & " from " & a.astName,
      l.line, l.col, l.code, l.filepath
    )


func `*`*(l, r: ASTRoot, env: Environment): ASTRoot =
  let
    a = l.eval(env)
    b = r.eval(env)
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
    var res = a.ArrayAST.val
    for i in 0..<b.IntAST.val:
      res.add a.ArrayAST.val[i]
    return arrAst(res)
  else:
    valueError(
      "Can not multiply " & a.astName & " multiply " & b.astName,
      l.line, l.col, l.code, l.filepath
    )


func `/`*(l, r: ASTRoot, env: Environment): ASTRoot =
  let
    a = l.eval(env)
    b = r.eval(env)
  if a.kind == akInt and b.kind == akInt:
    if b.IntAST.val == 0:
      zeroDivisionError(
        "Can not divide by 0",
        l.line, l.col, l.code, l.filepath
      )
    return floatAST(a.IntAST.val / b.IntAST.val)
  elif a.kind == akInt and b.kind == akFloat:
    if b.FloatAST.val == 0.0:
      zeroDivisionError(
        "Can not divide by 0",
        l.line, l.col, l.code, l.filepath
      )
    return floatAst(a.IntAST.val.float / b.FloatAST.val)
  elif a.kind == akFloat and b.kind == akInt:
    if b.IntAST.val == 0:
      zeroDivisionError(
        "Can not divide by 0",
        l.line, l.col, l.code, l.filepath
      )
    return floatAst(a.FloatAST.val / b.IntAST.val.float)
  elif a.kind == akFloat and b.kind == akFloat:
    if b.FloatAST.val == 0.0:
      zeroDivisionError(
        "Can not divide by 0",
        l.line, l.col, l.code, l.filepath
      )
    return floatAst(a.FloatAST.val / b.FloatAST.val)
  else:
    valueError(
      "Can not divide " & a.astName & " by " & a.astName,
      l.line, l.col, l.code, l.filepath
    )


func `//`*(l, r: ASTRoot, env: Environment): ASTRoot =
  let
    a = l.eval(env)
    b = r.eval(env)
  if a.kind == akInt and b.kind == akInt:
    if b.IntAST.val == 0:
      zeroDivisionError(
        "Can not divide by 0",
        l.line, l.col, l.code, l.filepath
      )
    return intAST(a.IntAST.val div b.IntAST.val)
  elif a.kind == akInt and b.kind == akFloat:
    if b.IntAST.val == 0:
      zeroDivisionError(
        "Can not divide by 0",
        l.line, l.col, l.code, l.filepath
      )
    return intAst(a.FloatAST.val.int div b.IntAST.val)
  else:
    valueError(
      "Can not divide " & a.astName & " by " & a.astName,
      l.line, l.col, l.code, l.filepath
    )


func `%`*(l, r: ASTRoot, env: Environment): ASTRoot =
  let
    a = l.eval(env)
    b = r.eval(env)
  if a.kind == akInt and b.kind == akInt:
    return intAst(a.IntAST.val mod b.IntAST.val)
  else:
    valueError(
      "Can not get module " & b.astName & " from " & a.astName,
      l.line, l.col, l.code, l.filepath
    )


func `==`*(l, r: ASTRoot, env: Environment): ASTRoot =
  let
    a = l.eval(env)
    b = r.eval(env)
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


template `!=`*(a, b: ASTRoot, env: Environment): ASTRoot =
  boolAst(not `==`(a, b, env).BoolAST.val)


func `>`*(l, r: ASTRoot, env: Environment): ASTRoot =
  let
    a = l.eval(env)
    b = r.eval(env)
  if a.kind == akInt and b.kind == akInt:
    return boolAst(a.IntAST.val > b.IntAST.val)
  elif a.kind == akFloat and b.kind == akFloat:
    return boolAst(a.FloatAST.val > b.FloatAST.val)
  elif a.kind == akInt and b.kind == akFloat:
    return boolAst(a.IntAST.val.float > b.FloatAST.val)
  elif a.kind == akFloat and b.kind == akInt:
    return boolAst(a.FloatAST.val > b.IntAST.val.float)
  valueError(
    "Can not get compare " & a.astName & " with " & b.astName,
    l.line, l.col, l.code, l.filepath
  )


template `>=`*(a, b: ASTRoot, env: Environment): ASTRoot =
  boolAst(`==`(a, b, env).BoolAST.val or `>`(a, b, env).BoolAST.val)


template `<`*(a, b: ASTRoot, env: Environment): ASTRoot =
  boolAst(not `>=`(a, b, env).BoolAST.val)


template `<=`*(a, b: ASTRoot, env: Environment): ASTRoot =
  boolAst(`==`(a, b, env).BoolAST.val or `<`(a, b, env).BoolAST.val)


template `and`*(a, b: ASTRoot): ASTRoot =
  boolAst(a.BoolAST.val and b.BoolAST.val)


template `or`*(a, b: ASTRoot): ASTRoot =
  boolAst(a.BoolAST.val or b.BoolAST.val)


template `not`*(a: ASTRoot): ASTRoot =
  boolAst(not a.BoolAST.val)


func `[]`*(l, r: ASTRoot, env: Environment): ASTRoot =
  let
    a = l.eval(env)
    b = r.eval(env)
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
      valueError(
        "Can not get element from " & a.astValue(env) & " at index " & b.astValue(env),
          r.line, r.col, r.code, r.filepath
      )
      # return arrAst(a.ArrayAST.val[^(-left)..right])
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
    for i in a.ObjectAST.val:
      if `==`(i.key, b, env):
        return i.val
    valueError(
      "Can not get element by key " & b.astValue(env) & " in dict " & a.astValue(env),
      r.line, r.col, r.code, r.filepath
    )
  valueError(
    "Can not get element from " & a.astValue(env) & " at index " & b.astValue(env),
      r.line, r.col, r.code, r.filepath
  )


func `[]=`*(a, b: ASTRoot, env: Environment, line, col: int, code, filepath: ptr string, val: ASTRoot) =
  if a.kind == akArr and b.kind == akInt:
    let idx = b.IntAST.val
    if idx < 0:
      a.ArrayAST.val[^(-b.IntAST.val)] = val
    else:
      a.ArrayAST.val[b.IntAST.val] = val
  elif a.kind == akObj:
    var i = 0
    while i < a.ObjectAST.val.len:
      if `==`(a.ObjectAST.val[i].key, b, env):
        a.ObjectAST.val[i].val = val
      inc i
  else:
    valueError(
      "Can not change element of " & a.astName,
      line, col, code, filepath
    )


evalFor NullAST: self
evalFor IntAST: self
evalFor FloatAST: self
evalFor StringAST: self
evalFor BoolAST: self


func evalNotOp*(self: NotOp, env: Environment): ASTRoot =
  not self.expr.eval(env)

func evalSliceExprAST*(self: SliceExprAST, env: Environment): ASTRoot =
  self.l = self.l.eval(env)
  self.r =
    if self.op == BinOperator.DotDot:
      self.r.eval(env)
    else:
      binOpAst(self.r, intAst(1), "-", self.l.line, self.l.col, self.l.code, self.l.filepath).eval(env)
  self.op = BinOperator.DotDot
  if self.l.kind != akInt:
    valueError(
      "Can not to use " & self.l.astValue(env) & " in slice",
      self.l.line, self.l.col, self.l.code, self.l.filepath
    )
  if self.r.kind != akInt:
    valueError(
      "Can not to use " & self.r.astValue(env) & " in slice",
      self.r.line, self.r.col, self.r.code, self.r.filepath
    )
  return self

func evalArrayAST*(self: ArrayAST, env: Environment): ASTRoot =
  for i in 0..<self.val.len:
    self.val[i] = self.val[i].eval(env)
  return self

func evalObjectAST*(self: ObjectAST, env: Environment): ASTRoot =
  var i = 0
  while i < self.val.len:
    self.val[i].key = self.val[i].key.eval(env)
    self.val[i].val = self.val[i].val.eval(env)
    inc i
  return self

func evalFuncStmt*(self: FuncStmt, env: Environment): ASTRoot =
  env.vars[self.name] = EnvVar(val: self)
  nullAst()

func evalCallExprAST*(self: CallExprAST, env: Environment): ASTRoot =
  if not env.vars.hasKey(self.name):
    valueError(
      "function " & self.name & " is not exists",
      self.line, self.col, self.code, self.filepath
    )
  if env.vars[self.name].val.kind != akFunc:
    valueError(
      self.name & " is not a function",
      self.line, self.col, self.code, self.filepath
    )
  var
    environment = newEnv(env)
    function = env.vars[self.name].val.FuncStmt
    args = env.vars[self.name].val.FuncStmt.args.val
    kwargs = env.vars[self.name].val.FuncStmt.kwargs.val
  if args.len != self.args.val.len and self.args.val.len > args.len + kwargs.len:
    valueError(
      $self.args.val.len & " arguments were passed, but " & $args.len & " were expected",
      self.line, self.col, self.code, self.filepath
    )
  var index = 0
  let
    argsLen = args.len
    kwargsLen = kwargs.len
    selfArgsLen = self.args.val.len
  for i in self.args.val:
    if index < argsLen:
      environment.vars[args[index].VarAST.name] = EnvVar(val: i.eval(env))
    else:
      environment.vars[kwargs[-(-(index-selfArgsLen)-kwargsLen)].key.VarAST.name] = EnvVar(val: i.eval(env))
    inc index
  for i in self.kwargs.val:
    var keyIsValid = false
    for j in kwargs:
      if i.key.VarAST.name == j.key.VarAST.name:
        keyIsValid = true
        break
    if not keyIsValid:
      valueError(
        "the function does not have a " & i.key.VarAST.name & " argument",
        self.line, self.col, self.code, self.filepath
      )
    environment.vars[i.key.VarAST.name] = EnvVar(val: i.val.eval(env))
  # set kwargs from function
  for i in kwargs:
    let key = i.key.VarAST.name
    if not environment.vars.hasKey(key):
      environment.vars[key] = EnvVar(val: i.val.eval(env))
    elif environment.vars[key].topLvl:
      environment.vars[key] = EnvVar(val: i.val.eval(env))
  return function.body.eval(environment)

func evalBracketExprAST*(self: BracketExprAST, env: Environment): ASTRoot =
  result = self.expr[self.index, env]
  for i in self.indexes:
    result = result[i.eval(env), env]
  result.line = self.line
  result.col = self.col


func evalVarAST*(self: VarAST, env: Environment): ASTRoot =
  if not env.vars.hasKey(self.name):
    usedBeforeAssign(
      "Variable " & self.name & " was not assigned before",
      self.line, self.col, self.code, self.filepath
    )
  env.vars[self.name].val

func evalUnaryOpAST*(self: UnaryOpAST, env: Environment): ASTRoot =
  let val = self.expr.eval(env)
  if val.kind == akInt:
    val.IntAST.val = -val.IntAST.val
    return val
  elif val.kind == akFloat:
    val.FloatAST.val = -val.FloatAST.val
    return val
  valueError(
    "Can not to apply unary operator '" & $self.op & "' to " & $self.expr,
    self.line, self.col, self.code, self.filepath
  )

func evalIncDecStmt*(self: IncDecStmt, env: Environment): ASTRoot =
  let val = self.expr.eval(env)
  case self.op:
    of BinOperator.PlusPlus:
      if val.kind == akInt:
        inc val.IntAST.val
        return val
      elif val.kind == akFloat:
        val.FloatAST.val = val.FloatAST.val + 1f
        return val
      valueError(
        "Can not increase " & $self.expr,
        self.line, self.col, self.code, self.filepath
      )
    of BinOperator.MinusMinus:
      if val.kind == akInt:
        dec val.IntAST.val
        return val
      elif val.kind == akFloat:
        val.FloatAST.val = val.FloatAST.val - 1f
        return val
      elif val.kind == akString:
        val.StringAST.val = val.StringAST.val[0..^2]
        return val
      valueError(
        "Can not decrease " & $self.expr,
        self.line, self.col, self.code, self.filepath
      )
    else:
      valueError(
        "Unknown inc/dec operator '" & self.op.op() & "' for " & $self.expr,
        self.line, self.col, self.code, self.filepath
      )

func evalStmtList*(self: StmtList, env: Environment): ASTRoot =
  var environment = newEnv(env)
  for s in self.statements:
    result = s.eval(environment)
    env.signal = environment.signal
    if env.signal in {sContinue, sBreak}:
      break
  if env.returnNew:
    env.vars = environment.vars
  return result

func evalBinOpAST*(self: BinOpAST, env: Environment): ASTRoot =
  case self.op:
    of BinOperator.Add:
      return `+`(self.l, self.r, env)
    of BinOperator.Minus:
      return `-`(self.l, self.r, env)
    of BinOperator.Multiply:
      return `*`(self.l, self.r, env)
    of BinOperator.Divide:
      return `/`(self.l, self.r, env)
    of BinOperator.Mod:
      return `%`(self.l, self.r, env)
    of BinOperator.Div:
      return `//`(self.l, self.r, env)
    of BinOperator.And:
      return self.l.eval(env) and self.r.eval(env)
    of BinOperator.Or:
      return self.l.eval(env) or self.r.eval(env)
    of BinOperator.Equals:
      return `==`(self.l, self.r, env)
    of BinOperator.NotEquals:
      return `!=`(self.l, self.r, env)
    of BinOperator.More:
      return `>`(self.l, self.r, env)
    of BinOperator.Less:
      return `<`(self.l, self.r, env)
    of BinOperator.MoreThan:
      return `>=`(self.l, self.r, env)
    of BinOperator.LessThan:
      return `<=`(self.l, self.r, env)
    else:
      syntaxError(
        "Unknown operator: '" & self.op.op() & "'",
        self.l.line, self.l.col, self.l.code, self.l.filepath
      )

func evalAssignStmt*(self: AssignStmt, env: Environment): ASTRoot =
  if self.isAssign and self.assignOp == BinOperator.Assignment:
    # var x = y
    # const x = y
    if env.vars.hasKey(self.name) and not env.vars[self.name].topLvl:
      assignedBefore(
        "Variable " & self.name & " was assigned before",
        self.line, self.col, self.code, self.filepath
      )
    env.vars[self.name] = EnvVar(
      val: self.expr.eval(env),
      isConst: self.isConst,
      topLvl: false
    )
  elif self.isAssign:
    # x += y
    # x //= 2
    if not env.vars.hasKey(self.name):
      usedBeforeAssign(
        "Variable " & self.name & " was not assigned before",
        self.line, self.col, self.code, self.filepath
      )
    if env.vars[self.name].isConst:
      valueError(
        "Const " & self.name & " can not be modified",
        self.line, self.col, self.code, self.filepath
      )
    env.vars[self.name].val = binOpAst(
      env.vars[self.name].val, self.expr, self.assignOp.op(), self.line, self.col, self.code, self.filepath
    ).eval(env)
  elif not self.isAssign:
    # x = y
    if not env.vars.hasKey(self.name):
      usedBeforeAssign(
        "Variable " & self.name & " was not assigned before",
        self.line, self.col, self.code, self.filepath
      )
    if env.vars[self.name].isConst:
      valueError(
        "Const " & self.name & " can not be modified",
        self.line, self.col, self.code, self.filepath
      )
    env.vars[self.name].val = self.expr.eval(env)
  nullAst()


func evalAssgnBracketStmt*(self: AssignBracketStmt, env: Environment): ASTRoot =
  let
    val =
      if self.op == BinOperator.None:
        self.val.eval(env)
      else:
        binOpAst(self.expr.eval(env), self.val.eval(env), self.op.op(), self.line, self.col, self.code, self.filepath).eval(env)
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
    values[i].v[
      values[i+1].i, env,
      self.expr.line, self.expr.col, self.expr.code,
      self.expr.filepath
    ] = values[i+1].v
  nullAst()


func evalIfStmt*(self: IfStmt, env: Environment): ASTRoot =
  var cond = self.condition.eval(env)
  if cond.toNimBool:
    self.body.StmtList.parent = akIfStmt
    return self.body.eval(env)
  if self.elifArray.len > 0:
    for i in self.elifArray:
      cond = i.condition.eval(env)
      if cond.toNimBool:
        i.body.StmtList.parent = akIfStmt
        return i.body.eval(env)
  if self.elseBranch.isSome:
    self.elseBranch.get.body.StmtList.parent = akIfStmt
    return self.elseBranch.get.body.eval(env)
  return nullAst()


func evalWhileStmt*(self: WhileStmt, env: Environment): ASTRoot =
  result = nullAst()
  self.body.StmtList.parent = akWhile
  while self.condition.eval(env).toNimBool:
    result = self.body.eval(env)
    case env.signal:
      of sContinue:
        env.signal = sNothing
        continue
      of sBreak:
        env.signal = sNothing
        return result
      else:
        discard


func evalPrintStmt*(self: PrintStmt, env: Environment): ASTRoot =
  var
    res = ""
    i = 0
  while i < self.data.len:
    if i == self.data.len-1:
      res &= $self.data[i].ast.eval(env).astValue(env)
    else:
      res &= $self.data[i].ast.eval(env).astValue(env) & ", "
    inc i
  {.noSideEffect.}:
    echo res
  nullAst()


func evalBreakStmt*(self: BreakStmt, env: Environment): ASTRoot =
  env.signal = sBreak
  return self

func evalContinueStmt*(self: ContinueStmt, env: Environment): ASTRoot =
  env.signal = sContinue
  return self

func evalSwapStmt*(self: SwapStmt, env: Environment): ASTRoot =
  if self.l.kind != self.r.kind or self.toL.kind != self.toR.kind:
    valueError(
      "Can not swap different types",
      self.l.line, self.l.col, self.l.code, self.l.filepath
    )
  case self.l.kind:
    of akVar:
      let
        l = self.toL.eval(env)
        r = self.toR.eval(env)
      discard assignStmtAst(self.l.VarAST.name, l, false, false, "=", l.line, l.col, l.code, l.filepath).eval(env)
      discard assignStmtAst(self.r.VarAST.name, r, false, false, "=", l.line, l.col, l.code, l.filepath).eval(env)
    of akBracketExpr:
      let
        l = self.toL.eval(env)
        r = self.toR.eval(env)
      discard assignBracket(self.l.BracketExprAST, l, "=", l.line, l.col, l.code, l.filepath).eval(env)
      discard assignBracket(self.r.BracketExprAST, r, "=", l.line, l.col, l.code, l.filepath).eval(env)
    else:
      valueError(
        "Can not swap this",
        self.l.line, self.l.col, self.l.code, self.l.filepath
      )
  nullAst()

func evalForInStmt*(self: ForInStmt, env: Environment): ASTRoot =
  let obj = self.obj.eval(env)
  var
    environment = newEnv(env)
    res = arrAst()
  # check for variables
  for i in self.vars:
    if i.kind != akVar:
      valueError(
        "Can not iterate over " & obj.astValue(environment) &
        " via " & i.astValue(environment),
        self.line, self.col, self.code, self.filepath
      )
  var bodyRes: ASTRoot
  case self.vars.len:
    of 1:
      let variable = self.vars[0].VarAST.name
      case obj.kind:
        of akArr:
          for i in obj.ArrayAST.val:
            environment.setDef(variable, i.eval(environment))
            bodyRes = self.body.eval(environment)
            case env.signal:
              of sContinue:
                env.signal = sNothing
                continue
              of sBreak:
                env.signal = sNothing
                break
              else:
                discard
            if bodyRes:
              res.val.add bodyRes
        of akSliceExpr:
          var integer = intAst(0)
          for i in obj.SliceExprAST.l.IntAST.val..obj.SliceExprAST.r.IntAST.val:
            integer.val = i
            environment.setDef(variable, integer)
            bodyRes = self.body.eval(environment)
            case env.signal:
              of sContinue:
                env.signal = sNothing
                continue
              of sBreak:
                env.signal = sNothing
                break
              else:
                discard
            if bodyRes:
              res.val.add bodyRes
        of akString:
          var str = stringAst("")
          for i in obj.StringAST.val:
            str.val = $i
            environment.setDef(variable, str)
            bodyRes = self.body.eval(environment)
            case env.signal:
              of sContinue:
                env.signal = sNothing
                continue
              of sBreak:
                env.signal = sNothing
                break
              else:
                discard
            if bodyRes:
              res.val.add bodyRes
        else:
          valueError(
            "Can not unpack " & $self.vars.len & " variables from " &
            obj.astValue(env),
            self.line, self.col, self.code, self.filepath
          )
    of 2:
      let
        variable1 = self.vars[0].VarAST.name
        variable2 = self.vars[1].VarAST.name
      case obj.kind:
        of akArr:
          var integer = intAst(0)
          for (index, value) in obj.ArrayAST.val.pairs:
            integer.val = index
            environment.setDef(variable2, value.eval(environment))
            environment.setDef(variable1, integer)
            bodyRes = self.body.eval(environment)
            case env.signal:
              of sContinue:
                env.signal = sNothing
                continue
              of sBreak:
                env.signal = sNothing
                break
              else:
                discard
            if bodyRes:
              res.val.add bodyRes
        of akString:
          var
            str = stringAst("")
            integer = intAst(0)
          for (index, value) in obj.StringAST.val.pairs:
            str.val = $value
            integer.val = index
            environment.setDef(variable2, str)
            environment.setDef(variable1, integer)
            bodyRes = self.body.eval(environment)
            case env.signal:
              of sContinue:
                env.signal = sNothing
                continue
              of sBreak:
                env.signal = sNothing
                break
              else:
                discard
            if bodyRes:
              res.val.add bodyRes
        else:
          valueError(
            "Can not unpack " & $self.vars.len & " variables from " &
            obj.astValue(env),
            self.line, self.col, self.code, self.filepath
          )
    else:
      valueError(
        "Can not unpack " & $self.vars.len & " variables from " &
        obj.astValue(env),
        self.line, self.col, self.code, self.filepath
      )
  return res

func evalForInGenerator*(self: ForInGenerator, env: Environment): ASTRoot =
  let
    obj = self.obj.eval(env)
  var
    environment = newEnv(env)
    res = arrAst()
  # check for variables
  for i in self.vars:
    if i.kind != akVar:
      valueError(
        "Can not iterate over " & obj.astValue(environment) &
        " via " & i.astValue(environment),
        self.line, self.col, self.code, self.filepath
      )
  case self.vars.len:
    of 1:
      let variable = self.vars[0].VarAST.name
      case obj.kind:
        of akArr:
          for i in obj.ArrayAST.val:
            environment.setDef(variable, i.eval(environment))
            let x = self.body.eval(environment)
            if self.condition.isNone:
              res.val.add x
            elif self.condition.get.eval(environment):
              res.val.add x
        of akSliceExpr:
          for i in obj.SliceExprAST.l.IntAST.val..obj.SliceExprAST.r.IntAST.val:
            environment.setDef(variable, intAst(i))
            let x = self.body.eval(environment)
            if self.condition.isNone:
              res.val.add x 
            elif self.condition.get.eval(environment):
              res.val.add x
        of akString:
          for i in obj.StringAST.val:
            environment.setDef(variable, stringAst($i))
            let x = self.body.eval(environment)
            if self.condition.isNone:
              res.val.add x 
            elif self.condition.get.eval(environment):
              res.val.add x
        else:
          valueError(
            "Can not unpack " & $self.vars.len & " variables from " &
            obj.astValue(env),
            self.line, self.col, self.code, self.filepath
          )
    of 2:
      let
        variable1 = self.vars[0].VarAST.name
        variable2 = self.vars[1].VarAST.name
      case obj.kind:
        of akArr:
          for (index, value) in obj.ArrayAST.val.pairs:
            environment.setDef(variable2, value.eval(environment))
            environment.setDef(variable1, intAst(index))
            let x = self.body.eval(environment)
            if self.condition.isNone:
              res.val.add x 
            elif self.condition.get.eval(environment):
              res.val.add x
        of akString:
          for (index, value) in obj.StringAST.val.pairs:
            environment.setDef(variable2, stringAst($value))
            environment.setDef(variable1, intAst(index))
            let x = self.body.eval(environment)
            if self.condition.isNone:
              res.val.add x 
            elif self.condition.get.eval(environment):
              res.val.add x
        else:
          valueError(
            "Can not unpack " & $self.vars.len & " variables from " &
            obj.astValue(env),
            self.line, self.col, self.code, self.filepath
          )
    else:
      valueError(
        "Can not unpack " & $self.vars.len & " variables from " &
        obj.astValue(env),
        self.line, self.col, self.code, self.filepath
      )
  return res


func eval*(self: ASTRoot, env: Environment): ASTRoot =
  case self.kind:
  of akString:
    self
  of akInt:
    self
  of akFloat:
    self
  of akBool:
    self
  of akNull:
    self
  of akNot:
    evalNotOp(self.NotOp, env)
  of akSliceExpr:
    evalSliceExprAST(self.SliceExprAST, env)
  of akArr:
    evalArrayAST(self.ArrayAST, env)
  of akObj:
    evalObjectAST(self.ObjectAST, env)
  of akFunc:
    evalFuncStmt(self.FuncStmt, env)
  of akCallExpr:
    evalCallExprAST(self.CallExprAST, env)
  of akBracketExpr:
    evalBracketExprAST(self.BracketExprAST, env)
  of akVar:
    evalVarAST(self.VarAST, env)
  of akUnaryOp:
    evalUnaryOpAST(self.UnaryOpAST, env)
  of akIncDec:
    evalIncDecStmt(self.IncDecStmt, env)
  of akStmtList:
    evalStmtList(self.StmtList, env)
  of akBinOp:
    evalBinOpAST(self.BinOpAST, env)
  of akAssign:
    evalAssignStmt(self.AssignStmt, env)
  of akAssignBracket:
    evalAssgnBracketStmt(self.AssignBracketStmt, env)
  of akIfStmt:
    evalIfStmt(self.IfStmt, env)
  of akWhile:
    evalWhileStmt(self.WhileStmt, env)
  of akPrint:
    evalPrintStmt(self.PrintStmt, env)
  of akBreak:
    evalBreakStmt(self.BreakStmt, env)
  of akContinue:
    evalContinueStmt(self.ContinueStmt, env)
  of akSwap:
    evalSwapStmt(self.SwapStmt, env)
  of akForInStmt:
    evalForInStmt(self.ForInStmt, env)
  of akForInGen:
    evalForInGenerator(self.ForInGenerator, env)
  else:
    nullAst()
