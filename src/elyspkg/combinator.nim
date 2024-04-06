import
  ./lexer,
  ./ast,
  strutils,
  options


type
  ResultKind* {.size: sizeof(int8).} = enum
    rkStr,
    rkPair,
    rkArr,
    rkBool,
    rkInt,
    rkFloat,
    rkFun,
    rkAst
  ResultFunc* = proc(res: Result): Option[Result]
  Result* = ref object
    case kind*: ResultKind
      of rkStr:
        val*: Option[string]
      of rkPair:
        valx*: Result
        valy*: Result
      of rkArr:
        arr*: seq[Result]
      of rkBool:
        valb*: bool
      of rkInt:
        vali*: int
      of rkFloat:
        valf*: float
      of rkFun:
        valfn*: ResultFunc
      of rkAst:
        ast*: ASTRoot
    pos*: int
  ProcessFunc* = proc(val: Result): Option[Result]
  LazyFunc* = proc(): Combinator
  Combinator* = ref object of RootObj
  Reserved* = ref object of Combinator
    tkn*: Token
  Tag* = ref object of Combinator
    kind*: TokenKind
  Concat* = ref object of Combinator
    left*: Combinator
    right*: Combinator
  Alt* = ref object of Combinator
    left*: Combinator
    right*: Combinator
  Opt* = ref object of Combinator
    c*: Combinator
  Rep* = ref object of Combinator
    c*: Combinator
  Process* = ref object of Combinator
    c*: Combinator
    f*: ProcessFunc
  Exp* = ref object of Combinator
    c*: Combinator
    sep*: Option[Combinator]
  Lazy* = ref object of Combinator
    c*: Option[Combinator]
    c_func*: LazyFunc
  Phrase* = ref object of Combinator
    c*: Combinator

  # AST help
  PrintStmt* = ref object of Stmt
    data: seq[Result]


proc reserved*(val: string, kind: TokenKind): Reserved = Reserved(tkn: Token(value: val, kind: kind))
proc tag*(kind: TokenKind): Tag = Tag(kind: kind)
proc concat*(l, r: Combinator): Concat = Concat(left: l, right: r)
proc alt*(l, r: Combinator): Alt = Alt(left: l, right: r)
proc opt*(c: Combinator): Opt = Opt(c: c)
proc rep*(c: Combinator): Rep = Rep(c: c)
proc process*(c: Combinator, f: ProcessFunc): Process = Process(c: c, f: f)
proc exp*(c: Combinator, sep: Option[Combinator]): Exp = Exp(c: c, sep: sep)
proc lazy*(c_func: LazyFunc): Lazy = Lazy(c: none[Combinator](), c_func: c_func)
proc phrase*(c: Combinator): Phrase = Phrase(c: c)


method `$`*(c: Combinator): string {.base.} = "Combinator()"
method `$`*(c: Reserved): string = "Reserved(" & $c.tkn.value & ", " & $c.tkn.kind & ")"
method `$`*(c: Tag): string = "Tag(" & $c.kind & ")"
method `$`*(c: Concat): string = "Concat(" & $c.left & ", " & $c.right & ")"
method `$`*(c: Alt): string = "Alt(" & $c.left & ", " & $c.right & ")"
method `$`*(c: Opt): string = "Opt(" & $c.c & ")"
method `$`*(c: Rep): string = "Rep(" & $c.c & ")"
method `$`*(c: Process): string = "Process(" & $c.c & ", func)"
method `$`*(c: Exp): string = "Exp(" & $c.c & ", " & $c.sep & ")"
method `$`*(c: Lazy): string = "Lazy(" & $c.c & ", func)"
method `$`*(c: Phrase): string = "Phrase(" & $c.c & ")"


proc astRes*(ast: ASTRoot): Option[Result] =
  Result(kind: rkAst, ast: ast).some

proc fnRes*(function: ResultFunc): Option[Result] =
  Result(
    kind: rkFun,
    valfn: function
  ).some

proc getVal*(res: Result): string =
  case res.kind:
    of rkStr:
      return $res.val
    of rkPair:
      return "(" & res.valx.getVal & ", " & res.valy.getVal & ")"
    of rkArr:
      var arr: seq[string] = @[]
      for i in res.arr:
        arr.add i.getVal
      return "[" & arr.join(", ") & "]"
    of rkBool:
      return $res.valb
    of rkInt:
      return $res.vali
    of rkFloat:
      return $res.valf
    of rkFun:
      return "function"
    of rkAst:
      return $res.ast

proc `$`*(res: Result): string =
  "Result(" & res.getVal & ")"


# --== AST ==-- #
proc printAst*(data: seq[Result]): PrintStmt =
  PrintStmt(data: data, kind: akPrint)
method `$`*(ast: PrintStmt): string = "PrintStmt(" & $ast.data & ")"
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


{.experimental: "callOperator".}

template `()`(obj: Combinator, tokens: seq[Token], pos: int): untyped =
  obj.call(tokens, pos)


proc `+`*(left, right: Combinator): Concat  {.inline.} =
  Concat(left: left, right: right)
proc `*`*(c: Combinator, sep: Option[Combinator]): Exp  {.inline.} =
  Exp(c: c, sep: sep)
proc `*`*(c: Combinator, sep: Combinator): Exp  {.inline.} =
  Exp(c: c, sep: sep.some)
proc `|`*(left, right: Combinator): Alt  {.inline.} =
  Alt(left: left, right: right)
proc `^`*(c: Combinator, f: ProcessFunc): Process  {.inline.} =
  Process(c: c, f: f)


method call*(c: Combinator, tokens: seq[Token], pos: int): Option[Result] {.base.} =
  return none[Result]()


method call*(c: Reserved, tokens: seq[Token], pos: int): Option[Result] =
  if pos < tokens.len and tokens[pos].value == c.tkn.value and tokens[pos].kind == c.tkn.kind:
    return Result(kind: rkStr, val: tokens[pos].value.some, pos: pos+1).some
  return none[Result]()

method call*(c: Tag, tokens: seq[Token], pos: int): Option[Result] =
  if pos < tokens.len and tokens[pos].kind == c.kind:
    return Result(kind: rkStr, val: tokens[pos].value.some, pos: pos+1).some
  return none[Result]()

method call*(c: Concat, tokens: seq[Token], pos: int): Option[Result] =
  let left = c.left(tokens, pos)
  if left.isSome:
    let right = c.right(tokens, left.get.pos)
    if right.isSome:
      return Result(kind: rkPair, valx: left.get, valy: right.get, pos: right.get.pos).some
  return none[Result]()

method call*(c: Alt, tokens: seq[Token], pos: int): Option[Result] =
  let left = c.left(tokens, pos)
  if left.isSome:
    return left
  return c.right(tokens, pos)

method call*(c: Opt, tokens: seq[Token], pos: int): Option[Result] =
  let res = c.c(tokens, pos)
  if res.isSome:
    return res
  return Result(kind: rkStr, val: none[string](), pos: pos).some

method call*(c: Rep, tokens: seq[Token], pos: int): Option[Result] =
  var
    res = c.c(tokens, pos)
    arr: seq[Result] = @[]
    i = pos
  while res.isSome:
    arr.add res.get
    i = res.get.pos
    res = c.c(tokens, i)
  Result(kind: rkArr, arr: arr, pos: i).some

method call*(c: Process, tokens: seq[Token], pos: int): Option[Result] =
  var res = c.c(tokens, pos)
  if res.isSome:
    let position = res.get.pos
    res = c.f(res.get)
    res.get.pos = position
    return res
  return none[Result]()

method call*(c: Exp, tokens: seq[Token], pos: int): Option[Result] =
  var res = c.c(tokens, pos)

  proc process_next_exp(r: Result): Option[Result] =
    r.valx.valfn(Result(kind: rkPair, valx: res.get, valy: r.valy))
  
  var
    next_c =
      if c.sep.isSome:
        (c.sep.get + c.c) ^ process_next_exp
      else:
        c.c ^ process_next_exp
    next_res = res
  while next_res.isSome:
    next_res = next_c(tokens, res.get.pos)
    if next_res.isSome:
      res = next_res
  return res

method call*(c: Lazy, tokens: seq[Token], pos: int): Option[Result] =
  if c.c.isNone:
    c.c = c.c_func().some
  return c.c.get()(tokens, pos)

method call*(c: Phrase, tokens: seq[Token], pos: int): Option[Result] =
  let res = c.c(tokens, pos)
  if res.isSome and res.get.pos == tokens.len:
    return res
  elif res.isSome:
    if res.get.kind == rkAst:
      raise newException(
        RuntimeError,
        "error at " & $res.get.pos & " token - " & res.get.getVal & " (" & $res.get.ast.kind & ")"
      )
    else:
      raise newException(
        RuntimeError,
        "error at " & $res.get.pos & " token. " & res.get.getVal
      )
  else:
    raise newException(RuntimeError, "Runtime error")
