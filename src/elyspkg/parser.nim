import
  ./lexer,
  ./combinator,
  ./ast,
  strutils,
  sequtils,
  options


proc keyword*(value: string): Reserved = reserved(value, TokenKind.tkKeyword)
proc operator*(value: string): Reserved = reserved(value, TokenKind.tkOp)


proc processBool*(res: Result): Option[Result] =
  case res.val.get:
    of "on", "true":
      return Result(kind: rkBool, valb: true).some
    of "off", "false":
      return Result(kind: rkBool, valb: false).some
    else:
      raise newException(ValueError, "unknown boolean value " & res.getVal)


proc processInt*(res: Result): Option[Result] =
  return Result(kind: rkInt, vali: res.val.get.parseInt).some


proc processFloat*(res: Result): Option[Result] =
  return Result(kind: rkFloat, valf: res.val.get.parseFloat).some


proc processNull*(res: Result): Option[Result] =
  return astRes(nullAst())


proc processString*(res: Result): Option[Result] =
  return astRes(stringAst(res.val.get()[1..^2]))


let
  idTag = tag(TokenKind.tkId)
  intNumber = tag(TokenKind.tkInt) ^ processInt
  floatNumber = tag(TokenKind.tkFloat) ^ processFloat
  boolean = tag(TokenKind.tkBool) ^ processBool
  null = keyword("null") ^ processNull
  str = tag(TokenKind.tkString) ^ processString
  aExprPrecedenceLevels = @[
    @["*", "/", "//"],
    @["+", "-", "%"]
  ]
  relationalOperators = @["==", "!=", ">=", "<=", "<", ">"]
  incDecOperators = @["--", "++"]
  unaryOperators = @["-"]
  assignOperators = @["//=", "+=", "-=", "*=", "/=", "="]
  bExprPrecedenceLevels = @[
      @["and", "&&"],
      @["or", "||"],
      @["in"]
  ]


proc processIntNumber(res: Result): Option[Result] = astRes(intAst(res.vali))
proc processFloatNumber(res: Result): Option[Result] = astRes(floatAst(res.valf))
proc processBoolean(res: Result): Option[Result] = astRes(boolAst(res.valb))
proc processVar(res: Result): Option[Result] = astRes(varAst(res.val.get))
proc processGroup(res: Result): Option[Result] = res.valx.valy.some


proc unaryOperatorStmt(): Combinator


proc aExprValue(): Combinator =
  (
    (intNumber ^ processIntNumber) |
    (floatNumber ^ processFloatNumber) |
    (idTag ^ processVar) |
    (boolean ^ processBoolean) |
    str |
    null
  )


proc aExpr(): Combinator
proc incDecStatement(): Combinator

proc aExprGroup(): Combinator =
  (operator("(") + lazy(aExpr) + operator(")")) ^ processGroup


proc aExprTerm(): Combinator =
  (
    incDecStatement() |
    unaryOperatorStmt() |
    aExprValue() |
    aExprGroup()
  )

proc processBinOp(res: Result): Option[Result] =
  Result(
    kind: rkFun,
    valfn: proc(r: Result): Option[Result] =
      astRes(binOpAst(r.valx.ast, r.valy.ast, res.val.get))
  ).some


proc anyOpInList(operators: seq[string]): Combinator =
  var opParsers: seq[Combinator] = @[]
  for op in operators:
    opParsers.add(operator(op))
  foldl(opParsers, a | b)


proc precedence(c: Combinator, levels: seq[seq[string]], combine: ProcessFunc): Combinator =
  result = c * (anyOpInList(levels[0]) ^ combine)
  for lvl in levels[1..^1]:
    result = result * (anyOpInList(lvl) ^ combine)



proc aExpr(): Combinator =
  precedence(aExprTerm(), aExprPrecedenceLevels, processBinOp)


proc expr(): Combinator =
  aExpr()


# ---=== Statements ===--- #

proc processAssignStmt(res: Result): Option[Result] =
  astRes(assignStmtAst(res.valx.valx.valy.val.get, res.valy.ast, false, true))
proc assignStmt(): Combinator =
  # var x = 12300
  (keyword("var") + idTag + operator("=") + expr()) ^ processAssignStmt

proc processAssignConstStmt(res: Result): Option[Result] =
  astRes(assignStmtAst(res.valx.valx.valy.val.get, res.valy.ast, true, true))
proc assignConstStmt(): Combinator =
  # const x = 100
  (keyword("const") + idTag + operator("=") + expr()) ^ processAssignConstStmt

proc processReAssignStmt(res: Result): Option[Result] =
  astRes(assignStmtAst(res.valx.valx.val.get, res.valy.ast, false, false, res.valx.valy.val.get))
proc reAssignStmt(): Combinator =
  # x = y
  (idTag + anyOpInList(assignOperators) + expr()) ^ processReAssignStmt

proc every(res: Result): Option[Result] =
  res.arr[0].valx.some

proc everyExpr(res: Result): Option[Result] =
  res.valx.some


proc processPrint(res: Result): Option[Result] =
  var arr: seq[Result] = @[]
  for i in res.valy.arr:
    arr.add i
  astRes(printAst(arr))
proc printStmt(): Combinator =
  # print(x)
  # print x
  # print x, y, z
  (
    keyword("print") + alt(
      (operator("(") + opt(
          rep((expr() + opt(operator(","))) ^ everyExpr)) + operator(")")) ^ processGroup,
      opt(rep((expr() + opt(operator(","))) ^ everyExpr)),
    )
  ) ^ processPrint


proc processEof(res: Result): Option[Result] =
  # \0
  astRes(eofStmt())


proc processUnaryOperatorStmt(res: Result): Option[Result] =
  astRes(unaryOpAst(res.valy.ast, res.valx.val.get))
proc unaryOperatorStmt(): Combinator =
  # -x
  (anyOpInList(unaryOperators) + (aExprValue() | aExprGroup())) ^ processUnaryOperatorStmt


proc processIncDec(res: Result): Option[Result] =
  if res.valx.val.get in incDecOperators:
    astRes(incDecStmt(varAst(res.valy.val.get), res.valx.val.get))
  else:
    astRes(incDecStmt(varAst(res.valx.val.get), res.valy.val.get))
proc incDecStatement(): Combinator =
  # x++
  # --x
  (
    (anyOpInList(incDecOperators) + idTag) |
    (idTag + anyOpInList(incDecOperators))
  ) ^ processIncDec


proc stmt(): Combinator =
  (
    printStmt() |
    assignStmt() |
    assignConstStmt() |
    reAssignStmt() |
    incDecStatement() |
    expr() |
    (tag(TokenKind.tkEof) ^ processEof)
  )


proc processStmtList(res: Result): Option[Result] =
  var asts: seq[ASTRoot] = @[]
  for i in res.arr:
    asts.add(i.valx.ast)
  astRes(statementList(asts))


proc stmtList(): Combinator =
  result = (
    rep(lazy(stmt) + opt(operator(";")))
  ) ^ processStmtList


proc elysParser*(tokens: seq[Token]): auto =
  phrase(stmtList()).call(tokens, 0)
