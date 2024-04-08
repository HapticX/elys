import
  ./lexer,
  ./combinator,
  ./ast,
  ./result,
  strutils,
  sequtils,
  options


func keyword*(value: string): Reserved = reserved(value, TokenKind.tkKeyword)
func operator*(value: string): Reserved = reserved(value, TokenKind.tkOp)


func processBool*(res: Result): Option[Result] =
  case res.val.get:
    of "on", "true":
      return Result(kind: rkBool, valb: true).some
    of "off", "false":
      return Result(kind: rkBool, valb: false).some
    else:
      raise newException(ValueError, "unknown boolean value " & res.getVal)


func processInt*(res: Result): Option[Result] =
  Result(kind: rkInt, vali: res.val.get.parseInt).some
func processFloat*(res: Result): Option[Result] =
  Result(kind: rkFloat, valf: res.val.get.parseFloat).some
func processNull*(res: Result): Option[Result] =
  astRes(nullAst())
func processString*(res: Result): Option[Result] =
  astRes(stringAst(res.val.get()[1..^2]))


func idTag(): Combinator =
  tag(TokenKind.tkId)

func exprPrecedenceLevels(): seq[seq[string]] =
  @[
    @["*", "/", "//"],
    @["+", "-", "%"],
    @["and", "&&"],
    @["or", "||"],
    @["in"],
    @["==", "!=", ">=", "<=", "<", ">"]
  ]
func incDecOperators(): seq[string] =
  @["--", "++"]
func unaryOperators(): seq[string] =
  @["-"]
func assignOperators(): seq[string] =
  @["//=", "+=", "-=", "*=", "/=", "="]


func processIntNumber(res: Result): Option[Result] = astRes(intAst(res.vali))
func processFloatNumber(res: Result): Option[Result] = astRes(floatAst(res.valf))
func processBoolean(res: Result): Option[Result] = astRes(boolAst(res.valb))
func processVar(res: Result): Option[Result] = astRes(varAst(res.val.get))
func processGroup(res: Result): Option[Result] = res.valx.valy.some


func exprValue(): Combinator =
  (
    ((tag(TokenKind.tkInt) ^ processInt) ^ processIntNumber) |
    ((tag(TokenKind.tkFloat) ^ processFloat) ^ processFloatNumber) |
    (idTag() ^ processVar) |
    ((tag(TokenKind.tkBool) ^ processBool) ^ processBoolean) |
    (tag(TokenKind.tkString) ^ processString) |
    (keyword("null") ^ processNull)
  )


func expr(): Combinator
func exprTerm(): Combinator
func exprTermPre(): Combinator
func incDecStatement(): Combinator
func unaryOperatorStmt(): Combinator
func stmtList(): Combinator
func ifStatement(): Combinator
func stmtListEmbed(): Combinator


func exprGroup(): Combinator =
  (operator"(" + lazy(expr) + operator")") ^ processGroup


func processBExprNot(res: Result): Option[Result] =
  astRes(notAst(res.valy.ast))
func bExprNot(): Combinator =
  ((operator"!" | operator"not") + lazy(expr)) ^ processBExprNot


func processExprArray(res: Result): Option[Result] =
  var arr: seq[ASTRoot] = @[]
  for i in res.arr:
    arr.add i.valx.ast
  astRes(arrAst(arr))
func exprArray(): Combinator =
  (
    (
      operator"[" + rep(
        lazy(expr) + opt(operator",")
      ) + operator"]"
    ) ^ processGroup
  ) ^ processExprArray


func processBracketExpr(res: Result): Option[Result] =
  var arr: seq[ASTRoot] = @[]
  for i in res.valy.arr:
    arr.add i.ast
  astRes(bracket(res.valx.valx.ast, res.valx.valy.ast, arr))
func bracketExpr(): Combinator =
  (
    lazy(exprTermPre) + ((operator"[" + lazy(expr) + operator"]") ^ processGroup) +
    rep(((operator"[" + lazy(expr) + operator"]") ^ processGroup))
  ) ^ processBracketExpr


func processSliceExpr(res: Result): Option[Result] =
  if res.valx.kind == rkStr:
    astRes(slice(intAst(0), res.valy.ast, res.valx.val.get))
  elif res.valy.kind == rkStr:
    astRes(slice(res.valx.ast, intAst(-1), res.valy.val.get))
  else:
    astRes(slice(res.valx.valx.ast, res.valy.ast, res.valx.valy.val.get))
func sliceExpr(): Combinator =
  (
    alt(
      (operator"..") + lazy(exprTermPre),
      alt(
        lazy(exprTermPre) + (operator".." | operator"..<") + lazy(exprTermPre),
        lazy(exprTermPre) + (operator".."),
      )
    )
  ) ^ processSliceExpr


func exprTermPre(): Combinator =
  (
    exprGroup() |
    lazy(ifStatement) |
    lazy(stmtListEmbed) |
    lazy(exprArray) |
    incDecStatement() |
    unaryOperatorStmt() |
    bExprNot() |
    exprValue()
  )


func exprTerm(): Combinator =
  (
    lazy(bracketExpr) |
    lazy(sliceExpr) |
    exprTermPre()
  )

func processBinOp(res: Result): Option[Result] =
  Result(
    kind: rkFun,
    valfn: proc(r: Result): Option[Result] =
      astRes(binOpAst(r.valx.ast, r.valy.ast, res.val.get))
  ).some


func anyOpInList(operators: seq[string]): Combinator =
  var opParsers: seq[Combinator] = @[]
  for op in operators:
    opParsers.add(operator(op))
  foldl(opParsers, a | b)


func precedence(c: Combinator, levels: seq[seq[string]], combine: ProcessFunc): Combinator =
  result = c * (anyOpInList(levels[0]) ^ combine)
  for lvl in levels[1..^1]:
    result = result * (anyOpInList(lvl) ^ combine)


func expr(): Combinator =
  precedence(exprTerm(), exprPrecedenceLevels(), processBinOp)


# ---=== Statements ===--- #

func processAssignStmt(res: Result): Option[Result] =
  astRes(assignStmtAst(res.valx.valx.valy.val.get, res.valy.ast, false, true))
func assignStmt(): Combinator =
  # var x = 12300
  (keyword"var" + idTag() + operator"=" + expr()) ^ processAssignStmt

func processAssignConstStmt(res: Result): Option[Result] =
  astRes(assignStmtAst(res.valx.valx.valy.val.get, res.valy.ast, true, true))
func assignConstStmt(): Combinator =
  # const x = 100
  (keyword"const" + idTag() + operator"=" + expr()) ^ processAssignConstStmt

func processReAssignStmt(res: Result): Option[Result] =
  astRes(assignStmtAst(res.valx.valx.val.get, res.valy.ast, false, false, res.valx.valy.val.get))
func reAssignStmt(): Combinator =
  # x = y
  (idTag() + anyOpInList(assignOperators()) + expr()) ^ processReAssignStmt

func processAssignBracketStmt(res: Result): Option[Result] =
  astRes(assignBracket(res.valx.valx.ast.BracketExprAST, res.valy.ast, res.valx.valy.val.get))
func assignBracketStmt(): Combinator =
  # x[z] = y
  (bracketExpr() + anyOpInList(assignOperators()) + expr()) ^ processAssignBracketStmt

func every(res: Result): Option[Result] =
  res.arr[0].valx.some

func everyExpr(res: Result): Option[Result] =
  res.valx.some


func processPrint(res: Result): Option[Result] =
  var arr: seq[Result] = @[]
  for i in res.valy.arr:
    arr.add i
  astRes(printAst(arr))
func printStmt(): Combinator =
  # print(x)
  # print x
  # print x, y, z
  (
    keyword"print" + alt(
      (operator"(" + opt(
          repSep(expr(), operator",")) + operator")") ^ processGroup,
      opt(repSep(expr(), operator",")),
    )
  ) ^ processPrint


func processIfStmt(res: Result): Option[Result] =
  let
    conditionIf = res.valx.valx.valx.valy
    bodyIf = res.valx.valx.valy
    elseBranch = res.valy
  var elifBranches: seq[ElifBranchStmt] = @[]
  for i in res.valx.valy.arr:
    elifBranches.add(elifBranchStmt(i.valx.valy.ast, i.valy.ast))
  astRes(ifStmt(
    conditionIf.ast,
    bodyIf.ast,
    elifBranches,
    if elseBranch.kind == rkStr:
      none[ElseBranchStmt]()
    else:
      elseBranchStmt(elseBranch.valy.ast).some
  ))
func ifStatement(): Combinator =
  # if cond {
  #   body
  # } elif cond {
  #   body
  # } else {
  #   body
  # }
  let
    condition = alt(
      (operator"(" + expr() + operator")") ^ processGroup,
      expr(),
    )
    stmts = (operator"{" + opt(lazy(stmtList)) + operator"}") ^ processGroup
  (
    keyword"if" + condition + stmts + rep(
      keyword"elif" + condition + stmts
    ) + opt(
      keyword"else" + stmts
    )
  ) ^ processIfStmt


func processEof(res: Result): Option[Result] =
  # \0
  astRes(EofStmt(kind: akEof))


func processUnaryOperatorStmt(res: Result): Option[Result] =
  astRes(unaryOpAst(res.valy.ast, res.valx.val.get))
func unaryOperatorStmt(): Combinator =
  # -x
  (anyOpInList(unaryOperators()) + (exprValue() | exprGroup())) ^ processUnaryOperatorStmt


func processIncDec(res: Result): Option[Result] =
  if res.valx.kind == rkStr:
    astRes(incDecStmt(res.valy.ast, res.valx.val.get))
  else:
    astRes(incDecStmt(res.valx.ast, res.valy.val.get))
func incDecStatement(): Combinator =
  # x++
  # --x
  (
    (anyOpInList(incDecOperators()) + (idTag() ^ processVar)) |
    ((idTag() ^ processVar) + anyOpInList(incDecOperators()))
  ) ^ processIncDec


func stmtListEmbed(): Combinator =
  (operator"{" + opt(lazy(stmtList)) + operator"}") ^ processGroup


func processWhileStatement(res: Result): Option[Result] =
  astRes(whileStmt(res.valx.valy.ast, res.valy.ast))
func whileStatement(): Combinator =
  (
    keyword"while" + alt(
      (operator"(" + expr() + operator")") ^ processGroup,
      expr(),
    ) + (operator"{" + opt(lazy(stmtList)) + operator"}") ^ processGroup
  ) ^ processWhileStatement


func processBreakStatement(res: Result): Option[Result] =
  astRes(BreakStmt(kind: akBreak))
func breakStatement(): Combinator =
  keyword"break" ^ processBreakStatement


func processContinueStatement(res: Result): Option[Result] =
  astRes(ContinueStmt(kind: akContinue))
func continueStatement(): Combinator =
  keyword"continue" ^ processContinueStatement


func processSwapStatement(res: Result): Option[Result] =
  astRes(swap(
    res.valx.valx.valx.valx.valx.valx.ast,
    res.valx.valx.valx.valx.valy.ast,
    res.valx.valx.valy.ast,
    res.valy.ast
  ))
func swapStatement(): Combinator =
  let expression = bracketExpr() | (idTag() ^ processVar)
  (
    expression + operator"," + expression + operator"=" + expr() + operator"," + expr()
  ) ^ processSwapStatement


func processForInStatement(res: Result): Option[Result] =
  var arr: seq[ASTRoot] = @[]
  for i in res.valx.valy.valx.valx.arr:
    arr.add i.ast
  astRes(forInStmt(arr, res.valx.valy.valy.ast, res.valy.ast))
func forInStatement(): Combinator =
  let conditionStatement = alt(
    (operator"(" + repSep(idTag() ^ processVar, operator",") + operator")") ^ processGroup,
    repSep(idTag() ^ processVar, operator","),
  ) + operator"in" + expr()
  (
    keyword("for") + alt(
      (operator"(" + conditionStatement + operator")") ^ processGroup,
      conditionStatement,
    ) +
    (operator"{" + opt(lazy(stmtList)) + operator"}") ^ processGroup
  ) ^ processForInStatement


func stmt(): Combinator =
  (
    ifStatement() |
    forInStatement() |
    whileStatement() |
    swapStatement() |
    printStmt() |
    assignBracketStmt() |
    assignStmt() |
    assignConstStmt() |
    reAssignStmt() |
    incDecStatement() |
    breakStatement() |
    continueStatement() |
    expr() |
    (tag(TokenKind.tkEof) ^ processEof)
  )


func processStmtList(res: Result): Option[Result] =
  var asts: seq[ASTRoot] = @[]
  for i in res.arr:
    asts.add(i.valx.ast)
  astRes(statementList(asts))


func stmtList(): Combinator =
  result = (
    rep(lazy(stmt) + opt(operator(";")))
  ) ^ processStmtList


func elysParser*(tokens: seq[Token]): auto =
  phrase(stmtList()).call(tokens, 0)
