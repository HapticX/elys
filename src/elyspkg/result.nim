import
  options,
  tables,
  strutils


type
  ASTKind* {.size: sizeof(int8).} = enum
    akRoot,
    akExpr, akNull, akInt, akFloat, akBool, akString, akArr, akVar, akBinOp, akUnaryOp,
    akTernary, akBracketExpr, akSliceExpr, akNot, akObj,
    akEof,
    akStmt, akStmtList, akAssign, akPrint, akIncDec, akElifBranch, akElseBranch,
    akIfStmt, akBreak, akContinue, akWhile, akAssignBracket, akSwap, akForInStmt
  ASTRoot* = ref object of RootObj
    kind*: ASTKind
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
  ObjectAST* = ref object of ASTExpr
    val*: seq[tuple[key, val: ASTRoot]]
  BracketExprAST* = ref object of ASTExpr
    index*: ASTRoot
    expr*: ASTRoot
    indexes*: seq[ASTRoot]
  SliceExprAST* = ref object of ASTExpr
    l*, r*: ASTRoot
    op*: string
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
  NotOp* = ref object of ASTExpr
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
  AssignBracketStmt* = ref object of Stmt
    expr*: BracketExprAST
    val*: ASTRoot
    op*: string
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
  SwapStmt* = ref object of Stmt
    l*, r*: ASTRoot
    toL*, toR*: ASTRoot
  ContinueStmt* = ref object of Stmt
  BreakStmt* = ref object of Stmt
  ForInStmt* = ref object of Stmt
    vars*: seq[ASTRoot]
    obj*: ASTRoot
    body*: ASTRoot
  PrintStmt* = ref object of Stmt
    data*: seq[Result]


func astRes*(ast: ASTRoot): Option[Result] =
  Result(kind: rkAst, ast: ast).some

func fnRes*(function: ResultFunc): Option[Result] =
  Result(
    kind: rkFun,
    valfn: function
  ).some

func `$`*(res: Result): string


func `$`*(ast: ASTRoot): string =
  case ast.kind:
    of akNull: "NullAST()"
    of akInt: "IntAST(" & $ast.IntAST.val & ")"
    of akFloat: "FloatAST(" & $ast.FloatAST.val & ")"
    of akString: "StringAST(" & $ast.StringAST.val & ")"
    of akBool: "BoolAST(" & $ast.BoolAST.val & ")"
    of akBinOp: "BinOpAST(" & $ast.BinOpAST.l & ", " & $ast.BinOpAST.r & ", " & ast.BinOpAST.op & ")"
    of akVar: "VarAST(" & ast.VarAST.name & ")"
    of akNot: "NotOp(" & $ast.NotOp.expr & ")"
    of akEof: "EOFStmt()"
    of akStmtList: "StmtList(" & ast.StmtList.statements.join(", ") & ")"
    of akIncDec:
      case ast.IncDecStmt.op:
        of "++":
          "Increment(" & $ast.IncDecStmt.expr & ")"
        of "--":
          "Decrement(" & $ast.IncDecStmt.expr & ")"
        else:
          ""
    of akElseBranch: "ElseBranchStmt(" & $ast.ElseBranchStmt.body & ")"
    of akElifBranch: "ElifBranchStmt(" & $ast.ElifBranchStmt.condition & ", " & $ast.ElifBranchStmt.body & ")"
    of akIfStmt:
      "IfStmt(" & $ast.IfStmt.condition & ", " & $ast.IfStmt.body &
      ", [" & ast.IfStmt.elifArray.join(", ") & "], " &
      $ast.IfStmt.elseBranch & ")"
    of akWhile: "WhileStmt(" & $ast.WhileStmt.condition & ", " & $ast.WhileStmt.body & ")"
    of akBreak: "BreakStmt()"
    of akContinue: "ContinueStmt()"
    of akArr: "ArrayAST(" & ast.ArrayAST.val.join(", ") & ")"
    of akObj:
      "ObjectAST(" & $ast.ObjectAST.val & ")"
    of akBracketExpr:
      "BracketExprAST(" & $ast.BracketExprAST.expr & ", " &
      $ast.BracketExprAST.index & ", " & $ast.BracketExprAST.indexes & ")"
    of akSliceExpr:
      "SliceExprAST(" & $ast.SliceExprAST.l & ast.SliceExprAST.op &
      $ast.SliceExprAST.r & ")"
    of akPrint:
      "PrintStmt(" & $ast.PrintStmt.data & ")"
    of akSwap:
      "SwapStmt(" & $ast.SwapStmt.l & ", " & $ast.SwapStmt.r & " = " &
      $ast.SwapStmt.toL & ", " & $ast.SwapStmt.toR & ")"
    of akForInStmt:
      "ForInStmt( (" & ast.ForInStmt.vars.join(", ") & ") in " & $ast.ForInStmt.obj &
      $ast.ForInStmt.body & ")"
    else:
      "ASTRoot(kind: " & $ast.kind & ")"

func getVal*(res: Result): string =
  case res.kind:
    of rkStr:
      $res.val
    of rkPair:
      "(" & res.valx.getVal & ", " & res.valy.getVal & ")"
    of rkArr:
      var arr: seq[string] = @[]
      for i in res.arr:
        arr.add i.getVal
      "[" & arr.join(", ") & "]"
    of rkBool:
      $res.valb
    of rkInt:
      $res.vali
    of rkFloat:
      $res.valf
    of rkFun:
      "function"
    of rkAst:
      $res.ast

func `$`*(res: Result): string =
  "Result(" & res.getVal & ")"
