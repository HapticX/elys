import
  std/options,
  std/strutils


type
  ASTKind* {.size: sizeof(int8).} = enum
    akRoot,
    akExpr, akNull, akInt, akFloat, akBool, akString, akArr, akVar, akBinOp, akUnaryOp,
    akTernary, akBracketExpr, akSliceExpr, akNot, akObj, akCallExpr,
    akEof,
    akStmt, akStmtList, akAssign, akPrint, akIncDec, akElifBranch, akElseBranch,
    akIfStmt, akBreak, akContinue, akWhile, akAssignBracket, akSwap, akForInStmt,
    akForInGen,
    akFunc
  ASTRoot* = ref object of RootObj
    kind*: ASTKind
    line*, col*: int
    code*, filepath*: ptr string
  ResultKind* {.size: sizeof(int8).} = enum
    rkStr,
    rkPair,
    rkArr,
    rkBool,
    rkInt,
    rkFloat,
    rkFun,
    rkAst
  BinOperator* {.size: sizeof(int8).} = enum
    Add,
    Minus,
    Multiply,
    Divide,
    Mod,
    Div,
    And,
    Or,
    Equals,
    NotEquals,
    More,
    Less,
    MoreThan,
    LessThan,
    DotDot,
    DotDotLess,
    QuestionMark,
    TwoDots,
    PlusPlus,
    MinusMinus,
    Assignment,
    None
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
    line*, col*: int
    source*, filepath*: ptr string

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
  CallExprAST* = ref object of ASTExpr
    name*: string
    args*: ArrayAST
    kwargs*: ObjectAST
  BracketExprAST* = ref object of ASTExpr
    index*: ASTRoot
    expr*: ASTRoot
    indexes*: seq[ASTRoot]
  SliceExprAST* = ref object of ASTExpr
    l*, r*: ASTRoot
    op*: BinOperator
  VarAST* = ref object of ASTExpr
    name*: string
  BinOpAST* = ref object of ASTExpr
    op*: BinOperator
    l*, r*: ASTRoot
  UnaryOpAST* = ref object of ASTExpr
    op*: BinOperator
    expr*: ASTRoot
  TernaryOpAST* = ref object of ASTExpr
    first*, second*, third*: ASTRoot
    op1*, op2*: BinOperator
  NotOp* = ref object of ASTExpr
    expr*: ASTRoot
  FuncStmt* = ref object of ASTExpr
    name*: string
    args*: ArrayAST
    kwargs*: ObjectAST
    body*: ASTRoot
    
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
    assignOp*: BinOperator
  AssignBracketStmt* = ref object of Stmt
    expr*: BracketExprAST
    val*: ASTRoot
    op*: BinOperator
  IncDecStmt* = ref object of Stmt
    op*: BinOperator
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
  ForInGenerator* = ref object of Stmt
    vars*: seq[ASTRoot]
    obj*: ASTRoot
    body*: ASTRoot
    condition*: Option[ASTRoot]
  PrintStmt* = ref object of Stmt
    data*: seq[Result]


func astRes*(ast: ASTRoot, line, col: int, src, filepath: ptr string): Option[Result] =
  ast.line = line
  ast.col = col
  ast.code = src
  ast.filepath = filepath
  Result(kind: rkAst, ast: ast, source: src, filepath: filepath, line: line, col: col).some

func fnRes*(function: ResultFunc, line, col: int, src, filepath: ptr string): Option[Result] =
  Result(
    kind: rkFun,
    valfn: function,
    line: line, col: col,
    source: src, filepath: filepath
  ).some

func `$`*(res: Result): string


func `$`*(ast: ASTRoot): string =
  case ast.kind:
    of akNull: "NullAST()"
    of akInt: "IntAST(" & $ast.IntAST.val & ")"
    of akFloat: "FloatAST(" & $ast.FloatAST.val & ")"
    of akString: "StringAST(" & $ast.StringAST.val & ")"
    of akBool: "BoolAST(" & $ast.BoolAST.val & ")"
    of akBinOp: "BinOpAST(" & $ast.BinOpAST.l & ", " & $ast.BinOpAST.r & ", " & $ast.BinOpAST.op & ")"
    of akVar: "VarAST(" & ast.VarAST.name & ")"
    of akNot: "NotOp(" & $ast.NotOp.expr & ")"
    of akEof: "EOFStmt()"
    of akStmtList: "StmtList(" & ast.StmtList.statements.join(", ") & ")"
    of akIncDec:
      case ast.IncDecStmt.op:
        of BinOperator.PlusPlus:
          "Increment(" & $ast.IncDecStmt.expr & ")"
        of BinOperator.MinusMinus:
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
    of akFunc:
      "FuncStmt(" & ast.FuncStmt.name & ")"
    of akCallExpr:
      "CallExprAST(" & ast.CallExprAST.name & ", " & $ast.CallExprAST.args & ")"
    of akSliceExpr:
      "SliceExprAST(" & $ast.SliceExprAST.l & $ast.SliceExprAST.op &
      $ast.SliceExprAST.r & ")"
    of akPrint:
      "PrintStmt(" & $ast.PrintStmt.data & ")"
    of akSwap:
      "SwapStmt(" & $ast.SwapStmt.l & ", " & $ast.SwapStmt.r & " = " &
      $ast.SwapStmt.toL & ", " & $ast.SwapStmt.toR & ")"
    of akForInStmt:
      "ForInStmt( (" & ast.ForInStmt.vars.join(", ") & ") in " & $ast.ForInStmt.obj &
      ", " & $ast.ForInStmt.body & ")"
    of akForInGen:
      "ForInGenerator( (" & ast.ForInGenerator.vars.join(", ") & ") in " & $ast.ForInGenerator.obj &
      ", " & $ast.ForInGenerator.condition & ")"
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


func op*(self: BinOperator): string =
  case self:
  of BinOperator.Add:
    "+"
  of BinOperator.Minus:
    "-"
  of BinOperator.Multiply:
    "*"
  of BinOperator.Divide:
    "/"
  of BinOperator.Mod:
    "%"
  of BinOperator.Div:
    "//"
  of BinOperator.And:
    "&&"
  of BinOperator.Or:
    "||"
  of BinOperator.Equals:
    "=="
  of BinOperator.NotEquals:
    "!="
  of BinOperator.More:
    ">"
  of BinOperator.Less:
    "<"
  of BinOperator.MoreThan:
    ">="
  of BinOperator.LessThan:
    "<="
  of BinOperator.DotDot:
    ".."
  of BinOperator.DotDotLess:
    "..<"
  of BinOperator.QuestionMark:
    "?"
  of BinOperator.TwoDots:
    ":"
  of BinOperator.PlusPlus:
    "++"
  of BinOperator.MinusMinus:
    "--"
  of BinOperator.Assignment:
    "="
  of BinOperator.None:
    ""
