import
  ./lexer,
  ./parser,
  ./result,
  ./ast,
  std/options


proc compile*(code: string, file: string = "$file"): ASTRoot =
  let tokens = code.parseForTokens
  var
    source: string = code
    sourcePointer: ptr string = addr source
    filepath: string = file
    filepathPointer: ptr string = addr filepath
  let parsed = tokens.elysParser(sourcePointer, filepathPointer)
  if parsed.isSome:
    var env = newEnv()
    discard parsed.get.ast.eval(env)
