import
  ./modules/lexer,
  ./modules/parser,
  ./modules/ast,
  options


proc exec*(code: string) =
  let
    tokens = code.parseForTokens
    parsed = tokens.elysParser
  if parsed.isSome:
    var env = newEnv()
    discard parsed.get.ast.eval(env)
