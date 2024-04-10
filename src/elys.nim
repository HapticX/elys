import
  ./elyspkg/lexer,
  ./elyspkg/parser,
  ./elyspkg/ast,
  ./elyspkg/result,
  options


when isMainModule:
  import
    terminal


func compile(code: string): ASTRoot =
  let tokens = code.parseForTokens
  # {.cast(noSideEffect).}:
  #   echo tokens
  let parsed = tokens.elysParser
  if parsed.isSome:
    var env = newEnv()
    discard parsed.get.ast.eval(env)


func exec*(code: string): ASTRoot {.discardable, exportc.} =
  compile(code)


when isMainModule:
  const VERSION = "0.4.0"

  styledEcho fgRed, "      _           "
  styledEcho fgRed, "  ___| |_   _ ___ "
  styledEcho fgRed, " / _ \\ | | | / __|"
  styledEcho fgRed, "|  __/ | |_| \\__ \\"
  styledEcho fgRed, " \\___|_|\\__, |___/"
  styledEcho fgRed, "        |___/     "
  echo ""

  styledEcho fgMagenta, "Elys scripting language ", fgYellow, "v", VERSION, fgMagenta, " by HapticX"

