import
  ./elyspkg/lexer,
  ./elyspkg/parser,
  ./elyspkg/ast,
  options


when isMainModule:
  import
    terminal


proc exec*(code: string) =
  let tokens = code.parseForTokens
  let parsed = tokens.elysParser
  if parsed.isSome:
    var env = newEnv()
    discard parsed.get.ast.eval(env)


when isMainModule:
  const VERSION = "0.2.0"

  styledEcho fgRed, "      _           "
  styledEcho fgRed, "  ___| |_   _ ___ "
  styledEcho fgRed, " / _ \\ | | | / __|"
  styledEcho fgRed, "|  __/ | |_| \\__ \\"
  styledEcho fgRed, " \\___|_|\\__, |___/"
  styledEcho fgRed, "        |___/     "
  echo ""

  styledEcho fgMagenta, "Elys scripting language ", fgYellow, "v", VERSION, fgMagenta, " by HapticX"
