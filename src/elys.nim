import
  ./elyspkg/lexer,
  ./elyspkg/parser,
  ./elyspkg/ast,
  ./elyspkg/result,
  options,
  strutils,
  os


when isMainModule:
  import
    terminal,
    std/cmdline,
    parseopt
  const VERSION = "0.4.0"


func compile(code: string, file: string = "$file"): ASTRoot =
  let tokens = code.parseForTokens
  # {.cast(noSideEffect).}:
  #   echo tokens
  var
    source: string = code
    sourcePointer: ptr string = addr source
    filepath: string = file
    filepathPointer: ptr string = addr filepath
  let parsed = tokens.elysParser(sourcePointer, filepathPointer)
  if parsed.isSome:
    var env = newEnv()
    discard parsed.get.ast.eval(env)


func exec*(code: string): ASTRoot {.discardable, exportc.} =
  compile(code)


when isMainModule:
  proc run(file: string, params: seq[string]): int =
    let filepath =
      if file.endsWith(".elys"):
        file
      else:
        file & ".elys"
    let filename = getCurrentDir() / filepath
    var
      f = open(filepath, fmRead)
      source = f.readAll()
    f.close()
    discard compile(source, filename)
    QuitSuccess
  
  proc runInteractive(): int =
    QuitSuccess

  proc main(version = false): int =
    ## Elys language CLI
    if not version:
      styledEcho fgRed, "      _           "
      styledEcho fgRed, "  ___| |_   _ ___ "
      styledEcho fgRed, " / _ \\ | | | / __|"
      styledEcho fgRed, "|  __/ | |_| \\__ \\"
      styledEcho fgRed, " \\___|_|\\__, |___/"
      styledEcho fgRed, "        |___/     "
      echo ""
    styledEcho fgMagenta, "Elys scripting language ", fgYellow, "v", VERSION, fgMagenta, " by HapticX"
    QuitSuccess
  
  proc helpMessage(): int =
    styledEcho fgMagenta, "Elys CLI"
    echo ""
    echo "Usage:"
    echo "  -h | --help    - shows this message"
    echo "  -v | --version - shows version"
    QuitSuccess
  
  let
    params = commandLineParams()
    p = initOptParser(params)
  # echo params
  # echo p

  if "-v" in params or "--version" in params:
    quit(main(true))
  elif "-h" in params or "--help" in params:
    quit(helpMessage())
  elif "-i" in params or "--interactive" in params:
    quit(runInteractive())
  elif params.len > 0 and not params[^1].startsWith("-"):
    quit(run(params[^1], params[0..^2]))
  elif params.len == 0:
    quit(main())
  else:
    styledEcho fgRed, "Unknown command. ", fgYellow, "Try elys --help"
